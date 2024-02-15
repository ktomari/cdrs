# Functions that load or help load the DRS data; and functions that help revise or subset DRS data.

#' Helper function to convert bracketed factor values to NA.
#'
#' @param vec_ a factor vector.
#' @param match_ a character vector of matching phrases.
#' @return factor vector.
#' @examples
#' tmp <- factor(letters[1:5])
#' convert_to_NA(tmp, c("a", "d"))
matches_to_NA <- function(
    vec_,
    match_ = NULL
    ){
  # Validate
  stopifnot(
    class(vec_) == "factor" |
      length(vec_) > 1
  )

  if(!is.null(match_)){
    stopifnot(
      length(vec_) > length(match_)
    )
  }

  # Get levels
  lvls_ <- levels(vec_)

  # Create regex pattern
  # If match_ is NULL, capture any <values>
  # otherwise, detect specific ones.
  pattern_ <- ifelse(
    is.null(match_),
    "^<.+>$",
    paste0(
      "^<(?:",
      paste0(match_, collapse = "|"),
      ")>$"
    )
  )

  # Create logical filter based on matching.
  fltr <- stringr::str_detect(
    string = lvls_,
    pattern = pattern_
  )

  # If there are any levels that match,
  # convert them to NA
  if(T %in% fltr){
    vec_[as.character(vec_) %in% lvls_[fltr]] <- NA
  }

  # return
  vec_ %>%
    forcats::fct_drop()
}

#' Revises DRS data such that different forms of &lt;Missing Levels&gt; may be converted to NA.
#'
#' Convert columns with missing values (eg. &lt;I Don't Know&gt;, ie values denoted by &lt;angle brackets&gt;) to `NA`. A dependency of cdrs_read().
#'
#' @param data_ A data.frame or tibble consisting of the data from the DRS.
#' @param dd Either a path to the zip/metadata folder, OR a data.frame or tibble consisting of the data dictionary (ie. sheet 'Variables' of the data dictionary xlsx file).
#' @param preserve_uncertainty logical. This preserves factor levels that indicate some kind of uncertainty for categorical variables (that are not a numeric entry). By default, this is `TRUE`. In effect, this preserves levels like `<I don't know>` as is. However, if this parameter is set to `FALSE`, levels that signify respondent uncertainty are converted to `NA` (ie. R's "Not Available" category). This latter case could be useful if you're primarily concerned with clear affirmatives and negatives. (Note, this parameter does not impact ordinal-likert variables with a "neutral" value.)
#' @param preserve_refused logical. This preserves factor levels that indicate that a user read, then intentionally skipped a question, such as <Decline to answer>. By default this parameter is `FALSE` with the assumption that statistical analysis on the DRS data is employed with the given survey weights. When this parameter is `FALSE`, these levels are converted to `NA` values (ie. R's "Not Available" category).
#' @param preserve_factor_numeric logical. This preserves the non-numeric responses that survey respondents may have had to questions concerning a numeric entry prompt. By default this parameter is `FALSE`, such that non-numeric responses like <I don't know> or <Not applicable> are converted to `NA` (ie. R's "Not Available" category). This facilitates easier estimation of numeric population parameters like the mean.
#' @param preserve_editorials logical. There are number of editorialized variables in the DRS data set. These were either created to align with census/marketing categories, like the gender binary, or had values erased for the sake of respondent non-identifiability. By default this value is `FALSE`, which thus converts these values to `NA` (ie. R's "Not Available" category).
#' @return A data.frame or tibble.
#' @export
cdrs_revise <- function(
    data_,
    dd,
    preserve_uncertainty = T,
    preserve_refused = F,
    preserve_factor_numeric = F,
    preserve_editorials = F
    ) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate inputs. ----
  stopifnot(
    base::is.data.frame(data_) |
      tibble::is_tibble(data_)
  )
  stopifnot(
    base::is.data.frame(dd) |
      tibble::is_tibble(dd) |
      base::is.character(dd)
  )
  stopifnot(
    base::is.logical(preserve_uncertainty) |
      base::is.logical(preserve_refused) |
      base::is.logical(preserve_factor_numeric) |
      base::is.logical(preserve_editorials)
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare Data Dictionary ----
  # First, if its a path, load the dd in.
  if(is.character(dd)){
    # if the character doesn't end in xlsx, its probably
    # a path to a zip or folder, so run cdrs_validate.
    if(stringr::str_detect(string = dd,
                           pattern = "\\.xlsx$",
                           negate = T)){
      fl <- cdrs_validate(dd)
      dd <- fl$path[fl$type == "dd"]
    }

    # load dd
    dd <- readxl::read_xlsx(path = dd,
                            sheet = "Variables")
  }

  # Second, see if the dd is properly formatted.
  # We expected no NA values in dd$Variable
  if(T %in% is.na(dd$Variable)){
    # clean up dd (remove empty lines)
    dd <- dd %>%
      tidyr::drop_na(name) %>%
      tidyr::fill(Variable, .direction = "down")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Revision ----
  out <- purrr::map2_dfc(data_, colnames(data_), function(x, nm){
    # Get DRS class based on data dictionary
    cl <- dd$value[dd$Variable == nm &
                     dd$name == "R Class"]

    # We only care about factors, so skip over the rest.
    if(!(cl %in% c('factor', 'factor - numeric'))){
      # return early
      return(x)
    }

    ## uncertainty levels ----
    if(!preserve_uncertainty &
       cl == "factor"){

      # convert string matches to factor(NA)
      x <- matches_to_NA(
        vec_ = x,
        match_ = c(
          "I don't know",
          "I Dont know why the Delta is important",
          "Unsure"
        )
      )
    }  # end of uncertainty levels

    ## refused levels ----
    if(!preserve_refused &
       cl == "factor"){

      # convert string matches to factor(NA)
      x <- matches_to_NA(
        vec_ = x,
        match_ = c(
          "Decline to answer",
          # Note, "Not applicable" does not apply to Q1a,
          # which is factor - numeric
          "Not applicable"
          )
      )
    }  # end of refused levels

    ## factor numeric levels ----
    if(!preserve_factor_numeric &
       cl == "factor - numeric"){

      # convert string matches to factor(NA)
      # string is "^<.+>$"
      x <- matches_to_NA(
        vec_ = x
      )

      # Convert factor to numeric.
      # Note, intermediate step of as.character required.
      x <- x %>%
        as.character() %>%
        as.numeric()

    }  # end of factor_numeric

    ## editorials levels ----
    if(!preserve_editorials &
       cl == "factor"){

      # convert string matches to factor(NA)
      x <- matches_to_NA(
        vec_ = x,
        match_ = c(
          "Erased",
          "Missing"
        )
      )

    }  # end of factor_numeric

    # return vector x
    x
  })

  # return df/tb
  out
}

#' Validate DRS data against a SHA256 hash.
#'
#' Checks and validates that the data stored in the given directory is valid according to the design of the DRS team. It should have three components, a csv, an excel file, and a txt file. This function is not usually called by the end-user, it is a dependency of cdrs_read.
#'
#' @param path_ is a file path to a directory containing the documents. This should not be a zip file.
#' @return fl, a tibble, including file name, path, and type of data set.
cdrs_validate <- function(
    path_) {

  # Validate File Exists ----
  stopifnot(
    file.exists(path_)
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Is path_ a Zip File? ----
  # If path_ leads to a zip file, then unzip it.
  # First, determine if its a zip.
  is_zip <- grepl(
    pattern = "\\.zip$",
    x = basename(path_),
    perl = TRUE
  )

  # Second, we manipulate the zip file: unzip it into a temporary directory.
  if (is_zip) {
    # get output directory name.
    out_dir <- basename(path_) %>%
      stringr::str_remove("\\.zip$")

    # Define the temporary directory to extract files to
    temp_dir <- tempdir()

    # Full output dir.
    out_dir <- file.path(temp_dir, out_dir)

    # Unzip the file
    utils::unzip(
      zipfile = path_,
      junkpaths = T,
      exdir = out_dir
    )

    # Replace old path.
    path_ <- out_dir
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate Files ----
  # Validate that all the necessary files are present.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load list of files (fl, ie. file list).
  fl <- tibble::tibble(
    file = list.files(path_),
    path = list.files(path_, full.names = TRUE)
  )

  # Please note, that there is an 'internal data' data.frame,
  # `necessary_files`, which is used below. It was defined in
  # raw-data/internal_data.R
  if (!exists("necessary_files")) {
    # This error is to help with development only.
    # It shouldn't be raised once the package is completed.
    stop("Missing 'necessary_files'")
  }

  # Determine if each file in the filelist is a necessary file,
  # and if so, determine type.
  fl$type <- purrr::map_vec(fl$file, function(path_) {
    # Obtain location of name in necessary_files
    i <- purrr::map_vec(necessary_files$regex, function(regex_) {
      stringr::str_detect(path_, regex_)
    }) |>
      which()

    # if file is not a necessary file.
    if (identical(i, integer(0))) {
      return(as.character(NA))
    }

    # Return
    necessary_files$name[i]
  })

  # Validate that all necessary files present.
  if (length(
    purrr::discard(
      fl$type,
      ~ is.na(.x)
    )
  ) != nrow(necessary_files)) {
    # Determine which files are missing.
    missing <- necessary_files$style[
      which(!(necessary_files$name %in% fl$type))
    ] %>%
      paste0(collapse = ", ")

    stop(stringr::str_glue("Error: \"{missing}\" file(s) missing."))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate Data ----
  # Validate that the data matches the hash checksum.
  # Otherwise, the data may be corrupt.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obtain stored hash value.
  hash <- fl %>%
    dplyr::filter(type == "hash") %>%
    dplyr::pull(path) %>%
    readLines(con = .)

  if (nchar(hash) != 64) {
    stop(".hash.text value is of improper length for SHA256.")
  }

  # Obtain path to data
  data <- fl %>%
    dplyr::filter(type == "data") %>%
    dplyr::pull(path)

  # Obtain SHA256 hash for the data.
  data_hash <- digest::digest(
    object = data,
    algo = "sha256",
    file = TRUE
  )

  if (!identical(data_hash, hash)) {
    stop("Data could not be validated. The SHA 256 hash value obtained for the data does not match the value provided by the DRS team.")
  }

  # return
  fl
}

#' Read the California Delta Residents Survey Dataset
#'
#' Loads, validates, and corrects factor levels in the Delta Residents Survey
#' 2023 data. Depends on cdrs_revise, cdrs_validate.
#'
#' @param path_ is a character vector of length 1. Provides the path to the directory containing all the relevant files, including the data dictionary, the data in csv form, and the hash.txt. This path may also lead to a zip file. By default the path is to the current working directory.
#' @param relevel_ either a character or list. This parameter determines if the data should be 1) revised according to the `"default"` settings of `cdrs_revise()`, which converts certain missing values like <Missing> to `NA`; 2) remain untouched, `"none"`, leaving any corrections to factor levels for post-reading; or 3) a list of logical values with the matching parameters of `cdrs_revise()`.
#' @return Returns a tibble of the Delta Residents Survey 2023 data set.
#' @export
#'
#' @examples
#' dat <- cdrs_read(
#'   path_ = system.file("extdata", "demo", package = "cdrs")
#' )
cdrs_read <- function(
    path_ = NULL,
    relevel_ = "default") {

  if(is.null(path_)){
    return(cdrs_read_example())
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate argument: path_
  stopifnot(
    is.character(path_) | length(path_) != 1
  )

  # Run cdrs_validate ----
  # Get a list of files as a tibble.
  fl <- cdrs_validate(path_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load Dictionary ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dd <- fl %>%
    dplyr::filter(type == "dd") %>%
    dplyr::pull(path) %>%
    readxl::read_xlsx(., sheet = "Variables")

  # clean up dd (remove empty lines)
  dd <- dd %>%
    tidyr::drop_na(name) %>%
    tidyr::fill(Variable, .direction = "down")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load Data ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Task 1. Determine the column types as specified in the
  # data dictionary.

  # Get column order
  col_order <- tibble::tibble(
    vars = fl %>%
      dplyr::filter(type == "data") %>%
      dplyr::pull(path) %>%
      readLines(
        con = .,
        n = 1
      ) %>%
      stringr::str_split_1(., "\\,")
  )

  # Get R class for each variable.
  r_class <- dd %>%
    dplyr::filter(name == "R Class") %>%
    dplyr::select(Variable, value) %>%
    # This sets us up for the `read_csv()` argument col_types.
    dplyr::mutate(readr_type = dplyr::case_when(
      value == "factor" ~ "f",
      value == "factor - numeric" ~ "f",
      stringr::str_detect(
        value,
        stringr::regex("posix",
          ignore_case = TRUE
        )
      ) ~ "T",
      value == "character" ~ "c",
      value == "numeric" ~ "n"
    ))

  # Merge col_order with r_class.
  # This helps us keep track of the actual order of columns in the CSV file.
  # Without this, we *could* end up assigning the wrong col_type to a column
  # as we load it using `read_csv()`.
  col_order <- col_order %>%
    dplyr::left_join(r_class, by = c("vars" = "Variable"))

  # Now read data according to readr_type.
  # The argument supplied to col_types looks something like:
  # "ffffffffTffffcffff..." with each character representing a column type.
  data <- readr::read_csv(
    file = fl$path[fl$type == "data"],
    col_types = paste0(col_order$readr_type,
      collapse = ""
    )
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set Order of Ordinal Values ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split data dictionary into multiple tibbles (by Variable/column)
  vars_ <- dd %>%
    # Note, this dplyr function is "experimental" and may change.
    dplyr::group_split(Variable)

  # Obtain metadata of ordinal variables only,
  # otherwise return NULL
  fcts_ <- purrr::map(vars_, function(tb) {
    # Obtain class of variable.
    cls <- tb %>%
      dplyr::filter(name == "R Class") %>%
      dplyr::pull(value)

    # If not a factor, return NULL.
    if (cls != "factor") {
      return(NULL)
    }

    # Obtain rows with info on factors only.
    fcts <- tb %>%
      dplyr::filter(name == "factors")

    # If no "factors" presented, return NULL.
    if (nrow(fcts) == 0) {
      return(NULL)
    }

    # If there are no encodings (ie. order is not important),
    # return NULL.
    if (all(purrr::map_vec(fcts$encoding, is.na))) {
      return(NULL)
    }

    # RETURN
    # Sort order of factors by:
    # 1. non-"missing" variables first,
    # 2. encoding value
    fcts %>%
      dplyr::mutate(std = stringr::str_detect(value,
        "\\<.+\\>",
        negate = TRUE
      )) %>%
      dplyr::arrange(dplyr::desc(std), encoding) %>%
      # return Variable, value, and encoding only.
      dplyr::select(Variable, value, encoding)
  }) # End of map()

  # remove NULL values
  fcts_ <- purrr::discard(fcts_, ~ is.null(.x))

  # Assign names to fcts_
  fcts_ <- fcts_ %>%
    purrr::set_names(
      purrr::map_vec(fcts_, function(tb) {
        tb$Variable[1]
      })
    )

  # Re-order ordinal variables
  data <- purrr::map2_dfc(data, names(data), function(col_, nm) {
    # Is this column an ordinal factor?
    if (nm %in% names(fcts_)) {
      # obtain tibble with Variable, value, encoding.
      fct <- fcts_[[nm]]
      # RETURN
      # Relevel factor.
      forcats::fct_relevel(col_, fct$value)
    } else {
      # Not an ordinal factor.
      # RETURN
      col_
    }
  }) # End of map()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert Levels ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(class(relevel_) == "character"){
    if (relevel_ == "default" ) {
      # Convert <Missingness> to NA
      data <- cdrs_revise(
        data_ = data,
        dd = dd
      )
    } else if (relevel_ == "none") {
      data <- cdrs_revise(
        data_ = data,
        dd = dd,
        preserve_uncertainty = T,
        preserve_refused = T,
        preserve_factor_numeric = T,
        preserve_editorials = T
      )
    }
  } else if(class(relevel_) == "list"){
    # Format data using provided list.
    data <- do.call(
      # apply cdrs_revise function...
      what = cdrs_revise,
      # with the following arguments:
      args = c(
        # regular inputs
        list(
          data_ = data,
          dd = dd),
        # list inputs
        relevel_)
    )
  } else {
    stop("Error: parameter `relevel_` in cdrs_read incorrectly defined. Please provide the string: default or none; or provide a list, eg list(preserve_uncertainty = T, preserve_refused = T, preserve_factor_numeric = T, preserve_editorials = T).")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort columns in the order as they appear in the data dictionary.
  data <- data %>%
    dplyr::select(
      dd$Variable %>% unique()
    )

  # # Delete temporary unzipped file.
  # if (is_zip) {
  #   unlink(x = path_, recursive = TRUE)
  # }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  data
}

#' Load fabricated example data.
#'
#' Using cdrs::cdrs_read(), loads demo data (inside extdata/demo). Do not draw
#' conclusions from this fabricated "data set". The data were created by
#' scrambling the public version of the DRS data.
#'
#' @param relevel_ character or list. See documentation for `?cdrs_read()`.
#' @return Returns a tibble of the a fictitious data set.
#' @export
#'
#' @examples
#' demo <- cdrs_read_example()
cdrs_read_example <- function(
    relevel_ = "default") {
  message("Loading fabricated DRS data. Do not draw conclusions from analyses on this synthesized data.")
  cdrs_read(
    path_ = system.file("extdata", "demo", package = "cdrs"),
    relevel_ = relevel_
  )
}

#' Prepare and subset DRS data for use with cdrs_design().
#'
#' Subsets data and removes missing values.
#'
#' @param data_ is the complete DRS dataset.
#' @param ... either a character vector of or unquoted column names.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   Q1 = c("Y", NA, "N"),
#'   Zone = c(1, 2, 3),
#'   WTFINAL = c(.8, 1, 1.2)
#' )
#' drs_subset <- cdrs_subset(data_ = df, Q1)
cdrs_subset <- function(
    data_,
    ...
) {

  # Require at least one column name
  cols <- rlang::ensyms(...)
  stopifnot(length(cols) > 0)

  # Convert symbols to strings
  cols_str <- sapply(cols, function(x) rlang::as_string(x))

  sub_ <- data_ %>%
    # Select the variable(s) of interest, and the Zone and weights columns
    dplyr::select(dplyr::all_of(cols_str), Zone, WTFINAL) %>%
    # Remove NA values from design columns: strata, weights
    dplyr::filter(!is.na(WTFINAL) & !is.na(Zone))

  # return
  sub_
}

#' Create composite indices in the 2023 Summary Report.
#'
#' This function largely reproduces the composite indices created for the Summary Report (albeit with some corrections).
#'
#' @param data_ is the full DRS data set (created by `cdrs_read`).
cdrs_composite_index <- function(
    data_
){
  # work through each row of composite_frame (internal cdrs data)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idx <- purrr::pmap_dfc(composite_frame, function(...){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # convert each row of composite_frame to a list
    cols <- rlang::enexprs(...)

    # get regular expression to match column names.
    regex_ <- cols$regex

    # subset data columns by regex.
    # eg. a data.frame of Q42a, Q42b, etc.
    sub_ <- data_ %>%
      dplyr::select(tidyselect::matches(regex_))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Error check: do we have all the columns we need?
    stopifnot(ncol(sub_) == cols$ncol)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create Index Score Vector
    if(!is.na(cols$ordered_levels_csv)){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Re-order levels & remove unwanted levels
      # Then create score.
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      sub_ <- purrr::map(sub_, function(sub_col){
        # ordered categorical variables
        cats_ <- cols$ordered_levels_csv %>%
          stringr::str_split_1(pattern = ",")

        # Ensure sub_col is a factor
        if (!is.factor(sub_col)) {
          sub_col <- factor(sub_col)
        }

        # Filter out levels not in cats_ and convert them to NA
        levels(sub_col) <- ifelse(
          levels(sub_col) %in% cats_,
          levels(sub_col),
          NA)

        # Update sub_col to have NA for values not in cats_
        sub_col[!(sub_col %in% cats_)] <- NA

        # Reorder the levels according to cats_
        # This also adds any missing levels from cats_ as levels in sub_col
        sub_col <- factor(sub_col, levels = cats_)

        # Now convert each category to a number.
        numeric_scores <- as.integer(sub_col) - 1
      })

      # Now, find the average score.
      index_vec <- purrr::pmap_vec(sub_, function(...){
        sub_cols <- rlang::enexprs(...)
        avg <- mean(unlist(sub_cols), na.rm = T)

        # return
        if(identical(avg, NaN)){
          NA_real_
        } else {
          avg
        }
      })
    } else {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Convert all dichotomous columns to score
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      index_vec <- purrr::pmap_vec(sub_, function(...){
        cols <- rlang::enexprs(...)
        # get a count of yes's
        cnt <- stringr::str_which(unlist(cols), "Yes") %>%
          length()

        # return
        if(identical(cnt, integer(0))){
          NA_real_
        } else {
          cnt
        }
      })
    }

    out <- data.frame(
      idx = index_vec
    )

    colnames(out) <- cols$index_name

    # RETURN (map)
    out
  })

  # RETURN (cdrs_composite_index)
  data_ %>%
    dplyr::bind_cols(idx)
}
