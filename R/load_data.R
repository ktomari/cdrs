# Functions that load or help load the DRS data; and functions that help revise or subset DRS data.

#' Read CDRS data dictionary.
#'
#' Helper function that reads the CDRS data dictionary in only.
#'
#' @param path_ is the path to the dictionary.
#' @return tibble.
#' @export
cdrs_read_dict <- function(
    path_
){
  # retrieve file list.
  fl <- cdrs_path(path_)

  # filter only data dictionary row.
  fl <- fl %>%
    dplyr::filter(type == "dd")

  # validation.
  stopifnot(nrow(fl) == 1)

  # load dd
  dd <- readxl::read_xlsx(path = fl$path,
                          sheet = "Variables")

  # Re-write smart quotes with straight quotes
  dd <- purrr::map_dfc(dd, txt_to_straight_quotes)

  # clean up dd (remove empty lines)
  dd <- dd %>%
    tidyr::drop_na(name) %>%
    tidyr::fill(Variable, .direction = "down")

  # return
  dd
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
    inherits(dd, "data.frame")
  )
  stopifnot(
    inherits(dd, "data.frame") |
      inherits(dd, "character")
  )
  stopifnot(
    inherits(preserve_uncertainty, "logical") |
      inherits(preserve_refused, "logical") |
      inherits(preserve_factor_numeric, "logical") |
      inherits(preserve_editorials, "logical")
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare Data Dictionary ----
  # First, if its a path, load the dd in.
  if(inherits(dd, "character")){
    # `dd` is a character, probably a path.
    dd <- cdrs_read_dict(dd)
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
  data_ <- purrr::map2_dfc(data_, colnames(data_), function(x, nm){
    # Get DRS class based on data dictionary
    cl <- dd$value[dd$Variable == nm &
                     dd$name == "R Class"]

    # We only care about factors, so skip over the rest.
    if(!(cl %in% c('factor', 'factor - numeric'))){
      # return early
      return(x)
    }

    # When revising, we may also encounter a
    # 'factor - numeric' column
    # but the revision has already been performed.
    # if some revision operation has already been performed.
    # For example, you read data into the R environment,
    # and then you want to edit it. If you pass Q1a, and
    # it is already numeric, you're out of luck.
    if(inherits(x, "numeric")){
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

  # revise dictionary ----
  # Drop unused factors in the data dictionary
  dd <- revise_dict(
    data_ = data_,
    dict_ = dd
  )

  # return list
  list(
    data = data_,
    dict = dd
  )
}

#' Validate DRS data against a SHA256 hash.
#'
#' Checks and validates that the data stored in the given directory is valid according to the design of the DRS team. It should have three components, a csv, an excel file, and a txt file. This function is not usually called by the end-user, it is a dependency of cdrs_read.
#'
#' @param path_ is a file path to a directory containing the documents. This should not be a zip file.
#' @return fl, a tibble, including file name, path, and type of data set.
cdrs_validate <- function(
    path_) {

  # Retrieve file list.
  fl <- cdrs_path(path_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate Files ----
  # Validate that all the necessary files are present.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Please note, that there is an 'internal data' data.frame,
  # `necessary_files`, which is used below. It was defined in
  # raw-data/internal_data.R
  stopifnot(exists("necessary_files"))

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
#' @param return_dict logical. If `TRUE` returns list with both the data dictionary and the data. The data dictionary is an optional parameter in some {cdrs} functions that adds additional metadata information for deliverables (eg. figure descriptions for plots).
#' @return Depending on `return_dict`, either returns a tibble of the Delta Residents Survey 2023 data set; or returns a list with two tibbles: the data dictionary, `dict` and the DRS data set, `data`.
#' @export
#'
#' @examples
#' dat <- cdrs_read(
#'   path_ = system.file("extdata", "demo", package = "cdrs")
#' )
cdrs_read <- function(
    path_ = NULL,
    relevel_ = "default",
    return_dict = TRUE
    ) {

  if(inherits(path_, "NULL")){
    return(cdrs_read_example())
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate argument: path_
  stopifnot(
    inherits(path_, "character") | length(path_) != 1
  )

  # Run cdrs_validate ----
  # Get a list of files as a tibble.
  fl <- cdrs_validate(path_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load Dictionary ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # starting with the file list,
  # find the data dictionary path
  # and then read it.
  dd_path <- fl %>%
    dplyr::filter(type == "dd") %>%
    dplyr::pull(path)

  dd <- cdrs_read_dict(dd_path)

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
  ) %>%
    # convert smart quotes to straight quotes.
    purrr::map_dfc(., txt_to_straight_quotes)

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
  # Convert Levels w/ cdrs_revise ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(inherits(relevel_, "character")){
    # if relevel_ is a character vector...
    if (relevel_ == "default" ) {
      # Convert <Missingness> to NA
      revised_list <- cdrs_revise(
        data_ = data,
        dd = dd
      )
    } else if (relevel_ == "none") {
      revised_list <- cdrs_revise(
        data_ = data,
        dd = dd,
        preserve_uncertainty = T,
        preserve_refused = T,
        preserve_factor_numeric = T,
        preserve_editorials = T
      )
    }
  } else if(inherits(relevel_, "list")){
    # Format data using provided list.
    revised_list <- do.call(
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
  revised_list$data <- revised_list$data %>%
    dplyr::select(
      revised_list$dict$Variable %>% unique()
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  if(return_dict){
    revised_list
  } else {
    revised_list$data
  }
}

#' Load fabricated example data.
#'
#' Using cdrs::cdrs_read(), loads demo data (inside extdata/demo). Do not draw
#' conclusions from this fabricated "data set". The data were created by
#' scrambling the public version of the DRS data.
#'
#' @param relevel_ character or list. See documentation for `?cdrs_read()`.
#' @param return_dict logical. See documentation for `?cdrs_read()`
#' @return Returns a tibble of the a fabricated data set, or a list with the fabricated data set and the data dictionary.
#' @export
#'
#' @examples
#' demo <- cdrs_read_example()
cdrs_read_example <- function(
    relevel_ = "default",
    return_dict = TRUE) {
  message("Loading fabricated DRS data. Do not draw conclusions from analyses of this synthesized data.")
  cdrs_read(
    path_ = system.file("extdata", "demo", package = "cdrs"),
    relevel_ = relevel_,
    return_dict = return_dict
  )
}
