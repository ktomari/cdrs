#' Convert &lt;Missing Levels&gt; to NA
#'
#' Convert columns with missing values (eg. &lt;I Don't Know&gt;, ie values denoted by &lt;angle brackets&gt;) to `NA`.
#'
#' @param data_ A data.frame or tibble consisting of the data from the DRS.
#' @return A data.frame or tibble.
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- data.frame(Q0_0 = c("1", "<Missing>", "3"))
#' cdrs_as_NA(df)
cdrs_as_NA <- function(
    data_
){
  # Validate dat is a df or tb.
  stopifnot(
    base::is.data.frame(data_) |
      tibble::is_tibble(data_)
  )

  # Convert values in df/tb, & return.
  data_ %>%
    dplyr::mutate(
      # change columns that are of type character.
      dplyr::across(.cols = tidyselect::where(base::is.character),
                    .fns = ~ dplyr::case_when(
                      # When string with angle brackets detected,
                      # assign NA value.
                      stringr::str_detect(.x, "\\<.+\\>") ~
                        NA_character_,
                      # Otherwise, return string.
                      TRUE ~ .x)
      ),
      # change columns that are of type factor.
      dplyr::across(.cols = tidyselect::where(base::is.factor),
                    .fns = ~ dplyr::case_when(
                      # When factor with angle brackets detected,
                      # assign NA value.
                      stringr::str_detect(as.character(.x), "\\<.+\\>") ~
                        NA_character_,
                      # Otherwise, return factor.
                      TRUE ~ base::as.character(.x)
                    ) %>%
                      # Then convert to factor, and
                      # drop any empty levels,
                      # ie. factors w/ angle brackets.
                      base::factor() %>%
                      forcats::fct_drop()

      )
    )
}

#' Validate DRS data against a SHA256 hash.
#'
#' Checks and validates that the data stored in the given directory is valid according to the design of the DRS team. It should have three components, a csv, an excel file, and a txt file. This function is not usually called by the end-user, it is a dependency of drs_read.
#'
#' @param path_ is a file path to a directory containing the documents. This should not be a zip file.
#' @return fl, a tibble, including file name, path, and type of data set.
#' @export
#' @importFrom magrittr %>%
cdrs_validate <- function(
    path_
){
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
  if(!exists('necessary_files')){
    # This error is to help with development only.
    # It shouldn't be raised once the package is completed.
    stop("Missing 'necessary_files'")
  }

  # Determine if each file in the filelist is a necessary file,
  # and if so, determine type.
  fl$type <- purrr::map_vec(fl$file, function(path_){
    # Obtain location of name in necessary_files
    i <- purrr::map_vec(necessary_files$regex, function(regex_){
      stringr::str_detect(path_, regex_)
    }) |>
      which()

    # if file is not a necessary file.
    if(identical(i, integer(0))){
      return(as.character(NA))
    }

    # Return
    necessary_files$name[i]
  })

  # Validate that all necessary files present.
  if(length(
    purrr::discard(fl$type,
                   ~is.na(.x))) != nrow(necessary_files)){

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

  if(nchar(hash) != 64){
    stop(".hash.text value is of improper length for SHA256.")
  }

  # Obtain path to data
  data <- fl %>%
    dplyr::filter(type == "data") %>%
    dplyr::pull(path)

  # Obtain SHA256 hash for the data.
  data_hash <- digest::digest(object = data,
                              algo = "sha256",
                              file = TRUE)

  if(!identical(data_hash, hash)){
    stop("Data could not be validated. The SHA 256 hash value obtained for the data does not match the value provided by the DRS team.")
  }

  # return
  fl
}

#' Read the California Delta Residents Survey Dataset
#'
#' Loads, validates, and corrects factor levels in the Delta Residents Survey
#' 2023 data. Depends on cdrs_as_NA, cdrs_validate.
#'
#' @param path_ is a character vector of length 1. Provides the path to the directory containing all the relevant files, including the data dictionary, the data in csv form, and the hash.txt. This path may also lead to a zip file. By default the path is to the current working directory.
#' @param convert_to_NA is a logical vector of length 1. Converts the variety of missingness levels (eg. &lt;Decline to answer&gt;) to `NA` values, simplifying calculations. Importantly, if there are columns that were originally factors (due to different missingness levels), but is principally a numeric column, this function will adjust these columns to type numeric.
#' @return Returns a tibble of the Delta Residents Survey 2023 data set.
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' dat <- cdrs_read(
#'  path_ = system.file("extdata", "demo", package = "cdrs")
#' )
cdrs_read <- function(
    path_ = getwd(),
    convert_to_NA = FALSE
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Validate argument: path_
  stopifnot(
    is.character(path_) | length(path_) != 1
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Is Zip File? ----
  # If path_ leads to a zip file, then unzip it.
  # First, determine if its a zip.
  is_zip <- grepl(pattern = "\\.zip$",
                  x = basename(path_),
                  perl = TRUE)

  # Second, we manipulate the zip file: unzip it into a temporary directory.
  if(is_zip){

    # get output directory name.
    out_dir <- basename(path_) %>%
      stringr::str_remove("\\.zip$")

    # Define the temporary directory to extract files to
    temp_dir <- tempdir()

    # Unzip the file
    utils::unzip(zipfile = path_,
                 exdir = temp_dir)

    # Replace old path.
    path_ <- file.path(temp_dir, out_dir)
  }

  # Run cdrs_validate ----
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
      readLines(con = .,
                n = 1) %>%
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
      stringr::str_detect(value,
                          stringr::regex("posix",
                                         ignore_case = TRUE)) ~ "T",
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
                       collapse = "")
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set Order of Ordinal Values ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split data dictionary into multiple tibbles (by Variable/column)
  vars_ <- dd %>%
    # Note, this function "experimental" and may change.
    dplyr::group_split(Variable)

  # Obtain metadata of ordinal variables only,
  # otherwise return NULL
  fcts_ <- purrr::map(vars_, function(tb){
    # Obtain class of variable.
    cls <- tb %>%
      dplyr::filter(name == "R Class") %>%
      dplyr::pull(value)

    # If not a factor, return NULL.
    if(cls != "factor"){
      return(NULL)
    }

    # Obtain rows with info on factors only.
    fcts <- tb %>%
      dplyr::filter(name == "factors")

    # If no "factors" presented, return NULL.
    if(nrow(fcts) == 0){
      return(NULL)
    }

    # If there are no encodings (ie. order is not important),
    # return NULL.
    if(all(purrr::map_vec(fcts$encoding, is.na))){
      return(NULL)
    }

    # RETURN
    # Sort order of factors by:
    # 1. non-"missing" variables first,
    # 2. encoding value
    fcts %>%
      dplyr::mutate(std = stringr::str_detect(value,
                                              "\\<.+\\>",
                                              negate = TRUE)) %>%
      dplyr::arrange(dplyr::desc(std), encoding) %>%
      # return Variable, value, and encoding only.
      dplyr::select(Variable, value, encoding)

  })   # End of map()

  # remove NULL values
  fcts_ <- purrr::discard(fcts_, ~is.null(.x))

  # Assign names to fcts_
  fcts_ <- fcts_ %>%
    purrr::set_names(
      purrr::map_vec(fcts_, function(tb){tb$Variable[1]})
      )

  # Re-order ordinal variables
  data <- purrr::map2_dfc(data, names(data), function(col_, nm){
    # Is this column an ordinal factor?
    if(nm %in% names(fcts_)){
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
  })  # End of map()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert NAs ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(convert_to_NA){
    # Convert <Missingness> to NA
    data <- cdrs_as_NA(data)

    # Convert Factor - Numeric to just Numeric
    data <- purrr::map2_dfc(data, names(data), function(col_, nm){
      cls <- r_class %>%
        dplyr::filter(Variable == nm) %>%
        dplyr::pull(value)

      # Return
      if(cls == "factor - numeric"){
        as.numeric(col_)
      } else {
        col_
      }
    })   # End of map()
  }  # End of if()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort columns in the order as they appear in the data dictionary.
  data <- data %>%
    dplyr::select(
      dd$Variable %>% unique()
      )

  # Delete temporary unzipped file.
  if(is_zip){
    unlink(x = path_, recursive = TRUE)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  data
}
