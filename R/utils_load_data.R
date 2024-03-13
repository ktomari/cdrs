#' Helper function to replace smart quotes with straight quotes.
#'
#' @param vec_ character.
#' @return vector.
#' @noRd
txt_to_straight_quotes <- function(
    vec_
){
  if(inherits(vec_, "factor")){
    # factor ----
    # Note,
    # "[\u201C\u201D]" are for "smart quotation marks",
    # and
    # "[\u2018\u2019]" are for "smart single quote marks".
    vec_ %>%
      forcats::fct_relabel(~gsub("[\u201C\u201D]",
                                 "\"",
                                 gsub("[\u2018\u2019]",
                                      "'", .x)))
  } else if(inherits(vec_, "character")) {
    # character ----
    gsub("[\u201C\u201D]", "\"",
         gsub("[\u2018\u2019]", "'", vec_))
  } else {
    # neither, do nothing ----
    vec_
  }
}

#' Helper function to convert bracketed factor levels to NA.
#'
#' Helper function to convert bracketed factor levels (ie. missingness levels like <Decline to answer>) to `NA`.
#'
#' @param vec_ a factor vector.
#' @param match_ a character vector of matching phrases.
#' @return factor vector.
#' @examples
#' input_vec <- factor(
#'   c(
#'     "Green",
#'     "Red",
#'     "<Decline to answer>",
#'     "Green",
#'     "<Decline to answer>",
#'     "<I don't know>"
#'    )
#' )
#'
#' # Only return NA for "Decline to answer",
#' # but not "I don't know"
#' cdrs::matches_to_NA(input_vec, "Decline to answer")
#'
#' @noRd
matches_to_NA <- function(
    vec_,
    match_ = NULL
){
  # return early
  if(!inherits(vec_, "factor")){
    return(vec_)
  }

  # Validate
  stopifnot(
    length(vec_) > 1
  )

  if(!inherits(match_, "NULL")){
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


#' Wrangle CDRS data path
#'
#' Generate list of files present in the path. If the file is a zip, it will unzip the file to a temporary directory. Through simple regular expressions, validate the file names.
#'
#' @param path_ path to data.
#' @return tibble of file paths.
#' @noRd
cdrs_path <- function(path_) {
  # Validate File Exists ----
  stopifnot(inherits(path_, "character"))
  stopifnot(file.exists(path_) |
              dir.exists(path_))

  # Please note, that there is an 'internal data' data.frame,
  # `necessary_files`, which is used below. It was defined in
  # raw-data/internal_data.R
  if (!exists("necessary_files")) {
    # This error is to help with development only.
    # It shouldn't be raised once the package is completed.
    stop("Missing 'necessary_files'")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What to do if the path leads to a File or Directory?
  if (!dir.exists(path_)) {
    # path is a FILE ----

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Capture file extension ----
    # (if there is one.)
    basenm <- basename(path_)
    ext_ <- stringr::str_extract(string = basenm,
                                 pattern = "(?<=\\.).{2,4}$")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Next, we manipulate the zip file: unzip it into a temporary directory.
    if (ext_ == "zip") {
      # get output directory name.
      out_dir <- basename(path_) %>%
        stringr::str_remove("\\.zip$")

      # Define the temporary directory to extract files to
      temp_dir <- tempdir()

      # Full output dir.
      out_dir <- file.path(temp_dir, out_dir)

      # Unzip the file
      utils::unzip(zipfile = path_,
                   junkpaths = T,
                   exdir = out_dir)

      # Generate file list
      fl <- tibble::tibble(
        file = list.files(out_dir),
        path = list.files(out_dir, full.names = TRUE)
      )

    } else {
      # This is not a directory, nor a zip file.
      # So we return a tibble noting what we believe it is.

      # Generate file list
      fl <- tibble::tibble(
        file = basename(path_),
        path = path_
      )
    }
  } else {
    # The path is a directory.
    # Generate file list
    fl <- tibble::tibble(
      file = list.files(path_),
      path = list.files(path_, full.names = TRUE)
    )
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

  # return file list.
  fl
}
