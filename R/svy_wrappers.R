# This file contains helpers, data prep and wraparounds for essential survey
# functions.

#' Process Column Names
#'
#' This utility function takes both quoted and unquoted column names as input
#' and returns a character vector of column names. It is designed to facilitate
#' flexible function arguments, allowing users to specify dataframe column names
#' either by directly referring to the column names without quotes (unquoted)
#' or by passing them as strings (quoted).
#'
#' @param ... Variable argument list where each argument can be either a
#' unquoted column name (symbol) or a quoted column name (character string).
#'
#' @return A character vector containing the processed column names.
#'
#' @examples
#' # Assuming df is a dataframe with columns named 'Col1', 'Col2', etc.
#' cdrs:::process_colnames(Col1, "Col2")
#' # Returns: c("Col1", "Col2")
#'
process_colnames <- function(...) {
  # @importFrom rlang enquos is.symbol as_string eval
  # Capture the dots arguments
  dots <- enquos(...)

  # Initialize an empty list to store column names
  cols_str <- c()

  # Iterate through each argument
  for (dot in dots) {
    # Check if the argument is a symbol (unquoted) or a character (quoted)
    if (is.symbol(dot)) {
      # Convert symbol to string
      cols_str <- c(cols_str, as_string(dot))
    } else if (is.character(eval(dot))) {
      # Evaluate and add character names directly
      cols_str <- c(cols_str, eval(dot))
    }
  }

  # Return the vector of column names
  return(cols_str)
}


#' Prepare and subset DRS data for use with cdrs_design().
#'
#' Subsets data and removes missing values.
#'
#' @param data_ is the complete DRS dataset.
#' @param drop_na logical, whether or not to drop NA values in `...` columns.
#' @param ... either a character vector of or unquoted column names.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   Q1 = c("Y", NA, "N"),
#'   Zone = c(1, 2, 3),
#'   WTFINAL = c(.8, 1, 1.2)
#' )
#' drs_subset <- cdrs_subset(data_ = df, drop_na = TRUE, Q1)
cdrs_subset <- function(
    data_,
    drop_na = T,
    ...) {
  # require at least one column name
  stopifnot(length(enquos(...)) > 0)

  # Use the utility function to process the column names
  # (This is in {cdrs}, defined above.)
  cols_str <- process_colnames(...)

  if ("Zone" %in% cols_str | "WTFINAL" %in% cols_str) {
    message("Note, you do not need to specify Zone or WTFINAL.")
  }

  sub_ <- data_ %>%
    # Select the variable(s) of interest, and the Zone and weights columns
    dplyr::select(dplyr::all_of(cols_str), Zone, WTFINAL) %>%
    # Remove NA values in the weight column
    dplyr::filter(!is.na(WTFINAL))

  if (drop_na) {
    # Remove missing values in the variable column(s)
    sub_ <- sub_ %>%
      dplyr::filter(!dplyr::if_any(tidyselect::all_of(cols_str),
                                   is.na))
  }

  # return
  sub_ %>%
    # Drop factors that are unused.
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(cols_str),
        ~ forcats::fct_drop(forcats::as_factor(.))
      )
    )
}

#' Create Survey Design Object.
#'
#' This function wraps around survey::svydesign() to appropriately create a
#' survey design object.
#'
#' @section Notice:
#'
#' The authors of this package are not subject matter experts in the field of
#' survey statistics, or statistics more broadly. Caution should be taken when
#' interpreting our notes, and when using our code. If you encounter any
#' errors, please contact the package maintainer.
#'
#' @section Conceptual Background:
#'
#' Like most surveys, the DRS has a complex sampling design, which involves
#' non-response and self-selection error. In order to mitigate bias, we need to
#' utilize "weights" that increase or decrease the importance of each
#' respondent's contribution to estimates of the population or any given
#' statistic. While it is not difficult to incorporate weights in calculating
#' frequencies, survey::svydesign greatly reduces the burden of calculating
#' variances. Moreover, we use survey::svydesign to ease documentation and
#' reproducibility. For details on how weights were constructed please visit
#' the project documentation.
#'
#' @param data_ A tibble or data.frame which contains the columns/variables of
#' interest, as well as the Zone and WTFINAL columns which concern the
#' stratification and weights used in specifying the survey design,
#' respectively.
#' @param set_fpc logical. Determines if we need to set the finite population
#' correction.
#' @return A R object of class survey.design (from {survey})
#' @export
#'
#' @examples
#' df <- data.frame(
#'   Q0_0 = c("1", NA, "3"),
#'   Zone = c(1, 1, 2),
#'   WTFINAL = c(1.32, 0.83, 0.98)
#' )
#' cdrs_design(df)
cdrs_design <- function(
    data_,
    set_fpc = F) {
  stopifnot(
    ("Zone" %in% names(data_)) | ("WTFINAL" %in% names(data_))
  )

  if (set_fpc) {
    data_ <- data_ %>%
      dplyr::left_join(zone_N, by = Zone)

    # return
    # Create survey design object
    survey::svydesign(
      # This arg specifies cluster ids. In our case, the DRS has no clusters.
      ids = ~1,
      # Here we set the finite population correction, which *may* matter only
      # in cases where Zone 1 residents are of concern. See docs.
      fpc = ~N,
      # The cleaned data set, including the removal of missing values.
      data = data_,
      # Specify the strata (in our case Zone geographies)
      strata = ~Zone,
      # Specify the column with weights.
      weights = ~WTFINAL
    )
  } else {
    # return
    # Create survey design object
    survey::svydesign(
      ids = ~1,
      # The finite population correction is largely unimportant in our case
      # since our response rate is not a substantial fraction of the total
      # population.
      fpc = NULL,
      data = data_,
      strata = ~Zone,
      weights = ~WTFINAL
    )
  }
}
