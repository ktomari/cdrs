#' Load fabricated example data.
#'
#' Using cdrs::cdrs_read(), loads demo data (inside extdata/demo). Do not draw
#' conclusions from this fabricated "data set". The data were created by
#' scrambling the public version of the DRS data.
#'
#' @param convert_to_NA logical. Whether to convert to NA or not.
#' @return Returns a tibble of the a fictitious data set.
#' @export
#'
#' @examples
#' demo <- cdrs_read_example(convert_to_NA = TRUE)
cdrs_read_example <- function(
    convert_to_NA = FALSE) {
  message("Loading fabricated DRS data.")
  cdrs_read(
    path_ = system.file("extdata", "demo", package = "cdrs"),
    convert_to_NA = convert_to_NA
  )
}
