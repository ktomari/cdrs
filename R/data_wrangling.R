# Functions to manipulate DRS data set.

#' Prepare and subset DRS data for use with cdrs_design().
#'
#' Subsets data and removes missing values.
#'
#' @param data_ is the complete DRS dataset.
#' @param cols_ a character vector of column names.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   Q1 = c("Y", NA, "N"),
#'   Zone = c(1, 2, 3),
#'   WTFINAL = c(.8, 1, 1.2)
#' )
#' drs_subset <- cdrs_subset(data_ = df, "Q1")
cdrs_subset <- function(
    data_,
    cols_
) {

  # Require at least one column name
  stopifnot(length(cols_) > 0)

  sub_ <- data_ %>%
    # Select the variable(s) of interest, and the Zone and weights columns
    dplyr::select(dplyr::all_of(cols_), Zone, WTFINAL) %>%
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
#' @return A tibble. the full DRS data set + composite scores.
#' @export
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
    # If not return NULL
    if(ncol(sub_) != cols$ncol){
      return(NULL)
    }

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


