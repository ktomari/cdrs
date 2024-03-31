# This file contains helpers, data prep and wraparounds for essential survey
# functions.

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
#'   Zone = factor(c(1, 1, 2)),
#'   WTFINAL = c(1.32, 0.83, 0.98)
#' )
#' cdrs_design(df)
cdrs_design <- function(
    data_,
    set_fpc = T) {
  stopifnot(
    ("Zone" %in% names(data_)) | ("WTFINAL" %in% names(data_))
  )

  if (set_fpc) {
    data_ <- data_ %>%
      dplyr::left_join(zone_N, by = "Zone")

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

#' Run a 2-way weighted cross-tabulation on DRS data.
#'
#' @param data_ data.frame or tibble, the DRS data set.
#' @param cols_ a character vector of column names.
#' @param set_fpc logical. `NULL` defaults to the default of `cdrs_design`. See documentation on `cdrs_design`.
#'
#' @return svyby object
#' @export
#'
#' @examples
#' results <- cdrs::cdrs_crosstab(
#'      data_ = cdrs::cdrs_read_example(return_dict = FALSE),
#'      cols_ = c("AGE_P", "Q1_1")
#'      )
cdrs_crosstab <- function(
    data_,
    cols_,
    set_fpc = NULL
){
  stopifnot(length(cols_) == 2 & class(cols_) == "character")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Subset
  data_ <- cdrs_subset(data_, cols_)

  # Complex survey design
  if(is.null(set_fpc)){
    design_ <- cdrs_design(data_)
  } else {
    design_ <- cdrs_design(data_, set_fpc = set_fpc)
  }

  # Perform contingency table
  results <- survey::svyby(
    formula = stats::as.formula(paste0("~", cols_[1])),
    by = stats::as.formula(paste0("~", cols_[2])),
    design = design_,
    FUN = survey::svytotal,
    keep.names = F,
    na.rm = T
  )

  # return
  results
}

#' Calculates CDRS survey variable proportion.
#'
#' This function is a wraparound function for survey::svymean. By default, it produces a tibble with the mean and standard error (calculated by linearization, see {survey} package for more details). Alternatively, return the 'svystat' object, which is simply the object yielded by survey::svymean.
#'
#' @param data_ is a tibble/data.frame of the DRS data set.
#' @param col_ is a character vector, with the column of interest.
#' @param return_stat is logical. Determines whether a tibble of proportions are returned, or if the "svystat" object is returned. In the latter case, {stats} functions like `confint` can be used on the "svystat" object to derive things like the confidence interval for each factor. See the documentation on `svymean` for detailed information on the "svystat" object and its "methods" (ie. functions associated with this class of objects).
#' @return either a tibble or svystat object.
#' @export
#' @importFrom methods is
#'
#' @examples
#' dat <- cdrs_read_example(return_dict = FALSE)
#' # numeric column
#' numeric_mean_tb <- cdrs_props(dat, "Q1a")
#' # single-response multiple choice
#' single_response_tb <- cdrs_props(dat, "Q2")
cdrs_props <- function(
    data_,
    col_,
    return_stat = FALSE
    ){
  # ~~~~~~~~~~~~~~~~
  # Input validation ----
  stopifnot(length(col_) == 1 &
              is(col_, "character"))

  # ~~~~~~~~~~~~~~~~
  # Complex survey design ----
  design_ <- cdrs_design(data_ = data_, set_fpc = F)

  # ~~~~~~~~~~~~~~~~
  # Proportions ----
  props_ <- survey::svymean(x = stats::as.formula(paste0(
    "~",
    stringr::str_glue("`{col_}`")
  )),
  design = design_,
  na.rm = T)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert to tibble ----
  # NOTE: As of this writing, there is no broom::tidy() method for
  # objects of class 'svystat'. Thus, we must manually tidy it.
  # We don't want the svystat object,
  # we want a nicely formatted tibble.
  if(!return_stat){
    # confusingly, names() on svystat object returns
    # either the variable name + factors or just the variable name,
    # ie. not column names.
    # So, for instance, we might see "Q1_1Yes",
    # or for numeric columns just, "Q1a"
    rownm <- names(props_)

    # Test if rownames are simply the variable, `col_`.
    # In essence this test allows us to see if there are factor levels.
    if(identical(rownm, col_)){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # col_ is the same as the rowname of props_.
      # In other words, we have no factor levels.
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      props_ <- tibble::tibble(
        variable = col_,
        mean = stats::coef(props_),
        SE = survey::SE(props_) %>%
          as.vector()
      ) %>%
        mutate(percent = round(mean * 100)) %>%
        mutate(percent_lab = paste0(percent, "%"))

    } else {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # the rownames contain factor levels
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      props_ <- tibble::tibble(
        variable = col_,
        levels = names(stats::coef(props_)),
        mean = stats::coef(props_),
        SE = survey::SE(props_) %>%
          as.vector()
      ) %>%
        # remove any backticks
        mutate(levels = stringr::str_remove_all(
          string = levels,
          pattern = '`')
        ) %>%
        # remove the col_ name, eg. "Q1_1",
        # from the factor levels, eg. "No" and "Yes"
        mutate(levels = stringr::str_remove(
          string = levels, pattern = col_) %>%
            # convert to a factor
            forcats::as_factor()
        ) %>%
        mutate(percent = round(mean * 100)) %>%
        mutate(percent_lab = paste0(percent, "%"))

    }  # end if(identical(rownm, col_)){

  }  # end if(!return_stat){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # return
  props_
}

#' Wrapper for survey::svyby with svymean
#'
#' Function that wraps around survey::svyby with FUN defined as survey::svymean.
#'
#' @param data_ tibble. DRS dataset
#' @param col_ character. Primary variable (name).
#' @param by_col character. Subsetting factor (name).
#' @param return_stat logical. Determines whether a tibble of proportions are returned, or if the "svyby" object is returned. In the latter case, {stats} functions like `confint` can be used on the "svyby" object to derive things like the confidence interval for each factor. See the documentation on `svymean` for detailed information on the "svyby" object and its "methods" (ie. functions associated with this class of objects).
cdrs_props_by <- function(
    data_,
    col_,
    by_col,
    return_stat = FALSE
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Complex survey design ----
  # Create complex survey design object.
  design_ <- cdrs_design(data_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Proportions ----
  # Get survey data means
  props_ <- survey::svyby(
    # define formula
    formula = stats::as.formula(paste0("~", col_)),
    # supply complex design object
    design = design_,
    # supply strata/factor by which to subset
    by = as.formula(paste0("~", by_col)),
    # choose desired function
    FUN = survey::svymean,
    # drop NA
    na.rm = T)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert to tibble ----
  if(!return_stat){
    # first convert to tibble
    # (Note, unlike svymean on its own, the row names will only be duplicate of
    # the first column, which is the `by_col`.)
    props_ <- tibble::as_tibble(props_)

    # then rename column names
    nm_ <- names(props_)

    # Are any column names simply the `col_`?
    filter1 <- nm_ == col_

    if(T %in% filter1){
      # rename the column named `col_`
      nm_[filter1] <- "stat"
    }

    # Are any column names simply the `by_col`?
    filter2 <- nm_ == by_col

    # Change `by_col` to factor
    if(T %in% filter2){
      # rename `by_col`
      nm_[filter2] <- "fcts"
    }

    # Do any columns have `col_` in them, but have other strings?
    filter3 <- xor(
      grepl(pattern = col_, x = nm_),
      filter1
    )

    if(T %in% filter3){
      # excise `col_`
      nm_[filter3] <- sub(pattern = col_,
                          replacement = "",
                          x = nm_[filter3])
    }

    names(props_) <- nm_
  }

  props_
}
