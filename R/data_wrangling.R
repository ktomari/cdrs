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

#' Removes angle brackets from missing variable levels.
#'
#' Remove angle brackets around missing variable levels. This is often used for plotting functions.
#'
#' @param data_ DRS data.frame or tibble.
#' @param cols_ is a character vector (or `NULL`) containing columns of interest.
#' @return the transformed DRS data set (tibble).
remove_angle_brackets <- function(
    data_,
    cols_ = NULL
    ){
  if(is.null(cols_)){
    # Apply this across all columns.
    # First, get columns with <Missing values>
    fltr <- purrr::map_vec(data_,
                           ~T %in% stringr::str_detect(.x, "\\<.+\\>"))
    remove_angle_brackets(
      data_ = data_,
      cols_ = fltr[fltr == T] %>%
        names()
    )
  } else {
    # apply to specified column (cols_).
    purrr::map2_dfc(data_, names(data_), function(col_, nm){
      if(nm %in% cols_){
        col_ %>%
          forcats::fct_relabel(
            .fun = ~stringr::str_remove_all(.x, "^\\<|\\>$")
          )
      } else {
        # not a specified column
        col_
      }
    })

  }
}


#' Creates an enriched data dictionary
#'
#' Creates a data dictionary where question labels are split into their "prompt" and "response" components.
#' @param dict is the DRS data dictionary.
#' @return a tibble of the DRS data dictionary.
#' @export
enrich_dict <- function(
    dict
){
  dict <- dict %>%
    mutate(group = stringr::str_extract(Variable,
                                        "(?<=^Q)\\d{1,2}[[:alpha:]]*")) %>%
    # exceptions of the above rule.
    mutate(group = dplyr::case_when(
      stringr::str_detect(Variable, "^Q13") ~ "13",
      .default = group
    ))

  # lets add a numeric ID for rearranging rows later.
  vars_ <- dict$Variable %>% unique()
  vars_ <- tibble::tibble(
    Variable = vars_,
    var_id = 1:length(vars_)
  )

  dict <- dict %>%
    dplyr::left_join(vars_, by = "Variable")

  # split by group, eg 1, 2, 3... etc
  splt <- dict %>%
    dplyr::group_split(group)

  dict2 <- purrr::map_dfr(splt, function(grp){
    # grp is a data.frame with multiple `Variable` values, eg.
    # all of Q1_'s: Q1_0, Q1_1, etc.
    # Check if it has a "Label" value in `name` column.
    if(!("Label" %in% grp$name)){
      # skip this group, as there is no label.
      return(grp)
    }

    # grab prompts (char vector)
    labs <- grp %>%
      dplyr::filter(name == "Label") %>%
      dplyr::pull(value)

    # split/unlist by white space.
    split_labs <- purrr::map(labs,
                             ~stringr::str_split_1(.x,
                                                   "(?<=\\.|\\?)\\s+"))

    # now track which values are duplicated
    # (this should preserve the order)
    dupes <- split_labs %>%
      unlist() %>%
      # subset duplicated values
      .[duplicated(.)] %>%
      # remove repeats
      unique()

    if(identical(dupes, character(0))){
      return(grp)
    }

    # go through each label (that is split up),
    # and create a data.frame with unique labels only,
    # eg, if the label is:
    # "2. Which best describes the area where you live? Urban"
    # We should get, "Urban"
    unique_labs <- purrr::map_dfr(split_labs, function(labs){
      val <- paste0(labs[!(labs %in% dupes)], collapse = " ")
      if(identical(val, "")){
        return(NULL)
      }

      # Special Cases.
      # 1. There are a view cases (eg Q6) where the answer begins with
      # "- Selected Choice"
      # remove this text.
      # 2. In the case of the Q13's, each number is "unique" because of
      # the alphabetical chr that follows 13. So remove it.
      val <- val %>%
        stringr::str_remove("^\\-\\sSelected\\sChoice") %>%
        stringr::str_remove("^13[[:alpha:]]\\.\\s") %>%
        stringr::str_squish()

      tibble::tibble(
        name = "unique label",
        value = val
      )
    })

    # Get the prompt. In the previous example, it would be:
    # "2. Which best describes the area where you live?"
    prompt <- tibble::tibble(
      name = "prompt",
      value = paste0(dupes, collapse = " ")
    )

    # Now split up our grp by each question,
    # eg. Q1_1
    split_vars <- grp %>%
      dplyr::group_split(Variable)

    out <- purrr::map_dfr(1:length(split_vars), function(i){
      split_vars[[i]] %>%
        dplyr::bind_rows(prompt) %>%
        dplyr::bind_rows(unique_labs[i,]) %>%
        tidyr::fill(Variable, .direction = "down") %>%
        tidyr::fill(var_id, .direction = "down")
    })

    # return
    out
  })

  # return
  dict2 %>%
    dplyr::arrange(var_id) %>%
    dplyr::select(-var_id, -group)
}
