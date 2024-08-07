
#' @title Prepare data for \{gt\} table.
#'
#' @description
#' Obtain metadata needed to create a table with `cdrs_gt_simple`.
#'
#' @param data_ data.frame. The full CDRS data set.
#' @param col1 character. The first QID of interest, eg "Q1_1" or "Zone".
#' @param col2 character. The second QID of interest.
#' @param dict_ data.frame. The CDRS data dictionary.
#' @param show_percents logical. Show percents in table.
#' @param show_count logical. Show (weighted) counts in table.
#' @param add_labs logical. Whether to add labels (ie. for the stubhead and spanner).
#' @param add_title logical. Whether to add the gt title and subtitle.
#' @param label_threshold numeric. The character at which to wrap labels using `stringr::str_wrap()`.
#' @param param_file character. Path to plot_parameters.xlsx.
#' @return list (named `prep_` in other \{cdrs\} functions).
#' @export
cdrs_gt_prep <- function(
    data_,
    col1,
    col2,
    dict_ = NULL,
    show_percents = TRUE,
    show_counts = FALSE,
    add_labs = TRUE,
    add_title = FALSE,
    label_threshold = 20,
    param_file = system.file("extdata",
                             "plot_parameters.xlsx",
                             package = "cdrs")
){

  # Input validation ----
  stopifnot(inherits(show_percents, "logical"))
  stopifnot(inherits(show_counts, "logical"))
  stopifnot(show_percents | show_counts)
  stopifnot(inherits(add_labs, "logical"))
  stopifnot(inherits(add_title, "logical"))

  # initialize output variable, a list object.
  prep_ <- list()

  # store variables to pass along to gt() functions.
  prep_$show_percents <- show_percents
  prep_$show_counts <- show_counts
  prep_$add_labs <- add_labs
  prep_$add_title <- add_title
  prep_$label_threshold <- label_threshold

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # remove <missingness> angle brackets
  data_ <- remove_angle_brackets(
    data_ = data_,
    cols_ = prep_$cols
  )

  if(inherits(dict_, "data.frame")){
    dict_ <- remove_angle_brackets(
      data_ = dict_,
      cols_ = "value"
    )
  }

  # subset data
  prep_$data <- data_ %>%
    cdrs_subset(cols_ = c(col1, col2))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Logic ----
  # First, lets look at the plot logic to see if we need to rearrange
  # col1 and col2.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # retrieve plot logic from plot_parameters.xlsx
  prep_$logic <- plt_logic(
    file_ = param_file,
    cols_ = c(col1, col2),
    suppress_warnings = TRUE
  ) %>%
    dplyr::mutate(suborder = 1:dplyr::n())

  # Error check
  # We don't want numeric variables
  # TODO offer opportunity to do quantiles
  stopifnot(!("numeric" %in% prep_$logic$plot_type1))

  # Sort
  # Generally speaking, we want the second variable to be categorical
  # if possible.
  order_ <- tibble::tibble(
    plot_type1 = c("dichotomous", "diverging", "ordinal", "categorical"),
    order = 1:4
  )

  prep_$logic <- prep_$logic %>%
    dplyr::left_join(order_, by = "plot_type1") %>%
    dplyr::arrange(order, suborder) %>%
    dplyr::select(-order, -suborder)

  # if the order is wrong,
  # flip col1 and col2
  if(!(prep_$logic$Variable[1] %in% col1)){
    tmp <- col1
    col1 <- col2
    col2 <- tmp
    rm(tmp)
  }

  # Store cols in output object
  prep_$col1 <- col1
  prep_$col2 <- col2

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Labels ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Strip dictionary of unneeded columns/Variables

  dict1 <- dict_ %>%
    dplyr::filter(Variable %in% col1) %>%
    enrich_dict()

  dict2 <- dict_ %>%
    dplyr::filter(Variable %in% col2) %>%
    enrich_dict()

  prep_$dict1 <- dict1
  prep_$dict2 <- dict2

  prep_$txt1 <- cdrs_plt_txt(
    dict = dict1,
    cols_ = col1,
    label_form = "default",
    title_form = "short",
    subtitle_ = T,
    caption_ = T
  )

  prep_$txt2 <- cdrs_plt_txt(
    dict = dict2,
    cols_ = col2,
    label_form = "default",
    title_form = "short",
    subtitle_ = T,
    caption_ = T
  )

  # return
  prep_
}


#' @title Simple Crosstabulation Combined Table
#'
#' @description
#' Calculates a contingency table showing weighted counts and row percentages as a \{gt\} table.
#' @param prep_ list. Output of `cdrs::cdrs_gt_prep()`
#' @return gt object.
#' @export
cdrs_gt_simple <- function(
    prep_
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Input validation ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(inherits(prep_, "list"))
  stopifnot(inherits(prep_$data, "data.frame"))
  stopifnot(inherits(prep_$col1, "character") &
              length(prep_$col1) == 1)
  stopifnot(inherits(prep_$col2, "character") &
              length(prep_$col2) == 1)

  # For easy access, create convenience variables for data and column names.
  data_ <- prep_$data
  cols_ <- c(prep_$col1, prep_$col2)

  xt <- list()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # cdrs_svytb ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get weighted counts
  if(prep_$show_counts){
    xt$cnt <- cdrs_svytb(
      data_ = data_,
      cols_ = cols_,
      is_rounded = T,
      is_props = F,
      is_table = F
    ) |>
      stats::addmargins()
  }


  # Get weighted proportions
  if(prep_$show_percents){
    xt$prp <- cdrs_svytb(
      data_ = data_,
      cols_ = cols_,
      is_rounded = F,
      is_props = T,
      is_table = F
    ) |>
      stats::addmargins()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean svytbs ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean counts.
  if(prep_$show_counts){
  xt$cnt_c <- xt$cnt %>%
    stats::ftable() %>%
    as.data.frame() %>%
    dplyr::rename(stub = cols_[1]) %>%
    tidyr::pivot_wider(names_from = cols_[2],
                       values_from = "Freq")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean proportions.
  if(prep_$show_percents){
  xt$prp_c <- xt$prp %>%
    as.data.frame() %>%
    dplyr::rename(stub = cols_[1]) %>%
    dplyr::mutate(percent = round(Freq * 100, digits = 1)) %>%
    dplyr::mutate(percent = paste0(percent, "%")) %>%
    dplyr::select(-Freq) %>%
    tidyr::pivot_wider(names_from = cols_[2],
                       values_from = "percent")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get levels ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get col2 levels
  # eg. if col2 is Zone,
  # then, col2_lvls is c("1", "2", "3")
  if(prep_$show_percents){
    col2_lvls <- xt$prp %>%
      as.data.frame() %>%
      dplyr::pull(tidyselect::all_of(cols_[2])) %>%
      as.character() %>%
      unique()

    # Get the col1/stub levels from the main data.frame cnt_c
    xt$col1_lvls <- xt$prp_c$stub %>%
      levels()

  } else {
    col2_lvls <- xt$cnt %>%
      as.data.frame() %>%
      dplyr::pull(tidyselect::all_of(cols_[2])) %>%
      as.character() %>%
      unique()

    # Get the col1/stub levels from the main data.frame cnt_c
    xt$col1_lvls <- xt$cnt_c$stub %>%
      levels()
  }

  xt$col2_lvls <- col2_lvls[col2_lvls != "Sum"]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort ----
  # This step may not be necessary.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort the prp_c data.frame by the cnt_c stub levels
  if(prep_$show_counts & prep_$show_percents){
    xt$prp_c <- xt$prp_c %>%
      dplyr::mutate(stub = factor(stub, levels = xt$col1_lvls)) %>%
      dplyr::arrange(stub)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Final Table ----
  # TODO see if gt::merge_n_pct() is better
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For combination table:
  # Now iterate through each column corresponding to
  # a level in cols_[2] and add the percentage.
  # For example, if cols_[2] is Zone,
  # there should be 3 columns, labeled "1", "2", and "3".
  # We'll splice the percentage into the count,
  # so it reads something like, "3 (0.1%)"
  if(prep_$show_counts & prep_$show_percents){
    # COMBINATION TABLE
    xt$final <- purrr::map_dfc(
      .x = 1:ncol(xt$cnt_c),
      .f = function(i){
        # get colname
        nm <- names(xt$cnt_c)[i]

        # create tibble to return
          tibble::tibble(
            # paste values together, eg. "3 (0.1%)"
            col = paste0(
              xt$cnt_c[[nm]],
              " (",
              xt$prp_c[[nm]],
              ")"
            )
          ) %>%
            # set name to col2 factor level
            magrittr::set_names(nm)
        # OLD Code
        # # If this is a col2 level,
        # # lets combine columns from both tables.
        # if(nm %in% xt$col2_lvls){
        #   # create tibble to return
        #   tibble::tibble(
        #     # paste values together, eg. "3 (0.1%)"
        #     col = paste0(
        #       xt$cnt_c[[nm]],
        #       " (",
        #       xt$prp_c[[nm]],
        #       ")"
        #     )
        #   ) %>%
        #     # set name to col2 factor level
        #     magrittr::set_names(nm) %>%
        #     return()
        # } else {
        #   # Return if not a col2 factor column
        #   return(xt$cnt_c[i])
        # }
      })
  } else if(prep_$show_counts){
    # COUNT TABLE
    xt$final <- xt$cnt_c
  } else if(prep_$show_percents){
    # PROPORTIONS TABLE
    xt$final <- xt$prp_c
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # gt table ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # base line weight
  lw <- 1

  # heavy line weight
  hlw <- lw * 2

  # base line color
  lclr <- "#333333"

  # heavy line color
  hclr <- "black"

  # Get the number of columns and rows
  num_cols <- ncol(xt$final)
  num_rows <- nrow(xt$final)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Spanner label ----
  # set up spanner label
  if(prep_$add_labs){
    if("label" %in% names(prep_$txt2$lab_df)){
      spanner_lab <- prep_$txt2$lab_df$label[1]
    } else {
      spanner_lab <- prep_$dict2 %>%
        dplyr::filter(name == "Label") %>%
        dplyr::pull(value) %>%
        as.character() %>%
        unique()
    }

    spanner_lab <- stringr::str_remove(
      string = spanner_lab,
      pattern = "^\\d{1,2}[[:alpha:]]*\\.\\s"
    )

    if(inherits(spanner_lab, "character")){
      if(length(spanner_lab) == 1){
        if(inherits(prep_$label_threshold, "numeric")){
          spanner_lab <- stringr::str_wrap(
            string = spanner_lab,
            width = prep_$label_threshold) %>%
            stringr::str_replace_all(pattern = "\n",
                            replacement = "<br>")
        }
      }
    }
  } else {
    spanner_lab <- cols_[2]
  }

  # Take the sums out.
  gt1 <- xt$final %>%
    gt::gt(
      rowname_col = "stub",
      # row_group_as_column = FALSE
    )  %>%
    # Stub text
    gt::tab_style(
      style = gt::cell_text(
        # color = "black",
        align = "left"
        # weight = "normal",
        # stretch = "condensed",
        # v_align = "bottom"
      ),
      locations = gt::cells_stub(
        rows = tidyselect::everything()
        )
    ) %>%
    # SPANNER
    gt::tab_spanner(
      label = gt::html(spanner_lab),
      columns = xt$col2_lvls
    ) %>%
    # SPANNER: text
    gt::tab_style(
      style = gt::cell_text(
        color = "black",
        align = "right",
        weight = "normal",
        stretch = "condensed",
        v_align = "bottom"
      ),
      locations = gt::cells_column_spanners(
        spanners = spanner_lab)
    ) %>%
    # bottom row
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = hclr,
        weight = hlw
        ),
      locations = gt::cells_body(
        columns = tidyselect::everything(),
        rows = num_rows
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = hclr,
        weight = hlw
      ),
      locations = gt::cells_stub(rows = num_rows)
    ) %>%
    # farthest right column
    gt::tab_style(
      style = gt::cell_borders(
        sides = "left",
        color = hclr,
        weight = hlw
        ),
      locations = gt::cells_body(
        columns = num_cols,
        rows = tidyselect::everything()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "left",
        color = hclr,
        weight = hlw
      ),
      locations = gt::cells_column_labels(columns = num_cols)
    )

  # stubhead ----
  if(prep_$add_labs){

    # get label
    if("label" %in% names(prep_$txt1$lab_df)){
      stub_lab <- prep_$txt1$lab_df$label[1]
    } else {
      stub_lab <- prep_$dict1 %>%
        dplyr::filter(name == "Label") %>%
        dplyr::pull(value)
    }

    stub_lab <- stringr::str_remove(
      string = stub_lab,
      pattern = "^\\d{1,2}[[:alpha:]]*\\.\\s"
    )

    if(inherits(stub_lab, "character")){
      if(length(stub_lab) == 1){

        if(inherits(prep_$label_threshold, "numeric")){
          stub_lab <- stub_lab %>%
            stringr::str_wrap(
              width = prep_$label_threshold) %>%
            stringr::str_replace_all(pattern = "\n",
                                     replacement = "<br>")
        }

        gt1 <- gt1 %>%
          gt::tab_stubhead(label = gt::html(stub_lab)) %>%
          gt::tab_style(
            style = gt::cell_text(
              align = "left",
              whitespace = "normal",
              stretch = "condensed"
            ),
            locations = gt::cells_stubhead()
          ) %>%
          # Add a vertical line to the right of the stubhead
          gt::tab_style(
            style = gt::cell_borders(
              sides = "right",
              color = "#D3D3D3",
              weight = gt::px(lw),  # Adjust the line thickness
              style = "solid"
            ),
            locations = gt::cells_stubhead()
          )
      }
    }
  } else {
    gt1 <- gt1 %>%
      gt::tab_stubhead(label = cols_[1]) %>%
      gt::tab_style(
        style = gt::cell_text(
          align = "left",
          whitespace = "normal",
          stretch = "condensed"
        ),
        locations = gt::cells_stubhead()
      ) %>%
      # Add a vertical line to the right of the stubhead
      gt::tab_style(
        style = gt::cell_borders(
          sides = "right",
          color = "#D3D3D3",
          weight = gt::px(lw),  # Adjust the line thickness
          style = "solid"
        ),
        locations = gt::cells_stubhead()
      )
  }

  # Title ----
  if(prep_$add_title){
    # create title
    title_ <- c(prep_$txt1$title, prep_$txt2$title) %>%
      unique()

    title_ <- paste0(title_, collapse = " by ") %>%
      paste0("Cross-tabulation Table: ", . )

    # create subtitle
    subtitle_ <- paste0("Questions ",
                        cols_[1] %>%
                          stringr::str_remove("^Q") %>%
                          stringr::str_replace(pattern = "\\_",
                                               replacement = "."),
                        " x ",
                        cols_[2]%>%
                          stringr::str_remove("^Q") %>%
                          stringr::str_replace(pattern = "\\_",
                                               replacement = ".")
                        )

    gt1 <- gt1 %>%
      gt::tab_header(
      title = title_,
      subtitle = subtitle_
    )
  }

  # return ----
  gt1
}
