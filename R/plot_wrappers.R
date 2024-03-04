# Helpers and wrappers for ggplot() and other plotting functions.

#' Creates qualitative palette of hex color values.
#'
#' These palettes were selected based on the greatest minimum distance between all sets of colors in the palette. This distance was calculated using `colorblindcheck::palette_check`, which creates a set of scores for each palette for each of the three major types of color vision deficiencies: deuteranopia, protanopia, and tritanopia.
#'
#' @param n_fct is the number of factors/colors we need.
#' @param seed is a numeric value that gives the seed for "random" color
#' selection within the appropriate palette.
#'
#' @return an palette (character vector).
#'
#' @examples
#' # Return first 5 colors of Okabe-Ito
#' cdrs:::qual_pal(5)
qual_pal <- function(n_fct, seed = NA){

  # Get initial palette
  # Palettes were examined using {colorblindcheck}
  if (n_fct < 9) {
    # Okabe-Ito is one of the best performing CVD-friendly palettes
    pal <- grDevices::palette.colors(n = 8,
                                     palette = "Okabe-Ito")
  } else if (n_fct < 13) {
    # Safe is also quite CVD-friendly.
    pal <- rcartocolor::carto_pal(n = 12,
                                  name = "Safe")
  } else if (n_fct < 27) {
    # Alphabet actually tied with Polychrome for min_dist
    # but Alphabet is prettier.
    pal <- grDevices::palette.colors(n = 26, palette = "Alphabet")
  } else if (n_fct < 37) {
    # Worst case scenario, this palette performs better than the
    # default ggplot gradient cut into 36 colors.
    pal <- grDevices::palette.colors(n = 36, palette = "Polychrome")
  } else {
    stop("Too many factors for default palettes!")
  }

  # Randomly assign colors
  if(!is.na(seed)){
    pal <- withr::with_seed(
      seed = seed,
      code = sample(x = pal,
                    size = n_fct)
    )
  } else {
    # or not.
    pal <- pal[1:n_fct]
  }

  # Return
  pal
}

#' Get factor proportions for plotting.
#'
#' @param data_ full data set.
#' @param col_ column name (as a character)
#' @return tibble of unweighted proportions.
#'
#' @examples
#' dat <- cdrs_read_example()
#' cdrs:::get_unwt_props(dat, "Q2")
get_unwt_props <- function(
    data_,
    col_
){

  props_ <- data_ %>%
    filter(!is.na(!!rlang::sym(col_))) %>%
    dplyr::group_by(!!rlang::sym(col_)) %>%
    dplyr::reframe(Count = dplyr::n()) %>%
    mutate(mean = (Count/sum(Count))) %>%
    dplyr::rename(levels := !!rlang::sym(col_)) %>%
    mutate(variable = col_) %>%
    select(variable, levels, Count, mean)

  # Add textual detail for plotting.
  props_ <- props_ %>%
    mutate(percent = round(mean * 100)) %>%
    mutate(percent = paste0(percent, "%"))

  # return
  props_
}

#' Create labels for plots.
#'
#' Choose which kinds of text decoration your plot will have.
#'
#' @param dict_ is the uncompromised data dictionary.
#' @param cols_ variable/column names of the DRS data.
#' @return tibble. A modified subset of the original dictionary.
cdrs_plt_labels <- function(
    dict_,
    cols_
){

}

#' Prepare data for plotting.
#'
#' Prepare DRS data for plotting.
#'
#' @param data_ the DRS data.
#' @param cols_ the columns of interest.
#' @param dict_ the data dictionary. If `NULL` no plot label decoration performed. In other words, the plot will not display textual descriptions.
#' @param level_ character. The name of the level you want to keep. This is useful for questions with dichotomous response options like `"Yes"`, that you wish to isolate. Should be used with `cdrs_plt_bar`.
#' @param remove_angle_brackets logical.
#' @param is_weighted logical.
#' @return object of class tibble, data set proportions.
#' @export
cdrs_plt_prep <- function(
    data_,
    cols_,
    dict_ = NULL,
    level_ = NULL,
    remove_angle_brackets = TRUE,
    is_weighted = TRUE
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Argument error check
  stopifnot(class(cols_) == "character" &
              length(cols_) > 0)

  stopifnot("data.frame" %in% class(data_) |
              "tibble" %in% class(data_))

  stopifnot("data.frame" %in% class(dict_) |
              "tibble" %in% class(dict_) |
              is.null(dict_))

  stopifnot(is.null(level_) |
              class(level_) == "character")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove angle brackets.
  if(remove_angle_brackets){
    data_ <- remove_angle_brackets(data_,
                                   cols_)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get proportions
  if(is_weighted){
    prep_ <- purrr::map_dfr(cols_,
                             ~cdrs_props(data_ = data_,
                                         col_ = .x))
  } else {
    prep_ <- purrr::map_dfr(cols_,
                             ~get_unwt_props(
                               data_ = data_,
                               col_ = .x
                             ))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove other levels
  if(!is.null(level_)){
    # Usually, we only want "Yes" for bar plots.
    # Note, `!!` is non-standard evaluation (NSE).
    # See Wickham's Advanced R.
    prep_ <- prep_ %>%
      dplyr::filter(levels == !!level_)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add descriptive details for plotting labels.
  if(!is.null(dict_)){

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Rewrite `variable` with "unique label".
    # Example: "Q13a" becomes "Rising sea levels".
    # In other words, this serves as the labels on the Y-axis for bar plots.
    if("repsonse_lab" %in% dict_$name){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # create table of values to replace "variable" column in prep_.
      # var_replacement will either be an empty tibble
      # with dim(var_replacement) == integer(c(0,0-)),
      # or it will offer a tibble we can use a join function with.
      var_replacement <- purrr::map_dfr(
        .x = cols_,
        .f = function(var){
          sub_dict <- dict_ %>%
            dplyr::filter(Variable == var)

          if("repsonse_lab" %in% sub_dict$name){
            tibble::tibble(
              variable = var,
              label = sub_dict %>%
                dplyr::filter(name == "repsonse_lab") %>%
                dplyr::pull(value)
            )
          } else {
            NULL
          }
        })

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # re-create variable column with labels derived from dictionary.
      if(!identical(
        dim(var_replacement),
        as.integer(c(0,0))
      )){
        prep_ <- prep_ %>%
          # join with table derived from dict.
          dplyr::left_join(var_replacement, by = "variable") %>%
          # In cases where nothing was matched for the `label`,
          # ie. its `NA`,
          # set it to the value in `variable`.
          dplyr::mutate(label = dplyr::case_when(
            is.na(label) ~ variable,
            .default = label
          )) %>%
          # Convert to factor.
          dplyr::mutate(label = forcats::as_factor(label)) %>%
          # Remove old `variable` column
          dplyr::select(-variable) %>%
          # Rename it.
          dplyr::rename(variable = label)
      }
    }

  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  prep_
}

#' Plot a pie chart.
#'
#' @param prep_ the tibble returned from `cdrs_plt_prep.`
#' @return an object of class ggplot.
#' @export
#'
#' @examples
#' dat <- cdrs_read_example()
#' prep_ <- cdrs_plt_prep(data_ = dat, cols_ = "Q2")
#' cdrs_plt_pie(prep_)
cdrs_plt_pie <- function(
    prep_
){
  # What number of factors do we have?
  n_fct <- nrow(prep_)

  # Create a palette
  pal <- qual_pal(n_fct)

  ggplot2::ggplot(
    data = prep_,
    mapping =
      ggplot2::aes(
        x = "",
        y = mean,
        fill = levels
      )) +
    # position_stack reverse = T is needed for text labeling to work
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      width = 1) +
    # ggplot2::geom_col(width = 1) +
    ggplot2::geom_label(
      ggplot2::aes(label = percent),
      position = ggplot2::position_stack(
        vjust = 0.5,
        reverse = TRUE),
      label.padding = ggplot2::unit(0.15, "lines"),
      fill = "#ffffff",
      color = "#333333",
      label.size = NA
      ) +
    # add colors
    ggplot2::scale_fill_manual(values = magrittr::set_names(pal, NULL)) +
    # text layered over pie
    ggplot2::coord_polar(theta = "y") +
    # ggplot2::scale_y_continuous(breaks = props_$pos,
    #                    labels = props_$percent) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      # axis.text = ggplot2::element_text(size = 15),
      # legend.position = "none", # Removes the legend
      panel.background = ggplot2::element_rect(fill = "white"))

}

#' Dichotomous Question Bar Plots.
#'
#' Plot questions from the DRS data set with only one level of interest (eg. questions where we only want to show "Yes" responses).
#'
#' @param prep_ the tibble returned from `cdrs_plt_prep.`
#' @return an object of class ggplot.
#' @export
cdrs_plt_bar <- function(
    prep_
){

  ggplot2::ggplot(
    data = prep_,
    mapping = ggplot2::aes(
      x = percent,
      y = variable
    )) +
    ggplot2::geom_bar(
      stat = "identity"
    ) +
    # Scale
    ggplot2::scale_x_continuous(limits = c(0,100),
                                # expand = expansion(mult = c(0, 0))
                                expand = c(0,0)) +
    # Y-axis Label
    ggplot2::ylab("") +
    ggplot2::xlab("Percent") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none"
    )

}

#' Creates stacked bar plot.
#'
#' Creates stacked bar plot which shows all levels of a variable. Used for both qualitative and likert scale responses.
#'
#' @param prep_ the tibble returned from `cdrs_plt_prep.`
#' @return an object of class ggplot.
#' @export
cdrs_plt_stacked <- function(
    prep_
){
  ggplot2::ggplot(data = prep_,
                  mapping = ggplot2::aes(x = mean,
                                         y = variable,
                                         fill = levels)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_brewer(type = "qual") +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", "25%", "50%", "75%", "100%"),
      expand = c(.05, .05)
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = "",
                  y = "") +
    ggplot2::theme_bw()
}


