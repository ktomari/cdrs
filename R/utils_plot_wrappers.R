# Unexported helper functions.

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
#' @noRd
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
#' @noRd
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

#' Create custom CDRS labels tibble.
#'
#' Creates custom CDRS data labels for each question in the public data set. These labels help in making succinct and clear plots. These labels are available as a CSV file in the {cdrs} package itself, under /extdata. This function makes the table (as a tibble) easily accessible.
#'
#' @param file_ path to file.
#' @param dict_ data dictionary
#'
#' @return tibble.
#' @noRd
plt_labels <- function(
    file_ = system.file("extdata",
                        "plot_parameters.xlsx",
                        package = "cdrs"),
    dict_ = NULL
){
  stopifnot(inherits(file_, "character"))
  stopifnot(inherits(dict_, "data.frame") | inherits(dict_, "NULL"))

  # read labels table from extdata
  labs <- readxl::read_xlsx(path = file_,
                            sheet = "labels",
                            col_types = "text"
  ) %>%
    purrr::map_dfc(., txt_to_straight_quotes)

  if(!is.null(dict_)){
    # return
    # Before returning `labs`,
    # we will see if there are "factors" listed in the `dict_`
    # (under `name` == "factors)
    # that are not present in the labs' level columns.
    # Case: dict_ may have various missingness values like,
    # <I don't know>
    # Which are not present in the labels csv.
    # Therefore, we add these missing levels to `labs`.
    labs %>%
      dplyr::filter(Variable %in% unique(dict_$Variable)) %>%
      tidyr::nest(.by = Variable) %>%
      dplyr::mutate(data = purrr::map2(
        # note, `data` is the column `nest` creates by default.
        data,
        Variable,
        function(tb, var){
          # if there are no "level"s,
          # eg. Q1_0.
          # Then return early.
          if(is.na(tb$level[1])){
            return(tb)
          }

          # browser()
          # get factors from dictionary.
          fcts_ <- dict_ %>%
            dplyr::filter(Variable == var &
                            name == "factors") %>%
            dplyr::pull(value)

          # remove angle brackets
          fcts_clean <- remove_angle_brackets(fcts_)
          tb_fcts <- remove_angle_brackets(tb$level)

          if(all(fcts_clean %in% tb_fcts)){
            # There are no issues.
            return(tb)
          }

          # Which factors are missing?
          fcts_clean <- fcts_clean[!(fcts_clean %in% tb_fcts)]

          # Create new tibble (to bind)
          tb2 <- tibble::tibble(
            level = fcts_clean,
            short_level = fcts_clean
          )

          # return
          tb %>%
            dplyr::bind_rows(tb2) %>%
            tidyr::fill(short_title, .direction = "down")
        })) %>%
      tidyr::unnest(data)
  } else {
    # return
    labs
  }
}

#' Table with logic for handling CDRS data.
#'
#' @param file_ path to file.
#' @param cols_ character. Columns to subset.
#' @noRd
plt_logic <- function(
    file_ = system.file("extdata",
                        "plot_parameters.xlsx",
                        package = "cdrs"),
    cols_ = NULL
){
  # Parameter Validation
  stopifnot(inherits(file_, "character"))
  stopifnot(inherits(cols_, "character") | inherits(cols_, "NULL"))

  logic_ <- readxl::read_xlsx(path = file_,
                              sheet = "logic",
                              col_types = "text")

  # If cols_ specified
  if(inherits(cols_, "character")){
    logic_ <- logic_ %>%
      dplyr::filter(Variable %in% cols_)

    if(length(unique(logic_$plot_type1)) != 1){
      warning(
        paste0(
          "Based on plt_logic, ",
          "these `cols_` appear to have incompatible plot types."
        )
      )
    }
  }

  # return
  logic_
}

#' Calculate plot wrap ratio
#'
#' @param title_size in pts.
#' @return numeric, representing max characters in title.
#' @noRd
plt_ratio <- function(title_size){
  floor(
    (-8.25 * title_size) + 218.5
  )
}

#' Add plot scale elements
#'
#' @param plt_ a ggplot2 object
#' @param prep_ an `list` derived from `cdrs_plt_prep`.
#' @return ggplot2 object.
#' @noRd
plt_decorate <- function(
    plt_,
    prep_
){
  stopifnot(inherits(plt_, "ggplot"))
  stopifnot(inherits(prep_, "list"))

  # retrieve available objects
  items_ <- names(prep_)

  # approx. font size ratio.
  max_char <- plt_ratio(prep_$title_size)

  # add labs
  plt_ <- plt_ +
    ggplot2::labs(
      title = {
        if("title" %in% items_){
          if(prep_$type %in% c("ordinal", "categorical")){
            stringr::str_wrap(
              prep_$title,
              width = floor((max_char) * .8)
            )
          } else {
            stringr::str_wrap(
              prep_$title,
              width = max_char
            )
          }
        } else {
          NULL
        }
      },
      subtitle = {
        if("subtitle" %in% items_){
          if(prep_$type == "categorical"){
            stringr::str_wrap(prep_$subtitle,
                              width = max_char/.9)
          } else {
            stringr::str_wrap(prep_$subtitle,
                              width = max_char/.85)
          }
        } else {
          NULL
        }
      },
      caption = {
        if("caption" %in% items_){
          if(prep_$type %in% c("ordinal", "categorical")){
            stringr::str_wrap(
              prep_$caption,
              width = floor((max_char/.7) * .5)
            )
          } else {
            stringr::str_wrap(
              prep_$caption,
              width = max_char/.7
            )
          }

        } else {
          NULL
        }
      }
    )

  # add font sizes ----
  # title
  if("title" %in% items_){
    plt_ <- plt_ +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = prep_$title_size
        )
      )
  } else {
    plt_ <- plt_ +
      ggplot2::theme(
        plot.title = ggplot2::element_blank()
      )
  }

  # subtitle
  if("subtitle" %in% items_){
    plt_ <- plt_ +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(
          size = (prep_$title_size * 0.85)
        )
      )
  } else {
    plt_ <- plt_ +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_blank()
      )
  }

  # caption
  if("caption" %in% items_){
    plt_ <- plt_ +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(
          size = (prep_$title_size * 0.7)
        )
      )
  } else {
    plt_ <- plt_ +
      ggplot2::theme(
        plot.caption = ggplot2::element_blank()
      )
  }

  # axis.title
  # eg. "Percent"
  plt_ <- plt_ +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        size = (prep_$title_size * 0.8)
      )
    )

  # axis.text,
  # ie. var_id font size
  # percent font size
  plt_ <- plt_ +
    ggplot2::theme(
      axis.text = ggplot2::element_text(
        size = (prep_$title_size * 0.7)
      )
    )

  plt_ <- plt_ +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = prep_$title_size,
        r = prep_$title_size,
        b = prep_$title_size,
        l = prep_$title_size,
        unit = "pt")
    )

  # legend spacing
  if(prep_$type %in% c("categorical")){
    plt_ <- plt_ +
      ggplot2::theme(
        legend.spacing.y = grid::unit(prep_$title_size/4, "pt")
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(byrow = TRUE))
  }

  # return
  plt_
}

#' Evaluate cumulative position.
#'
#' A function to assign a position for geom_text/label on a scale of 0-100
#' To set position of each element in a factorized group.
#'
#' @param x is a vector of percentages that sum to 100.
#' @noRd
cumpos <- function(x){
  # confirm x is a vector
  if(length(x) == 1){
    warning("There appears to be an error in fn_cumpos. `x` should be length > 1")
  }

  # if you supplied the proportions, and not percentages...
  # convert to percentages
  if(sum(x) < 1.1 & sum(x) > 0.9){
    x <- x * 100
  }

  # Algorithm:
  # Make a loop for each value in the vector `x`
  # If this is the first value, it means nothing special happens...
  # It just divided the percent by 2 to get the midway point.
  # Otherwise, if this is a following iteration in the vector,
  # Find the midway of that percentage, then add it to the previous percentages
  tmp <- 0
  for(i in 1:length(x)){
    if(i == 1){
      tmp <- x[i] * 0.5
    } else {
      tmp <- append(
        tmp,
        (x[i] * 0.5) + sum(x[1:(i - 1)])
      )
    }
  }
  # return vector
  100 - tmp
}
