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
                            col_types = "text",
                            na = c("", "NA")
  ) %>%
    purrr::map_dfc(., txt_to_straight_quotes)

  if(!inherits(dict_, "NULL")){
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

#' Table with plotting logic for handling CDRS data.
#'
#' This function simply loads some parameters regarding the plot types associated with each set of questions.
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
                              col_types = "text",
                              na = c("", "NA"))

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

#' Load palette options.
#'
#' @param file_ path to file.
#' @noRd
plt_pal_guide <- function(
    file_ = system.file("extdata",
                        "plot_parameters.xlsx",
                        package = "cdrs")
){
  # Parameter Validation
  stopifnot(inherits(file_, "character"))
  # stopifnot(inherits(cols_, "character") | inherits(cols_, "NULL"))

  pal_guide <- readxl::read_xlsx(path = file_,
                              sheet = "palettes",
                              col_types = "text",
                              na = c("", "NA"))

  # return
  pal_guide %>%
    mutate(pal_start = as.numeric(pal_start))
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
          if(prep_$type %in% c("dichotomous",
                               "ordinal",
                               "categorical")){
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

#' A function that executes a palette function.
#'
#' This function largely serves as a wraparound for different color palette functions. This function executes a provided palette function (with its appropriate namespace, eg. grDevices) that is in string form. This assumes the palette function has the inputs `n` and `palette` which is true for most palette functions in both base and external packages.
#'
#' @param fun_ string with 'namespace::function'.
#' @param n_ string or integer with number of colors.
#' @param pal_ string with palette name.
#' @param options_ string or `NULL`. If a string, it simply pastes values into function call. The string should take the form: `type = 'qualitative'`
#' @return character vector of hex color codes.
#' @noRd
pal_execute <- function(
    fun_ = "grDevices::palette.colors",
    n_ = NA,
    pal_ = "Okabe-Ito",
    options_ = NA
){

  # derive namespace
  namespace_ <- sub(
    pattern = "\\:{2}.+",
    replacement = "",
    x = fun_,
    perl = TRUE
  )

  # validation
  stopifnot(rlang::is_installed(namespace_))

  if(is.na(n_)){
    n_ <- 8
  }

  func <- paste0(
    fun_,
    "(n = ",
    n_,
    ", palette = \'",
    pal_,
    "\'"
  )

  # add options
  if(!is.na(options_)){
    func <- paste0(func, ", ", options_)
  }

  # close fun call
  func <- paste0(func, ")")

  # Execute the function call
  hex <- tryCatch(
    eval(str2lang(func)),
    error = function(e){
      NA
    },
    warning = function(e){
      NA
    })

  if(is.na(hex[1]) & n_ >= 5){
    # Recursively call this function with less values.
    # We assume here that no palette is < 5.
    hex <- pal_execute(
      fun_ = fun_,
      n_ = (n_ - 1),
      pal_ = pal_,
      options_ = options_
    )
  }

  # return
  hex
}


#' Organize inputs for `pal_execute`
#'
#' A simple manager for the logic needed around the rather simple `pal_execute` function. This function is designed to execute the `guide_` correctly; if not then resolve issues; and if not that, raise an error.
#'
#' @param factors_ is a character or factor vector of the values (ie. DRS qid levels) that need to mapped onto hex colors.
#' @param guide_ is a data.frame or tibble derived from plot_parameters.xlsx > palettes.
#' @return a tibble with two columns: factors, pal.
#' @noRd
pal_mapper <- function(
    factors_,
    guide_
){
  # validation
  stopifnot(inherits(factors_, "factor") |
              inherits(factors_, "character"))
  stopifnot(inherits(guide_, "data.frame"))
  stopifnot(nrow(guide_) == 1)

  # retrieve starting iteration (for subsetting `hex`)
  start_i <- ifelse(
    test = is.na(guide_$pal_start),
    yes = 1,
    no = as.integer(guide_$pal_start)
  )

  # sometimes we want to expand the range of colors
  # retrieved from the palette function, eg.
  # a sequential gradient becomes too light
  pal_diff <- ifelse(
    test = is.na(guide_$pal_diff),
    yes = 0,
    no = as.integer(guide_$pal_diff)
  )

  # determine appropriate numbers of colors to return
  # combine factor length and starting iter
  size_ <- length(factors_) + start_i + pal_diff - 1

  hex <- pal_execute(
    fun_ = guide_$pal_fun,
    n_ = size_,
    pal_ = guide_$pal_name,
    options_ = guide_$pal_options
  )

  # `pal_execute` is designed to recursively call itself
  # until `n_ < 5`. Therefore, check length of `hex`

  # Possibility that hex is NA
  if(is.na(hex[1])){
    stop("An error occurred in pal_mapper(). pal_execute() returned NA.")
  }

  if(length(hex) > length(factors_) &
     !is.na(start_i)){
    # here hex is longer, so we need to sort and subset.
    # but first, lets make sure start_i within hex's range.
    if(start_i > length(hex)){
      # this would cause an issue normally, but since we can
      # still return something, we just return the original hex
      # starting from the beginning of hex.
      return(
        tibble::tibble(
          factors = factors_,
          pal = hex[1:length(factors_)]
        )
      )
    }
    # sort hex
    h1 <- hex[start_i:length(hex)]
    h2 <- hex[1:(start_i - 1)]
    hex <- c(h1,h2)
    hex <- hex[1:length(factors_)]
    # return
    return(
      tibble::tibble(
        factors = factors_,
        pal = hex
      )
    )
  } else if (length(hex) < length(factors_)){
    stop(
      stringr::str_glue(
        "Selected palette '{guide_$pal_fun}' is too small."
        )
      )
  } else if(length(hex) == length(factors_)){
    # return
    return(
      tibble::tibble(
        factors = factors_,
        pal = hex
      )
    )
  } else {
    stop("An error occurred in pal_mapper().")
  }
}

#' Create a color palette.
#'
#' This function largely serves as a wraparound for different color palette functions. However,
#'
#' @param prep_ list from `cdrs_plt_prep`
#' @param reverse_ logical. direction of palette
#' @param randomize_ logical. randomize palette order
#' @return list `prep_` with only element `props` changed.
#' @noRd
pal_main <- function(
    prep_,
    missingness_pal = TRUE,
    reverse_ = FALSE,
    randomize_ = FALSE
){
  # validation
  stopifnot(inherits(prep_, "list"))

  stopifnot(all(
    c(
      "props",
      "logic",
      "type"
    ) %in%
      names(prep_)
  )
  )

  # get pal_id if there is one.
  pal_id_ <- prep_$logic$pal_id %>%
    unique()

  # get palette guide
  palg <- plt_pal_guide()

  # subset palette guide
  if(is.na(pal_id_)){
    guide_main <- palg %>%
      dplyr::filter(plot_type == prep_$type &
                      is.na(pal_id))
  } else {
    guide_main <- palg %>%
      dplyr::filter(plot_type == prep_$type &
                      pal_id == pal_id_)
  }

  # Are we assigning colors based on `variable` or `levels`?
  # Cases:
  if(length(unique(prep_$props$levels)) == 1 &
     length(unique(prep_$props$variable)) > 1){
    # Case 1. Dichotomous, Multiple `variable`, eg Q1_0:5
    # by variable
    varying_col <- "variable"
  } else if(length(unique(prep_$props$levels)) > 1 &
            length(unique(prep_$props$variable)) > 1){
    # Case 2. Stacked, Multiple `variable`, eg. Q19a:b
    varying_col <- "levels"
  } else {
    # Case 3. Stacked, Single `variable`, eg. Q10
    # Case 4. Pie, eg. Q2
    varying_col <- "levels"
  }

  # retrieve factors (either `variable` or `levels`)
  fcts_ <- prep_$props %>%
    dplyr::select(tidyselect::all_of(varying_col),
                  encoding) %>%
    dplyr::distinct()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get hex colors,
  # depending on if we want missingness as a separate palette (or not).
  if(missingness_pal &
     (T %in% (as.integer(fcts_$encoding) < 0))
     ){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Missingness & Rest of Levels Separate

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Missingness levels
    # subset palette guide
    if(is.na(pal_id_)){
      guide_miss <- palg %>%
        dplyr::filter(plot_type == "missingness" &
                        is.na(pal_id))
    } else {
      guide_miss <- palg %>%
        dplyr::filter(plot_type == "missingness" &
                        pal_id == pal_id_)

      if(nrow(guide_miss) == 0){
        guide_miss <- palg %>%
          dplyr::filter(plot_type == "missingness" &
                          is.na(pal_id))
      }
    }

    fcts_miss <- fcts_ %>%
      dplyr::filter(as.integer(encoding) < 0) %>%
      dplyr::pull(levels)

    pal_miss <- pal_mapper(
      factors_ = fcts_miss,
      guide_ = guide_miss
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Normal levels
    fcts_main <- fcts_ %>%
      dplyr::filter(as.integer(encoding) >= 0) %>%
      dplyr::pull(levels)

    pal_main <- pal_mapper(
      factors_ = fcts_main,
      guide_ = guide_main
    )

    if(reverse_){
      pal_main$pal <- rev(pal_main$pal)
    } else if (randomize_){
      pal_main$pal <- sample(pal_main$pal)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Combine Normal and Missingness Levels
    pal_ <- pal_main %>%
      dplyr::bind_rows(pal_miss)

  } else {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Ignore missingness values,
    # and get hex colors for every unique value.
    pal_ <- pal_mapper(
      factors_ = fcts_[[varying_col]],
      guide_ = guide_main
    )

    if(reverse_){
      pal_$pal <- rev(pal_$pal)
    } else if (randomize_){
      pal_$pal <- sample(pal_$pal)
    }
  }

  # rename 'factors' column to whatever varying_col is set to.
  names(pal_)[1] <- varying_col


  # Merge props and pal
  prep_$props <- prep_$props %>%
    dplyr::left_join(
      y = pal_,
      by = varying_col
    )

  # return
  prep_
}


