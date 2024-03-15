# Exported helper functions and wrappers for ggplot() functions.

#' Create text for plots.
#'
#' Create different kinds of text decoration for your plot, including labels that appear along the graph axis, titles, subtitles & captions.
#'
#' @param dict_ is the data dictionary. All columns in `cols_` must be present in `dict_$Variable`.
#' @param cols_ variable/column names of the DRS data.
#' @param label_form character. Options include "short", "alphabet", `NULL`. These labels correspond to the table created by `plt_labels()`. If "short", either the variable `short_label` or `short_lvl` is drawn from the labels table and table for recoding factors accordingly is returned. If "alphabet", a table of for recoding factors aligned with alphabetical characters is returned. If `NULL` no table is returned.
#' @param title_form character. Options include "short", "long" or `NULL`. If "short" the `short_title` is retrieved from the labels table (see `plt_labels()`), If "long", the Label value is retrieved from `dict_`. If `NULL`, nothing is returned.
#' @param subtitle_ logical. If `TRUE` it returns the long title (ie. Label) from the `dict_`.
#' @param caption_ logical. If `TRUE` a detailed message include valid response counts and missingness variable counts returned.
#' @param param_file character. Path to custom parameters xlsx document. See inst/extdata/plot_parameters.xlsx for file structure.
#' @return list. The list could be empty, or have one or more of the following: 'labels', 'title', 'subtitle', and/or 'caption'.
#' @export
cdrs_plt_txt <- function(
    dict_,
    cols_,
    label_form = NULL,
    title_form = NULL,
    subtitle_ = FALSE,
    caption_ = FALSE,
    param_file = system.file("extdata",
                             "plot_parameters.xlsx",
                             package = "cdrs")
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Error check.
  stopifnot(inherits(dict_, "data.frame"))

  stopifnot(inherits(cols_, "character"))

  stopifnot(inherits(label_form, "character") |
              inherits(label_form, "NULL"))

  stopifnot(inherits(title_form, "character") |
              inherits(title_form, "NULL"))

  stopifnot(inherits(subtitle_, "logical"))

  stopifnot(inherits(caption_, "logical"))

  stopifnot(
    inherits(param_file, "character") |
      inherits(param_file, "data.frame")
  )

  if(inherits(param_file, "data.frame")){
    stopifnot(
      names(param_file) %in% c("Variable",
                             "short_title",
                             "label",
                             "short_label",
                             "level",
                             "short_level")
    )
  }

  # Which column names available in dict_?
  valid_cols <- cols_ %in% (dict_$Variable %>% unique())

  # Are all column names in dict_?
  if(!all(valid_cols)){
    stop(paste0(
      "The following column names were erroneously supplied to ",
      "cdrs_plt_labels(): ",
      paste0(cols_[!valid_cols], collapse = ", "),
      "."
    ))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Strip dictionary of unneeded columns/Variables
  dict_ <- dict_ %>%
    dplyr::filter(Variable %in% cols_)

  dict_ <- enrich_dict(dict_)

  # load labels, and remove unneeded columns/Variables
  if(inherits(param_file, "data.frame")){
    labs <- param_file
  } else {
    labs <- plt_labels(
      file_ = param_file,
      dict_ = dict_)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # initialize output list.
  out <- list()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # labels/levels ----
  # First, retrieve labels or levels.
  # This section creates `labels_`
  if(!inherits(label_form, "NULL")){

    if(label_form == "short"){
      out$labels <- labs %>%
        dplyr::select(-short_title) %>%
        dplyr::select(tidyselect::where(~!all(is.na(.))))

    } else if (label_form == "alphabet"){
      out$labels <- labs %>%
        dplyr::select(tidyselect::where(~!all(is.na(.)))) %>%
        dplyr::select(tidyselect::any_of(c("Variable",
                                           "label",
                                           "level"))) %>%
        dplyr::mutate(alphabet = dplyr::case_when(
          dplyr::row_number() <= 26 ~ letters[dplyr::row_number()],
          .default = paste0(letters[dplyr::row_number() %% 26],
                            floor(dplyr::row_number()/26))
        )
        )
    }

  } else {
    # In this case, default factors are used, and because
    # of this we don't need labels_
    out$labels <- NULL
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Next retrieve other plot text decoration
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # title ----
  if(inherits(title_form, "NULL")){
    out$title <- NULL
  } else if(title_form == "short"){
    out$title <- labs %>%
      dplyr::filter(Variable %in% cols_) %>%
      dplyr::filter(!is.na(short_title)) %>%
      dplyr::pull("short_title") %>%
      unique() %>%
      paste0(., collapse = " & ")
  } else if(title_form == "long"){
    if("prompt_lab" %in% dict_$name){
      out$title <- dict_ %>%
        dplyr::filter(name == "prompt_lab") %>%
        generate_grp() %>%
        dplyr::mutate(value = paste0("Q", grp, ". ", value)) %>%
        dplyr::pull(value) %>%
        unique() %>%
        stringr::str_squish() %>%
        paste0(., collapse = " & ")
    } else {
      out$title <- dict_ %>%
        dplyr::filter(name == "Label") %>%
        dplyr::pull(value) %>%
        unique%>%
        stringr::str_squish() %>%
        paste0(., collapse = " & ")

    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # subtitle ----
  if(!subtitle_){
    out$subtitle <- NULL
  } else {

    if("prompt_lab" %in% dict_$name){
      out$subtitle <- dict_ %>%
        dplyr::filter(name == "prompt_lab") %>%
        generate_grp() %>%
        dplyr::mutate(value = paste0("Q", grp, ". ", value)) %>%
        dplyr::pull(value) %>%
        unique() %>%
        stringr::str_squish() %>%
        paste0(., collapse = " & ")
    } else {
      out$subtitle <- NULL
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # captions ----
  if(!caption_){
    out$captions <- NULL
  } else {

    ## missingness ----
    # First, create missing levels description text.
    # eg. "Decline to answer" Q2: 0%, ...
    if(T %in% stringr::str_detect(dict_$value, "^\\<.+\\>$")){

      missingness_txt <- dict_ %>%
        # generate_group() %>%
        dplyr::filter(name == "factors") %>%
        dplyr::select(-name, -percent) %>%
        dplyr::filter(encoding != "-97") %>%
        dplyr::filter(stringr::str_detect(value, "^\\<.+\\>$"))

      if(nrow(missingness_txt) == 0){
        missingness_txt <- NULL
      } else {
        missingness_txt <- missingness_txt %>%
          dplyr::mutate(value = stringr::str_remove_all(value, "^\\<|\\>$")) %>%
          dplyr::group_by(value) %>%
          tidyr::nest(nested = c("Variable", "frequency")) %>%
          dplyr::mutate(nested = purrr::map(nested, function(grp_tb){
            # ~~~~~~~~~~~~~~~~~~~~~
            # Objective:
            # Place all values into one string,
            # reducing variables within a group wherever possible.
            # Example output might look like:
            # "Q1 (n = 2), Q13a (n = 9), ..."
            # ~~~~~~~~~~~~~~~~~~~~~
            # returns tibble with:
            # txt [chr]

            # We begin by seeing if we find common value for each grouping.
            grp_tb <- grp_tb %>%
              generate_grp() %>%
              dplyr::group_by(grp) %>%
              dplyr::mutate(all_equal = length(unique(frequency)) == 1) %>%
              dplyr::ungroup() %>%
              # Now that we know if all of one group is equal, eg.
              # all of Q1_0, Q1_1,...etc are all equal to 2,
              # we will equate all these "Variable" values to
              # its group. Following the example above, everything becomes,
              # Q1.
              dplyr::mutate(Variable = dplyr::case_when(
                all_equal & !is.na(grp) ~ paste0("Q", grp),
                .default = Variable
              )) %>%
              # remove unneeded columns
              dplyr::select(-grp, -all_equal) %>%
              # Because some "Variable"s have been converted to their group,
              # eg, from c(Q1_0, Q1_1, ...) to c("Q1", "Q1", ...),
              # we now have duplicate rows. Let's weed these out.
              dplyr::distinct() %>%
              # Finally, flatten these values to a single string in `txt` within
              # this nested tibble.
              dplyr::reframe(txt = paste0(
                Variable,
                " (n = ",
                frequency,
                ")",
                collapse = ", "
              ))

            # return
            grp_tb
          })
          ) %>%
          tidyr::unnest(nested) %>%
          # Now add the missing factor to the `txt` we generated in the nested col.
          dplyr::reframe(val_txt = paste0('"', value, '" ', txt, ".")) %>%
          dplyr::pull(val_txt) %>%
          # Reduce everything to a single string.
          paste0(., collapse = " ") %>%
          paste0("The raw frequencies of missing values that are not conveyed by the graph follows. ", .)
      }
    } else {
      missingness_txt <- NULL
    }

    ## response count ----

    response_cnt <- dict_ %>%
      dplyr::filter(Variable %in% cols_) %>%
      dplyr::filter(name == "Valid Responses") %>%
      generate_grp() %>%
      dplyr::mutate(grp = dplyr::case_when(
        is.na(grp) ~ Variable,
        .default = paste0("Q", grp)
      )) %>%
      dplyr::group_by(grp) %>%
      tidyr::nest(nested = c(Variable, value)) %>%
      dplyr::mutate(nested = purrr::map(nested, function(grp_tb){
        if(length(unique(grp_tb$value)) == 1){
          tibble::tibble(txt = grp_tb$value[1])
        } else {
          tibble::tibble(txt = grp_tb$value)
        }
      })) %>%
      tidyr::unnest(nested) %>%
      dplyr::mutate(txt = paste0(grp, " (n = ", txt, ")")) %>%
      dplyr::pull(txt)

    if(length(response_cnt) > 1){
      response_cnt <- NULL
    }

    ## supplemental caption ----
    if("supplemental_caption" %in% names(labs)){
      supplemental_caption <- labs$supplemental_caption %>% unique()
    } else {
      supplemental_caption <- ""
    }
    ## store caption ----
    # Now we store everything for output
    out$caption <- paste0(
      "California Delta Residents Survey (2023) data were collected in the first quarter of 2023. ",
      "Total (n = ",
      dict_$value[dict_$name == "Total (n)"][1],
      "). ",
      ifelse(!inherits(response_cnt, "NULL"),
             response_cnt %>%
               paste0(collapse = ", ") %>%
               paste0("Valid responses ", ., "."),
             ""),
      ifelse(is.null(missingness_txt),
             "",
             paste0(c(" ",
               missingness_txt))),
      supplemental_caption
    ) %>%
      stringr::str_squish()

    ## alphabetical cap ----
    # If we have alphabet, add to captions.
    if(inherits(label_form, "character")){
      if(label_form == "alphabet"){
        if("level" %in% names(out$labels)){
          cap_alpha <- out$labels %>%
            dplyr::mutate(alpha_txt = paste0(alphabet, ". ", level))
        } else {
          cap_alpha <- out$labels %>%
            dplyr::mutate(alpha_txt = paste0(alphabet, ". ", label))
        }

        alpha_text <- paste0(cap_alpha$alpha_txt, collapse = ", ")

        out$caption <- paste0(
          out$caption,
          "Factors: ",
          alpha_text
        )
      }
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # return
  out
}

#' Prepare data for plotting.
#'
#' Prepare DRS data for plotting.
#'
#' @param data_ the DRS data.
#' @param cols_ the columns of interest.
#' @param dict_ the data dictionary. If `NULL` no plot label decoration performed. In other words, the plot will not display textual descriptions.
#' @param remove_angle_brackets logical. Determines whether to erase angle brackets from <missingness> values.
#' @param is_weighted logical.
#' @param txt_options either NULL or a list providing parameters for `cdrs_plt_txt()`.
#' @param sort_ logical. Sort variables/levels by magnitude of the mean.
#' @param title_size numeric. The size of the font for the title. All other fonts scale linearly to title_size, even if a title isn't included.
#' @param param_file character. Path to custom parameters xlsx document. See inst/extdata/plot_parameters.xlsx for file structure.
#' @return object of class tibble, data set proportions.
#' @export
cdrs_plt_prep <- function(
    data_,
    cols_,
    dict_ = NULL,
    remove_angle_brackets = TRUE,
    is_weighted = TRUE,
    txt_options = NULL,
    sort_ = TRUE,
    title_size = 14,
    param_file = system.file("extdata",
                             "plot_parameters.xlsx",
                             package = "cdrs")
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Argument error check
  stopifnot(inherits(cols_, "character") &
              length(cols_) > 0)

  stopifnot(inherits(data_, "data.frame"))

  stopifnot(inherits(dict_, "data.frame") |
              inherits(dict_, "NULL"))

  stopifnot(inherits(txt_options, "list") |
              inherits(txt_options, "NULL"))

  stopifnot(inherits(title_size, "numeric"))

  stopifnot(
    inherits(param_file, "character") |
      inherits(param_file, "data.frame")
  )

  if(inherits(param_file, "data.frame")){
    stopifnot(
      names(param_file) %in% c("Variable",
                             "short_title",
                             "label",
                             "short_label",
                             "level",
                             "short_level")
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create output list
  out <- list()

  # Add static elements
  out$title_size <- title_size

  # character limit before text wrap is performed.
  axis_wrap <- 20
  legend_wrap <- 20

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine plotting logic
  # (ie. what type of plot)
  logic_ <- plt_logic(
    file_ = param_file,
    cols_ = cols_
  )

  # set plot type
  out$type <- logic_$plot_type1 %>%
    unique()

  # Force sort_ to be FALSE if ordinal,
  # and there's only one variable.
  # (Note, in cases where there are two ordinal variables,
  # what is sorted is not the levels, but the variables themselves.
  # And where there is only one variable that is "categorical",
  # the levels do get sorted.)
  if(out$type == "ordinal" &
     length(unique(logic_$Variable)) == 1){
    sort_ <- FALSE
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Strip dictionary of unneeded columns
  dict_ <- dict_ %>%
    dplyr::filter(Variable %in% cols_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove angle brackets ----
  if(remove_angle_brackets){
    data_ <- remove_angle_brackets(data_,
                                   cols_)
    dict_ <- remove_angle_brackets(dict_, cols_ = "value")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get proportions ----
  # tibble columns:
  # variable, levels, mean, SE, percent, percent_lab
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

  # For stacked plots, small values of label overlap
  if(out$type == "ordinal"){
    prep_ <- prep_ %>%
      dplyr::mutate(percent_lab = dplyr::case_when(
        percent < 10 ~ NA_character_,
        .default = percent_lab
      ))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Filter one level ----
  # Usually, we only want "Yes" for dichotomous questions for bar plots,
  # so this is the use case.
  if(out$type == "dichotomous"){

    # get affirmative level
    level_ <- logic_$affirmative_level %>%
      unique()

    # validation
    stopifnot(length(level_) == 1)

    # filter affirmative level only
    prep_ <- prep_ %>%
      dplyr::filter(levels %in% level_) %>%
      dplyr::mutate(levels = forcats::fct_drop(levels))

    # Remove unused levels from dictionary.
    dict_ <- dict_ %>%
      dplyr::filter(Variable %in% cols_) %>%
      dplyr::mutate(levels_filter = dplyr::case_when(
        name == "factors" & value == level_ ~ T,
        name != "factors" ~ T,
        .default = F
      )) %>%
      dplyr::filter(levels_filter) %>%
      dplyr::select(-levels_filter)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort ----
  # Do we want to arrange levels/variables by size?
  if(sort_){
    if(out$type %in% c("dichotomous", "ordinal")){

      prep_ <- prep_ %>%
        tidyr::nest(.by = variable,
                    .key = "nested") %>%
        dplyr::mutate(max_val = purrr::map_vec(nested, function(tb){
          levels_ <- levels(tb$levels)

          lvl_ <- levels_[1]
          # return
          tb$mean[tb$levels==lvl_]

        })) %>%
        dplyr::arrange(max_val) %>%
        dplyr::select(-max_val) %>%
        tidyr::unnest("nested") %>%
        dplyr::mutate(variable = forcats::as_factor(variable))

    } else if(out$type %in% c("categorical")) {
      prep_ <- prep_ %>%
        dplyr::arrange(mean) %>%
        dplyr::mutate(levels = forcats::as_factor(as.character(levels)))
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run cdrs_plt_txt ----
  # Add descriptive details for plotting labels.
  if(inherits(dict_, "data.frame")){

    if(inherits(txt_options, "NULL")){
      txt_ <- cdrs_plt_txt(
        dict_ = dict_,
        cols_ = cols_,
        label_form = "short",
        title_form = NULL,
        subtitle_ = F,
        caption_ = F,
        param_file = param_file
      )
    } else {
      txt_ <- do.call(
        what = cdrs_plt_txt,
        args = c(
          list(
            dict_ = dict_,
            cols_ = cols_,
            param_file = param_file
          ),
          txt_options
        )
      )
    }

    # create variable and level id's
    if("labels" %in% names(txt_)){

      if("alphabet" %in% names(txt_$labels)){

        if(out$type %in% c("dichotomous")){
          prep_ <- prep_ %>%
            dplyr::left_join(y = txt_$labels %>%
                               dplyr::select(Variable, alphabet),
                             by = c("variable" = "Variable")) %>%
            dplyr::rename(var_id = alphabet) %>%
            dplyr::mutate(var_id = forcats::as_factor(var_id))
        } else if(out$type %in% c("categorical")) {
          prep_ <- prep_ %>%
            dplyr::left_join(y = txt_$labels %>%
                               dplyr::select(level, alphabet),
                             by = c("levels" = "level")) %>%
            dplyr::rename(lvl_id = alphabet) %>%
            dplyr::mutate(lvl_id = forcats::as_factor(lvl_id))
        }
      }

      if("short_label" %in% names(txt_$labels)){
        # determine first if we should str_wrap text
        should_wrap <- txt_$labels$short_label %>%
          unique() %>%
          purrr::map_vec(.,
                         ~stringr::str_length(.x) > axis_wrap)

        should_wrap <- T %in% should_wrap

        if(should_wrap){
          txt_$labels$short_label <- purrr::map_vec(
            .x = txt_$labels$short_label,
            .f = ~stringr::str_wrap(.x, width = axis_wrap)
          )
        }

        # now create var_id
        prep_ <- prep_ %>%
          dplyr::left_join(y = txt_$labels %>%
                      dplyr::select(Variable, short_label),
                    by = c("variable" = "Variable")) %>%
          dplyr::rename(var_id = short_label) %>%
          dplyr::mutate(var_id = forcats::as_factor(var_id))
      }

      if("short_level" %in% names(txt_$labels)){
        # determine first if we should str_wrap text
        should_wrap <- txt_$labels$short_level %>%
          unique() %>%
          purrr::map_vec(
            .x = .,
            .f = ~stringr::str_length(.x) > legend_wrap)

        should_wrap <- T %in% should_wrap

        if(should_wrap){
          txt_$labels$short_level <- purrr::map_vec(
            .x = txt_$labels$short_level,
            .f = ~stringr::str_wrap(.x, width = legend_wrap)
          )
        }

        prep_ <- prep_ %>%
          dplyr::left_join(y = txt_$labels %>%
                             dplyr::select(level, short_level),
                    by = c("levels" = "level")) %>%
          dplyr::rename(lvl_id = short_level) %>%
          dplyr::mutate(lvl_id = forcats::as_factor(lvl_id))
      }
    } else {
      # When no 'labels' from cdrs_plt_txt provided,
      # we want to wrap long text.

      if(out$type %in% c("categorical")){
        prep_ <- prep_ %>%
          mutate(levels = stringr::str_wrap(levels,
                                           width = axis_wrap) %>%
                   forcats::as_factor())
      }
      # else if(out$type %in% c("dichotomous")){
      #   prep_ <- prep_ %>%
      #     mutate(variable = stringr::str_wrap(variable,
      #                                         width = axis_wrap) %>%
      #              forcats::as_factor())
      # }


    }

    if(out$type %in% c("ordinal")){
      prep_ <- prep_ %>%
        dplyr::mutate(percent_lab = forcats::as_factor(
          as.character(percent_lab)))
    }

    out <- append(out, txt_)
    out <- append(out, list(
      prep = prep_
      ))
  } else {
    out <- append(out, list(prep = prep_))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return
  out
}

#' Plot a pie chart.
#'
#' @param prep_ the tibble returned from `cdrs_plt_prep.`
#' @return an object of class ggplot.
#' @export
cdrs_plt_pie <- function(
    prep_
){
  # What number of factors do we have?
  n_fct <- nrow(prep_$prep)

  # Create a palette
  pal <- qual_pal(n_fct)

  # Determine level id column
  if("lvl_id" %in% names(prep_$prep)){
    fill_ <- "lvl_id"
  } else {
    fill_ <- "levels"
  }

  plt_ <- ggplot2::ggplot(
    data = prep_$prep,
    mapping =
      ggplot2::aes(
        x = "",
        y = mean,
        fill = !!rlang::sym(fill_)
      )) +
    # position_stack reverse = T is needed for text labeling to work
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      width = 1) +
    # ggplot2::geom_col(width = 1) +
    ggplot2::geom_label(
      ggplot2::aes(label = percent_lab),
      position = ggplot2::position_stack(
        vjust = 0.5,
        reverse = TRUE),
      label.padding = ggplot2::unit(0.15, "lines"),
      fill = "#ffffff",
      color = "#333333",
      label.size = NA,
      na.rm = T
      ) +
    # add colors
    ggplot2::scale_fill_manual(values = magrittr::set_names(pal, NULL)) +
    # text layered over pie
    ggplot2::coord_polar(theta = "y")
    # ggplot2::scale_y_continuous(breaks = props_$pos,
    #                    labels = props_$percent) +

  plt_ <- plt_decorate(plt_ = plt_,
                       prep_ = prep_) +
    # overwrite some theme elements
    # TODO fix redundancy
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      # axis.text = ggplot2::element_text(size = 15),
      # legend.position = "none", # Removes the legend
      panel.background = ggplot2::element_rect(fill = "white"
                                               ),
      legend.title = ggplot2::element_blank()
      )

  # return
  plt_
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
  stopifnot(inherits(prep_, "list"))
  stopifnot(c("type", "prep") %in% names(prep_))
  if(prep_$type != "dichotomous"){
    warning(
      stringr::str_glue(
        "Passing {prep_$type} type plot into cdrs_plt_bar."
      )
    )
  }

  if("var_id" %in% names(prep_$prep)){
    y_ <- "var_id"
  } else {
    y_ <- "variable"
  }

  plt_ <- ggplot2::ggplot(
    data = prep_$prep,
    mapping = ggplot2::aes(
      x = percent,
      y = !!rlang::sym(y_)
    )) +
    ggplot2::geom_bar(
      stat = "identity"
    ) +
    # Scale
    ggplot2::scale_x_continuous(
      limits = c(0,100),
      # expand = expansion(mult = c(0, 0))
      expand = c(0,0)) +
    # Y-axis Label
    ggplot2::ylab("") +
    ggplot2::xlab("Percent") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none"
    )

  plt_ <- plt_decorate(plt_ = plt_,
                       prep_ = prep_)

  # return
  plt_
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



  if("var_id" %in% names(prep_$prep)){
    y_ <- "var_id"
  } else {
    y_ <- "variable"
  }

  # Set up data for geom_label()
  # (Note, the problem is placing the label correctly
  # is actually quite difficult. This function determine where
  # along the line between 1:100 or in our case 0:1 where a label
  # should exist.)
  prep_$prep <- prep_$prep %>%
    dplyr::group_by(!!rlang::sym(y_)) %>%
    mutate(pos = cumpos(mean)/100)

  # Create plot.
  plt_ <- ggplot2::ggplot(
    data = prep_$prep,
    mapping = ggplot2::aes(
      x = mean,
      y = !!rlang::sym(y_),
      fill = levels
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack()
    ) +
    {
      if(prep_$type == "ordinal"){
        ggplot2::scale_fill_brewer(type = "div")
      } else {
        ggplot2::scale_fill_brewer(type = "qual")
      }
    } +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", "25%", "50%", "75%", "100%"),
      expand = c(.05, .05)
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        # importantly, here we provide the position
        # that we derived manually.
        x = pos,
        label = percent_lab
        ),
      size = (prep_$title_size/4),
      label.padding = ggplot2::unit(0.15, "lines"),
      fill = "#ffffff",
      color = "#333333",
      label.size = NA,
      na.rm = T,
      show.legend = F
    ) +
    ggplot2::labs(x = "",
                  y = "") +
    ggplot2::theme_bw()

  # add title, subtitle, caption, etc.
  plt_ <- plt_decorate(plt_ = plt_,
                       prep_ = prep_) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )

  # return
  plt_
}
