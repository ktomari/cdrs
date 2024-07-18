# (Exported) plot set up functions and
# wrappers for ggplot() functions.

#' Create text for plots.
#'
#' Create different kinds of text decoration for your plot, including labels that appear along the graph axis, titles, subtitles & captions.
#'
#' @param dict_ is the data dictionary. All columns in `cols_` must be present in `dict_$Variable`.
#' @param cols_ character. variable/column names of the DRS data.
#' @param label_form character. Options include "short", "alphabet", "default", and `NULL`. These labels correspond to the table created by `plt_labels()`. If "short", either the variable `short_label` or `short_lvl` is drawn from the labels table and table for recoding factors accordingly is returned. If "alphabet", a table of for recoding factors aligned with alphabetical characters is returned. If either "qid" or `NULL` no table is returned because the y-axis label will either remain the default (eg. Q1_0, Q1_1, etc), or it will be axis.text element blank (ie. there will be no label).
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

  # init labs ----
  # load labels (from data.frame or character path to xlsx)
  # column names: Variable, short_title,
  # label, short_label, level, short_level.
  if(inherits(param_file, "data.frame")){
    labs <- param_file
  } else {
    # Function `plt_labels()` in 'utils_plot_wrappers.R'
    labs <- plt_labels(
      file_ = param_file,
      dict_ = dict_)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # initialize output lists.
  out <- list()
  captions <- list(
    about_survey = "California Delta Residents Survey (2023) data were collected in the first quarter of 2023."
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # labels/levels ----
  # First, retrieve labels or levels.
  # This section creates `labels_`
  if(!inherits(label_form, "NULL")){

    if(label_form %in% "short"){
      out$labels <- labs %>%
        dplyr::select(-short_title, -full_title) %>%
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
    } else if(label_form == "default") {
      out$labels <- NULL
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
    out$title <- labs %>%
      dplyr::filter(Variable %in% cols_) %>%
      dplyr::filter(!is.na("full_title")) %>%
      dplyr::mutate(full_title = paste0("Q", id, ". ", full_title)) %>%
      dplyr::pull("full_title") %>%
      unique() %>%
      paste0(., collapse = " & ")

    # if we choose "long", we don't want it reproduced in the subtitle
    # so, we turn off subtitle
    subtitle <- FALSE
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # subtitle ----
  if(!subtitle_){
    out$subtitle <- NULL
  } else {

    # Note, new approach (20240710):
    # Instead of deriving the full question for the subtitle from the
    # dictionary, we now derive it from inst/extdata/plot_parameters.xlsx
    # under the 'labels' sheet.
    out$subtitle <- labs %>%
      dplyr::filter(Variable %in% cols_) %>%
      dplyr::filter(!is.na("full_title")) %>%
      dplyr::mutate(full_title = paste0("Q", id, ". ", full_title)) %>%
      dplyr::pull("full_title") %>%
      unique() %>%
      paste0(., collapse = " & ")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # captions ----
  if(!caption_){
    captions <- NULL
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

    # store to output
    captions$missingness <- missingness_txt

    # response count ----

    response_cnt <- dict_ %>%
      dplyr::filter(Variable %in% cols_) %>%
      dplyr::filter(name == "Valid Responses") %>%
      generate_grp() %>%
      dplyr::mutate(grp = dplyr::case_when(
        # This is needed for variables like AGE_P
        # that `generate_grp` yields only NA values.
        # Usually,`generate_grp` produces a value like "1" or "1a".
        is.na(grp) ~ Variable,
        .default = grp
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
      dplyr::mutate(txt = paste0(
        " (n = ",
        txt,
        ")."
      )) %>%
      dplyr::pull(txt)

    # store to output
    if(length(response_cnt) > 1){
      captions$response_cnt <- NULL
    } else {
      captions$response_cnt <- response_cnt %>%
        paste0(collapse = ", ") %>%
        paste0("Valid responses for this variable ", .)
    }

    # labs qid notes ----
    # These are survey question specific notes.
    # See inst/extdata/plot_parameters.xlsx
    if("caption_note" %in% names(labs)){
      qid_notes <- labs$caption_note %>%
        unique()

    } else {
      qid_notes <- NULL
    }

    # store to output
    captions$qid_notes <- qid_notes

    # other captions ----
    # Now we store everything for output
    captions$n_total <- paste0(
      "Total survey responses (n = ",
      dict_$value[dict_$name == "Total (n)"][1],
      ")."
    )

    # alphabetical cap ----
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

        captions$alpha <- paste0(
          "Factors: ",
          alpha_text
        )
      }
    }
  }

  # Store captions to output list
  out$captions <- captions[!is.na(captions)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # return ----
  out
}

#' Prepare data for plotting.
#'
#' Prepare DRS data for plotting.
#'
#' @param data_ tibble. the DRS data.
#' @param cols_ character. A vector of the columns of interest.
#' @param dict_ tibble. the data dictionary. If `NULL` no plot label decoration performed. In other words, the plot will not display textual descriptions.
#' @param remove_angle_brackets logical. Determines whether to erase angle brackets from <missingness> values.
#' @param is_weighted logical.
#' @param txt_options list. Either `NULL` or a list providing parameters for `cdrs_plt_txt()`.
#' @param sort_ logical. Sort variables/levels by magnitude of the mean.
#' @param title_size numeric. The size of the font for the title. All other fonts scale linearly to title_size, even if a title isn't included.
#' @param label_threshold numeric. The percent threshold by which we should stop displaying labels (in `geom_label`) for stacked bar plots. If no threshold is desired, enter `NULL`.
#' @param param_file character. Path to custom parameters xlsx document. See inst/extdata/plot_parameters.xlsx for file structure.
#' @return a list object, often named `prep_` in the \{cdrs\} package.
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
    label_threshold = 10,
    param_file = system.file("extdata",
                             "plot_parameters.xlsx",
                             package = "cdrs")
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Input validation ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                               "id",
                               "full_title",
                               "short_title",
                               "label",
                               "short_label",
                               "level",
                               "short_level")
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup and Initialize Output Lists ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  out <- list()
  captions <- list()

  # Add static elements
  out$title_size <- title_size

  # character limit before text wrap is performed.
  # TODO adjust to scale with changing font size.
  # y-axis wrap limit
  axis_wrap <- 20
  # legend wrap limit
  legend_wrap <- 20

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine plotting logic ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # (ie. what type of plot)
  # Function in utils_plot_wrappers.R. Returns tibble.
  # Includes information like, "categorical" or which palette to use.
  # Sample row from tibble:
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Variable | plot_type1  | pal_id   | affirmative_level | notes
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Q4_0     | dichotomous | purples1	| Yes               |
  logic_ <- plt_logic(
    file_ = param_file,
    cols_ = cols_
  )

  # add "logic" to output list.
  out$logic <- logic_

  # set plot type
  out$type <- logic_$plot_type1 %>%
    unique()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Force sort_ to be FALSE if ordinal,
  # and there's only one variable.
  # (Note, in cases where there are two ordinal variables,
  # what is sorted is not the levels, but the variables themselves.
  # And where there is only one variable that is "categorical",
  # the levels do get sorted.)
  if(out$type %in% c("ordinal", "diverging") &
     length(unique(logic_$Variable)) == 1){
    sort_ <- FALSE
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Strip dictionary of unneeded columns
  dict_ <- dict_ %>%
    dplyr::filter(Variable %in% cols_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove angle brackets ----
  if(remove_angle_brackets &
     out$type %in% c("ordinal",
                     "diverging",
                     "dichotomous",
                     "categorical")){

    data_ <- remove_angle_brackets(data_,
                                   cols_)
    dict_ <- remove_angle_brackets(dict_, cols_ = "value")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get Proportions ----
  # tibble columns:
  # variable, levels, mean, SE, percent, percent_lab
  if(out$type %in% c(
    "ordinal",
    "diverging",
    "dichotomous",
    "categorical"
  )){
    if(is_weighted){
      props_ <- purrr::map_dfr(cols_,
                               ~cdrs_props(data_ = data_,
                                           col_ = .x))
    } else {
      props_ <- purrr::map_dfr(cols_,
                               ~get_unwt_props(
                                 data_ = data_,
                                 col_ = .x
                               ))
    }
  } else {
    stop(
      stringr::str_glue(
        "Issue: cdrs_plt_prep cannot handle '{out$type}' columns at this time."
      )
    )
  }


  # add encoding column into props (used in pal_maker)
  props_ <- props_ %>%
    dplyr::left_join(
      # subset the dict before joining.
      y = dict_ %>%
        dplyr::filter(name == "factors") %>%
        dplyr::select(Variable, value, encoding),
      by = c("variable" = "Variable", "levels" = "value"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Type-specific Label Fixes ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Small ordinal labels
  # ~~~~~~~~~~~~~~~~~~~~~
  # For stacked plots, small values of label overlap, so drop them.
  # (This is performed in this function because we need to convert percent_lab
  # into a factor in the section 'Final cleanup')
  if(out$type %in% c("ordinal", "diverging") &
     !inherits(label_threshold, "NULL")){
    props_ <- props_ %>%
      dplyr::mutate(percent_lab = dplyr::case_when(
        # drop labels by a certain threshold (default: <10%)
        percent < label_threshold ~ NA_character_,
        .default = percent_lab
      ))

    # assign threshold to output.
    out$label_threshold <- label_threshold
  }

  # ~~~~~~~~~~~~~~~~~~~~~
  # Filter out 'No' level in dichotomous types
  # ~~~~~~~~~~~~~~~~~~~~~
  # Usually, we only want "Yes" for dichotomous questions for bar plots,
  # so this is the use case.
  if(out$type == "dichotomous"){

    # get affirmative level
    level_ <- logic_$affirmative_level %>%
      unique()

    # validation
    stopifnot(length(level_) == 1)

    # filter affirmative level only
    props_ <- props_ %>%
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Do we want to arrange levels/variables by size?
  if(sort_){
    if(out$type %in% c("dichotomous",
                       "ordinal",
                       "diverging")){

      props_ <- props_ %>%
        tidyr::nest(.by = variable,
                    .key = "nested") %>%
        dplyr::mutate(max_val = purrr::map_vec(
          nested,
          function(tb){
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
      props_ <- props_ %>%
        dplyr::arrange(mean) %>%
        dplyr::mutate(levels = forcats::as_factor(
          as.character(
            levels)
        )
        )
    }
  }  # End if(sort_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run cdrs_plt_txt ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Retrieve descriptive details for plotting labels.
  # (Creates `txt_` object.)
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

    # Y-axis text labels.
    # By default, let's make sure the y-axis is visible.
    out$yaxis <- TRUE

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create `var_id` column ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create variable and level id's used in the labeling of plots.
    if("labels" %in% names(txt_) &
       out$type %in% c("dichotomous",
                       "categorical")){

      # Letter-based label, eg. a), b), c), ...
      if("alphabet" %in% names(txt_$labels)){

        if(out$type %in% c("dichotomous")){
          props_ <- props_ %>%
            dplyr::left_join(y = txt_$labels %>%
                               dplyr::select(Variable, alphabet),
                             by = c("variable" = "Variable")) %>%
            dplyr::rename(var_id = alphabet) %>%
            dplyr::mutate(var_id = forcats::as_factor(var_id))

        } else if(out$type %in% c("categorical")) {
          props_ <- props_ %>%
            dplyr::left_join(y = txt_$labels %>%
                               dplyr::select(level, alphabet),
                             by = c("levels" = "level")) %>%
            dplyr::rename(lvl_id = alphabet) %>%
            dplyr::mutate(lvl_id = forcats::as_factor(lvl_id))
        }
      }  # END if("alphabet" ...)

      # Wrapping short LABELS (from dictionary).
      # See `cdrs_plt_txt` in this document for more details.
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
        props_ <- props_ %>%
          dplyr::left_join(y = txt_$labels %>%
                      dplyr::select(Variable, short_label),
                    by = c("variable" = "Variable")) %>%
          dplyr::rename(var_id = short_label) %>%
          dplyr::mutate(var_id = forcats::as_factor(var_id))

      }  # END if("short_label"

      # Wrapping short LEVELS (from dictionary).
      if ("short_level" %in% names(txt_$labels)) {
        # determine first if we should str_wrap text
        should_wrap <- txt_$labels$short_level %>%
          unique() %>%
          purrr::map_vec(
            .x = .,
            .f = ~ stringr::str_length(.x) > legend_wrap
            )

        should_wrap <- T %in% should_wrap

        if(should_wrap) {
          txt_$labels$short_level <- purrr::map_vec(
            .x = txt_$labels$short_level,
            .f = ~ stringr::str_wrap(.x, width = legend_wrap)
          )
        }

        props_ <- props_ %>%
          dplyr::left_join(
            y = txt_$labels %>%
              dplyr::select(level, short_level),
            by = c("levels" = "level")
          ) %>%
          dplyr::rename(lvl_id = short_level) %>%
          dplyr::mutate(lvl_id = forcats::as_factor(lvl_id))
      }
    } else {
      # When no 'labels' from cdrs_plt_txt provided,
      # we have one of two cases:
      # 1) label_form is NULL.
      # So we specify that we will have no y-axis text labels
      # (ie. theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()))
      # ...OR...
      # 2) label_form is "default".
      # So, for variables where we use levels,
      # we want to make sure to wrap long text,
      # but otherwise, we don't need to do anything
      # (so we get labels as is, eg "Q1_1").

      # if we assign txt_options...
      if (!inherits(txt_options, "NULL")) {
        # and if we assign txt_options$label_form to be NULL,
        # we want to make sure yaxis is turned off.
        if (inherits(txt_options$label_form, "NULL")) {
          out$yaxis <- FALSE
        }
      }

      # Wrap LEVELS for Categorical Variables
      if(out$type %in% c("categorical")) {
        props_ <- props_ %>%
          mutate(levels = stringr::str_wrap(levels,
                                            width = axis_wrap) %>%
                   forcats::as_factor())
      }

      # else if(out$type %in% c("dichotomous")){
      #   props_ <- props_ %>%
      #     mutate(variable = stringr::str_wrap(variable,
      #                                         width = axis_wrap) %>%
      #              forcats::as_factor())
      # }

    }


    # convert `variable` to factor
    props_$variable <- forcats::as_factor(props_$variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Set `props_` Factors ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # We perform a few `as_factor` and other factor-related operations on
    # `props_` before we finish it up.
    if(out$type %in% c("diverging")){
      # flip levels around
      if("var_id" %in% names(props_)){
        props_ <- props_ %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(levels = forcats::fct_rev(levels)) %>%
          dplyr::mutate(percent_lab = forcats::as_factor(percent_lab)) %>%
          dplyr::mutate(var_id = forcats::as_factor(var_id)) %>%
          dplyr::arrange(variable, levels) %>%
          dplyr::ungroup()
      } else {
        props_ <- props_ %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(levels = forcats::fct_rev(levels)) %>%
          dplyr::mutate(percent_lab = forcats::as_factor(percent_lab)) %>%
          # dplyr::mutate(var_id = forcats::as_factor(var_id)) %>%
          dplyr::arrange(variable, levels) %>%
          dplyr::ungroup()
      }
    }

    if(out$type %in% c("ordinal", "diverging")){
      props_ <- props_ %>%
        dplyr::mutate(percent_lab = forcats::as_factor(
          as.character(percent_lab)))
    }

    out <- append(out, txt_)
    out <- append(out, list(
      props = props_
      ))

    # End of `if(inherits(dict_, "data.frame")){`
  } else {
    # Simply append the proportions if no dictionary is supplied.
    out <- append(out, list(props = props_))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Final caption/title edit ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Let's note here if the data are unweighted.
  if (("caption" %in% names(out)) &
      !is_weighted) {
    captions$wgt <- "The results depicted here are unweighted. This means that the depicted values have not be adjusted to proportionately represent all Delta residents."
  }

  if(
    ("title" %in% names(out)) &
    !is_weighted
  ){
    out$title <- paste0(out$title, " (Unweighted) ")
  }

  # add captions back into `out`
  if("captions" %in% names(txt_)){
    out$captions <- append(
      txt_$captions,
      captions
    )
  } else {
    out$captions <- captions
  }

  # Convert NA values in captions to NULL so that when paste0() is executed,
  # these values are dropped.
  out$captions <- purrr::map(
    .x = out$captions,
    .f = function(x){
      if(NA %in% x){
        NULL
      } else {
        x
      }
    })

  # Remove NULL values from list of captions
  out$captions <- out$captions[
    purrr::map_lgl(.x = out$captions,
                   .f = ~!is.null(.x))
  ]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return ----
  out
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot a pie chart with ggplot2.
#'
#' @param prep_ the tibble returned from `cdrs_plt_prep.`
#' @return an object of class ggplot.
#' @export
cdrs_plt_pie <- function(
    prep_
){

  prep_ <- pal_main(
    prep_ = prep_
  )

  # extract palette
  plt_pal <- prep_$props$pal %>% unique()

  # Determine level id column
  if("lvl_id" %in% names(prep_$props)){
    fill_ <- "lvl_id"
  } else {
    fill_ <- "levels"
  }

  plt_ <- ggplot2::ggplot(
    data = prep_$props,
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
      # label.padding = ggplot2::unit(0.15, "lines"),
      fill = "#ffffff",
      # color = "#333333",
      size = prep_$title_size * 0.7,
      label.size = 0,
      size.unit = "pt",
      na.rm = T
      ) +
    # add colors
    ggplot2::scale_fill_manual(values = plt_pal) +
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

#' Dichotomous Question Bar Plots with ggplot2.
#'
#' Plot questions from the DRS data set with only one level of interest (eg. questions where we only want to show "Yes" responses).
#'
#' @param prep_ the tibble returned from `cdrs_plt_prep.`
#' @return an object of class ggplot.
#' @export
cdrs_plt_bar <- function(
    prep_
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Input validation ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(inherits(prep_, "list"))
  stopifnot(c("type", "props") %in% names(prep_))
  if(prep_$type != "dichotomous"){
    warning(
      stringr::str_glue(
        "Passing {prep_$type} type plot into cdrs_plt_bar."
      )
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add color palette to prep_$props
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  prep_ <- pal_main(
    prep_ = prep_,
    reverse_ = TRUE
  )

  # extract palette
  plt_pal <- prep_$props$pal %>%
    unique()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine which column in the "proportions" (ie. prep_$props) tibble should
  # be the Y-axis variable. This can be displayed as a QID,
  # or a short piece of text (ie. `var_id).
  # (It may also be missing entirely if prep_$yaxis is FALSE,
  # but we still need to set the y variable column for the graph,
  # even as we drop any text labels.)
  if("var_id" %in% names(prep_$props)){
    y_ <- "var_id"
  } else {
    warning(
      paste0(
        "In cdrs_plt_bar, the `variable` column was selected to be displayed.",
        " Ideally, `var_id` should be selected."
      )
    )
    y_ <- "variable"
  }

  # Create in-graph labels.
  prep_$props <- prep_$props %>%
    # Create a label for high value labels, eg 81%
    mutate(txt_hi = ifelse(percent > 80, paste0(percent, "%"), NA)) %>%
    # and low value labels, eg 79%.
    mutate(txt_lo = ifelse(percent <= 80, paste0(percent, "%"), NA))

  prep_$props <- prep_$props %>%
    # As `txt_hi` values will overlap bars in a barplot,
    # we need to determine their color.
    # TODO plot background awareness.
    # Assumes plot background is white!
    mutate(txt_color = dplyr::case_when(
      !is.na(txt_hi) & dark_pal ~ "#ffffff",
      .default = "#000000"
    ))

  # create ggplot2 object
  plt_ <- ggplot2::ggplot(
    data = prep_$props,
    mapping = ggplot2::aes(
      x = percent,
      y = !!rlang::sym(y_),
      fill = variable
    )) +
    ggplot2::geom_bar(
      stat = "identity"
    ) +
    ggplot2::scale_fill_manual(
      values = rep(plt_pal, nrow(prep_$props))
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add geom_text (percent floating on graph)

  # approx. font size ratio.
  max_char <- plt_ratio(prep_$title_size)

  # size of label
  # (Proportion this using plt_ratio because as title size increases,
  # the graph proportionally decreases.)
  label_size <- floor(max_char * .05)

  # geom_text relative to top of bar (in units of percent).
  prep_$props <- prep_$props %>%
    dplyr::mutate(label_hjust = dplyr::case_when(
      !is.na(txt_hi) ~ label_size * 1.8,
      .default = label_size * 0.8
    ))

  # add geom_text
  plt_ <- plt_ +
  # Text label high
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        x = percent - label_hjust,
        label = txt_hi,
      ),
      color = prep_$props$txt_color,
      data = prep_$props,
      size = label_size,
      # position = ggplot2::position_dodge2(width = 0.5, reverse = TRUE),
      na.rm = TRUE,
      hjust = 0
    ) +
    # Text label low
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        x = percent + label_hjust,
        label = txt_lo,
      ),
      color = prep_$props$txt_color,
      data = prep_$props,
      size = label_size,
      # position = ggplot2::position_dodge2(width = 0.5, reverse = TRUE),
      na.rm = TRUE,
      hjust = 0
    )
    # ggplot2::geom_text(
    #   mapping = ggplot2::aes(
    #     x = .percent,
    #     label = .txt_lo,
    #     # group = Zone,
    #     y = .txt_lo + label_vjust
    #   ),
    #   data = prep_$props,
    #   size = label_size,
    #   position = ggplot2::position_dodge2(
    #     width = 0.5,
    #     reverse = T
    #   ),
    #   na.rm = T
    # )

  # return
  plt_
}

#' Creates stacked bar plot with ggplot2.
#'
#' Creates stacked bar plot which shows all levels of a variable. Used for both qualitative and likert scale responses.
#'
#' @param prep_ the tibble returned from `cdrs_plt_prep.`
#' @return an object of class ggplot.
#' @export
cdrs_plt_stacked <- function(
    prep_
){

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine which column in the "proportions" (ie. prep_$props) tibble should
  # be the Y-axis variable. This can be displayed as a QID,
  # or a short piece of text (ie. `var_id).
  # (It may also be missing entirely if prep_$yaxis is FALSE,
  # but we still need to set the y variable column for the graph,
  # even as we drop any text labels.)
  if("var_id" %in% names(prep_$props)){
    y_ <- "var_id"
  } else {
    warning(
      paste0(
        "In cdrs_plt_bar, the `variable` column was selected to be displayed.",
        " Ideally, `var_id` should be selected."
      )
    )
    y_ <- "variable"
  }

  prep_ <- pal_main(
    prep_ = prep_
  )

  # extract palette
  plt_pal <- prep_$props$pal %>% unique()

  # Set up data for geom_label()
  # (Note, the problem is placing the label correctly
  # is actually quite difficult. This function determines where
  # along the line between 1:100 or in our case 0:1 a label
  # should exist.)
  prep_$props <- prep_$props %>%
    dplyr::group_by(!!rlang::sym(y_)) %>%
    mutate(pos = cumpos(mean)/100)

  # Create a label for some legend items.
  # This occurs when the in-graph labels have to be cut due to
  # space. In such cases, where the label has been omitted,
  # we want to add the percent to the legend instead.
  prep_$props <- prep_$props %>%
    dplyr::mutate(legend_lab = dplyr::case_when(
                  percent < prep_$label_threshold ~ paste0(levels,
                                                           " (",
                                                           percent,
                                                           "%)"),
                  .default = levels)
    )

  # Create plot.
  plt_ <- ggplot2::ggplot(
    data = prep_$props,
    mapping = ggplot2::aes(
      x = mean,
      y = !!rlang::sym(y_),
      fill = levels
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack()
    ) +
    ggplot2::scale_fill_manual(
      values = plt_pal,
      # guide = ggplot2::guide_legend(reverse = TRUE),
      labels = prep_$props$legend_lab
      ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", "25%", "50%", "75%", "100%"),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        # importantly, here we provide the position
        # that we derived manually.
        x = pos,
        label = percent_lab
        ),
      size = prep_$title_size * 0.7,
      size.unit = "pt",
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
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 1,
        l = 0,
        unit = "lines"
      ),
      # Adjusting bottom margin
      # legend.spacing.y = grid::unit(2, "lines"),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "#333333",
        linewidth = 1,
        fill = NA
      ),
      plot.margin = grid::unit(c(1,  # t
                                 1,  # r
                                 1,  # b
                                 1), # l
                               'lines')
    )

  # return
  plt_
}

#' Prepare and plot a histogram with ggplot2.
#'
#' Prepare and create a histogram. Unlike other plots, histograms do not take
#' objects from `cdrs_plt_prep`.
#'
#' @param data_ tibble. the DRS data.
#' @param col_ character. A vector of the columns of interest.
#' @param dict_ tibble. the data dictionary. If `NULL` no plot label decoration performed. In other words, the plot will not display textual descriptions.
#' @param txt_options list. Either `NULL` or a list providing parameters for `cdrs_plt_txt()`.
#' @param title_size numeric. The size of the font for the title. All other fonts scale linearly to title_size, even if a title isn't included.
#' @param param_file character. Path to custom parameters xlsx document. See inst/extdata/plot_parameters.xlsx for file structure.
#' @return ggplot2 object.
cdrs_plt_hist <- function(
    data_,
    col_,
    dict_ = NULL,
    txt_options = NULL,
    title_size = 14,
    param_file = system.file("extdata",
                             "plot_parameters.xlsx",
                             package = "cdrs")
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Input validation ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(inherits(col_, "character") &
              length(col_) > 0)

  stopifnot(inherits(data_, "data.frame"))

  stopifnot(inherits(dict_, "data.frame") |
              inherits(dict_, "NULL"))

  stopifnot(inherits(txt_options, "list") |
              inherits(txt_options, "NULL"))

  if(!inherits(txt_options, "NULL")){
    if(!inherits(txt_options$label_form, "NULL")){
      stopifnot(txt_options$label_form %in% c(
        "short",
        "default"
      ))
    }
  }

  stopifnot(inherits(title_size, "numeric"))

  stopifnot(
    inherits(param_file, "character") |
      inherits(param_file, "data.frame")
  )

  if(inherits(param_file, "data.frame")){
    stopifnot(
      names(param_file) %in% c("Variable",
                               "id",
                               "full_title",
                               "short_title",
                               "label",
                               "short_label",
                               "level",
                               "short_level")
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup ggplot data obj ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # create survey design object
  design_ <- cdrs_design(
    data_ = data_
  )

  # Create svyhist object to obtain more accurate estimates
  # (compared to using ggplot2::aes(weight)).
  # Note, we derive the svyhist object to access its attributes,
  # and we place survey::svyhist() in this function to prevent
  # the automatic plotting that occurs.
  svyhist_obj <- svyhist.invisible(design_ = design_,
                                   col_ = col_)

  # Extract the breaks and counts
  brks <- svyhist_obj$breaks
  cnts <- svyhist_obj$counts

  # Determine plot size
  # Y (expand by 10%)
  ymax <- round(max(cnts, na.rm = T) * 1.2)
  # X (expand by one break size)
  brk_size <- mean(diff(brks))
  xmax <- brks[length(brks)] + brk_size

  # Create a data frame for ggplot2
  hist_tb <- tibble::tibble(
    breaks = brks[-length(brks)],  # Exclude the last break
    counts = cnts
  ) %>%
    dplyr::mutate(counts = round(counts)) %>%
    dplyr::filter(counts != 0)

  # Adjust the breaks to align with geom_bar
  hist_tb$breaks_adj <- hist_tb$breaks +
    (brk_size / 2)

  # Plotting Setup ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Run cdrs_plt_txt ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Retrieve descriptive details for plotting labels.
  # (Creates `txt_` object.)
  if(inherits(dict_, "data.frame")){

    if(inherits(txt_options, "NULL")){
      txt_ <- cdrs_plt_txt(
        dict_ = dict_,
        cols_ = col_,
        label_form = "short",
        title_form = NULL,
        subtitle_ = F,
        caption_ = F,
        param_file = param_file
      )
    } else {
      # Before we run cdrs_plt_txt, we have to do some
      # jerry-rigging because that function is not set up
      # for numeric variables
      # (largely because of missing information in dict)
      default_label <- FALSE
      if("label_form" %in% names(txt_options)){
        if(txt_options$label_form == "default"){
          txt_options$label_form <- "short"
          default_label <- TRUE
        }
      }

      txt_ <- do.call(
        what = cdrs_plt_txt,
        args = c(
          list(
            dict_ = dict_,
            cols_ = col_,
            param_file = param_file
          ),
          txt_options
        )
      )
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot ----
  # Create the ggplot2 histogram
  plt_ <- ggplot2::ggplot(
    data = hist_tb,
    mapping = ggplot2::aes(x = breaks_adj,
                           y = counts)) +
    ggplot2::geom_bar(stat = "identity",
                      # TODO add custom colors using plot_parameters.xlsx
                      fill = "#009E73",
                      alpha = 1,
                      width = brk_size) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = round(counts, 1)),
      vjust = -0.5) +
    ggplot2::scale_x_continuous(
      limits = c(0, xmax),
      breaks = seq(0, xmax, by = brk_size),
      expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      limits = c(0, ymax),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = {
        if(default_label){
          # eg. Years in the Delta
          txt_$labels$label
        } else if(inherits(txt_options, "NULL")){
          # eg. (blank)
          NULL
        } else {
          # eg. Years
          txt_$labels$short_label
        }
      },
      y = "Count",
      title = {
        if("title" %in% names(txt_)){
          txt_$title
        } else {
          NULL
        }
      }) +
    ggplot2::theme_bw()

  plt_ <- plt_decorate(
    plt_ = plt_,
    prep_ = list(
      title_size = title_size,
      type = "numeric",
      title = txt_$title,
      subtitle = txt_$subtitle,
      captions = txt_$captions,
      yaxis = TRUE
    )
  )

  # remove vertical lines
  plt_ <- plt_ +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )

  # return
  plt_
}
