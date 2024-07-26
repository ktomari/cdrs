# (Exported) plot set up functions and
# wrappers for ggplot() functions.

#' Create text for plots.
#'
#' Create different kinds of text decoration for your plot, including labels that appear along the graph axis, titles, subtitles & captions.
#'
#' @param dict_ is the data dictionary. All columns in `cols_` must be present in `dict_$Variable`.
#' @param cols_ character. variable/column names of the DRS data.
#' @param label_form character. Options include "short", "alphabet", "default", "qid", and `NULL`. These labels correspond to the table created by `plt_labels()`. If "short", either the variable `short_label` or `short_lvl` is drawn from the labels table and a table for recoding factors is returned accordingly. If "alphabet", a table of for recoding factors aligned with alphabetical characters is returned. If "default", a table with the full labels as they appear in the survey is returned. If "qid", then a table with qid labels (eg. Q1_0) is returned. If `NULL`, no table is returned because the y-axis label will be blank.
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
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # SHORT: Hand-tailored shortened labels/levels..
      # For details, examine the plot_parameters.xlsx > "labels" tab.
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Candidates for removal
      drop_cols <- c("short_title", "full_title")

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Note, we keep `level` and `label`
      # because we need to do a join later.
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # drop candidates (above) and empty columns
      out$lab_df <- labs %>%
        dplyr::select(-tidyselect::any_of(drop_cols)) %>%
        dplyr::select(tidyselect::where(~!all(is.na(.))))

    } else if (label_form == "alphabet"){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      out$lab_df <- labs %>%
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
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # DEFAULT: Essentially, the longer labels/levels,
      # usually derived directly from the actual survey options.
      # For details, examine the plot_parameters.xlsx > "labels" tab.
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Candidates for removal
      drop_cols <- c("short_title",
                     "full_title",
                     # unlike in label_form == "short,
                     # we remove the `short_label` and `short_level`
                     # because we do not need them.
                     "short_label",
                     "short_level")

      # drop candidates (above) and empty columns
      out$lab_df <- labs %>%
        dplyr::select(-tidyselect::any_of(drop_cols)) %>%
        dplyr::select(tidyselect::where(~!all(is.na(.))))
    }

  } else {
    # In this case, default factors are used, and because
    # of this we don't need labels_
    out$lab_df <- NULL
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
        if("level" %in% names(out$lab_df)){
          cap_alpha <- out$lab_df %>%
            dplyr::mutate(alpha_txt = paste0(alphabet, ". ", level))
        } else {
          cap_alpha <- out$lab_df %>%
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
  # Design considerations
  # - Most objects are stored in a centralized list object that is passed
  # between functions.
  # - An exception to the above is made for the data (tibble). Once this data
  # has been converted into a plot friendly data structure, it is not needed
  # beyond the limits of this function.
  # - Due to the complexity of operations in this function, this function
  # relies on several other helper functions, spread out between this script
  # and utils_plot_wrappers. Generally speaking, functions in this script are
  # made to be accessible to package users who may need different components
  # to adjust text on plots, or produce more advanced shiny applications.
  # - Conceptually, these are groups of operations and functions performed
  # in this function:
  # a. There are a few functions called in `cdrs_plt_prep` that
  # draw metadata on the CDRS survey data from plot_parameters.xslx. These
  # operations select the right textual format and combine them with the core
  # output data structure (ie. with the props object).
  # b. There are functions and operations that work specifically with numeric
  # survey variables, eg Q1a.
  # c. There are functions and operations that work on variable that produce
  # weighted proportions (ie. `props`).
  # d. And finally there is a list that keeps track of information for the
  # caption.

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
  # primary output list
  prep_ <- list()
  # captions output list
  prep_$captions <- list()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # store dictionary to output
  if(!inherits(dict_, "NULL")){
    prep_$dict <- dict_

    # Strip dictionary of unneeded columns
    prep_$dict <- prep_$dict %>%
      dplyr::filter(Variable %in% cols_)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # store variable of interest
  prep_$cols <- cols_

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add static elements
  prep_$title_size <- title_size

  # The character limit before text wrap is performed...
  # TODO adjust to scale with changing font size.
  # y-axis wrap limit
  prep_$axis_wrap <- 20
  # legend wrap limit
  prep_$legend_wrap <- 20

  # Percent threshold for geom_text before omission.
  # For more information, see cdrs_plt_stacked.
  prep_$label_threshold <- label_threshold

  # Y-axis text labels, eg. Resident, Farms in the Delta, etc.
  # By default, let's make sure the y-axis is visible.
  # (There are cases where it defaults to the qid, which does not
  # contribute anything meaningful. In those cases,
  # we'll set it to `FALSE`.)
  prep_$yaxis <- TRUE

  # Assign sorting indicator (logical)
  prep_$sort <- sort_

  # Assign text options (for cdrs_plt_txt)
  prep_$txt_options <- txt_options

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign captions based on inputs.
  # Let's note here if the data are unweighted.
  if(!is_weighted) {
    prep_$captions$wgt <- "The results depicted here are unweighted. This means that the depicted values have not be adjusted to proportionately represent all Delta residents."
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot Logic ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This operation involves obtaining the pertinent metadata information
  # from plot_parameters.xlsx (or user-provided parameters).
  # This includes information like, "what kind of plot?"
  # For example, "categorical" or which palette to use.
  # IN: utils_plot_wrappers.R.
  # RETURN: tibble.
  # Sample row from tibble:
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Variable | plot_type1  | pal_id   | affirmative_level | notes
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Q4_0     | dichotomous | purples1	| Yes               |
  prep_$logic <- plt_logic(
    file_ = param_file,
    cols_ = prep_$cols
  )

  # Set plot type (for quick-access)
  prep_$type <- prep_$logic$plot_type1 %>%
    unique()

  # Error check.
  # As of the time of this writing (2024-07-23),
  # there is no option to display an unweighted histogram.
  if(prep_$type %in% "numeric" & !is_weighted){
    stop("Only the weighted version of numeric variables are permitted.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove angle brackets
  # Missingness variables stored as factors/character vectors
  # which include levels like "<Decline to answer>" need to reformatted,
  # eg. "Decline to answer"
  if(remove_angle_brackets &
     prep_$type %in% c("ordinal",
                       "diverging",
                       "dichotomous",
                       "categorical")){

    data_ <- remove_angle_brackets(
      data_ = data_,
      cols_ = prep_$cols
    )
    prep_$dict <- remove_angle_brackets(
      data_ = prep_$dict,
      cols_ = "value"
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # > cdrs_plt_txt ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Retrieve descriptive details for plotting labels.
  # (Creates `txt_` object.)
  if(inherits(prep_$dict, "data.frame")){

    if(inherits(prep_$txt_options, "NULL")){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 1) txt_options not provided
      prep_$plt_txt <- cdrs_plt_txt(
        dict_ = prep_$dict,
        cols_ = prep_$cols,
        label_form = "short",
        title_form = NULL,
        subtitle_ = F,
        caption_ = F,
        param_file = param_file
      )
    } else {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 2) txt_options provided as argument to this fn, cdrs_plt_prep.

      # Special adjustment for numeric variable, eg Q1a.
      if(prep_$type %in% "numeric"){

        # The issue here is that there is little metadata in the dictionary
        # for the numeric variables.
        if("label_form" %in% names(prep_$txt_options)){
          if(prep_$txt_options$label_form == "default"){
            warning("label_form cannot be 'default' for numeric columns.")
            prep_$txt_options$label_form <- "short"
          }
        }
      }

      # run cdrs_plt_txt with txt_options
      prep_$plt_txt <- do.call(
        # Use this function...
        what = cdrs_plt_txt,
        # with these arguments.
        args = c(
          list(
            dict_ = prep_$dict,
            cols_ = prep_$cols,
            param_file = param_file
          ),
          prep_$txt_options
        )
      )
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Final operations for plt_txt

    prep_$captions <- append(
      prep_$captions,
      prep_$plt_txt$captions
    )

    prep_$plt_txt$captions <- NULL

    # TODO add font size calculator here.
    prep_$title <- prep_$plt_txt$title
    prep_$plt_txt$title <- NULL
    prep_$subtitle <- prep_$plt_txt$subtitle
    prep_$plt_txt$subtitle <- NULL

  }  # END if(inherits(prep_$dict, "data.frame")){

  # Adjust title
  if(
    ("title" %in% names(prep_)) &
    !is_weighted
  ){
    prep_$title <- paste0(prep_$title, " (Unweighted)")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # numeric bypass ----
  # Since numeric data doesn't produce "proportions",
  # we bypass it here with an early `return()`.
  if(prep_$type %in% "numeric"){

    prep_$hist_tb <- cdrs_hist_table(
      data_ = data_,
      col_ = cols_
    )

    prep_$captions$wgt <- "This is a sample-weighted histogram. This means that the heights of the bars represent the estimated proportion of the entire population that falls within each bin, adjusted for the sampling weights. Sampling weights account for the fact that some groups in the sample might be overrepresented or underrepresented compared to the actual population. By using these weights, the histogram provides a more accurate representation of the distribution in the entire population, rather than just the sample."

    prep_$captions <- plt_filter_caps(prep_$captions)

    return(prep_)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get Proportions ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get weighted or unweighted proportions of a survey variable,
  # eg Q1_0:Q1_5
  if(prep_$type %in% c(
    "ordinal",
    "diverging",
    "dichotomous",
    "categorical"
  )){
    if(is_weighted){
      prep_$props <- purrr::map_dfr(prep_$cols,
                               ~cdrs_props(data_ = data_,
                                           col_ = .x))
    } else {
      prep_$props <- purrr::map_dfr(prep_$cols,
                               ~get_unwt_props(
                                 data_ = data_,
                                 col_ = .x
                               ))
    }
  } else {
    stop(
      stringr::str_glue(
        "Issue: cdrs_plt_prep cannot handle '{prep_$type}' columns at this time."
      )
    )
  }  # END if(prep_$type %in% c("ordinal"...)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # > enrich_props ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Enrich proportions
  prep_ <- enrich_props(prep_)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Drop NA and NULL values from captions.
  prep_$captions <- plt_filter_caps(prep_$captions)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return ----
  prep_
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

  # TODO incorporate this into prep
  # Should we add a percentage value to the legend?
  add_legend_percent <- TRUE

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


  # To make life easier,
  # let's add percentages to the legend items
  if(add_legend_percent){
    prep_$props <- prep_$props %>%
      dplyr::mutate(legend_lab = paste0(
        !!rlang::sym(fill_),
        " (",
        percent,
        "%)"
      )
      )
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
    ggplot2::scale_fill_manual(
      values = plt_pal,
      labels = if(add_legend_percent){
        prep_$props$legend_lab
      } else {
        NULL
      }
      ) +
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

  # TODO
  base_theme <- sample(c("theme_bw", "theme_dark"))

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
      legend.position = "none",
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
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
  label_size <- (prep_$title_size * 0.6)

  # geom_text relative to top of bar (in units of percent).
  prep_$props <- prep_$props %>%
    dplyr::mutate(label_hjust = dplyr::case_when(
      !is.na(txt_hi) ~ label_size * .80,
      .default = label_size * .50
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
      size.unit = "pt",
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
      size.unit = "pt",
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
    message(
      paste0(
        "Notice. ",
        "In cdrs_plt_stacked, ",
        "the `variable` column was selected to be displayed. ",
        "Ideally, `var_id` should be selected."
      )
    )
    y_ <- "variable"
  }

  # Currently, there is no support for combining plots, and hence
  # there is no support for "alphabetical" labels.
  # Therefore, we'll nullify the y-axis text.
  if("alphabet" %in% prep_$txt_options$label_form){
    prep_$yaxis <- FALSE
    warning("At this time, label_form == 'alphabet' is not supported by cdrs_plt_stacked")
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
    ggplot2::labs(x = NULL,
                  y = NULL) +
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

  # if yaxis if FALSE, we don't want anything displayed
  if(!prep_$yaxis){
    plt_ <- plt_ +
      theme(axis.text.y = ggplot2::element_blank())
  }

  # return
  plt_
}

#' @title Prepare and plot a histogram with ggplot2.
#'
#' @description
#' Creates a ggplot2 histogram with sample-weighted data ultimately dervied from `survey::svyhist`.
#'
#' @param prep_ list. An object returned by `cdrs_plt_prep`.
#' @return ggplot2 object.
cdrs_plt_hist <- function(
    prep_
){

  # Determine size of each cut
  brk_size <- mean(diff(prep_$hist_tb$breaks))

  # Determine plot size
  # Expand Y direction by some percentage
  ymax <- round(max(prep_$hist_tb$rounded_counts, na.rm = T) * 1.2)

  # Expand X direction by one empty bar
  xmax <- prep_$hist_tb$breaks[length(prep_$hist_tb$breaks)] + brk_size

  # Drop empty rows.
  hist_tb <- prep_$hist_tb %>%
    dplyr::filter(counts != 0)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot ----
  # Create the ggplot2 histogram
  plt_ <- ggplot2::ggplot(
    data = hist_tb,
    mapping = ggplot2::aes(x = breaks_adj,
                           y = rounded_counts)) +
    ggplot2::geom_bar(stat = "identity",
                      # TODO add custom colors using plot_parameters.xlsx
                      fill = "#009E73",
                      alpha = 1,
                      width = brk_size) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = rounded_counts),
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
        if(inherits(prep_$plt_txt$lab_df,
                    "data.frame")){
          # eg. Years in the Delta
          prep_$plt_txt$lab_df$label[1]
        } else {
          # eg. (blank)
          NULL
        }
      },
      y = "Count") +
    ggplot2::theme_bw()

  plt_ <- plt_decorate(
    plt_ = plt_,
    prep_ = prep_
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
