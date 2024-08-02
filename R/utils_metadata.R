
#' Helper for remove_angle_brackets
#'
#' @param vect_ is a character or factor.
#' @return character vector.
#' @noRd
remove_vct_brackets <- function(
    vect_){
  # input validation ----
  stopifnot(inherits(vect_, "character") |
              inherits(vect_, "factor"))

  # Pattern to match strings starting and ending with angle brackets
  pattern <- "^<(.*)>$"

  # Replace the matched pattern with the group captured by parentheses
  stringr::str_replace(vect_, pattern, "\\1")
}

#' Removes angle brackets from missing variable levels.
#'
#' Remove angle brackets around missing variable levels. This is often used for plotting functions.
#'
#' @param data_ DRS data.frame, tibble or vector.
#' @param cols_ is a character vector (or `NULL`) containing name of columns of interest.
#' @return the transformed DRS data set (tibble), or transformed factor/character vector.
remove_angle_brackets <- function(
    data_,
    cols_ = NULL
){
  stopifnot(inherits(data_, "data.frame") |
              inherits(data_, "factor") |
              inherits(data_, "character"))

  if(inherits(data_, "factor")){
    # factor ----
    data_ %>%
      forcats::fct_relabel(
        .fun = remove_vct_brackets
      ) %>%
      return()

  } else if(inherits(data_, "character")) {
    # character ----
    data_ %>%
      remove_vct_brackets() %>%
      return()

  } else if(is.null(cols_)){
    # data.frame, no cols_ ----
    # Apply this across all columns.
    # First, get columns with <Missing values>
    fltr <- purrr::map_vec(
      data_,
      ~T %in% stringr::str_detect(.x, "\\<.+\\>"))
    remove_angle_brackets(
      data_ = data_,
      cols_ = fltr[fltr == T] %>%
        names()
    )
  } else {
    # data.frame, with cols_ ----
    # apply to specified column (cols_).
    purrr::map2_dfc(data_, names(data_), function(col_, nm){
      if(nm %in% cols_){
        col_ %>%
          forcats::fct_relabel(
            .fun = remove_vct_brackets
          )
      } else {
        # not a specified column
        col_
      }
    })

  }
}

#' Add a grouping column.
#'
#' Add a new column in the data set based on qid grouping variable. This
#' character column will be filled with values like 1, 1a, 2, 3, and so on.
#'
#' @param dict_ the data dictionary
#' @param var_ the column for the variable ID's.
#' @return tibble of dict_ with new column 'grp'
generate_grp <- function(
    dict_,
    var_ = "Variable"
    ){
  dict_ %>%
    dplyr::mutate(grp = dplyr::case_when(
      stringr::str_detect(Variable, "^Q13") ~ "13",
      stringr::str_detect(Variable, "^Q41") ~ "41",
      stringr::str_detect(Variable, "^Q42") ~ "42",
      .default = stringr::str_extract(Variable,
                                      "(?<=Q)\\d{1,2}[[:alpha:]]*")
    ))
}

#' Creates an enriched data dictionary
#'
#' Creates a data dictionary where question labels are split into their "prompt" and "response" components. This function heavily relies on tidyr's nesting functions.
#'
#' @param dict is the DRS data dictionary.
#' @return a tibble of the DRS data dictionary.
#' @export
enrich_dict <- function(
    dict
){
  # Input validation
  stopifnot(inherits(dict, "data.frame"))

  if(dict$Variable[1] %in% "Zone"){
    warning_c("enrich_dict1", "The dictionary for Zone cannot be enriched.")
    return(dict)
  }

  # Sometimes we subset the dictionary for a specific set of variables.
  # So, we need to see if the "Label" is present.
  if(!("Label" %in% dict$name)){
    # return early.
    return(dict)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # First, create a tibble with:
  # "Variable" the original qid.
  # "prompt_lab" the prompt for the survey question
  # "response_lab" the response for the survey question.
  labs <- dict %>%
    # extract Label rows only
    dplyr::filter(name == "Label") %>%
    # select columns of interest
    dplyr::select(Variable, value) %>%
    # create a grouping column based qid integer+alphabetical,
    # eg. group 1, 1a, 2, etc.
    generate_grp() %>%
    # remove those without groups
    dplyr::filter(!is.na(grp)) %>%
    # lump together by group
    dplyr::group_by(grp) %>%
    # place columns into a nest
    tidyr::nest(nested = c(Variable, value)) %>%
    # add a nested column "clauses" to
    # the original nested column "nested"
    dplyr::mutate(nested = purrr::map(nested, function(grp_tb){
      # Split up `Label` by punctuation + whitespace.
      # returns tibble with:
      # Variable [chr], value [chr], clauses [list]
      grp_tb %>%
        # remove numbering, eg. "41a. "
        dplyr::mutate(value = stringr::str_remove(
          value,
          "^\\d{1,2}[[:alpha:]]*\\.\\s*")) %>%
        # create nested "clauses" column
        dplyr::mutate(clauses = purrr::map(
          value,
          ~stringr::str_split_1(.x,
                                "(?<=\\.|\\?)\\s+")))
    })
    ) %>%
    # created new nested column "dupes" [character vector]
    dplyr::mutate(dupes = purrr::map(nested, function(grp_tb){
      # now track which values are duplicated
      dupes <- grp_tb$clauses %>%
        unlist() %>%
        # subset duplicated values
        .[duplicated(.)] %>%
        # remove repeats
        unique()

      # returns vector
      dupes
    })) %>%
    # flatten dupes into "prompt"
    dplyr::mutate(prompt_lab = dupes %>%
                    unlist() %>%
                    paste0(collapse = " ") %>%
                    stringr::str_squish()
    ) %>%
    # replace empty values in "prompt_lab"
    dplyr::mutate(prompt_lab = dplyr::case_when(
      prompt_lab == "" ~ NA_character_,
      .default = prompt_lab
    )) %>%
    # created flattened 'response_lab' values.
    dplyr::mutate(nested = purrr::map(nested, function(grp_tb){
      grp_tb %>%
        mutate(response_lab = purrr::map_vec(clauses, function(vec){
          if(length(unlist(dupes)) == 0){
            return(NA)
          }

          # remove duplicates from our vector of clauses
          # then collapse it,
          # and return.
          vec[!(vec %in% unlist(dupes))] %>%
            paste0(collapse = " ") %>%
            stringr::str_remove(
              pattern = "\\-\\sSelected\\sChoice\\s"
            )
        })) %>%
        select(Variable, response_lab)
    })) %>%
    # remove (nested) dupes column
    dplyr::select(-dupes) %>%
    # unnest "nested" which contains c("Variable", "response")
    tidyr::unnest(nested) %>%
    dplyr::ungroup() %>%
    dplyr::select(-grp) %>%
    # remove empty rows
    dplyr::filter(!is.na(prompt_lab)) %>%
    # pivot these two columns,
    # producing tibble with cols:
    # Variable, name, value
    tidyr::pivot_longer(cols = prompt_lab,
                        names_to = "name_prompt",
                        values_to = "value_prompt") %>%
    tidyr::pivot_longer(cols = response_lab,
                        names_to = "name_response",
                        values_to = "value_response")

  # Now add these values back into the dictionary.
  out <- dict %>%
    # group by question
    dplyr::group_by(Variable) %>%
    # take all the other columns of the dict,
    # and nest them.
    tidyr::nest(nested = c(name,
                           value,
                           encoding,
                           frequency,
                           percent)) %>%
    # now join the `labs` object, by the qid/Variable.
    dplyr::left_join(labs, by = "Variable") %>%
    # now we'll bind it each "name" and "value" from `labs`
    # to the nested tb.
    dplyr::mutate(nested = purrr::map(
      nested,
      function(grp_tb){
        # check to see if pulling "name" from the top level tb
        # yields these two values:
        # "prompt_lab"
        # "response_lab"
        if(identical(name_response,
                     "response_lab")){
          # take the nested tb
          grp_tb %>%
            # bind a new tibble composed of
            # name: c("prompt_lab", "response_lab")
            # value: c(...)
            # eg. c("Please select a residential designation",
            # "Urban")
            dplyr::bind_rows(
              tibble::tibble(
                name = c(name_prompt, name_response),
                value = c(value_prompt, value_response)
              )
            )
        } else {
          # if there are no labs split up...
          # return the tb.
          grp_tb
        }
      })) %>%
    # de-select name and value (from `labs`)
    dplyr::select(Variable, nested) %>%
    # Because each `Variable` is duplicated when `left_join`
    # occurs, we must remove duplicate rows.
    dplyr::distinct() %>%
    # unnest and ungroup
    tidyr::unnest(nested) %>%
    dplyr::ungroup()

  # return
  out
}

#' Revise data dictionary based on changed values in data.
#'
#' Following systematic changes in the data (such as those performed in `cdrs_revise`), revise the data dictionary to reflect actual data values.
#'
#' @param data_ revised DRS data.
#' @param dict_ original data dictionary
#' @return tibble (dict)
revise_dict <- function(
    data_,
    dict_
){
  # arg validation
  stopifnot(inherits(data_, "data.frame"))
  stopifnot(inherits(dict_, "data.frame"))

  # augment dictionary
  dict_ %>%
    # place all columns except variable into 'nested'
    tidyr::nest(.by = Variable,
                    .key = "nested") %>%
    mutate(nested = purrr::map2(nested, Variable, function(tb, var_){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Early skip.

      # derive dictionary class.
      dict_class <- tb %>%
        dplyr::filter(name == "R Class") %>%
        dplyr::pull(value)

      # Skip variables that are listed in the dictionary as a
      # non-factor R class.
      # (Why? They would not have had any changes done upon them
      # in cdrs_revise.)
      if(!(dict_class %in% c("factor", "factor - numeric"))){
        return(tb)
      }

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Start by getting data
      dat1 <- data_ %>%
        dplyr::pull(tidyselect::all_of(var_))

      # Now get data without NA
      dat2 <- dat1[!is.na(dat1)]

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Get descriptive stats.
      info <- list()

      # R class
      info$rclass <- tibble::tibble(
        name = "R Class",
        value = class(dat1)
      )

      # Total N
      info$total_n <- tibble::tibble(
        name = "Total (n)",
        value = length(dat1) %>%
          as.character()
      )

      # NA count
      info$sys_miss <- tibble::tibble(
        name = "System (missing)",
        value = which(is.na(dat1)) %>%
          length() %>%
          as.character()
      )

      # On rare occasions, column is all NA.
      if(as.integer(info$total_n$value) ==
         as.integer(info$sys_miss$value)){
        # set default dictionary values
        tb$value[tb$name == "Total (n)"] <- info$total_n$value
        tb$value[tb$name == "System (missing)"] <- info$sys_miss$value
        # return early
        return(
          tb %>%
            dplyr::filter(name %in% c(
              "Label",
              "R Class",
              "Total (n)",
              "System (missing)"
            ))
        )
      }

      # Valid Responses
      info$valid_n <- tibble::tibble(
        name = "Valid Responses",
        value = dat2 %>%
          length() %>%
          as.character()
      )

      # What unique values are present?
      uniq_ <- dat2 %>%
        unique()

      # Unique Values N
      info$unique_n <- tibble::tibble(
        name = "Unique Values",
        value = uniq_ %>%
          length() %>%
          as.character()
      )

      # Which <missingness> values were originally in the data?
      # (Used for info$notes)
      original_missing <- tb %>%
        dplyr::filter(name == "factors",
                      stringr::str_detect(value, "^\\<.+\\>$")) %>%
        dplyr::pull(value)


      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Begin keeping notes.
      info$notes <- ifelse(
        test = F %in% (original_missing %in% uniq_) &
          info$sys_miss$value > 0,
        yes = "For this revised data set, 'System (missing)' no longer signifies that a respondent 'never saw the question'.",
        no = ""
      )

      if("Notes" %in% tb$name){
        info$notes <- c(
          tb$value[tb$name == "Notes"],
          info$notes
        )
      }

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Changes in factors
      if(
        info$rclass$value == "factor" &
        as.integer(info$unique_n$value) < 10
      ){

        # Original factors
        fct1 <- tb$value[tb$name == "factors"]

        # Current factors
        fct2 <- unique(dat2) %>%
          as.character()

        # Dropped factors
        dropped_fct <- fct1[!(fct1 %in% fct2)]

        # if there are dropped factors
        if(!identical(dropped_fct, character(0))){
          info$notes <- c(
            info$notes,
            stringr::str_glue(
              "Dropped factor(s): {paste0(dropped_fct, collapse = ', ')}."
            )
          )
        }

        # What is the frequency for the current factors?
        fcts <- table(dat2) %>%
          tibble::as_tibble()

        stopifnot(ncol(fcts) == 2)

        names(fcts) <- c("value", "frequency")

        tb_join <- tb %>%
          dplyr::filter(name == "factors") %>%
          dplyr::select(value, encoding)

        fcts <- fcts %>%
          dplyr::mutate(name = "factors") %>%
          dplyr::mutate(percent = paste0(
            (frequency/length(dat1) * 100),
            "%"
          )) %>%
          dplyr::left_join(
            y = tb_join,
            by = "value"
          ) %>%
          dplyr::select(name, value, encoding, frequency, percent)

        info$fct <- purrr::map_dfc(fcts, ~as.character(.x))

      }  # end factors

      # Convert Notes to a tibble.
      info$notes <- tibble::tibble(
        name = "Notes",
        value = paste0(info$notes, collapse = " ")
      )

      # bind rows
      info <- dplyr::bind_rows(info)

      # finally, add rows that `tb` has,
      # but that `info` doesn't have.
      tbn <- tb$name %>% unique()
      ifn <- info$name %>% unique()

      # get `name` exclusively in tb
      tbn <- tbn[!(tbn %in% ifn)]
      tbn <- tbn[stringr::str_detect(tbn, "factors", negate = T)]

      supplement <- tb %>%
        dplyr::filter(name %in% tbn)

      info <- dplyr::bind_rows(info, supplement)

      # return
      info
    })) %>%
    tidyr::unnest(nested)
}
