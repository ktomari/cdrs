
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
        ~stringr::str_remove_all(., "[<>]")
      ) %>%
      return()

  } else if(inherits(data_, "character")) {
    # character ----
    data_ %>%
      stringr::str_remove_all(., "[<>]") %>%
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
            .fun = ~stringr::str_remove_all(.x, "^\\<|\\>$")
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

