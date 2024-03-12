example_data_list <- cdrs_read_example(return_dict = T)
example_data <- example_data_list$data
example_dict <- example_data_list$dict

test_that("qual_pal. Okabe-Ito 5, seed = 1.", {

  pal <- c(
    black = "#000000",
    bluishgreen = "#009E73",
    reddishpurple = "#CC79A7",
    orange = "#E69F00",
    blue = "#0072B2"
  )

  expect_identical(
    object = qual_pal(5, seed = 1),
    expected = pal
  )
})

test_that("get_unwt_props. Demo SEX_P.",{
  props_ <- example_data %>%
    filter(!is.na(SEX_P)) %>%
    dplyr::group_by(SEX_P) %>%
    dplyr::reframe(Count = dplyr::n()) %>%
    mutate(mean = (Count/sum(Count))) %>%
    dplyr::rename(levels = SEX_P) %>%
    mutate(variable = "SEX_P") %>%
    select(variable, levels, Count, mean)

  # Add textual detail for plotting.
  props_ <- props_ %>%
    mutate(percent = round(mean * 100)) %>%
    mutate(percent = paste0(percent, "%"))

  expect_identical(
    object = get_unwt_props(example_data, "SEX_P"),
    expected = props_
  )

})

test_that("cdrs_plt_txt - NULL",{
  expect_identical(
    object = cdrs_plt_txt(
      dict_ = example_dict,
      cols_ = paste0("Q1_", 0:5),
      qid_labels = NULL,
      title_ = NULL,
      subtitle_ = FALSE,
      caption_ = FALSE
    ),
    expected = list()
  )
})

# TODO fix
# test_that("cdrs_plt_prep - dichotomous",{
#
#   short_label <- plt_labels() %>%
#     dplyr::filter(Variable %in% paste0("Q1_", 0:5)) %>%
#     dplyr::pull(short_label)
#
#   expected_ <- tibble::tibble(
#     variable = paste0("Q1_", 0:5),
#     levels = factor(rep("Yes", 6)),
#     percent_lab = paste0(c(69, 49, 45, 50, 44, 43), "%"),
#     var_id = short_label
#   )
#
#   expect_identical(
#     # note, we do a light manipulation of the `object`
#     # because those columns are either critical, or
#     # as in the case of percent_lab, indicate some calculation,
#     # without forcing us to worry about decimal differences.
#     object = {
#       cdrs_plt_prep(
#       data_ = example_data,
#       cols_ = paste0("Q1_", 0:5),
#       dict_ = example_dict,
#       remove_angle_brackets = TRUE,
#       is_weighted = TRUE
#     ) %>%
#       `[[`("prep") %>%
#       dplyr::select(variable, levels, percent_lab, var_id) %>%
#       droplevels()
#       },
#     expected = expected_
#   )
# })
