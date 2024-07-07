example_data_list <- cdrs_read_example(return_dict = T)
example_data <- example_data_list$data
example_dict <- example_data_list$dict

test_that("qual_pal. Okabe-Ito 5, seed = 1.", {

  pal <- c(
    "#000000",  # black =
    "#009E73",  # bluishgreen =
    "#CC79A7",  # reddishpurple =
    "#E69F00",  # orange =
    "#0072B2"  # blue =
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
    mutate(percent_lab = forcats::as_factor(paste0(percent, "%")))

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
      label_form = NULL,
      title_form = NULL,
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

# test_that("cdrs_map_prep - Q1_1",{
#   # run test fun
#   obj <- cdrs_map_prep(
#     data_ = example_data,
#     col_ = "Q1_1",
#     geo_var = "geoid.county"
#   )
#
#   # Note, we're only comparing `stat` output,
#   # rounded to 6 digits.
#   expected_ <- c(0.3711247,
#                  0.5631657,
#                  0.2400812,
#                  0.2513739,
#                  0.6821373) |>
#     round(digits = 6)
#
#
#   expect_equal(
#     object = round(
#       obj$props$stat,
#       digits = 6
#     ),
#     expected = expected_
#   )
# })

# test_that("cdrs_map_prep - Q2",{
#   # run test fun
#   obj <- cdrs_map_prep(
#     data_ = example_data,
#     col_ = "Q2",
#     level_ = "Historic or Delta \"legacy\" town",
#     geo_var = "geoid.county"
#   )
#
#   expected_ <- c(
#     0.17592463,
#     0.06913833,
#     0.13205641,
#     0.38534312,
#     0.31936640
#   ) |>
#     round(digits = 6)
#
#   expect_equal(
#     object = round(
#       obj$props$stat,
#       digits = 6
#     ),
#     expected = expected_
#   )
# })
