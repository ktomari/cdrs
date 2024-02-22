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
  data_ <- cdrs_read_example()

  props_ <- data_ %>%
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
    object = get_unwt_props(cdrs_read_example(), "SEX_P"),
    expected = props_
  )

})
