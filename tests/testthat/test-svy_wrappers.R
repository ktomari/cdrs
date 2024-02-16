test_that("svydesign w/ fpc works with example",{
  demo <- cdrs_read_example()
  demo <- cdrs_subset(data_ = demo,
                      cols_ = "Q3_5")
  expect_snapshot(x = cdrs_design(
    data_ = demo,
    set_fpc = T
  ))
})

test_that("crosstab w/ works with example",{
  demo <- cdrs_read_example()
  expect_snapshot(x = cdrs_crosstab(
    data_ = demo,
    cols_ = c("SEX_P", "Q3_5")
  ))
})
