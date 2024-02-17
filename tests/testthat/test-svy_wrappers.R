test_that("svydesign w/ fpc works with example",{
  demo <- cdrs_read_example()
  demo <- cdrs_subset(data_ = demo,
                      cols_ = "Q3_5")
  svy_design_obj <- cdrs_design(
    data_ = demo,
    set_fpc = T
  )
  # now we'll serialize it for the snapshot
  # this is a custom serialization function
  # see helper-serialization.R.
  serialized <- serialize_survey_design(svy_design_obj)
  expect_snapshot(x = serialized)
})

test_that("crosstab w/ works with example",{
  demo <- cdrs_read_example()
  expect_snapshot(x = cdrs_crosstab(
    data_ = demo,
    cols_ = c("SEX_P", "Q3_5")
  ))
})