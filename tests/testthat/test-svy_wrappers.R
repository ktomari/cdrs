test_that("svydesign w/ fpc works with example",{
  demo <- cdrs_read_example()
  demo <- cdrs_subset(demo, Q3_5)
  expect_snapshot(x = cdrs_design(
    data_ = demo,
    set_fpc = T
  ))
})
