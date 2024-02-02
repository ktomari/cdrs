test_that("read_example works",{
  expect_snapshot(x = cdrs_read_example())
})

test_that("read equal example", {
  data_path <- system.file("extdata",
                           "demo",
                           package = "cdrs")

  expect_identical(
    cdrs_read(path_ = data_path,
              convert_to_NA = FALSE),
    cdrs_read_example(convert_to_NA = FALSE)
  )
})

test_that("NA conversions work", {
  # character
  df <- data.frame(a = c("test1", "test2", "<test3>"))
  res <- data.frame(a = c("test1", "test2", NA))
  expect_identical(
    cdrs_as_NA(df),
    res
  )
  # factor
  df$a <- factor(df$a)
  res$a <- factor(res$a)
  expect_identical(
    cdrs_as_NA(df),
    res
  )
})
