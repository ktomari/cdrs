test_that("read_example works",{
  expect_snapshot(x = cdrs_read_example())
})

test_that("read equal example dir", {
  data_path <- system.file("extdata",
                           "demo",
                           package = "cdrs")

  expect_identical(
    cdrs_read(path_ = data_path),
    cdrs_read_example()
  )
})

test_that("read equal example zip", {
  data_path <- system.file("extdata",
                           "DRS_demo_2023_12_01.zip",
                           package = "cdrs")

  expect_identical(
    cdrs_read(path_ = data_path),
    cdrs_read_example()
  )
})

test_that("NA conversions work", {

  input_vec <- factor(
    c(
      "Green",
      "Red",
      "<Decline to answer>",
      "Green",
      "<Decline to answer>",
      "<I don't know>"
    )
  )

  expected_vec <- factor(
    c(
      "Green",
      "Red",
      NA,
      "Green",
      NA,
      "<I don't know>"
    )
  )

  expect_identical(
    matches_to_NA(
      vec_ = input_vec,
      match_ = "Decline to answer"
    ),
    expected_vec
  )
})

test_that("basic data subset with unquoted var", {
  # create input data.frame
  input_df <- data.frame(
    Q1 = factor(c("Y", NA, "N", "Y")),
    Q2 = factor(c("Blue", "Blue", "Green", "Red")),
    Zone = factor(c(1, 2, 3, 1)),
    WTFINAL = c(.8, 1, 1.2, NA)
  )

  # create expected data.frame by,
  # removing NA values in Q1 and WTFINAL
  # and selecting the three columns we expect.
  expected_df <- input_df[
      !is.na(input_df$WTFINAL),
    c("Q1", "Zone", "WTFINAL")
  ]

  # Because of how dplyr::filter works, it resets the rownames.
  # By setting the rownames of the expected df to NULL,
  # we effectively do the same.
  rownames(expected_df) <- NULL

  # test
  expect_identical(
    cdrs_subset(data_ = input_df, "Q1"),
    expected_df
  )
})
