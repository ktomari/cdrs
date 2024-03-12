test_that("basic data subset with quoted var", {
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
