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
