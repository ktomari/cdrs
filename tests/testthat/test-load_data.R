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




