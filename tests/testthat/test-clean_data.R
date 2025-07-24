
TreMs <- clean_data(MasterThesisData2024)
testthat::test_that("clean_data works", {
  expect_length(TreMs, 101)
  expect_equal(nrow(TreMs), 533)
})
