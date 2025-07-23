
TreMs <- clean_data(jovanovic2025::MasterThesisData2024)
test_that("clean_data works", {
  expect_length(TreMs, 100)
  expect_equal(nrow(TreMs), 533)
})
