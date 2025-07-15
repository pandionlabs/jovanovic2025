library(here)
MasterThesisData <- read.table(
  file = here("data/raw/MasterThesisData2024.csv"),
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  stringsAsFactors = TRUE,
  dec = "."
)
TreMs <- clean_data(MasterThesisData)
test_that("clean_data works", {
  expect_length(TreMs, 98)
  expect_equal(nrow(TreMs), 533)
})
