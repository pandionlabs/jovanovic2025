## code to prepare `MasterThesisData2024` dataset goes here
MasterThesisData2024 <- read.table(
  file = "data-raw/MasterThesisData2024.csv",
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  stringsAsFactors = TRUE,
  dec = "."
)
usethis::use_data(MasterThesisData2024, overwrite = TRUE)
