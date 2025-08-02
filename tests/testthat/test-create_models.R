TreMs <- clean_data(MasterThesisData2024)
test_that("create model runs", {
  richeness <- create_model("Richness", "poisson", TreMs)
  expect_s3_class(richeness, "tbl_df")
  expect_s3_class(richeness$model[[1]], "glmmTMB")
})


test_that("create models", {
  models <- create_models(TreMs)
  expect_s3_class(models, "tbl_df")
  expect_equal(nrow(models), 42L)
  expect_length(models, 8L)
})