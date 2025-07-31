#' Get model formula
#'
#' @param x A dataframe of models from create_models()
#' @param model_index Row number from x to get the formula from
#'
#' @returns A model formula 
#'
#' @export
#' @examples
#' \dontrun{
#' get_formula(mod_df, 1)
#' }
get_formula <- function(x, model_index) {
  cli::cli_h1("Formula")
  cli::cli_text(x[model_index,]$model[[1]]$call)
}

#' Get model simulated residuals
#'
#' @param x A dataframe of models from create_models()
#' @param model_index Row number from x to get the residuals from
#'
#' @returns Model simulated residuals from DHARMa::simulateResiduals
#'
#' @export
#' @examples
#' \dontrun{
#' get_residuals(mod_df, 1)
#' }
get_residuals <- function(x, model_index) {
  cli::cli_h1("Model residuals")
  DHARMa::simulateResiduals(x[model_index,]$model[[1]])
}

#' Test a model for outliers
#'
#' @param x A dataframe of models from create_models()
#' @param model_index Row number from x to get the outliers from
#'
#' @returns Result of DHARMa::testOutliers
#'
#' @export
#' @examples
#' \dontrun{
#' get_residuals(mod_df, 1)
#' }
get_test_outliers <- function(x, model_index) {
  cli::cli_h1("Model outliers")
  x[model_index,]$mod_outliers
}

#' Test a model for dispersion
#'
#' @param x A dataframe of models from create_models()
#' @param model_index Row number from x to get the dispersion test result from
#'
#' @returns Result of DHARMa::testDispersion
#'
#' @export
#' @examples
#' \dontrun{
#' get_test_dispersion(mod_df, 1)
#' }
get_test_dispersion <- function(x, model_index) {
  cli::cli_h1("Model dispersion")
  x[model_index,]$mod_dispersion
}

#' Test a model for zero inflation
#'
#' @param x A dataframe of models from create_models()
#' @param model_index Row number from x to get the zero inflation test result from
#'
#' @returns Result of DHARMa::testZeroInflation
#'
#' @export
#' @examples
#' \dontrun{
#' get_test_zero_inflation(mod_df, 1)
#' }
get_test_zero_inflation <- function(x, model_index) {
  cli::cli_h1("Model zero inflation")
  x[model_index,]$mod_zero_inflation
}

