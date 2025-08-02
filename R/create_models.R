#' Create all the models
#'
#' @param TreMs A TreMs dataframe
#'
#' @returns A dataframe containing all the models
#'
#' @export
#' @examples
#' \dontrun{
#' mod_df <- create_models(TreMs)
#' }
create_models <- function(TreMs) {
  get_model_cols() |>
    purrr::pmap(purrr::partial(create_model, TreMs = TreMs)) |>
    dplyr::bind_rows()
}

#' Build a model and return fit statistics
#'
#' @param variable Character specifying the y variable for the model
#' @param model_family Model family passed to glmmTMB::glmmTMB
#' @param TreMs A TreMs dataframe
#' 
#' @details
#' Builds a glmmTMB::glmmTMB model using the formula 
#' \{variable\} ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay + (1|Plot)
#' Where the Y variable is selected from the available columns in TreMs
#'
#' @returns A tibble containing a model and statistics
#'
#' @export
#' @examples
#' \dontrun{
#' TreMs <- clean_data(MasterThesisData2024)
#' richness <- create_model("Richness", "poisson", TreMs)
#' }
create_model <- function(variable, model_family, TreMs) {
  model <- mod_residuals <- NULL
  cli::cli_alert_info("fitting models {variable}")
  tibble::tibble(
    variable = variable,
    model = list(glmmTMB::glmmTMB(
      "{variable} ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot)" |>
        glue::glue() |>
        stats::formula(),
      TreMs,
      family = model_family
    ))
  ) |>
    dplyr::mutate(
      mod_summary = model |> purrr::pluck(1) |> summary() |> list(),
      mod_residuals = model |> purrr::pluck(1) |> DHARMa::simulateResiduals() |> list(),
      mod_outliers = mod_residuals |>
        purrr::pluck(1) |>
        DHARMa::testOutliers(plot = FALSE) |>
        list(),
      mod_dispersion = mod_residuals |>
        purrr::pluck(1) |>
        DHARMa::testDispersion(plot = FALSE) |>
        list(),
      mod_zero_inflation = mod_residuals |>
        purrr::pluck(1) |>
        DHARMa::testZeroInflation(plot = FALSE) |>
        list(),
    )
}

#' Make a tibble with model variables and families for create_models
#'
#' @returns A tibble with columns for variable and model_family
#'
#' @export
get_model_cols <- function() {
  tibble::tribble(
    ~variable,                ~model_family,
    "DecomposedCrack",           "poisson",
    "WoodpeckerCavities",        "poisson",
    "Concavities",               "poisson",
    "WoodpeckerConcavities",     "poisson",
    "Rotholes",                  "poisson",
    "InsectGalleries",           "poisson",
    "ExposedSapwood",            glmmTMB::nbinom2,
    "ExposedHeartwood",          glmmTMB::nbinom2,
    "ExposedSapwoodHeartwood",   glmmTMB::nbinom2,
    "PerennialFungi",            glmmTMB::nbinom2,
    "Ephermalfungi",             glmmTMB::nbinom2,
    "EphrmalPerennialFungi",     glmmTMB::nbinom2,
    "Epiphytes",                 glmmTMB::nbinom2,
    "DeadwooodShelter",          glmmTMB::nbinom2,
    "StumpStructures",           "poisson",
    "DeadwooodShelterStumpStructures", glmmTMB::nbinom2,
    "LogStructures",             glmmTMB::nbinom2,
    "WoodyDebris",               glmmTMB::nbinom2,
    "ExposedRoots",              glmmTMB::nbinom2,
    "Abundance",                 glmmTMB::nbinom2,
    "Richness",                  glmmTMB::nbinom2
  )
}
