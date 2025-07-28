create_models <- function(TreMs) {
  get_model_cols() |>
    purrr::pmap(purrr::partial(create_model, TreMs = TreMs)) |>
    dplyr::bind_rows()
}

create_model <- function(variable, model_family, TreMs) {
  cli::cli_alert_info("fitting models {variable}")
  tibble::tibble(
    variable = variable,
    model = list(glmmTMB::glmmTMB(
      "{variable} ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot)" |>
        glue::glue() |>
        formula(),
      TreMs,
      family = model_family
    ))
  ) |>
    dplyr::mutate(
      summary = model |> purrr::pluck(1) |> summary() |> list(),
      residuals = model |> purrr::pluck(1) |> DHARMa::simulateResiduals() |> list(),
      outliers = residuals |>
        purrr::pluck(1) |>
        DHARMa::testOutliers(plot = FALSE) |>
        list(),
      dispersion = residuals |>
        purrr::pluck(1) |>
        DHARMa::testDispersion(plot = FALSE) |>
        list(),
      zero_inflation = residuals |>
        purrr::pluck(1) |>
        DHARMa::testZeroInflation(plot = FALSE) |>
        list(),
    )
}

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
