#' Clean raw data for analysis
#'
#' @param x A dataframe probably from MasterThesisData2024.csv
#'
#' @returns A dataframe ready for analysis
#' @export
#'
#' @examples
#' \dontrun{

#' TreMs <- clean_data(MasterThesisData2024)
#' }

clean_data <- function(x) {
  TreMs <- load_data(x)
  TreMs <- summarize_microhabitats(TreMs)
  TreMs <- group_data(TreMs)
  return(TreMs)
}


#' Load the data and prepare it for analysis
#'
#' @param MasterThesisData A dataframe probably from MasterThesisData2024.csv
#'
#' @returns A dataframe of tree data
#' @export
#'
#' @examples
#' \dontrun{
#'   TreMs <- load_data(MasterThesisData2024)
#' }
load_data <- function(MasterThesisData) {
  TreMs <- subset(MasterThesisData, select = -c(45:49, 52:55, 66:70))
  TreMs <- TreMs[-534, ]

  # Tree species
  TreMs$Plot <- as.factor(TreMs$Plot)
  TreMs$Treedata.Treespecies <- factor(
    TreMs$Treedata.Treespecies,
    levels = c(1, 2, 3, 5, 11, 15, 17, 18, 19),
    labels = c(
      "Picea abies",
      "Abies alba",
      "Fagus sylvatica",
      "Pinus sylvestris",
      "Tilia cordata",
      "Larix decidua",
      "Dead conifer",
      "Dead broadleaf",
      "Dead no identification"
    )
  )

  # tree species groups
  species_grouped_conversion <- tibble::tribble(
~Species,                ~GroupedTreeSpecies,
"Picea abies",           "Coniferous spp.",
"Abies alba",            "Coniferous spp.",
"Fagus sylvatica",       "Broadleaf spp.",
"Pinus sylvestris",      "Coniferous spp.",
"Tilia cordata",         "Broadleaf spp.",
"Larix decidua",         "Coniferous spp.",
"Dead conifer",          "Coniferous spp.",
"Dead broadleaf",        "Broadleaf spp.",
"Dead no identification", NA
)
  TreMs <- TreMs |>
    dplyr::left_join(
      species_grouped_conversion,
      by = c("Treedata.Treespecies" = "Species")
    ) |>
    dplyr::relocate(GroupedTreeSpecies, .before = 4)

  # type of deadwood
  TreMs <- TreMs |>
    dplyr::mutate(
      Treedata.Type_of_deadwood = factor(
        Treedata.Type_of_deadwood,
        levels = c(1, 2, 3, 4, 5, 6),
        labels = c(
          "Stump (<1.3m) (natural)",
          "Stump (<1.3m) (artificial)",
          "Entire lying tree (natural)",
          "Entire lying tree (artificial)",
          "Log/piece of wood (natural)",
          "Log/piece of wood (artificial)"
        )
      )
    )

  TreMs <- TreMs |>
    dplyr::mutate(
      GroupedDeadwoodType = dplyr::case_when(
        grepl("Log", Treedata.Type_of_deadwood) ~ "Log",
        grepl("Entire lying tree", Treedata.Type_of_deadwood) ~ "Entire tree",
        grepl("Stump", Treedata.Type_of_deadwood) ~ "Stump",
        # TRUE ~ NA_character_ # Handle any cases that do not match the above categories, it
      )
    ) |>
    dplyr::relocate(GroupedDeadwoodType, .before = 6)

  TreMs <- TreMs |>
    dplyr::mutate(
      Treedata.Tree_Decay = factor(
        Treedata.Tree_Decay,
        levels = c(1, 2, 3, 4, 5),
        labels = c(
          "Decay stage 1",
          "Decay stage 2",
          "Decay stage 3",
          "Decay stage 4",
          "Decay stage 5"
        )
      ),
      Treedata.GroundContact = factor(
        Treedata.GroundContact,
        levels = c(0, 1, 2, 3),
        labels = c(
          "Stump",
          "Less than 1/3 ground contact",
          "Between 1/3 and 2/3 ground contact",
          "More than 2/3 ground contact"
        )
      ),
      Treedata.LitterCoverage = factor(
        Treedata.LitterCoverage,
        levels = c(1, 2, 3),
        labels = c(
          "Less than 1/3 vegetation coverage",
          "Between 1/3 and 2/3 vegetation coverage",
          "More than 2/3 vegetation coverage"
        )
      ),
      Treedata.SunExposure = factor(
        Treedata.SunExposure,
        levels = c(1, 2),
        labels = c("Sun exposed", "Sun not exposed")
      )
    )

  # origin of deadwood
  TreMs <- TreMs |>
    dplyr::mutate(
      Origin = dplyr::case_when(
        grepl("artificial", Treedata.Type_of_deadwood) ~ "Artificial",
        grepl("natural", Treedata.Type_of_deadwood) ~ "Natural",
        TRUE ~ NA_character_ # Handle any cases that do not match the above categories
      ) |>
        factor()
    ) |>
    dplyr::relocate(Origin, .before = 7)

  TreMs
}


#' Summarizes microhabitats in the TreMs dataset
#'
#' @param TreMs a dataframe from load_data
#'
#' @description Combines single type of microhabitat observations into larger groups
#'
#' @returns A dataframe of summarized microhabitat data
#' @export
#'
#' @examples
#' \dontrun{
#'   TreMs <- load_data(MasterThesisData2024)
#'   TreMs <- summarize_microhabitats(TreMs)
#' }
summarize_microhabitats <- function(TreMs) {
  TreMs <- TreMs |>
    dplyr::rowwise() |>
    dplyr::mutate(
      DecomposedCrack = sum(dplyr::c_across(c( 
        Microhabitats.cracksandscars.IN31,    
      ))),
      WoodpeckerCavities = sum(dplyr::c_across(c(
        Microhabitats.Cavities.Woodpeckercavities.CV11,
        Microhabitats.Cavities.Woodpeckercavities.CV12,
        Microhabitats.Cavities.Woodpeckercavities.CV13,
        Microhabitats.Cavities.Woodpeckercavities.CV15
      ))),
      Concavities = sum(dplyr::c_across(c(
        Microhabitats.Cavities.Woodpeckercavities.CV14,
        Microhabitats.Dendrotelms.CV41,
        Microhabitats.Dendrotelms.CV42,
        Microhabitats.Dendrotelms.CV43,
        Microhabitats.Dendrotelms.CV44,
        Microhabitats.rootbuttresscavities.GR12,
        Microhabitats.rootbuttresscavities.GR13,
        group_ti3vx98.Ground_cavity_10_cm_opening
      ))),
      WoodpeckerConcavities = WoodpeckerCavities + Concavities,
      Rotholes = sum(dplyr::c_across(c(
        Microhabitats.Cavities.Trunkmouldcavities.CV21,
        Microhabitats.Cavities.Trunkmouldcavities.CV22,
        Microhabitats.Cavities.Trunkmouldcavities.CV23,
        Microhabitats.Cavities.Trunkmouldcavities.CV24,
        Microhabitats.Cavities.Trunkmouldcavities.CV25,
        Microhabitats.Cavities.Trunkmouldcavities.Cv26,
        Microhabitats.Cavities.Trunkmouldcavities._30_cm_open_top_no_ground_contact,
        Microhabitats.Branchholes.CV32,
        Microhabitats.Branchholes.CV33
      ))),
      InsectGalleries = sum(dplyr::c_across(c(
        Microhabitats.Insectgalleries.Large_bore_hole_2_cm,
        Microhabitats.Insectgalleries.CV51_001
      ))),
      ExposedSapwood = sum(dplyr::c_across(c(
        Microhabitats.barkloss.IN11,
        Microhabitats.barkloss.IN13,
        Microhabitats.cracksandscars.IN34,
        Microhabitats.Bark.BA11,
        Microhabitats.Bark.BA12,
      ))),
      ExposedHeartwood = sum(dplyr::c_across(c(
        Microhabitats.Exposedheartwood.IN21,
        Microhabitats.Exposedheartwood.IN22,
        Microhabitats.Exposedheartwood.IN23,
        Microhabitats.Exposedheartwood.IN24,
        Microhabitats.cracksandscars.IN33,
        group_ti3vx98.Multiple_smaller_cracks_in_the_deadwood,
        DecomposedCrack
      ))),
      ExposedSapwoodHeartwood = ExposedSapwood + ExposedHeartwood,
      PerennialFungi = sum(dplyr::c_across(c(
        Microhabitats.fruitingbodiesfungi.EP12
      ))),
      Ephermalfungi = sum(dplyr::c_across(c(
        Microhabitats.fruitingbodiesfungi.EP11,
        Microhabitats.fruitingbodiesfungi.EP13,
        Microhabitats.fruitingbodiesfungi.EP14,
        Microhabitats.fruitingbodiesfungi.EP21,
        Other.fungi..corticoid.fungi...others.,
        Fungi.cluster.
      ))),
      EphrmalPerennialFungi = PerennialFungi + Ephermalfungi,
      Epiphytes = sum(dplyr::c_across(c(
        Microhabitats.epiphyticcryptophanerogmas.Epiphytic_bryophytes_by_mosses_liverworts,
        Microhabitats.epiphyticcryptophanerogmas.Epiphytic_foliose_an_hens_coverage_10_,
        Microhabitats.epiphyticcryptophanerogmas.EP33,
        Microhabitats.epiphyticcryptophanerogmas.EP34,
        Microhabitats.epiphyticcryptophanerogmas.EP35
      ))),
      DeadwooodShelter = sum(dplyr::c_across(c(
        group_ti3vx98.Wood_shelter_decay_3,
        group_ti3vx98.Wood_shelter_decay_3_001,
        group_ti3vx98.Wood_shelter_tunnel
      ))),
      StumpStructures = sum(dplyr::c_across(c(
        group_ti3vx98.Hollow_stump_hole_i_stump_big_10cm,
        group_ti3vx98.Hole_in_the_top_of_t_stump_small_5cm,
        group_ti3vx98.Litter_pool_small_5_10cm,
        group_ti3vx98.Litter_pool_big_10cm
      ))),
      LogStructures = sum(dplyr::c_across(c(
        group_ti3vx98.Log_pipe_5cm_diameter,
        group_ti3vx98.Multiple_small_pipes,
        Log.litter.pool.,
        group_ti3vx98.Branch_breakage_hole,
        Decomposed.woodpecker.feeding.cavity.
      ))),
      WoodyDebris = sum(dplyr::c_across(c(
        group_ti3vx98.Mixed_coarse_and_fin_bris_decay_3_A4,
        group_ti3vx98.Fine_woody_debris_decay_3_A4
      ))),
      ExposedRoots = sum(dplyr::c_across(c(
        group_ti3vx98.Root_plates.and.exposed.roots
      )))
    )

  summary_cols <- c(
    "DecomposedCrack",
    "WoodpeckerCavities",
    "Concavities",
    "WoodpeckerConcavities",
    "Rotholes",
    "InsectGalleries",
    "ExposedSapwood",
    "ExposedHeartwood",
    "ExposedSapwoodHeartwood",
    "PerennialFungi",
    "Ephermalfungi",
    "EphrmalPerennialFungi",
    "Epiphytes",
    "DeadwooodShelter",
    "StumpStructures",
    "LogStructures",
    "WoodyDebris",
    "ExposedRoots"
  )

  TreMs <- TreMs |>
    dplyr::mutate(dplyr::across(dplyr::all_of(summary_cols), as.integer))

  TreMs <- TreMs |>
    dplyr::mutate(
      Abundance = sum(dplyr::c_across(dplyr::all_of(summary_cols))) |> as.integer(),
      Richness = sum(dplyr::c_across(dplyr::all_of(summary_cols)) > 0, na.rm = TRUE) |>
        as.integer()
    )

  TreMs
}


#' Group data by species and deadwood type
#'
#' @param TreMs A dataframe from summarize_microhabitats
#'
#' @returns A dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   TreMs <- load_data(MasterThesisData2024)
#'   TreMs <- summarize_microhabitats(TreMs)
#'   TreMs <- group_data(TreMs)
#' }
group_data <- function(TreMs) {
  species_grouping <- tibble::tribble(
  ~species,           ~species_short,  ~species_group,
  "Fagus sylvatica",  "F. sylvatica",  "Conifer",
  "Picea abies",      "P. abies",      "Conifer",
  "Abies alba",       "A. alba",       "Conifer",
  "Dead broadleaf",   "Broadleaf",     "Broadleaf",
  "Tilia cordata",    "Broadleaf",     "Broadleaf",
  "Dead conifer",     "Conifer",       "Conifer",
  "Pinus sylvestris", "Conifer",       "Conifer",
  "Larix decidua",    "Conifer",       "Conifer",
  "Dead no identification", "No ID",    "No ID"
)

  deadwood_grouping <- tibble::tribble(
  ~deadwood_type,                         ~deadwood_group,
  "Stump (<1.3m) (natural)",             "Stump",
  "Stump (<1.3m) (artificial)",          "Stump",
  "Entire lying tree (natural)",         "Log/Entire Tree",
  "Entire lying tree (artificial)",      "Log/Entire Tree",
  "Log/piece of wood (natural)",         "Log/Entire Tree",
  "Log/piece of wood (artificial)",      "Log/Entire Tree"
)
  TreMs <- TreMs |>
    dplyr::left_join(species_grouping, by = c("Treedata.Treespecies" = "species")) |>
    dplyr::left_join(
      deadwood_grouping,
      by = c("Treedata.Type_of_deadwood" = "deadwood_type")
    ) |>
    dplyr::mutate(
      DeadwoodIdentitiesGrouped = paste(species_short, deadwood_group) |>
        factor(),
      TreeIdentities2 = paste(species_group, deadwood_group) |> factor()
    ) |>
    dplyr::select(
      -species_short,
      -species_group,
      -deadwood_group
    )

  return(TreMs)
}
