#' Clean raw data for analysis
#'
#' @param x A dataframe probably from MasterThesisData2024.csv
#'
#' @returns A dataframe ready for analysis
#' @export
#'
#' @examples
#' \dontrun{
#' MasterThesisData <- read.table(
#' file = "data/raw/MasterThesisData2024.csv",
#' header = TRUE,
#' sep = ",",
#' na.strings = "NA",
#' stringsAsFactors = TRUE,
#' dec = "."
#' )
#' TreMs <- clean_data(MasterThesisData)
#' }

clean_data <- function(MasterThesisData) {
  TreMs <- load_data(MasterThesisData)
  TreMs <- summarize_microhabitats(TreMs)
  TreMs <- group_data(TreMs)
  return(TreMs)
}


#' Loads the data and prepares it for the analysis
#' Mainly it converts from identifies to actual descriptions
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
  species_grouped_conversion <- tribble(
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
#' it combines single type of microhabitat observations into larger groups
summarize_microhabitats <- function(TreMs) {
  # Handle NAs?
  TreMs[c(330, 335), 'Microhabitats.cracksandscars.IN31'] <- 0
  TreMs[c(12, 22, 329, 381, 418), 'Microhabitats.cracksandscars.IN32'] <- 0

  TreMs <- TreMs |>
    rowwise() |>
    mutate(
      DecomposedCrack = sum(c_across(c(
        Microhabitats.cracksandscars.IN31,
        Microhabitats.cracksandscars.IN32
      ))),
      WoodpeckerCavities = sum(c_across(c(
        Microhabitats.Cavities.Woodpeckercavities.CV12,
        Microhabitats.Cavities.Woodpeckercavities.CV13,
        Microhabitats.Cavities.Woodpeckercavities.CV14,
        Microhabitats.Cavities.Trunkmouldcavities.CV21
      ))),
      Concavities = sum(c_across(c(
        Microhabitats.Cavities.Woodpeckercavities.CV15,
        Microhabitats.Dendrotelms.CV42,
        Microhabitats.Dendrotelms.CV43,
        Microhabitats.Dendrotelms.CV44,
        Microhabitats.Insectgalleries.CV51_001,
        Microhabitats.rootbuttresscavities.GR13,
        Microhabitats.fruitingbodiesfungi.EP11,
        group_ti3vx98.Wood_shelter_decay_3
      ))),
      Rotholes = sum(c_across(c(
        Microhabitats.Cavities.Trunkmouldcavities.CV22,
        Microhabitats.Cavities.Trunkmouldcavities.CV23,
        Microhabitats.Cavities.Trunkmouldcavities.CV24,
        Microhabitats.Cavities.Trunkmouldcavities.CV25,
        Microhabitats.Cavities.Trunkmouldcavities.Cv26,
        Microhabitats.Cavities.Trunkmouldcavities._30_cm_open_top_no_ground_contact,
        Microhabitats.Branchholes.CV32,
        Microhabitats.Branchholes.CV33,
        Microhabitats.Dendrotelms.CV41
      ))),
      InsectGalleries = sum(c_across(c(
        Microhabitats.Insectgalleries.Large_bore_hole_2_cm,
        Microhabitats.barkloss.IN11
      ))),
      WoodpeckerConcavities = sum(c_across(c(
        Microhabitats.Cavities.Woodpeckercavities.CV12,
        Microhabitats.Cavities.Woodpeckercavities.CV13,
        Microhabitats.Cavities.Woodpeckercavities.CV14,
        Microhabitats.Cavities.Woodpeckercavities.CV15,
        Microhabitats.Cavities.Trunkmouldcavities.CV21,
        Microhabitats.Dendrotelms.CV42,
        Microhabitats.Dendrotelms.CV43,
        Microhabitats.Dendrotelms.CV44,
        Microhabitats.Insectgalleries.CV51_001,
        Microhabitats.rootbuttresscavities.GR13,
        Microhabitats.fruitingbodiesfungi.EP11,
        group_ti3vx98.Wood_shelter_decay_3
      ))),
      ExposedSapwood = sum(c_across(c(
        Microhabitats.barkloss.IN13,
        Microhabitats.Exposedheartwood.IN21,
        Microhabitats.Bark.BA11,
        Microhabitats.Bark.BA12,
        Microhabitats.rootbuttresscavities.GR12
      ))),
      ExposedHeartwood = sum(c_across(c(
        Microhabitats.Exposedheartwood.IN22,
        Microhabitats.Exposedheartwood.IN23,
        Microhabitats.Exposedheartwood.IN24,
        Microhabitats.cracksandscars.IN31,
        Microhabitats.cracksandscars.IN32,
        Microhabitats.cracksandscars.IN33,
        Microhabitats.cracksandscars.IN34,
        Log.litter.pool.,
        WoodpeckerCavities
      ))),
      PerennialFungi = sum(c_across(c(
        Microhabitats.fruitingbodiesfungi.EP13
      ))),
      Ephermalfungi = sum(c_across(c(
        Microhabitats.fruitingbodiesfungi.EP12,
        Microhabitats.fruitingbodiesfungi.EP14,
        Microhabitats.fruitingbodiesfungi.EP21,
        Microhabitats.epiphyticcryptophanerogmas.Epiphytic_bryophytes_by_mosses_liverworts,
        Decomposed.woodpecker.feeding.cavity.,
        DecomposedCrack
      ))),
      Epiphytes = sum(c_across(c(
        Microhabitats.epiphyticcryptophanerogmas.Epiphytic_foliose_an_hens_coverage_10_,
        Microhabitats.epiphyticcryptophanerogmas.EP33,
        Microhabitats.epiphyticcryptophanerogmas.EP34,
        Microhabitats.epiphyticcryptophanerogmas.EP35,
        group_ti3vx98.Root_plates.and.exposed.roots
      ))),
      DeadwooodShelter = sum(c_across(c(
        group_ti3vx98.Wood_shelter_decay_3_001,
        group_ti3vx98.Wood_shelter_tunnel,
        group_ti3vx98.Hollow_stump_hole_i_stump_big_10cm,
        group_ti3vx98.Hole_in_the_top_of_t_stump_small_5cm
      ))),
      StumpStructures = sum(c_across(c(
        group_ti3vx98.Hole_in_the_top_of_t_stump_small_5cm,
        group_ti3vx98.Fine_woody_debris_decay_3_A4,
        group_ti3vx98.Litter_pool_big_10cm,
        group_ti3vx98.Log_pipe_5cm_diameter
      ))),
      LogStructures = sum(c_across(c(
        group_ti3vx98.Multiple_small_pipes,
        group_ti3vx98.Branch_breakage_hole,
        group_ti3vx98.Multiple_smaller_cracks_in_the_deadwood,
        Other.fungi..corticoid.fungi...others.,
        Fungi.cluster.
      ))),
      WoodyDebris = sum(c_across(c(
        group_ti3vx98.Mixed_coarse_and_fin_bris_decay_3_A4,
        group_ti3vx98.Litter_pool_small_5_10cm
      ))),
      ExposedRoots = sum(c_across(c(
        group_ti3vx98.Ground_cavity_10_cm_opening
      )))
    )

  summary_cols <- c(
    "DecomposedCrack",
    "WoodpeckerCavities",
    "Concavities",
    "Rotholes",
    "InsectGalleries",
    "WoodpeckerConcavities",
    "ExposedSapwood",
    "ExposedHeartwood",
    "PerennialFungi",
    "Ephermalfungi",
    "Epiphytes",
    "DeadwooodShelter",
    "StumpStructures",
    "LogStructures",
    "WoodyDebris",
    "ExposedRoots"
  )

  TreMs <- TreMs |>
    dplyr::mutate(across(all_of(summary_cols), as.integer))

  TreMs <- TreMs |>
    dplyr::mutate(
      Abundance = sum(c_across(all_of(summary_cols))) |> as.integer(),
      Richness = sum(c_across(all_of(summary_cols)) > 0, na.rm = TRUE) |>
        as.integer()
    )

  TreMs
}

#' Groups data by species and deadwood type
group_data <- function(TreMs) {
  species_grouping <- tribble(
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

  deadwood_grouping <- tribble(
  ~deadwood_type,                         ~deadwood_group,
  "Stump (<1.3m) (natural)",             "Stump",
  "Stump (<1.3m) (artificial)",          "Stump",
  "Entire lying tree (natural)",         "Log/Entire Tree",
  "Entire lying tree (artificial)",      "Log/Entire Tree",
  "Log/piece of wood (natural)",         "Log/Entire Tree",
  "Log/piece of wood (artificial)",      "Log/Entire Tree"
)
  TreMs <- TreMs |>
    left_join(species_grouping, by = c("Treedata.Treespecies" = "species")) |>
    left_join(
      deadwood_grouping,
      by = c("Treedata.Type_of_deadwood" = "deadwood_type")
    ) |>
    mutate(
      DeadwoodIdentitiesGrouped = paste(species_short, deadwood_group) |>
        factor(),
      TreeIdentities2 = paste(species_group, deadwood_group) |> factor()
    ) |>
    select(
      -species_short,
      -species_group,
      -deadwood_group
    )
}
