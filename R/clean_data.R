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
  TreMs <- subset(MasterThesisData, select = -c(45:49, 52:55, 66:70))
  TreMs <- TreMs[-534, ]

  #Turing into categorical
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

  species_grouped_conversion <- tribble(
  ~Species,            ~GroupedTreeSpecies,
  "Picea abies",       "Coniferous spp.",
  "Abies alba",        "Coniferous spp.",
  "Fagus sylvatica",   "Broadleaf spp.",
  "Pinus sylvestris",  "Coniferous spp.",
  "Tilia cordata",     "Broadleaf spp.",
  "Larix decidua",     "Coniferous spp.",
  "Dead conifer",      "Coniferous spp.",
  "Dead broadleaf",    "Broadleaf spp.",
  "Dead no identification", NA
)
  TreMs <- TreMs |>
    dplyr::left_join(
      species_grouped_conversion,
      by = c("Treedata.Treespecies" = "Species")
    ) |>
    dplyr::relocate(GroupedTreeSpecies, .before = 4)

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

  #Creating a new column for decomposed crack and removing the data from the cracks columns

  #Forming groups according to Larrieu et al. (2018) and forming new groups for the new TreMs

  #Not enough for woodpecker cavitites so combined with concavities
  # TreMs$WoodpeckerCavities <- rowSums(TreMs[,
  #   Microhabitats.Cavities.Woodpeckercavities.CV12,
  #   Microhabitats.Cavities.Woodpeckercavities.CV13,
  #   Microhabitats.Cavities.Woodpeckercavities.CV14,
  #   Microhabitats.Cavities.Trunkmouldcavities.CV21
  # ])
  # TreMs$Concavities <- rowSums(TreMs[,
  #   Microhabitats.Cavities.Woodpeckercavities.CV15,
  #   Microhabitats.Dendrotelms.CV42,
  #   Microhabitats.Dendrotelms.CV43,
  #   Microhabitats.Dendrotelms.CV44,
  #   Microhabitats.Insectgalleries.CV51_001,
  #   Microhabitats.rootbuttresscavities.GR13,
  #   Microhabitats.fruitingbodiesfungi.EP11,
  #   group_ti3vx98.Wood_shelter_decay_3
  # ])
  # TreMs <- TreMs |>
  #   dplyr::relocate(WoodpeckerCavities, .before = 81)

  # # changing before to 81 Sam
  # TreMs <- TreMs |>
  #   dplyr::relocate(Concavities, .before = 81)

  TreMs[c(330, 335), 'Microhabitats.cracksandscars.IN31'] <- 0
  TreMs[c(12, 22, 329, 381, 418), 'Microhabitats.cracksandscars.IN32'] <- 0

  TreMs2 <- TreMs %>%
    mutate(
      DecomposedCrack = rowSums(select(
        .,
        Microhabitats.cracksandscars.IN31,
        Microhabitats.cracksandscars.IN32
      )),
      WoodpeckerCavities = rowSums(select(
        .,
        Microhabitats.Cavities.Woodpeckercavities.CV12,
        Microhabitats.Cavities.Woodpeckercavities.CV13,
        Microhabitats.Cavities.Woodpeckercavities.CV14,
        Microhabitats.Cavities.Trunkmouldcavities.CV21
      )),
      Concavities = rowSums(select(
        .,
        Microhabitats.Cavities.Woodpeckercavities.CV15,
        Microhabitats.Dendrotelms.CV42,
        Microhabitats.Dendrotelms.CV43,
        Microhabitats.Dendrotelms.CV44,
        Microhabitats.Insectgalleries.CV51_001,
        Microhabitats.rootbuttresscavities.GR13,
        Microhabitats.fruitingbodiesfungi.EP11,
        group_ti3vx98.Wood_shelter_decay_3
      )),
      Rotholes = rowSums(select(
        .,
        Microhabitats.Cavities.Trunkmouldcavities.CV22,
        Microhabitats.Cavities.Trunkmouldcavities.CV23,
        Microhabitats.Cavities.Trunkmouldcavities.CV24,
        Microhabitats.Cavities.Trunkmouldcavities.CV25,
        Microhabitats.Cavities.Trunkmouldcavities.Cv26,
        Microhabitats.Cavities.Trunkmouldcavities._30_cm_open_top_no_ground_contact,
        Microhabitats.Branchholes.CV32,
        Microhabitats.Branchholes.CV33,
        Microhabitats.Dendrotelms.CV41
      )),
      InsectGalleries = rowSums(select(
        .,
        Microhabitats.Insectgalleries.Large_bore_hole_2_cm,
        Microhabitats.barkloss.IN11
      )),
      WoodpeckerConcavities = rowSums(select(
        .,
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
      )),
      ExposedSapwood = rowSums(select(
        .,
        Microhabitats.barkloss.IN13,
        Microhabitats.Exposedheartwood.IN21,
        Microhabitats.Bark.BA11,
        Microhabitats.Bark.BA12,
        Microhabitats.rootbuttresscavities.GR12
      )),
      ExposedHeartwood = rowSums(select(
        .,
        Microhabitats.Exposedheartwood.IN22,
        Microhabitats.Exposedheartwood.IN23,
        Microhabitats.Exposedheartwood.IN24,
        Microhabitats.cracksandscars.IN31,
        Microhabitats.cracksandscars.IN32,
        Microhabitats.cracksandscars.IN33,
        Microhabitats.cracksandscars.IN34,
        Log.litter.pool.,
        WoodpeckerCavities
      )),
      PerennialFungi = rowSums(select(
        .,
        Microhabitats.fruitingbodiesfungi.EP13
      )),
      Ephermalfungi = rowSums(select(
        .,
        Microhabitats.fruitingbodiesfungi.EP12,
        Microhabitats.fruitingbodiesfungi.EP14,
        Microhabitats.fruitingbodiesfungi.EP21,
        Microhabitats.epiphyticcryptophanerogmas.Epiphytic_bryophytes_by_mosses_liverworts,
        Decomposed.woodpecker.feeding.cavity.,
        DecomposedCrack
      )),
      Epiphytes = rowSums(select(
        .,
        Microhabitats.epiphyticcryptophanerogmas.Epiphytic_foliose_an_hens_coverage_10_,
        Microhabitats.epiphyticcryptophanerogmas.EP33,
        Microhabitats.epiphyticcryptophanerogmas.EP34,
        Microhabitats.epiphyticcryptophanerogmas.EP35,
        group_ti3vx98.Root_plates.and.exposed.roots
      )),
      DeadwooodShelter = rowSums(select(
        .,
        group_ti3vx98.Wood_shelter_decay_3_001,
        group_ti3vx98.Wood_shelter_tunnel,
        group_ti3vx98.Hollow_stump_hole_i_stump_big_10cm,
        group_ti3vx98.Hole_in_the_top_of_t_stump_small_5cm
      )),
      StumpStructures = rowSums(select(
        .,
        group_ti3vx98.Hole_in_the_top_of_t_stump_small_5cm,
        group_ti3vx98.Fine_woody_debris_decay_3_A4,
        group_ti3vx98.Litter_pool_big_10cm,
        group_ti3vx98.Log_pipe_5cm_diameter
      )),
      LogStructures = rowSums(select(
        .,
        group_ti3vx98.Multiple_small_pipes,
        group_ti3vx98.Branch_breakage_hole,
        group_ti3vx98.Multiple_smaller_cracks_in_the_deadwood,
        Other.fungi..corticoid.fungi...others.,
        Fungi.cluster.
      )),
      WoodyDebris = rowSums(select(
        .,
        group_ti3vx98.Mixed_coarse_and_fin_bris_decay_3_A4,
        group_ti3vx98.Litter_pool_small_5_10cm
      )),
      ExposedRoots = rowSums(select(
        .,
        group_ti3vx98.Ground_cavity_10_cm_opening
      )),
    )

  # Handle NAs?
 

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
      Abundance = rowSums(select(., all_of(summary_cols))) |> as.integer(),
      Richness = rowSums(select(., all_of(summary_cols)) > 0, na.rm = TRUE) |>
        as.integer()
    )

  # TreMs$Rotholes <- rowSums(TreMs[, c(22:30)])
  # TreMs$InsectGalleries <- rowSums(TreMs[, c(35, 36)])
  # TreMs$WoodpeckerConcavities <- rowSums(TreMs[, c(17:21, 31:34, 49, 50, 62)]) #Concavities + woodpecker cavities
  # TreMs$ExposedSapwood <- rowSums(TreMs[, c(37, 38, 46:48)])
  # TreMs$ExposedHeartwood <- rowSums(TreMs[, c(39:45, 75, 80)])
  # TreMs$PerennialFungi <- rowSums(TreMs[, 52, drop = FALSE]) #Had to do this to read it as a matrix
  # TreMs$Ephermalfungi <- rowSums(TreMs[, c(51, 53:55, 77, 79)])
  # TreMs$Epiphytes <- rowSums(TreMs[, c(56:60)])
  # TreMs$DeadwooodShelter <- rowSums(TreMs[, c(63:66)])
  # TreMs$StumpStructures <- rowSums(TreMs[, c(66, 67, 70, 71)])
  # TreMs$LogStructures <- rowSums(TreMs[, c(72:74, 76, 78)]) #Decomposed woodpecker feeding cavity included here - log excavations
  # TreMs$WoodyDebris <- rowSums(TreMs[, c(68, 69)])
  # TreMs$ExposedRoots <- rowSums(TreMs[, 61, drop = FALSE])

  # TreMs$Rotholes <- as.integer(TreMs$Rotholes)
  # TreMs$InsectGalleries <- as.integer(TreMs$InsectGalleries)
  # TreMs$WoodpeckerConcavities <- as.integer(TreMs$WoodpeckerConcavities)
  # TreMs$ExposedSapwood <- as.integer(TreMs$ExposedSapwood)
  # TreMs$ExposedHeartwood <- as.integer(TreMs$ExposedHeartwood)
  # TreMs$PerennialFungi <- as.integer(TreMs$PerennialFungi)
  # TreMs$Ephermalfungi <- as.integer(TreMs$Ephermalfungi)
  # TreMs$Epiphytes <- as.integer(TreMs$Epiphytes)
  # TreMs$DeadwooodShelter <- as.integer(TreMs$DeadwooodShelter)
  # TreMs$StumpStructures <- as.integer(TreMs$StumpStructures)
  # TreMs$LogStructures <- as.integer(TreMs$LogStructures)
  # TreMs$WoodyDebris <- as.integer(TreMs$WoodyDebris)
  # TreMs$ExposedRoots <- as.integer(TreMs$ExposedRoots)

  ##Calculating abundance and richness
  # off by one error. changing 95 to 94 Sam
  # TreMs$Abundance <- rowSums(TreMs[, c(83:94)])
  # TreMs$Richness <- rowSums(TreMs[83:95] > 0, na.rm = TRUE)

  # TreMs$Abundance <- as.integer(TreMs$Abundance)
  # TreMs$Richness <- as.integer(TreMs$Richness)

  #### DESCRIPTIVE STATISTICS ####

  #Deadwood identities with logs and trees grouped

  # Create species grouping lookup
  species_grouping <- tribble(
  ~species, ~species_short, ~species_group,
  "Fagus sylvatica", "F. sylvatica", "Conifer",
  "Picea abies", "P. abies", "Conifer",
  "Abies alba", "A. alba", "Conifer",
  "Dead broadleaf", "Broadleaf", "Broadleaf",
  "Tilia cordata", "Broadleaf", "Broadleaf",
  "Dead conifer", "Conifer", "Conifer",
  "Pinus sylvestris", "Conifer", "Conifer",
  "Larix decidua", "Conifer", "Conifer",
  "Dead no identification", "No ID", "No ID"
)

  # Create deadwood type grouping lookup
  deadwood_grouping <- tribble(
  ~deadwood_type, ~deadwood_group,
  "Stump (<1.3m) (natural)", "Stump",
  "Stump (<1.3m) (artificial)", "Stump",
  "Entire lying tree (natural)", "Log/Entire Tree",
  "Entire lying tree (artificial)", "Log/Entire Tree",
  "Log/piece of wood (natural)", "Log/Entire Tree",
  "Log/piece of wood (artificial)", "Log/Entire Tree"
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

  return(TreMs)
}
