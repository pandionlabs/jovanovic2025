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

clean_data <- function(x) {
  TreMs <- subset(x, select = -c(45:49, 52:55, 66:70))
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

  # Grouping into conifers and broadleafs
  TreMs$GroupedTreeSpecies <- rep("Coniferous spp.", 533) #Creating a new column at the end of the table where all of the rows are filled with "Coniferous spp"
  for (i in 1:nrow(TreMs)) {
    #Then using the loop to go into row #3 and if the name of the species matches what I said it will change it to either No ID or Dead Broadleaf
    if (TreMs[i, 3] == "Dead no identification") {
      TreMs[i, "GroupedTreeSpecies"] <- "No identification"
    } else if (
      TreMs[i, 3] %in% c("Fagus sylvatica", "Tilia cordata", "Dead broadleaf")
    ) {
      TreMs[i, "GroupedTreeSpecies"] <- "Broadleaf spp."
    }
  }

  TreMs$GroupedTreeSpecies <- rep("Coniferous spp.", nrow(TreMs))

  for (i in 1:nrow(TreMs)) {
    if (TreMs[i, 3] == "Dead no identification") {
      TreMs[i, "GroupedTreeSpecies"] <- NA # Assigning NA to GroupedTreeSpecies
    } else if (
      TreMs[i, 3] %in% c("Fagus sylvatica", "Tilia cordata", "Dead broadleaf")
    ) {
      TreMs[i, "GroupedTreeSpecies"] <- "Broadleaf spp."
    }
  }

  TreMs <- TreMs |>
    dplyr::relocate(GroupedTreeSpecies, .before = 4)

  TreMs$GroupedTreeSpecies <- as.factor(TreMs$GroupedTreeSpecies)

  TreMs$Treedata.Type_of_deadwood <- as.factor(TreMs$Treedata.Type_of_deadwood)
  TreMs$Treedata.Type_of_deadwood <- factor(
    TreMs$Treedata.Type_of_deadwood,
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
  TreMs <- TreMs |>
    dplyr::mutate(
      GroupedDeadwoodType = dplyr::case_when(
        grepl("Log", Treedata.Type_of_deadwood) ~ "Log",
        grepl("Entire lying tree", Treedata.Type_of_deadwood) ~ "Entire tree",
        grepl("Stump", Treedata.Type_of_deadwood) ~ "Stump",
        TRUE ~ NA_character_ # Handle any cases that do not match the above categories
      )
    )

  TreMs <- TreMs |>
    dplyr::relocate(GroupedDeadwoodType, .before = 6)

  TreMs$GroupedDeadwoodType <- as.factor(TreMs$GroupedDeadwoodType)

  TreMs$Treedata.Tree_Decay <- as.factor(TreMs$Treedata.Tree_Decay)
  TreMs$Treedata.Tree_Decay <- factor(
    TreMs$Treedata.Tree_Decay,
    levels = c(1, 2, 3, 4, 5),
    labels = c(
      "Decay stage 1",
      "Decay stage 2",
      "Decay stage 3",
      "Decay stage 4",
      "Decay stage 5"
    )
  )

  TreMs$Treedata.GroundContact <- as.factor(TreMs$Treedata.GroundContact)
  TreMs$Treedata.GroundContact <- factor(
    TreMs$Treedata.GroundContact,
    levels = c(0, 1, 2, 3),
    labels = c(
      "Stump",
      "Less than 1/3 ground contact",
      "Between 1/3 and 2/3 ground contact",
      "More than 2/3 ground contact"
    )
  )

  TreMs$Treedata.LitterCoverage <- as.factor(TreMs$Treedata.LitterCoverage)
  TreMs$Treedata.LitterCoverage <- factor(
    TreMs$Treedata.LitterCoverage,
    levels = c(1, 2, 3),
    labels = c(
      "Less than 1/3 vegetation coverage",
      "Between 1/3 and 2/3 vegetation coverage",
      "More than 2/3 vegetation coverage"
    )
  )

  TreMs$Treedata.SunExposure <- as.factor(TreMs$Treedata.SunExposure)
  TreMs$Treedata.SunExposure <- factor(
    TreMs$Treedata.SunExposure,
    levels = c(1, 2),
    labels = c("Sun exposed", "Sun not exposed")
  )

  #New category for origin (arificial or natural)
  TreMs <- TreMs |>
    dplyr::mutate(
      Origin = dplyr::case_when(
        grepl("artificial", Treedata.Type_of_deadwood) ~ "Artificial",
        grepl("natural", Treedata.Type_of_deadwood) ~ "Natural",
        TRUE ~ NA_character_ # Handle any cases that do not match the above categories
      )
    )
  TreMs <- TreMs |>
    dplyr::relocate(Origin, .before = 7)

  TreMs$Origin <- as.factor(TreMs$Origin)

  ########Struring the TreM data

  #Creating a new column for decomposed crack and removing the data from the cracks columns

  TreMs$DecomposedCrack <- rowSums(TreMs[, c(42, 43)])
  TreMs[c(330, 335), 'Microhabitats.cracksandscars.IN31'] <- 0
  TreMs[c(12, 22, 329, 381, 418), 'Microhabitats.cracksandscars.IN32'] <- 0
  TreMs$DecomposedCrack <- as.integer(TreMs$DecomposedCrack)

  #Forming groups according to Larrieu et al. (2018) and forming new groups for the new TreMs

  #Not enough for woodpecker cavitites so combined with concavities
  TreMs$WoodpeckerCavities <- rowSums(TreMs[, c(17, 18, 19, 21)])
  TreMs$Concavities <- rowSums(TreMs[, c(20, 31:34, 49, 50, 62)])
  TreMs <- TreMs |>
    dplyr::relocate(WoodpeckerCavities, .before = 81)

  # changing before to 81 Sam
  TreMs <- TreMs |>
    dplyr::relocate(Concavities, .before = 81)

  TreMs$Rotholes <- rowSums(TreMs[, c(22:30)])
  TreMs$InsectGalleries <- rowSums(TreMs[, c(35, 36)])
  TreMs$WoodpeckerConcavities <- rowSums(TreMs[, c(17:21, 31:34, 49, 50, 62)]) #Concavities + woodpecker cavities
  TreMs$ExposedSapwood <- rowSums(TreMs[, c(37, 38, 46:48)])
  TreMs$ExposedHeartwood <- rowSums(TreMs[, c(39:45, 75, 80)])
  TreMs$PerennialFungi <- rowSums(TreMs[, 52, drop = FALSE]) #Had to do this to read it as a matrix
  TreMs$Ephermalfungi <- rowSums(TreMs[, c(51, 53:55, 77, 79)])
  TreMs$Epiphytes <- rowSums(TreMs[, c(56:60)])
  TreMs$DeadwooodShelter <- rowSums(TreMs[, c(63:66)])
  TreMs$StumpStructures <- rowSums(TreMs[, c(66, 67, 70, 71)])
  TreMs$LogStructures <- rowSums(TreMs[, c(72:74, 76, 78)]) #Decomposed woodpecker feeding cavity included here - log excavations
  TreMs$WoodyDebris <- rowSums(TreMs[, c(68, 69)])
  TreMs$ExposedRoots <- rowSums(TreMs[, 61, drop = FALSE])

  TreMs$Rotholes <- as.integer(TreMs$Rotholes)
  TreMs$InsectGalleries <- as.integer(TreMs$InsectGalleries)
  TreMs$WoodpeckerConcavities <- as.integer(TreMs$WoodpeckerConcavities)
  TreMs$ExposedSapwood <- as.integer(TreMs$ExposedSapwood)
  TreMs$ExposedHeartwood <- as.integer(TreMs$ExposedHeartwood)
  TreMs$PerennialFungi <- as.integer(TreMs$PerennialFungi)
  TreMs$Ephermalfungi <- as.integer(TreMs$Ephermalfungi)
  TreMs$Epiphytes <- as.integer(TreMs$Epiphytes)
  TreMs$DeadwooodShelter <- as.integer(TreMs$DeadwooodShelter)
  TreMs$StumpStructures <- as.integer(TreMs$StumpStructures)
  TreMs$LogStructures <- as.integer(TreMs$LogStructures)
  TreMs$WoodyDebris <- as.integer(TreMs$WoodyDebris)
  TreMs$ExposedRoots <- as.integer(TreMs$ExposedRoots)

  ##Calculating abundance and richness
  # off by one error. changing 95 to 94 Sam
  TreMs$Abundance <- rowSums(TreMs[, c(83:94)])
  TreMs$Richness <- rowSums(TreMs[83:95] > 0, na.rm = TRUE)

  TreMs$Abundance <- as.integer(TreMs$Abundance)
  TreMs$Richness <- as.integer(TreMs$Richness)

  #### DESCRIPTIVE STATISTICS ####

  #Deadwood identities with logs and trees grouped

  TreMs$DeadwoodIdentitiesGrouped <- rep(0, 533)
  last_col <- length(TreMs) # used to be 105
  for (i in 1:nrow(TreMs)) {
    species <- TreMs[i, 3]
    deadwood_type <- TreMs[i, 5]

    # Fagus sylvatica
    if (species == "Fagus sylvatica") {
      if (
        deadwood_type %in%
          c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")
      ) {
        TreMs[i, last_col] <- "F. sylvatica Stump"
      } else if (
        deadwood_type %in%
          c(
            "Entire lying tree (natural)",
            "Entire lying tree (artificial)",
            "Log/piece of wood (natural)",
            "Log/piece of wood (artificial)"
          )
      ) {
        TreMs[i, last_col] <- "F. sylvatica Log/Entire Tree"
      }
    } else if (species == "Picea abies") {
      # Picea abies
      if (
        deadwood_type %in%
          c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")
      ) {
        TreMs[i, last_col] <- "P. abies Stump"
      } else if (
        deadwood_type %in%
          c(
            "Entire lying tree (natural)",
            "Entire lying tree (artificial)",
            "Log/piece of wood (natural)",
            "Log/piece of wood (artificial)"
          )
      ) {
        TreMs[i, last_col] <- "P. abies Log/Entire Tree"
      }
    } else if (species == "Abies alba") {
      # Abies alba
      if (
        deadwood_type %in%
          c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")
      ) {
        TreMs[i, last_col] <- "A. alba Stump"
      } else if (
        deadwood_type %in%
          c(
            "Entire lying tree (natural)",
            "Entire lying tree (artificial)",
            "Log/piece of wood (natural)",
            "Log/piece of wood (artificial)"
          )
      ) {
        TreMs[i, last_col] <- "A. alba Log/Entire Tree"
      }
    } else if (species %in% c("Dead broadleaf", "Tilia cordata")) {
      # Dead broadleaf and Tilia cordata
      if (
        deadwood_type %in%
          c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")
      ) {
        TreMs[i, last_col] <- "Broadleaf Stump"
      } else if (
        deadwood_type %in%
          c(
            "Entire lying tree (natural)",
            "Entire lying tree (artificial)",
            "Log/piece of wood (natural)",
            "Log/piece of wood (artificial)"
          )
      ) {
        TreMs[i, last_col] <- "Broadleaf Log/Entire Tree"
      }
    } else if (
      species %in% c("Dead conifer", "Pinus sylvestris", "Larix decidua")
    ) {
      # Dead conifer, Pinus sylvestris, and Larix decidua
      if (
        deadwood_type %in%
          c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")
      ) {
        TreMs[i, last_col] <- "Conifer Stump"
      } else if (
        deadwood_type %in%
          c(
            "Entire lying tree (natural)",
            "Entire lying tree (artificial)",
            "Log/piece of wood (natural)",
            "Log/piece of wood (artificial)"
          )
      ) {
        TreMs[i, last_col] <- "Conifer Log/Entire Tree"
      }
    } else if (species == "Dead no identification") {
      # Dead no identification
      if (
        deadwood_type %in%
          c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")
      ) {
        TreMs[i, last_col] <- "No ID Stump"
      } else if (
        deadwood_type %in%
          c(
            "Entire lying tree (natural)",
            "Entire lying tree (artificial)",
            "Log/piece of wood (natural)",
            "Log/piece of wood (artificial)"
          )
      ) {
        TreMs[i, last_col] <- "No ID Log/Entire Tree"
      }
    }
  }

  # unique(TreMs$DeadwoodIdentitiesGrouped) # no col named DeadwoodIdentities Sam

  TreMs$TreeIdentities2 <- rep(0, 533)
  last_col <- length(TreMs) # used to be 105
  for (i in 1:nrow(TreMs)) {
    # So what happens if the input is "A. alba Log/Entire Tree"?
    # that is one of the inputs and it seems to trigger "No ID"
    # but I think it should be "Conifer Log/Entire Tree"
    if (
      TreMs$DeadwoodIdentitiesGrouped[i] %in%
        c(
          "A. alba Entire Tree",
          "A. alba Log",
          "Conifer Entire Tree",
          "Conifer Log",
          "P. abies Entire Tree",
          "P. abies Log"
        )
    ) {
      TreMs[i, last_col] <- "Conifer Log/Entire Tree"
    } else if (
      TreMs$DeadwoodIdentitiesGrouped[i] %in%
        c("A. alba Stump", "Conifer Stump", "P. abies Stump")
    ) {
      TreMs[i, last_col] <- "Conifer Stump"
    } else if (
      TreMs$DeadwoodIdentitiesGrouped[i] %in%
        c(
          "Broadleaf Entire tree",
          "Broadleaf Log",
          "F. sylvatica Entire Tree",
          "F. sylvatica Log"
        )
    ) {
      TreMs[i, last_col] <- "Broadleaf Log/Entire tree"
    } else if (
      TreMs$DeadwoodIdentitiesGrouped[i] %in%
        c("Broadleaf Stump", "F. sylvatica Stump")
    ) {
      TreMs[i, last_col] <- "Broadleaf Stump"
    } else {
      TreMs[i, last_col] <- "No ID"
    }
  }

  TreMs$TreeIdentities2 <- as.factor(TreMs$TreeIdentities2)

  return(TreMs)
}
