# #MASTER THESIS - DATA EXPLORATION 

# ##################  Package installation and loading #########

# # Valuable packages
# install.packages("data.table")
# install.packages("tidyverse")
# install.packages("rlang")
# install.packages("dplyr")
# install.packages("vegan")
# install.packages("pastecs")
# install.packages("ggplot2")
# install.packages("lattice")
# install.packages("reshape2")
# install.packages("plyr")
# install.packages("DHARMa")
# # install.packages("glmmTMB")
# install.packages("car")
# install.packages("emmeans")
# install.packages("effects")
# install.packages("multcomp")
# install.packages("MuMIn")
# install.packages("broom")
# install.packages("broom.mixed")
# install.packages("dotwhisker")
# install.packages("texreg")
# install.packages("xtable")
# install.packages("sjPlot")
# install.packages("ggeffects")
# install.packages("cowplot")
# install.packages("lme4")
# install.packages("sjstats")
# install.packages("sjmisc")
# install.packages("DHARMa")
# install.packages("glmmTMB", type="source")
# install.packages("ggeffects")
# install.packages("Rtools")
# install.packages("ggplot2")
# install.packages("viridis")
# install.packages("multcompView")



# Visualise with library and check for updates
library(data.table)
# library(tidyverse)
library(dplyr)
library(vegan)
library(pastecs)
library(ggplot2); theme_set(theme_bw())
library(lattice)
library(reshape2)
library(plyr)
library(DHARMa)
library(glmmTMB)
library(car)
library(emmeans)
library(effects)
library(multcomp)
library(MuMIn)
library(broom)
library(broom.mixed)
library(dotwhisker)
library(texreg)
library(xtable)
library(sjPlot)
library(ggeffects)
library(cowplot)
library(lme4)
library(sjstats)
library(sjmisc)
# library(DHARMa)
library(glmmTMB)
library(lme4)
library(ggeffects)
library(ggplot2)
library(viridis)
library(multcompView)
library("ggpubr") ### uhoh

##################### Import the data #######################

# Import the data from a tab delimited ascii file.
MasterThesisData <- read.table(file = "data/raw/MasterThesisData2024.csv",
                         header = TRUE,
                         sep = ",",
                         na.strings = "NA",
                         stringsAsFactors = TRUE,
                         dec = ".")
# file = " "              Name of the text file inside the working directory.
# header = TRUE           First row in the text file contains headers.
# dec = '.'               Points are used for decimal separation in the text file. 
#                         Change to  dec = ","  if required.
# sep = ','               Delimiter for csv files
# na.strings = "NA"       NA in the text file is considered as a missing value.
# stringsAsFactors = TRUE Columns with text entries (except for the headers) are
#                         imported as categorical variables. This used to be the
#                         default setting, but it changed in R version 4+. 

#Subset without the TreMs I didn't have 
#Excluded Trems CCrown deadwood - DE11, DE12, DE13, DE14, DE15; Excrescences - GR21, GR22, GR31, GR32; Nests and Microsoils - NE11, OT, 21,OT22; Sap and resin flow - OT11, OT12) 
colnames(MasterThesisData)
TreMs <- subset(MasterThesisData, select = -c(45:49,52:55,66:70))
TreMs <- TreMs[-534, ]

#########Structuring the data 

#Turing into categorical 

TreMs$Plot <- as.factor(TreMs$Plot)
class(TreMs$Plot)
levels(TreMs$Plot)

TreMs$Treedata.Treespecies <- factor(TreMs$Treedata.Treespecies,
                                               levels = c(1,2,3,5,11,15,17,18,19),
                                               labels = c("Picea abies","Abies alba","Fagus sylvatica","Pinus sylvestris",
                                                          "Tilia cordata","Larix decidua","Dead conifer","Dead broadleaf","Dead no identification"))
#Grouping into conifers and broadleafs
TreMs$GroupedTreeSpecies <- rep("Coniferous spp.", 533) #Creating a new column at the end of the table where all of the rows are filled with "Coniferous spp"
for (i in 1:nrow(TreMs)) {                      #Then using the loop to go into row #3 and if the name of the species matches what I said it will change it to either No ID or Dead Broadleaf
  if (TreMs[i, 3] == "Dead no identification") {
    TreMs[i, "GroupedTreeSpecies"] <- "No identification"
  } else if (TreMs[i, 3] %in% c("Fagus sylvatica", "Tilia cordata", "Dead broadleaf")) {
    TreMs[i, "GroupedTreeSpecies"] <- "Broadleaf spp."
  }
}


TreMs$GroupedTreeSpecies <- rep("Coniferous spp.", nrow(TreMs))

for (i in 1:nrow(TreMs)) {                      
  if (TreMs[i, 3] == "Dead no identification") {
    TreMs[i, "GroupedTreeSpecies"] <- NA  # Assigning NA to GroupedTreeSpecies
  } else if (TreMs[i, 3] %in% c("Fagus sylvatica", "Tilia cordata", "Dead broadleaf")) {
    TreMs[i, "GroupedTreeSpecies"] <- "Broadleaf spp."
  }
}

TreMs <- TreMs |>
  relocate(GroupedTreeSpecies, .before = 4)

TreMs$GroupedTreeSpecies <- as.factor(TreMs$GroupedTreeSpecies)
levels(TreMs$GroupedTreeSpecies)

TreMs$Treedata.Type_of_deadwood <- as.factor (TreMs$Treedata.Type_of_deadwood)
TreMs$Treedata.Type_of_deadwood <- factor(TreMs$Treedata.Type_of_deadwood,
                                               levels = c(1,2,3,4,5,6),
                                               labels = c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)", 
                                                          "Entire lying tree (natural)", "Entire lying tree (artificial)", 
                                                          "Log/piece of wood (natural)", "Log/piece of wood (artificial)"))
TreMs <- TreMs |>
  mutate(GroupedDeadwoodType = case_when(
    grepl("Log", Treedata.Type_of_deadwood) ~ "Log",
    grepl("Entire lying tree", Treedata.Type_of_deadwood) ~ "Entire tree",
    grepl("Stump", Treedata.Type_of_deadwood) ~ "Stump",
    TRUE ~ NA_character_  # Handle any cases that do not match the above categories
  ))

TreMs <- TreMs |>
  relocate(GroupedDeadwoodType, .before = 6)

TreMs$GroupedDeadwoodType <- as.factor (TreMs$GroupedDeadwoodType)
levels(TreMs$GroupedDeadwoodType)

TreMs$Treedata.Tree_Decay <- as.factor (TreMs$Treedata.Tree_Decay)
TreMs$Treedata.Tree_Decay <- factor(TreMs$Treedata.Tree_Decay,
                                          levels = c(1,2,3,4,5),
                                          labels = c("Decay stage 1", "Decay stage 2","Decay stage 3","Decay stage 4","Decay stage 5"))

TreMs$Treedata.GroundContact <- as.factor(TreMs$Treedata.GroundContact)
TreMs$Treedata.GroundContact <- factor(TreMs$Treedata.GroundContact,
                                            levels = c(0,1,2,3),
                                            labels = c("Stump", "Less than 1/3 ground contact", "Between 1/3 and 2/3 ground contact", "More than 2/3 ground contact"))

TreMs$Treedata.LitterCoverage <- as.factor(TreMs$Treedata.LitterCoverage)
TreMs$Treedata.LitterCoverage <- factor(TreMs$Treedata.LitterCoverage,
                                             levels = c(1,2,3),
                                             labels = c("Less than 1/3 vegetation coverage", "Between 1/3 and 2/3 vegetation coverage", "More than 2/3 vegetation coverage"))

TreMs$Treedata.SunExposure <- as.factor(TreMs$Treedata.SunExposure)
TreMs$Treedata.SunExposure <- factor(TreMs$Treedata.SunExposure,
                                          levels = c(1,2),
                                          labels = c("Sun exposed", "Sun not exposed"))

#New category for origin (arificial or natural)
TreMs <- TreMs |>
  mutate(Origin = case_when(
    grepl("artificial", Treedata.Type_of_deadwood) ~ "Artificial",
    grepl("natural", Treedata.Type_of_deadwood) ~ "Natural",
    TRUE ~ NA_character_  # Handle any cases that do not match the above categories
  ))
TreMs <- TreMs |>
  relocate(Origin, .before = 7)

TreMs$Origin <- as.factor (TreMs$Origin)

str(TreMs)


########Struring the TreM data

#Creating a new column for decomposed crack and removing the data from the cracks columns 

TreMs$DecomposedCrack <- rowSums(TreMs[, c(42,43)])
TreMs[c(330,335), 'Microhabitats.cracksandscars.IN31'] <- 0
TreMs[c(12,22,329,381,418), 'Microhabitats.cracksandscars.IN32'] <- 0
TreMs$DecomposedCrack <- as.integer (TreMs$DecomposedCrack)

#Forming groups according to Larrieu et al. (2018) and forming new groups for the new TreMs

#Not enough for woodpecker cavitites so combined with concavities 
TreMs$WoodpeckerCavities <- rowSums(TreMs[, c(17,18,19,21)])
TreMs$Concavities <- rowSums(TreMs[, c(20,31:34,49,50,62)])
TreMs <- TreMs |>
  relocate(WoodpeckerCavities, .before = 81)

# changing before to 81 Sam
TreMs <- TreMs |>
  relocate(Concavities, .before = 81)

TreMs$Rotholes <- rowSums(TreMs[, c(22:30)])
TreMs$InsectGalleries <- rowSums(TreMs[, c(35,36)])
TreMs$WoodpeckerConcavities <- rowSums(TreMs[, c(17:21,31:34,49,50,62)]) #Concavities + woodpecker cavities 
TreMs$ExposedSapwood <- rowSums(TreMs[, c(37,38,46:48)])
TreMs$ExposedHeartwood <- rowSums(TreMs[, c(39:45,75,80)])
TreMs$PerennialFungi <- rowSums(TreMs[, 52, drop = FALSE]) #Had to do this to read it as a matrix
TreMs$Ephermalfungi <- rowSums(TreMs[, c(51,53:55,77,79)])
TreMs$Epiphytes <- rowSums(TreMs[, c(56:60)])
TreMs$DeadwooodShelter <- rowSums(TreMs[, c(63:66)])
TreMs$StumpStructures <- rowSums(TreMs[, c(66,67,70,71)])
TreMs$LogStructures <- rowSums(TreMs[, c(72:74,76,78)]) #Decomposed woodpecker feeding cavity included here - log excavations 
TreMs$WoodyDebris <- rowSums(TreMs[, c(68,69)])
TreMs$ExposedRoots <- rowSums(TreMs[, 61, drop = FALSE])

colnames(TreMs)

TreMs$Rotholes <- as.integer (TreMs$Rotholes)
TreMs$InsectGalleries <- as.integer (TreMs$InsectGalleries)
TreMs$WoodpeckerConcavities <- as.integer (TreMs$WoodpeckerConcavities)
TreMs$ExposedSapwood <- as.integer (TreMs$ExposedSapwood)
TreMs$ExposedHeartwood <- as.integer (TreMs$ExposedHeartwood)
TreMs$PerennialFungi <- as.integer (TreMs$PerennialFungi)
TreMs$Ephermalfungi <- as.integer (TreMs$Ephermalfungi)
TreMs$Epiphytes <- as.integer (TreMs$Epiphytes)
TreMs$DeadwooodShelter <- as.integer (TreMs$DeadwooodShelter)
TreMs$StumpStructures <- as.integer (TreMs$StumpStructures)
TreMs$LogStructures <- as.integer (TreMs$LogStructures)
TreMs$WoodyDebris <- as.integer (TreMs$WoodyDebris)
TreMs$ExposedRoots <- as.integer (TreMs$ExposedRoots)


##Calculating abundance and richness
# off by one error. changing 95 to 94 Sam
TreMs$Abundance <- rowSums(TreMs[, c(83:94)])
TreMs$Richness <- rowSums(TreMs[83:95]>0, na.rm = TRUE)

TreMs$Abundance <- as.integer(TreMs$Abundance)
TreMs$Richness <- as.integer(TreMs$Richness)

colnames(TreMs)

#### DESCRIPTIVE STATISTICS ####

#Deadwood identities with logs and trees grouped 

TreMs$DeadwoodIdentitiesGrouped <- rep(0, 533)
last_col <- length(TreMs) # used to be 105
for (i in 1:nrow(TreMs)) {
  
  species <- TreMs[i, 3]
  deadwood_type <- TreMs[i, 5]
  
  # Fagus sylvatica
  if (species == "Fagus sylvatica") {
    if (deadwood_type %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")) {
      TreMs[i, last_col] <- "F. sylvatica Stump"
    } else if (deadwood_type %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)", 
                                    "Log/piece of wood (natural)", "Log/piece of wood (artificial)")) {
      TreMs[i, last_col] <- "F. sylvatica Log/Entire Tree"
    }
  }
  
  # Picea abies
  else if (species == "Picea abies") {
    if (deadwood_type %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")) {
      TreMs[i, last_col] <- "P. abies Stump"
    } else if (deadwood_type %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)", 
                                    "Log/piece of wood (natural)", "Log/piece of wood (artificial)")) {
      TreMs[i, last_col] <- "P. abies Log/Entire Tree"
    }
  }
  
  # Abies alba
  else if (species == "Abies alba") {
    if (deadwood_type %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")) {
      TreMs[i, last_col] <- "A. alba Stump"
    } else if (deadwood_type %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)", 
                                    "Log/piece of wood (natural)", "Log/piece of wood (artificial)")) {
      TreMs[i, last_col] <- "A. alba Log/Entire Tree"
    }
  }
  
  # Dead broadleaf and Tilia cordata
  else if (species %in% c("Dead broadleaf", "Tilia cordata")) {
    if (deadwood_type %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")) {
      TreMs[i, last_col] <- "Broadleaf Stump"
    } else if (deadwood_type %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)", 
                                    "Log/piece of wood (natural)", "Log/piece of wood (artificial)")) {
      TreMs[i, last_col] <- "Broadleaf Log/Entire Tree"
    }
  }
  
  # Dead conifer, Pinus sylvestris, and Larix decidua
  else if (species %in% c("Dead conifer", "Pinus sylvestris", "Larix decidua")) {
    if (deadwood_type %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")) {
      TreMs[i, last_col] <- "Conifer Stump"
    } else if (deadwood_type %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)", 
                                    "Log/piece of wood (natural)", "Log/piece of wood (artificial)")) {
      TreMs[i, last_col] <- "Conifer Log/Entire Tree"
    }
  }
  
  # Dead no identification
  else if (species == "Dead no identification") {
    if (deadwood_type %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")) {
      TreMs[i, last_col] <- "No ID Stump"
    } else if (deadwood_type %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)", 
                                    "Log/piece of wood (natural)", "Log/piece of wood (artificial)")) {
      TreMs[i, last_col] <- "No ID Log/Entire Tree"
    }
  }
}


unique(TreMs$DeadwoodIdentitiesGrouped) # no col named DeadwoodIdentities Sam


TreMs$TreeIdentities2 <- rep(0, 533)
last_col <- length(TreMs) # used to be 105
for (i in 1:nrow(TreMs)) {
  # So what happens if the input is "A. alba Log/Entire Tree"?
  # that is one of the inputs and it seems to trigger "No ID"
  # but I think it should be "Conifer Log/Entire Tree"
  if (TreMs$DeadwoodIdentitiesGrouped[i] %in% c("A. alba Entire Tree", "A. alba Log", "Conifer Entire Tree", "Conifer Log", "P. abies Entire Tree", "P. abies Log")) {
    TreMs[i, last_col] <- "Conifer Log/Entire Tree"
  }
  
  else if (TreMs$DeadwoodIdentitiesGrouped[i] %in% c("A. alba Stump", "Conifer Stump", "P. abies Stump")) {
    TreMs[i, last_col] <- "Conifer Stump"
  }
  
  else if (TreMs$DeadwoodIdentitiesGrouped[i] %in% c("Broadleaf Entire tree", "Broadleaf Log", "F. sylvatica Entire Tree", "F. sylvatica Log")) {
    TreMs[i, last_col] <- "Broadleaf Log/Entire tree"
  }
  
  else if (TreMs$DeadwoodIdentitiesGrouped[i] %in% c("Broadleaf Stump", "F. sylvatica Stump")) {
    TreMs[i, last_col] <- "Broadleaf Stump"
  }
  

  else {
    TreMs[i, last_col] <- "No ID"
  }
}

TreMs$TreeIdentities2 <- as.factor (TreMs$TreeIdentities2)

#Number 

counttable <- as.data.frame(table(TreMs$TreeIdentities2))
write.table(counttable, file = "data/derivatives/Count_Table.csv", sep = ",", quote = FALSE, row.names = F)

counttable$Percentage <- (counttable$Freq / sum(counttable$Freq)) * 100
write.table(counttable, file = "data/derivatives/Count_Percentage_Table.csv", sep = ",", quote = FALSE, row.names = F)

counttable2 <- as.data.frame(table(TreMs$DeadwoodIdentitiesGrouped))
write.table(counttable2, file = "data/derivatives/Count_Table2.csv", sep = ",", quote = FALSE, row.names = F)

counttable2$Percentage <- (counttable2$Freq / sum(counttable2$Freq)) * 100
write.table(counttable2, file = "data/derivatives/Count_Percentage_Table2.csv", sep = ",", quote = FALSE, row.names = F)

#DBH
meantable <- aggregate(TreMs[,9], list(TreMs$DeadwoodIdentitiesGrouped),mean)
write.table(meantable, file = "data/derivatives/DBH_Table_Mean.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,9], list(TreMs$TreeIdentities2),mean)
write.table(meantable, file = "data/derivatives/DBH_Table_Mean2.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,9], list(TreMs$DeadwoodIdentitiesGrouped),sd)
write.table(meantable, file = "data/derivatives/DBH_Table_SD.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,9], list(TreMs$TreeIdentities2),sd)
write.table(meantable, file = "data/derivatives/DBH_Table_SD2.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,9], list(TreMs$DeadwoodIdentitiesGrouped),min)
write.table(meantable, file = "data/derivatives/DBH_Table_Min.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,9], list(TreMs$TreeIdentities2),min)
write.table(meantable, file = "data/derivatives/DBH_Table_Min2.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,9], list(TreMs$DeadwoodIdentitiesGrouped),max)
write.table(meantable, file = "data/derivatives/DBH_Table_Max.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,9], list(TreMs$TreeIdentities2),max)
write.table(meantable, file = "data/derivatives/DBH_Table_Max2.csv", sep = ",", quote = FALSE, row.names = F)

#Abundance

meantable <- aggregate(TreMs[,96], list(TreMs$DeadwoodIdentitiesGrouped),mean)
write.table(meantable, file = "data/derivatives/Abundance_Table_Mean.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,96], list(TreMs$TreeIdentities2),mean)
write.table(meantable, file = "data/derivatives/Abundance_Table_Mean2.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,96], list(TreMs$DeadwoodIdentitiesGrouped),sd)
write.table(meantable, file = "data/derivatives/Abundance_Table_SD.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,96], list(TreMs$TreeIdentities2),sd)
write.table(meantable, file = "data/derivatives/Abundance_Table_SD2.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,96], list(TreMs$DeadwoodIdentitiesGrouped),max)
write.table(meantable, file = "data/derivatives/Abundance_Table_Max.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,96], list(TreMs$TreeIdentities2),max)
write.table(meantable, file = "data/derivatives/Abundance_Table_Max2.csv", sep = ",", quote = FALSE, row.names = F)

#Richness

meantable <- aggregate(TreMs[,97], list(TreMs$DeadwoodIdentitiesGrouped),mean)
write.table(meantable, file = "data/derivatives/Richness_Table_Mean.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,97], list(TreMs$TreeIdentities2),mean) # TODO NAS sam
write.table(meantable, file = "data/derivatives/Richness_Table_Mean2.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,97], list(TreMs$DeadwoodIdentitiesGrouped),sd)
write.table(meantable, file = "data/derivatives/Richness_Table_SD.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,97], list(TreMs$TreeIdentities2),sd) # TODO NAS sam
write.table(meantable, file = "data/derivatives/Richness_Table_SD2.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,97], list(TreMs$DeadwoodIdentitiesGrouped),max)
write.table(meantable, file = "data/derivatives/Richness_Table_MAX.csv", sep = ",", quote = FALSE, row.names = F)

meantable <- aggregate(TreMs[,97], list(TreMs$TreeIdentities2),max)
write.table(meantable, file = "data/derivatives/Richness_Table_MAX2.csv", sep = ",", quote = FALSE, row.names = F)

#Per decay stage

count_table <- as.data.frame(table(TreMs$TreeIdentities2, TreMs$Treedata.Tree_Decay))
count_table2 <- as.data.frame(table(TreMs$DeadwoodIdentitiesGrouped, TreMs$Treedata.Tree_Decay))

# Rename columns for clarity
colnames(count_table) <- c("TreeIdentities2", "Decay_Stage", "Count")

# Save to CSV
write.table(count_table, file = "data/derivatives/Count_Table_by_Decay_Stage.csv", sep = ",", quote = FALSE, row.names = FALSE)

# Print the count table to verify
print(count_table)

#Raw data histogram - not every importnt, graphs won't be included in the paper

DeadwoodTypeData <- read.table(file = "data/raw/Deadwood type graph.csv",
                               header = TRUE,
                               sep = ",",
                               na.strings = "NA",
                               stringsAsFactors = TRUE,
                               dec = ".")

DeadwoodTypeData$Type <- ifelse(grepl("stumps", DeadwoodTypeData$X), "Stumps", "Downed Deadwood")

# Add another column for Tree Type (Broadleaf or Coniferous)
DeadwoodTypeData$TreeSpecies <- ifelse(grepl("Broadleaf", DeadwoodTypeData$X), "Broadleaf", "Coniferous")

ggplot(DeadwoodTypeData, aes(x = Type, y = Mean.TreM.abundance, fill = TreeSpecies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "TreM Abundance by Category",
       x = "Deadwood Type",
       y = "Mean TreM Abundance") +
  scale_fill_manual(values = c("Broadleaf" = "#ffb703", "Coniferous" = "#023047")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),   # Title text size
    axis.title.x = element_text(size = 16),                # X-axis label text size
    axis.title.y = element_text(size = 16),                # Y-axis label text size
    axis.text.x = element_text(size = 14),                 # X-axis tick text size
    axis.text.y = element_text(size = 14),                 # Y-axis tick text size
    legend.title = element_text(size = 16),                # Legend title text size
    legend.text = element_text(size = 14)
    )  

ggplot(DeadwoodTypeData, aes(x = Type, y = Mean.TreM.richness, fill = TreeSpecies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "TreM Abundance by Category",
       x = "Deadwood Type",
       y = "Mean TreM Richnesse") +
  scale_fill_manual(values = c("Broadleaf" = "#ffb703", "Coniferous" = "#023047")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),   # Title text size
    axis.title.x = element_text(size = 16),                # X-axis label text size
    axis.title.y = element_text(size = 16),                # Y-axis label text size
    axis.text.x = element_text(size = 14),                 # X-axis tick text size
    axis.text.y = element_text(size = 14),                 # Y-axis tick text size
    legend.title = element_text(size = 16),                # Legend title text size
    legend.text = element_text(size = 14)
  )  



#Total TreMs per deadwood identity - probably won't be included anywhere, but would be good to know if it's correct

TreMs_total <- aggregate(Abundance ~ TreeIdentities2, data = TreMs, FUN = sum, na.rm = TRUE)
print(TreMs_total)

total_trees <- aggregate(Richness ~ TreeIdentities2, data = TreMs, FUN = length)

# Step 2: Calculate the number of trees with Richness >= 1
trees_with_richness <- aggregate(Richness ~ TreeIdentities2, data = TreMs, FUN = function(x) sum(x >= 1))

# Step 3: Merge the two data frames
tree_percentages <- merge(total_trees, trees_with_richness, by = "TreeIdentities2")

# Step 4: Calculate the percentage of trees with Richness >= 1
tree_percentages$Percentage_with_Richness <- (tree_percentages$Richness.y / tree_percentages$Richness.x) * 100

# Rename columns for clarity
colnames(tree_percentages) <- c("TreeIdentities2", "Total_Trees", "Trees_with_Richness", "Percentage_with_Richness")

# View the result
print(tree_percentages)

barplot(tree_percentages$Percentage_with_Richness,
        names.arg = tree_percentages$TreeIdentities2,
        main = "Percentage of Trees with Richness ≥ 1 per Tree Identity",
        ylab = "Percentage",
        xlab = "Tree Identity",
        col = "lightblue",       # Set the color of the bars
        las = 2,                 # Make the x-axis labels perpendicular for better readability
        ylim = c(0, 100))        # Set y-axis limits from 0 to 100

# Optionally add grid lines for better visibility
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

###########BIPARTITE NETWORK

#Creating deadwood identities 
TreMs$DeadwoodIdentities <- rep(0,533)
last_col <- length(TreMs) # used to be 80 Sam
for (i in 1:nrow(TreMs)) {
  if (TreMs[i,3] %in% c("Fagus sylvatica")
      && TreMs[i,5] %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)")){
    TreMs[i,last_col] <- "F. sylvatica Stump"
  }
  
  else if (TreMs[i,3] %in% c("Fagus sylvatica")
           && TreMs[i,5] %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)"))
  {TreMs[i,last_col] <- "F. sylvatica Entire Tree"}
  else if (TreMs[i,3] %in% c("Fagus sylvatica")
           && TreMs[i,5] %in% c("Log/piece of wood (natural)", "Log/piece of wood (artificial)"))
  {TreMs[i,last_col] <- "F. sylvatica Log"}
  else if (TreMs[i,3] %in% c("Picea abies")
           && TreMs[i,5] %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)"))
  {TreMs[i,last_col] <- "P. abies Stump"}
  else if (TreMs[i,3] %in% c("Picea abies")
           && TreMs[i,5] %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)"))
  {TreMs[i,last_col] <- "P. abies Entire Tree"}
  else if (TreMs[i,3] %in% c("Picea abies")
           && TreMs[i,5] %in% c("Log/piece of wood (natural)", "Log/piece of wood (artificial)"))
  {TreMs[i,last_col] <- "P. abies Log"}
  else if (TreMs[i,3] %in% c("Abies alba")
           && TreMs[i,5] %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)"))
  {TreMs[i,last_col] <- "A. alba Stump"}
  else if (TreMs[i,3] %in% c("Abies alba")
           && TreMs[i,5] %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)"))
  {TreMs[i,last_col] <- "A. alba Entire Tree"}
  else if (TreMs[i,3] %in% c("Abies alba")
           && TreMs[i,5] %in% c("Log/piece of wood (natural)", "Log/piece of wood (artificial)"))
  {TreMs[i,last_col] <- "A. alba Log"}
  else if (TreMs[i,3] %in% c("Dead broadleaf","Tilia cordata")
           && TreMs[i,5] %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)"))
  {TreMs[i,last_col] <- "Broadleaf Stump"}
  else if (TreMs[i,3] %in% c("Dead broadleaf", "Tilia cordata")
           && TreMs[i,5] %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)"))
  {TreMs[i,last_col] <- "Broadleaf Entire tree"}
  else if (TreMs[i,3] %in% c("Dead broadleaf","Tilia cordata")
           && TreMs[i,5] %in% c("Log/piece of wood (natural)", "Log/piece of wood (artificial)"))
  {TreMs[i,last_col] <- "Broadleaf Log"}
  else if (TreMs[i,3] %in% c("Dead conifer","Pinus sylvestris","Larix decidua")
           && TreMs[i,5] %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)"))
  {TreMs[i,last_col] <- "Conifer Stump"}
  else if (TreMs[i,3] %in% c("Dead conifer","Pinus sylvestris","Larix decidua")
           && TreMs[i,5] %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)"))
  {TreMs[i,last_col] <- "Conifer Entire Tree"}
  else if (TreMs[i,3] %in% c("Dead conifer","Pinus sylvestris","Larix decidua")
           && TreMs[i,5] %in% c("Log/piece of wood (natural)", "Log/piece of wood (artificial)"))
  {TreMs[i,last_col] <- "Conifer Log"}
  else if (TreMs[i,3] %in% c("Dead no identification")
           && TreMs[i,5] %in% c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)"))
  {TreMs[i,last_col] <- "No ID Stump"}
  else if (TreMs[i,3] %in% c("Dead no identification")
           && TreMs[i,5] %in% c("Entire lying tree (natural)", "Entire lying tree (artificial)"))
  {TreMs[i,last_col] <- "No ID Entire Tree"}
  else 
  {TreMs[i, last_col] <- "No ID Log"}
}
TreMs <- TreMs |>
  relocate(DeadwoodIdentities, .before = 8)

#Creating a new data frame with fewer identities 
IdentitiesTreMs <- TreMs |>
  dplyr::filter(DeadwoodIdentities != "No ID Stump" & DeadwoodIdentities != "No ID Log" & DeadwoodIdentities != "No ID Entire Tree"  & DeadwoodIdentities != "Conifer Log" & DeadwoodIdentities != "Conifer Stump"  & DeadwoodIdentities != "Conifer Entire Tree" & DeadwoodIdentities != "Broadleaf Log"  & DeadwoodIdentities != "Broadleaf Stump" & DeadwoodIdentities != "Broadleaf Entire tree")

meantable <-aggregate(IdentitiesTreMs[,c(81:84,86:95)], list(IdentitiesTreMs$DeadwoodIdentities), mean)
write.table(meantable, file = "data/derivatives/Aggregation.csv", sep = ",", quote = FALSE, row.names = F )
colnames(IdentitiesTreMs)

#Creating the mean and sum table for decay - decided to use sum instead of mean
meantable <- 
TreMs |> 
  dplyr::select(
    WoodpeckerCavities, 
    Concavities, 
    Rotholes, 
    InsectGalleries, 
    ExposedHeartwood, 
    PerennialFungi, 
    Ephermalfungi, 
    Epiphytes, 
    DeadwooodShelter, 
    StumpStructures, 
    LogStructures, 
    WoodyDebris, 
    ExposedRoots
    # ExposedSapwoodReduced # TODO this col doesn't exist yet
  ) |> 
aggregate(#Here including woodpecker cavities and concavities separately 
                       list(TreMs$Treedata.Tree_Decay), 
                       mean)
write.table(meantable, file = "data/derivatives/AggregationDecay.csv", sep = ",", quote = FALSE, row.names = F )

sumtable <- 
  TreMs |> 
  dplyr::select(
    WoodpeckerCavities, # 81
    Concavities, # 82
    Rotholes, # 83
    InsectGalleries, # 84
    ExposedHeartwood, # 87
    PerennialFungi, # 88
    Ephermalfungi, # 89
    Epiphytes, # 90
    DeadwooodShelter, # 91
    StumpStructures, # 92
    LogStructures, # 93
    WoodyDebris, # 94
    ExposedRoots # 95
    # ExposedSapwoodReduced # 98 # TODO this col doesn't exist yet
  ) |> 
  aggregate(#Here including woodpecker cavities and concavities separately 
                       list(TreMs$Treedata.Tree_Decay), 
                       sum)
write.table(sumtable, file = "data/derivatives/AggregationDecaySum.csv", sep = ",", quote = FALSE, row.names = F )

colnames(IdentitiesTreMs)

#Sum table for deadwood identity
sumtable <- IdentitiesTreMs |> 
  dplyr::select(
    WoodpeckerCavities, # 81
    Concavities, # 82
    Rotholes, # 83
    InsectGalleries, # 84
    ExposedHeartwood, # 87
    PerennialFungi, # 88
    Ephermalfungi, # 89
    Epiphytes, # 90
    DeadwooodShelter, # 91
    StumpStructures, # 92
    LogStructures, # 93
    WoodyDebris, # 94
    ExposedRoots # 95
    # ExposedSapwoodReduced # 98 # TODO this col doesn't exist yet
  ) |> aggregate(list(IdentitiesTreMs$DeadwoodIdentities), sum)
write.table(sumtable, file = "data/derivatives/AggregationSum.csv", sep = ",", quote = FALSE, row.names = F )


#Box plot with Tukey test - this didn't work so this section was just a test and isn't needed

anova_model <- aov(PerennialFungi ~ Treedata.Tree_Decay, data = TreMs)
TUKEY <- TukeyHSD(anova_model, 'Treedata.Tree_Decay', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")
# TODO I've removed this section because it doesn't work
# generate_label_df <- function(TUKEY, Treedata.Tree_Decay){
  
#   # Extract labels and factor levels from Tukey post-hoc 
#   Tukey.levels <- TUKEY[[Treedata.Tree_Decay]][,4]
#   Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
#   #I need to put the labels in the same order as in the boxplot :
#   Tukey.labels$Treedata.Tree_Deay=rownames(Tukey.labels)
#   Tukey.labels=Tukey.labels[order(Tukey.labels$Treedata.Tree_Deay) , ]
#   return(Tukey.labels)
# }

# # Error in `if (k2 == 0) ...`:! missing value where TRUE/FALSE needed Sam
# LABELS <- generate_label_df(TUKEY , "Treedata.Tree_Decay")

# my_colors <- c( 
#   rgb(143,199,74,maxColorValue = 255),
#   rgb(242,104,34,maxColorValue = 255), 
#   rgb(111,145,202,maxColorValue = 255)
# )

# a <- boxplot(TreMs$PerennialFungi ~ TreMs$Treedata.Tree_Decay , ylim=c(min(TreMs$PerennialFungi) , 1.1*max(TreMs$PerennialFungi)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="Perennial fungi" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
# over <- 0.1*max( a$stats[nrow(a$stats),] )

# text( c(1:nlevels(TreMs$Treedata.Tree_Decay)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )

# tukey_letters <- multcompLetters4(anova_model, tukey_result)
# tukey_letters_df <- as.data.frame.list(tukey_letters$Treedata.Tree_Decay)
# tukey_letters_df$Treedata.Tree_Decay <- rownames(tukey_letters_df)
# colnames(tukey_letters_df) <- c("Letters", "Treedata.Tree_Decay")

# boxplot_stats <- aggregate(PerennialFungi ~ Treedata.Tree_Decay, data = TreMs, FUN = median, na.rm = TRUE)
# colnames(boxplot_stats) <- c("Treedata.Tree_Decay", "median_TreM")

# plot_data <- merge(boxplot_stats, tukey_letters_df, by = "Treedata.Tree_Decay")

# str(boxplot_stats)
# str(plot_data)

# ggplot(TreMs, aes(x = Treedata.Tree_Decay, y = PerennialFungi)) +
#   geom_boxplot(aes(fill = Treedata.Tree_Decay)) +
#   labs(title = "Box Plot of TreM Abundance by Decay Stage with Tukey Test Results",
#        x = "Decay Stage",
#        y = "Rothole Abundance") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Pastel1") +
#   geom_text(data = plot_data, aes(x = Treedata.Tree_Decay, y = median_TreM + 2, label = Letters), # Adjust "+ 2" for label position
#             color = "black", size = 5)

##############GLMM################## 

#Main model: Tree species + DBH + Decay
#Deadwood type did not work for richness 
#Running the model for abundance, richness and each individual group

#Abundance 
#Decidious, decay 2 and DBH significant 
#Checked for overdispersion with poisson first - the residuals were even more significant 
Model_Abundance <- glmmTMB(Abundance ~ GroupedTreeSpecies + Treedata.DBH_cm +  
                          Treedata.Tree_Decay +
                          (1|Plot),
                        TreMs, family = nbinom2)

summary(Model_Abundance)

Model_Abundance2 <- glmmTMB(Abundance ~ GroupedTreeSpecies + Treedata.DBH_cm +  
                             Treedata.Tree_Decay,
                           TreMs, family = nbinom2)

summary(Model_Abundance2)


Residuals_Model_Abundance <- simulateResiduals (Model_Abundance)
plot(Residuals_Model_Abundance) #Problems
testOutliers(Residuals_Model_Abundance) 
testDispersion(Residuals_Model_Abundance)
testZeroInflation(Residuals_Model_Abundance)
# r.squaredGLMM(Residuals_Model_Abundance) #doesn't work with Dharma

#Richness
#DBH, decay stages 3, 4 and 5 significant
#Overdispersed
Model_Richness <- glmmTMB(Richness ~ Treedata.DBH_cm + GroupedTreeSpecies +
                             Treedata.Tree_Decay +
                             (1|Plot),
                           TreMs, family = nbinom2)

summary(Model_Richness)

Residuals_Model_Richness <- simulateResiduals (Model_Richness)
plot(Residuals_Model_Richness) #Problems
testOutliers(Residuals_Model_Richness) 
testDispersion(Residuals_Model_Richness)
testZeroInflation(Residuals_Model_Richness)
# r.squaredGLMM(Residuals_Model_Model_Richness) #doesn't work with Dharma

#Rotholes
#Fitted with poisson since there was no over dispersion
Model_Rotholes <- glmmTMB(Rotholes ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot),
                          TreMs, family = poisson())

summary(Model_Rotholes)

Residuals_Model_Rotholes <- simulateResiduals (Model_Rotholes)
plot(Residuals_Model_Rotholes) #No problems
testOutliers(Residuals_Model_Rotholes) 
testDispersion(Residuals_Model_Rotholes)
testZeroInflation(Residuals_Model_Rotholes)
# r.squaredGLMM(Residuals_Model_Rotholes) #doesn't work with Dharma

#Insect galleries 
#Fitted with poisson since there was no overdispersion 
#Nothing is significant 
Model_InsectGal <- glmmTMB(InsectGalleries ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot),
                          TreMs, family = poisson())

summary(Model_InsectGal)

Residuals_Model_InsectGal <- simulateResiduals (Model_InsectGal)
plot(Residuals_Model_InsectGal) #No problems
testOutliers(Residuals_Model_InsectGal) 
testDispersion(Residuals_Model_InsectGal)
testZeroInflation(Residuals_Model_InsectGal)
# r.squaredGLMM(Residuals_Model_InsectGal) #doesn't work with Dharma

#WoodpeckerConcavities
#Fitted with poisson since there was no overdispersion 
#DBH and decay stage 2

Model_WoodpeckerConcavities <- glmmTMB(WoodpeckerConcavities ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot),
                          TreMs, family = poisson())

summary(Model_WoodpeckerConcavities)
# Changed Model_Concavities to Model_WoodpeckerConcavities Sam
Residuals_Model_Concavities <- simulateResiduals(Model_WoodpeckerConcavities)
plot(Residuals_Model_Concavities) #No problems
testOutliers(Residuals_Model_Concavities) 
testDispersion(Residuals_Model_Concavities)
testZeroInflation(Residuals_Model_Concavities)
# r.squaredGLMM(Residuals_Model_Concavities) #doesn't work with Dharma

#Exposed sapwood
#Nothing significant - becauase of bark loss

Model_Sapwood <- glmmTMB(ExposedSapwood ~ Treedata.DBH_cm + GroupedTreeSpecies +
                               Treedata.Tree_Decay +
                               (1|Plot),
                             TreMs, family = nbinom2)

summary(Model_Sapwood)

Residuals_Model_Sapwood <- simulateResiduals (Model_Sapwood)
plot(Residuals_Model_Sapwood) 
testOutliers(Residuals_Model_Sapwood) 
testDispersion(Residuals_Model_Sapwood)
testZeroInflation(Residuals_Model_Sapwood)
# r.squaredGLMM(Residuals_Model_Model_Sapwood) #doesn't work with Dharma


#Exposed sapwood without barkloss
#Decay 5 and 4 significant - that makes more sense!
#Fitted with nbinom2, data was overdispersed 

TreMs$ExposedSapwoodReduced <- rowSums(TreMs[, c(46:48)])

Model_Sapwood_2 <- glmmTMB(ExposedSapwoodReduced ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                           Treedata.Tree_Decay +
                           (1|Plot),
                         TreMs, family = nbinom2)

summary(Model_Sapwood_2)

Residuals_Model_Sapwood_2 <- simulateResiduals (Model_Sapwood_2)
plot(Residuals_Model_Sapwood_2) #No problems
testOutliers(Residuals_Model_Sapwood_2) 
testDispersion(Residuals_Model_Sapwood_2)
testZeroInflation(Residuals_Model_Sapwood_2)
# r.squaredGLMM(Residuals_Model_Sapwood_2) #doesn't work with Dharma


#Exposed heartwood
#Doesn't work because of lack of data

Model_Heartwood <- glmmTMB(ExposedHeartwood ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                           Treedata.Tree_Decay +
                           (1|Plot),
                         TreMs, family = nbinom2)

summary(Model_Heartwood)

#Grouped sapwood and heartwood (withour bark)
TreMs$GroupedSapwoodHeartwood <- 
TreMs |> 
  dplyr::select(ExposedHeartwood, ExposedSapwoodReduced) |> 
rowSums()
#Fitted with nbinom2

Model_HeartSap <- glmmTMB(GroupedSapwoodHeartwood ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                             Treedata.Tree_Decay +
                             (1|Plot),
                           TreMs, family = nbinom2)

summary(Model_HeartSap)

Residuals_Model_HeartSap <- simulateResiduals (Model_HeartSap)
plot(Residuals_Model_HeartSap) #No problems
testOutliers(Residuals_Model_HeartSap) 
testDispersion(Residuals_Model_HeartSap)
testZeroInflation(Residuals_Model_HeartSap)


#Perennial fungi 
#Deciduous, decay stage 4 and 5
#Fitted with nbinom2
# TODO this model doesn't really work
Model_Perennials <- glmmTMB(PerennialFungi ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot),
                          TreMs, family = nbinom2)

summary(Model_Perennials)

Residuals_Model_Perennials <- simulateResiduals (Model_Perennials)
plot(Residuals_Model_Perennials) #No problems
testOutliers(Residuals_Model_Perennials) 
# testDispersion(Residuals_Model_Perennials) # TODO this doesn't work
testZeroInflation(Residuals_Model_Perennials)

#Ephermal fungi
# #Doesn't work 
# Model_Ephermals <- glmmTMB(Ephermalfungi ~ Treedata.DBH_cm + GroupedTreeSpecies +
#                               Treedata.Tree_Decay +
#                               (1|Plot),
#                             TreMs, family = nbinom2)

# summary(Model_Ephermals)

# #Grouped all fungi
#Same as perennials (cause that's most of the data )
TreMs$AllFungi <- rowSums(TreMs[, c(88,89)])

#Fitted with nbinom2

Model_AllFungi <- glmmTMB(AllFungi ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                             Treedata.Tree_Decay +
                             (1|Plot),
                           TreMs, family = nbinom2)

summary(Model_AllFungi)

Residuals_Model_AllFungi <- simulateResiduals (Model_AllFungi)
# plot(Residuals_Model_AllFungi) #Some problems
testOutliers(Residuals_Model_AllFungi) 
testDispersion(Residuals_Model_AllFungi)
testZeroInflation(Residuals_Model_AllFungi)
# r.squaredGLMM(Residuals_Model_AllFungi) #doesn't work with Dharma

#Epiphytes
#Fitted with nbinom2
Model_Epiphytes <- glmmTMB(Epiphytes ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot),
                          TreMs, family = nbinom2)

summary(Model_Epiphytes)

Residuals_Model_Epiphytes <- simulateResiduals (Model_Epiphytes)
plot(Residuals_Model_Epiphytes) #Problems
testOutliers(Residuals_Model_Epiphytes) 
testDispersion(Residuals_Model_Epiphytes)
testZeroInflation(Residuals_Model_Epiphytes)
# r.squaredGLMM(Residuals_Model_Epiphytes) #doesn't work with Dharma
     
#Deadwood shelter 
#Fitted with nbinom2
#No ID and decay 3,4,5 are significant 
Model_Shelters <- glmmTMB(DeadwooodShelter ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                            Treedata.Tree_Decay +
                            (1|Plot),
                          TreMs, family = nbinom2)

summary(Model_Shelters)

Residuals_Model_Shelters <- simulateResiduals (Model_Shelters)
plot(Residuals_Model_Shelters) #No problems
testOutliers(Residuals_Model_Shelters) 
testDispersion(Residuals_Model_Shelters)
testZeroInflation(Residuals_Model_Shelters)
# r.squaredGLMM(Residuals_Model_Shelters) #doesn't work with Dharma

#Stump structures 
#Fitted with poisson
#NA for AIC but I'm getting p value for the variables 

Model_StumpStruct <- glmmTMB(StumpStructures ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                               Treedata.Tree_Decay +
                               (1|Plot),
                             TreMs, family = poisson())

summary(Model_StumpStruct)

Residuals_Model_StumpStruct <- simulateResiduals (Model_StumpStruct)
plot(Residuals_Model_StumpStruct) #Small problems
testOutliers(Residuals_Model_StumpStruct) 
testDispersion(Residuals_Model_StumpStruct)
testZeroInflation(Residuals_Model_StumpStruct)
# r.squaredGLMM(Residuals_Model_StumpStruct) #doesn't work with Dharma

#Log structures 
#Fitted with nbinom2
#Nothing significant 
Model_LogStruct <- glmmTMB(LogStructures ~ Treedata.DBH_cm + GroupedTreeSpecies +
                               Treedata.Tree_Decay +
                               (1|Plot),
                             TreMs, family = nbinom2)

summary(Model_LogStruct)

Residuals_Model_LogStruct <- simulateResiduals (Model_LogStruct)
plot(Residuals_Model_LogStruct) #No problems
testOutliers(Residuals_Model_LogStruct) 
testDispersion(Residuals_Model_LogStruct)
testZeroInflation(Residuals_Model_LogStruct)
# r.squaredGLMM(Residuals_Model_LogStruct) #doesn't work with Dharma

#Stump and log structures grouped
#Fitted with nbinom2
#DBH significant
TreMs$GroupedLogStump <- rowSums(TreMs[, c(92,93)])

Model_LogStump <- glmmTMB(GroupedLogStump ~ Treedata.DBH_cm + GroupedTreeSpecies +
                            Treedata.Tree_Decay +
                            (1|Plot),
                          TreMs, family = nbinom2)

summary(Model_LogStump)

Residuals_Model_LogStump <- simulateResiduals (Model_LogStump)
plot(Residuals_Model_LogStump) #No problems
testOutliers(Residuals_Model_LogStump) 
testDispersion(Residuals_Model_LogStump)
testZeroInflation(Residuals_Model_LogStump)

#Woody debris 
#Fitted with poisson, no overdispersion

Model_WoodyDebris <- glmmTMB(WoodyDebris ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                             Treedata.Tree_Decay +
                             (1|Plot),
                           TreMs, family = poisson())

summary(Model_WoodyDebris)

Residuals_Model_WoodyDebris <- simulateResiduals(Model_WoodyDebris)
plot(Residuals_Model_WoodyDebris) #No problems
testOutliers(Model_WoodyDebris) 
testDispersion(Model_WoodyDebris)
testZeroInflation(Model_WoodyDebris)
r.squaredGLMM(Model_WoodyDebris) # This does work

#Exposed roots 
#Nothing is signficiant with both nbinom and binomial
#Fitted with poisson 

Model_ExposedRoots <- glmmTMB(ExposedRoots ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Decay +
                               (1|Plot),
                             TreMs, family = poisson())

summary(Model_ExposedRoots)

Residuals_Model_ExposedRoots <- simulateResiduals (Model_ExposedRoots)
plot(Residuals_Model_ExposedRoots) #No problems
testOutliers(Model_ExposedRoots) 
testDispersion(Model_ExposedRoots)
testZeroInflation(Model_ExposedRoots)
r.squaredGLMM(Model_ExposedRoots) # This does work

#PLOTS
#Have to check for each model what is group and what is facet so that it doesn't plot wrong!!!! str(predicted dataset)

#Abundance - the confidence level line is weird, how to fix?
summary(Model_Abundance)

Predicted_Abundance <- ggpredict(Model_Abundance, 
                                 terms = c("Treedata.DBH_cm", 
                                           "GroupedTreeSpecies[Coniferous spp., Broadleaf spp.]", 
                                           "Treedata.Tree_Decay[Decay stage 1, Decay stage 2]"), 
                                 se = TRUE # TODO This maybe doesn't go anywhere
                                )

plotABUNDANCE <- ggplot(Predicted_Abundance, aes(x = x, y = predicted, colour= group)) +
  geom_smooth(aes(colour = facet, linetype = group), linewidth = 1) +
  geom_ribbon(data = Predicted_Abundance, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=facet, colour=NULL), 
              alpha=.05, show.legend = TRUE)+
  geom_rug(data = TreMs, mapping = aes(x = Treedata.DBH_cm, y = Abundance), 
           col = "steelblue", alpha = 0.1, linewidth = 1) +
  xlab("Diameter (cm)") + ylab("TreM Abundance") +
  scale_x_continuous(n.breaks = 5) +
  theme_bw() + 
  theme(text = element_text(size = 15), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba")) +
  scale_linetype_manual(values = c("solid", "dashed"))
levels(TreMs$GroupedTreeSpecies)

# print(plotABUNDANCE)
plotABUNDANCE

ggsave(filename = "data/derivatives/plotABUNDANCE.png", plot = plotABUNDANCE, 
       width = 7, height = 5, units = "in", dpi = 300)

#Richness

summary(Model_Richness)

Predicted_Richness <- ggpredict(Model_Richness, 
                                 terms = c("Treedata.DBH_cm", 
                                           "Treedata.Tree_Decay[Decay stage 1, Decay stage 3, Decay stage 4,Decay stage 5]"))
str(Predicted_Richness)

# TODO find where conf.low and conf.high come from
# skipping below 
# plotRICHNESS <- ggplot() + 
#   geom_smooth(data = Predicted_Richness, mapping = aes(x = x, y = predicted, colour= group)) + 
#   geom_ribbon(data = Predicted_Richness, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), alpha=.05, show.legend = TRUE) + 
#   geom_rug(data = TreMs, mapping = aes(x = Treedata.DBH_cm, y = Richness), col="steelblue", alpha=0.1, size=1) + 
#   xlab("Diameter (cm)") + 
#   ylab("TreM Richness") + 
#   scale_x_continuous(n.breaks = 5) + 
#   theme_bw() + 
#   theme(
#     text = element_text(size = 15),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black"),
#     legend.title = element_blank(),
#     plot.margin = margin(0.5, 2, 0.5, 0.5)
#   ) + 
#   scale_linetype(guide = "none") + 
#   scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 3" = "#2f4858", "Decay stage 4" = "#f6ae2d", "Decay stage 5" = "#f26419")) + 
#   scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 3" = "#2f4858", "Decay stage 4" = "#f6ae2d", "Decay stage 5" = "#f26419"))

# plotRICHNESS


# ggsave(filename = "data/derivatives/plotRICHNESS.png", plot = plotRICHNESS, 
#        width = 7, height = 5, units = "in", dpi = 300)

# figTremsDBH <- ggarrange(plotABUNDANCE, plotRICHNESS,
#                          ncol = 2, nrow = 1, 
#                          common.legend = TRUE, 
#                          legend = "bottom",
#                          combine = TRUE)  # Add this line to combine legends
# figTremsDBH

# # Save to PDF
# pdf(file = "FigureTreMs.pdf", width = 6.5, height = 4.5)
# print(figTremsDBH)
# dev.off()

# skipping end

#Rotholes

summary(Model_Rotholes)

Predicted_Rotholes <- ggpredict(Model_Rotholes, 
                                terms = c("GroupedTreeSpecies[Coniferous spp., Broadleaf spp.]",
                                  "Treedata.Tree_Decay[Decay stage 1,Decay stage 5]"),
                                se = TRUE)

boxplotROTHOLES <- ggplot(Predicted_Rotholes, aes(x = x, y = predicted, fill = group)) +
  geom_col(position = "dodge", width = 0.7) +  # Columns for predicted values
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.7), 
                width = 0.25) +  # Error bars based on confidence intervals
  labs(
       x = "Tree species",
       y = "Rotholes",
       fill = "Decay stage") +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", 
                               "Decay stage 5" = "#f26419")) +
  theme_minimal()

# TODO out of order boxplotSAPWOOD is created later
figTremsBox <- ggarrange(boxplotROTHOLES, boxplotSAPWOOD, boxPERENNIAL, boxSHELT,
                          ncol = 2, nrow =2, 
                          common.legend = TRUE, 
                          legend="bottom")

figTremsBox

pdf(file = "FigureTreMsBox.pdf", width = 6.5, height = 4.5)
print(figTremsBox)
dev.off()

str(Predicted_Rotholes)

#Insect galleries 
summary(Model_InsectGal)

#WoodpeckerConcavities 

summary(Model_WoodpeckerConcavities)

Predicted_Concavities <- ggpredict(Model_WoodpeckerConcavities, 
                                terms = c("Treedata.DBH_cm", 
                                          "Treedata.Tree_Decay[Decay stage 1, Decay stage 2]"), 
                                se = TRUE)
str(Predicted_Concavities)

plotCONCAV <- ggplot()+
  geom_smooth (data= Predicted_Concavities, mapping = aes(x = x, y = predicted, colour= group)) +
  geom_ribbon(data = Predicted_Concavities, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  
  geom_rug (data= TreMs,mapping= aes (x= Treedata.DBH_cm, y= WoodpeckerConcavities), col="steelblue",alpha=0.1, size=1)+
  xlab("Diameter (cm)") + ylab("Woodpecker Cavities and Concavities")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=8), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba")) +
  theme(legend.title=element_blank())

plotCONCAV

# TODO out of order plotEPIPHYTES is created later
figTremsDBH <- ggarrange(plotCONCAV, plotEPIPHYTES,
                         ncol = 2, nrow = 1, 
                         common.legend = TRUE, 
                         legend = "bottom",
                         combine = TRUE)  # Add this line to combine legends
figTremsDBH

# Save to PDF
pdf(file = "FigureTreMsDBH2.pdf", width = 6.5, height = 4.5)
print(figTremsDBH)
dev.off()

#Exposed sapwood

summary(Model_Sapwood)
#Nothing significant

#Exposed sapwood without bark loss 
summary(Model_Sapwood_2)

Predicted_Sapwood <- ggpredict(Model_Sapwood_2, 
                                terms = c("Treedata.Tree_Decay[Decay stage 1,Decay stage 4, Decay stage 5]"),
                                se = TRUE)
str(Predicted_Sapwood)

boxplotSAPWOOD <- ggplot(Predicted_Sapwood, aes(x = x, y = predicted, fill = x)) +
  geom_col(position = "dodge", width = 0.7) +  # Columns for predicted values
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.7), 
                width = 0.25) +  # Error bars based on confidence intervals
  labs(,
       x = "Decay stage",
       y = "Exposed Sapwood",
       fill = "Decay stage") +
  scale_fill_manual(values = c("Decay stage 1" = "#849324",
                               "Decay stage 4" = "#f6ae2d",
                               "Decay stage 5" = "#f26419")) +
  theme_minimal()

#Grouped heartwood and sapwood (without bark loss)
#Decay 4 and 5 significant

summary(Model_HeartSap)

Predicted_HeartSap <- ggpredict(Model_HeartSap, 
                               terms = c("Treedata.Tree_Decay[Decay stage 1,Decay stage 4, Decay stage 5]"),
                               se = TRUE)
str(Predicted_HeartSap)

boxSAPHEART <- ggplot(Predicted_HeartSap, aes(x = x, y = predicted, fill = x)) +
  geom_col(position = "dodge", width = 0.7) +  # Columns for predicted values
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.7), 
                width = 0.25) +  # Error bars based on confidence intervals
  labs(title = "Exposed heartwood and sapwood",
       x = "Decay stage",
       y = "Tree Injuries and Exposed wood", 
       fill = "Decay stage") +
  scale_fill_manual(values = c("Decay stage 1" = "#849324",
                               "Decay stage 4" = "#f6ae2d",
                               "Decay stage 5" = "#f26419")) +
  theme_minimal()

# TODO out of order allfungi created later
figTremsBox <- ggarrange(boxSAPHEART, allfungi,
                         ncol = 2, nrow =1)

figTremsBox

#Perennials 

summary(Model_Perennials)
# TODO ! Prediction is not possible for unknown fixed effects: GroupedTreeSpecies
# # Probably some factor levels in 'newdata' require fitting a new model.
# # skipping sam
# Predicted_Perennials <- ggpredict(Model_Perennials, 
#                                 terms = c("GroupedTreeSpecies[Coniferous spp., Deciduous spp.]",
#                                           "Treedata.Tree_Decay[Decay stage 1,Decay stage 4, Decay stage 5]"),
#                                 se = TRUE)
# str(Predicted_Perennials)

# boxPERENNIAL <- ggplot(Predicted_Perennials, aes(x = x, y = predicted, fill = group)) +
#   geom_col(position = "dodge", width = 0.7) +  # Columns for predicted values
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
#                 position = position_dodge(0.7), 
#                 width = 0.25) +  # Error bars based on confidence intervals
#   labs(,
#        x = "Tree species",
#        y = "Perennial Fungi",
#        fill = "Decay stage") +
#   scale_fill_manual(values = c("Decay stage 1" = "#849324",
#                                "Decay stage 4" = "#f6ae2d",
#                                "Decay stage 5" = "#f26419")) +
#   theme_minimal()


# str(Predicted_Perennials)

#Ephermal fungi - doesn't work, not enough data 
#All fungi 
summary(Model_AllFungi)

Predicted_AllFungi <- ggpredict(Model_AllFungi, 
                                  terms = c("GroupedTreeSpecies[Coniferous spp., Broadleaf spp.]",
                                            "Treedata.Tree_Decay[Decay stage 1,Decay stage 4, Decay stage 5]"),
                                  se = TRUE)
str(Predicted_AllFungi)

levels(TreMs$GroupedTreeSpecies)

allfungi <- ggplot(Predicted_AllFungi, aes(x = x, y = predicted, fill = group)) +
  geom_col(position = "dodge", width = 0.7) +  # Columns for predicted values
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.7), 
                width = 0.25) +  # Error bars based on confidence intervals
  labs(title = "Perennial and Ephemeral fungi",
       x = "Tree species",
       y = "Perennial and Ephemeral fungi",
       fill = "Decay stage") +
  scale_fill_manual(values = c("Decay stage 1" = "#849324",
                               "Decay stage 4" = "#f6ae2d",
                               "Decay stage 5" = "#f26419")) +
  theme_minimal()

allfungi

#Epiphytes
summary(Model_Epiphytes)

Predicted_Epiphytes <- ggpredict(Model_Epiphytes, 
                                   terms = c("Treedata.DBH_cm"), 
                                   se = TRUE)
str(Predicted_Epiphytes)

plotEPIPHYTES <- ggplot() +
  geom_smooth(data = Predicted_Epiphytes, 
              mapping = aes(x = x, y = predicted), 
              colour = "black") +  # Set the line color to black
  geom_ribbon(data = Predicted_Epiphytes, 
              mapping = aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, colour = NULL), 
              alpha = .05, show.legend = FALSE) +
  geom_rug(data = TreMs, 
           mapping = aes(x = Treedata.DBH_cm, y = Epiphytes), 
           col = "steelblue", alpha = 0.1, size = 1) +
  xlab("Diameter (cm)") + 
  ylab("Epiphytes") + 
  scale_x_continuous(n.breaks = 5) +
  theme(text = element_text(size = 8),  
        legend.title = element_blank(), 
        plot.margin = margin(0.5, 2, 0.5, 0.5)) +
  scale_linetype(guide = "none") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_viridis(discrete = TRUE, option = "D") + 
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.title = element_blank())


plotEPIPHYTES


#Deadwood shelter 

summary(Model_Shelters)

Predicted_Shelters <- ggpredict(Model_Shelters, 
                                terms = c("Treedata.Tree_Decay[Decay stage 1,Decay stage 3, Decay stage 4, Decay stage 5]"),
                                se = TRUE)
str(Predicted_Shelters)

boxSHELT <- ggplot(Predicted_Shelters, aes(x = x, y = predicted, fill = x)) +
  geom_col(position = "dodge", width = 0.7) +  # Columns for predicted values
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.7), 
                width = 0.25) +  # Error bars based on confidence intervals
  labs(
       x = "Decay stage",
       y = "Deadwood shelters", 
       fill = "Decay stage") +
  scale_fill_manual(values = c("Decay stage 1" = "#849324",
                               "Decay stage 3" = "#2f4858",
                               "Decay stage 4" = "#f6ae2d",
                               "Decay stage 5" = "#f26419")) +
  theme_minimal()



#Stump structures 

summary(Model_StumpStruct)
# TODO ! Prediction is not possible for unknown fixed effects: GroupedTreeSpecies
# # Probably some factor levels in 'newdata' require fitting a new model.
# # Skipping Sam
# #Options 1 - the lines are flat because all my predicted values are close to 0 
# Predicted_StumpStruct <- ggpredict(Model_StumpStruct, 
#                                    terms = c("Treedata.DBH_cm", 
#                                              "GroupedTreeSpecies[Coniferous spp., Deciduous spp.]"))
# str(Predicted_StumpStruct)

# plotSTUMPS <- ggplot()+
#   geom_smooth (data= Predicted_StumpStruct, mapping = aes(x = x, y = predicted, colour= group)) +
#   geom_ribbon(data = Predicted_StumpStruct, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
#               alpha=.05, show.legend = FALSE)+
  
#   geom_rug (data= TreMs,mapping= aes (x= Treedata.DBH_cm, y= StumpStructures), col="steelblue",alpha=0.1, size=1)+
#   xlab("Diameter at Breast Height (cm)") + ylab("Stump Structures")+ 
#   scale_x_continuous(n.breaks =5)+
#   theme(text = element_text(size=8), 
#         legend.title = element_blank(),
#         plot.margin = margin(0.5, 2, 0.5, 0.5))+
#   scale_linetype(guide = "none")+
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   scale_color_manual(values = c("Coniferous spp." = "#003566", "Deciduous spp." = "#ffb703")) +
#   scale_fill_manual(values = c("Coniferous spp." = "#003566", "Deciduous spp." = "#ffb703")) +
#   theme(legend.title=element_blank())

# plotSTUMPS


# #Option 2 - adjusting the y axis but then I have some abnormally low numbers 
# plotSTUMPS <- ggplot() +
#   geom_smooth(data = Predicted_StumpStruct, 
#               mapping = aes(x = x, y = predicted, color = group), 
#               method = "loess", se = FALSE) +
#   geom_ribbon(data = Predicted_StumpStruct, 
#               mapping = aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
#               alpha = 0.2, color = NA) +
#   geom_rug(data = TreMs, mapping = aes(x = Treedata.DBH_cm, y = StumpStructures), 
#            color = "steelblue", alpha = 0.1, size = 1) +
#   labs(title = "Predicted Stump Structures",
#        x = "Diameter at Breast Height (cm)", 
#        y = "Stump Structures") +
#   scale_x_continuous(n.breaks = 5) +
#   scale_y_continuous(limits = c(0, max(Predicted_StumpStruct$predicted) * 1.2)) +
#   scale_color_viridis(discrete = TRUE, option = "D") +
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal() +
#   theme(text = element_text(size = 8), 
#         legend.title = element_blank(),
#         plot.margin = margin(0.5, 2, 0.5, 0.5),
#         panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         axis.line = element_line(colour = "black"))

# print(plotSTUMPS)

#Log structures 
summary(Model_LogStruct)

#Nothing is significant - they depend on length 

#Log and stump structures - DBH significant  
summary(Model_LogStump)
#Also values very close to 0, flat line

Predicted_LogStump <- ggpredict(Model_LogStump, 
                                 terms = c("Treedata.DBH_cm"), 
                                 se = TRUE)
str(Predicted_LogStump)

plotLOGSTUMP <- ggplot()+
  geom_smooth (data= Predicted_LogStump, mapping = aes(x = x, y = predicted)) +
  geom_ribbon(data = Predicted_LogStump, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  
  geom_rug (data= TreMs,mapping= aes (x= Treedata.DBH_cm, y= GroupedLogStump), col="steelblue",alpha=0.1, size=1)+
  xlab("Diameter at Breast Height (cm)") + ylab("Log and Stump structures")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=8), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)+
  theme(legend.title=element_blank())

plotLOGSTUMP


#Woody debris - also very small values close to 0 

Predicted_WoodyDebris <- ggpredict(Model_WoodyDebris, 
                                 terms = c("Treedata.DBH_cm"), 
                                 se = TRUE)
str(Predicted_WoodyDebris)

plotDEBRIS <- ggplot()+
  geom_smooth (data= Predicted_WoodyDebris, mapping = aes(x = x, y = predicted)) +
  geom_ribbon(data = Predicted_WoodyDebris, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  
  geom_rug (data= TreMs,mapping= aes (x= Treedata.DBH_cm, y= WoodyDebris), col="steelblue",alpha=0.1, size=1)+
  xlab("Diameter at Breast Height (cm)") + ylab("Woody Debris")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=8), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)+
  theme(legend.title=element_blank())

plotDEBRIS


####STUMP and LOG DATASETS - I tried to run the model for the deadwood types separately, pribably won't be included

StumpData <- (TreMs[which(TreMs$GroupedDeadwoodType=="Stump"),])
LogData <- TreMs[which(TreMs$GroupedDeadwoodType != "Stump"),]

#Stump Abundance 
Model_Abundance_Stump <- glmmTMB(Abundance ~ GroupedTreeSpecies + Treedata.DBH_cm +  
                             Treedata.Tree_Decay +
                             (1|Plot),
                           StumpData, family = nbinom2)

summary(Model_Abundance_Stump)

Residuals_Model_Abundance_Stump <- simulateResiduals (Model_Abundance_Stump)
plot(Residuals_Model_Abundance_Stump) #Problems
testOutliers(Residuals_Model_Abundance_Stump) 
testDispersion(Residuals_Model_Abundance_Stump)
testZeroInflation(Residuals_Model_Abundance_Stump)


Predicted_AbundanceStump <- ggpredict(Model_Abundance_Stump, 
                                      terms = c("Treedata.DBH_cm", 
                                                "Treedata.Tree_Decay[Decay stage 1, Decay stage 5]"))

plotABUNDANCESTUMP <- ggplot()+
  geom_smooth (data= Predicted_AbundanceStump, mapping = aes(x = x, y = predicted, colour= group)) +
  geom_ribbon(data = Predicted_AbundanceStump, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  geom_rug (data= StumpData,mapping= aes (x= Treedata.DBH_cm, y= Abundance), col="steelblue",alpha=0.1, size=1)+
  xlab("Diameter (cm)") + ylab("TreM Abundance on Stumps")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=8), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 5" = "#f26419")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 5" = "#f26419")) +
  theme(legend.title=element_blank())

plotABUNDANCESTUMP


#Log abundance 

Model_Abundance_Log <- glmmTMB(Abundance ~ GroupedTreeSpecies + Treedata.DBH_cm + 
                                   Treedata.Tree_Decay +
                                   (1|Plot),
                                 LogData, family = nbinom2)

summary(Model_Abundance_Log)

Residuals_Model_Abundance_Log <- simulateResiduals (Model_Abundance_Log)
plot(Residuals_Model_Abundance_Log) #Problems
testOutliers(Residuals_Model_Abundance_Log) 
testDispersion(Residuals_Model_Abundance_Log)
testZeroInflation(Residuals_Model_Abundance_Log)

Predicted_AbundanceLog <- ggpredict(Model_Abundance_Log, 
                                      terms = c("Treedata.DBH_cm", 
                                                "Treedata.Tree_Decay[Decay stage 1, Decay stage 2, Decay stage 4]"))

plotABUNDANCELOG <- ggplot()+
  geom_smooth (data= Predicted_AbundanceLog, mapping = aes(x = x, y = predicted, colour= group)) +
  geom_ribbon(data = Predicted_AbundanceLog, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  geom_rug (data= LogData,mapping= aes (x= Treedata.DBH_cm, y= Abundance), col="steelblue",alpha=0.1, size=1)+
  xlab("Diameter (cm)") + ylab("TreM Abundance on Downed Deadwood")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=14), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba", "Decay stage 4" = "#f6ae2d")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba", "Decay stage 4" = "#f6ae2d")) +
  theme(legend.title=element_blank())

plotABUNDANCELOG

#Stump richness

Model_Richness_Stump <- glmmTMB(Richness ~ GroupedTreeSpecies + Treedata.DBH_cm +  
                                   Treedata.Tree_Decay +
                                   (1|Plot),
                                 StumpData, family = nbinom2)

summary(Model_Richness_Stump)

Residuals_Model_Richness_Stump <- simulateResiduals (Model_Richness_Stump)
plot(Residuals_Model_Richness_Stump) #Problems
testOutliers(Residuals_Model_Richness_Stump) 
testDispersion(Residuals_Model_Richness_Stump)
testZeroInflation(Residuals_Model_Richness_Stump)

Predicted_RichnessStump <- ggpredict(Model_Richness_Stump, 
                                      terms = c("Treedata.DBH_cm", 
                                                "Treedata.Tree_Decay[Decay stage 1, Decay stage 5]"))

plotRICHNESSSTUMP <- ggplot()+
  geom_smooth (data= Predicted_RichnessStump, mapping = aes(x = x, y = predicted, colour= group)) +
  geom_ribbon(data = Predicted_RichnessStump, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  geom_rug (data= StumpData,mapping= aes (x= Treedata.DBH_cm, y= Richness), col="steelblue",alpha=0.1, size=1)+
  xlab("Diameter (cm)") + ylab("TreM Richness on Stumps")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=15), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 5" = "#f26419")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 5" = "#f26419")) +
  theme(legend.title=element_blank())

plotRICHNESSSTUMP


#Log richness

Model_Richness_Log <- glmmTMB(Richness ~ GroupedTreeSpecies + Treedata.DBH_cm +  
                                  Treedata.Tree_Decay +
                                  (1|Plot),
                                LogData, family = nbinom2)

summary(Model_Richness_Log)

Residuals_Model_Richness_Log <- simulateResiduals (Model_Richness_Log)
plot(Residuals_Model_Richness_Log) #Problems
testOutliers(Residuals_Model_Richness_Log) 
testDispersion(Residuals_Model_Richness_Log)
testZeroInflation(Residuals_Model_Richness_Log)

Predicted_RichnessLog <- ggpredict(Model_Richness_Log, 
                                    terms = c("Treedata.DBH_cm", 
                                              "Treedata.Tree_Decay[Decay stage 1, Decay stage 3, Decay stage 4, Decay stage 5]"))

plotRICHNESSLOG <- ggplot()+
  geom_smooth (data= Predicted_RichnessLog, mapping = aes(x = x, y = predicted, colour= group)) +
  geom_ribbon(data = Predicted_RichnessLog, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  geom_rug (data= LogData,mapping= aes (x= Treedata.DBH_cm, y= Richness), col="steelblue",alpha=0.1, size=1)+
  xlab("Diameter (cm)") + ylab("TreM Richness on Downed Deadwood")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=8), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 3" = "#2f4858", "Decay stage 4" = "#f6ae2d","Decay stage 5" = "#f26419")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 3" = "#2f4858", "Decay stage 4" = "#f6ae2d","Decay stage 5" = "#f26419")) +
  theme(legend.title=element_blank())

plotRICHNESSLOG

figTrems <- ggarrange(plotABUNDANCESTUMP, plotRICHNESSSTUMP,
                         ncol = 2, nrow = 2, 
                         common.legend = TRUE, 
                         legend = "bottom",
                         combine = TRUE)  # Add this line to combine legends
figTrems

# Save to PDF
pdf(file = "data/derivatives/FigureTreMs.pdf", width = 6.5, height = 4.5)
dev.off()

par(mar = c(5, 6, 4, 2))  # Increase left margin (second number) for y-axis label space
figTrems <- ggarrange(plotABUNDANCELOG, plotRICHNESSLOG,
                      ncol = 2, nrow = 2, 
                      common.legend = TRUE, 
                      legend = "bottom",
                      combine = TRUE)  # Add this line to combine legends
figTrems


###### MODEL WITH LENGTH ######
#Tried a model with length for logs only - won't be included

#Log abundance 

Model2_Abundance_Log <- glmmTMB(Abundance ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                                 Treedata.Tree_Decay +
                                 (1|Plot),
                               LogData, family = nbinom2)

summary(Model2_Abundance_Log)

Residuals_Model2_Abundance_Log <- simulateResiduals (Model2_Abundance_Log)
testOutliers(Residuals_Model2_Abundance_Log) 
testDispersion(Residuals_Model2_Abundance_Log)
testZeroInflation(Residuals_Model2_Abundance_Log)

#Log richness

Model2_Richness_Log <- glmmTMB(Richness ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                                  Treedata.Tree_Decay +
                                  (1|Plot),
                                LogData, family = nbinom2)

summary(Model2_Richness_Log)

Residuals_Model2_Richness_Log <- simulateResiduals (Model2_Richness_Log)
testOutliers(Residuals_Model2_Richness_Log) 
testDispersion(Residuals_Model2_Richness_Log)
testZeroInflation(Residuals_Model2_Richness_Log)

#TreM forms
LogData$Cavities <- rowSums(LogData[, c(83:85)])
LogData$TreeInjuries <- rowSums(LogData[, c(86,87)])
LogData$FruitingBodies <- rowSums(LogData[, c(88,89)])
LogData$DeadwoodShelterForm <- rowSums(LogData[, c(91,93)])
#Plus those where it's only one column, didn't remake it

#Cavities 
#Fitted with poisson, no overdispersion 
#Length not significant (neither is DBH here - weird )
Model2_Cavities_Log <- glmmTMB(Cavities ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                                 Treedata.Tree_Decay +
                                 (1|Plot),
                               LogData, family = poisson())

summary(Model2_Cavities_Log)

Residuals_Model2_Cavities_Log <- simulateResiduals (Model2_Cavities_Log)
testOutliers(Residuals_Model2_Cavities_Log) 
testDispersion(Residuals_Model2_Cavities_Log)
testZeroInflation(Residuals_Model2_Cavities_Log)

#Tree injuries  
#Length highly significant!
#Both poisson and ninom2 have a problem with residuals but I think poisson significance is smaller??
Model2_Injuries_Log <- glmmTMB(TreeInjuries ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                                 Treedata.Tree_Decay +
                                 (1|Plot),
                               LogData, family = poisson())

summary(Model2_Injuries_Log)

Residuals_Model2_Injuries_Log <- simulateResiduals (Model2_Injuries_Log)
testOutliers(Residuals_Model2_Injuries_Log) 
testDispersion(Residuals_Model2_Injuries_Log)
testZeroInflation(Residuals_Model2_Injuries_Log)

#Fungi fruiting bodies 
#Fitted with nbinom2, overdispersion with poisson 
Model2_Fungi_Log <- glmmTMB(FruitingBodies ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                                 Treedata.Tree_Decay +
                                 (1|Plot),
                               LogData, family = nbinom2)

summary(Model2_Fungi_Log)

Residuals_Model2_Fungi_Log <- simulateResiduals (Model2_Fungi_Log)
testOutliers(Residuals_Model2_Fungi_Log) 
testDispersion(Residuals_Model2_Fungi_Log)
testZeroInflation(Residuals_Model2_Fungi_Log)

#Epiphytes - nothing
#Fitted with nbinom2
Model2_Epiphytes_Log <- glmmTMB(Epiphytes ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                                Treedata.Tree_Decay +
                                (1|Plot),
                              LogData, family = nbinom2)

summary(Model2_Epiphytes_Log)

Residuals_Model2_Epiphytes_Log <- simulateResiduals (Model2_Epiphytes_Log)
testOutliers(Residuals_Model2_Epiphytes_Log) 
testDispersion(Residuals_Model2_Epiphytes_Log)
testZeroInflation(Residuals_Model2_Epiphytes_Log)

#Deadwoodshelter (including log structures) - length significant
#Fitted with poisson, no overdispersion
Model2_Shelter_Log <- glmmTMB(DeadwoodShelterForm ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                              Treedata.Tree_Decay +
                              (1|Plot),
                            LogData, family = poisson())

summary(Model2_Shelter_Log)

Residuals_Model2_Shelter_Log <- simulateResiduals (Model2_Shelter_Log)
testOutliers(Residuals_Model2_Shelter_Log) 
testDispersion(Residuals_Model2_Shelter_Log)
testZeroInflation(Residuals_Model2_Shelter_Log)

#Woody debris 
#Fitted with poission
#Nothing significant 

Model2_Debris_Log <- glmmTMB(WoodyDebris ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                                Treedata.Tree_Decay +
                                (1|Plot),
                              LogData, family = poisson())

summary(Model2_Debris_Log)

Residuals_Model2_Debris_Log <- simulateResiduals (Model2_Debris_Log)
testOutliers(Residuals_Model2_Debris_Log) 
testDispersion(Residuals_Model2_Debris_Log)
testZeroInflation(Residuals_Model2_Debris_Log)

#Exposed roots
#Fitted with poisson
#Nothing significant 

Model2_Roots_Log <- glmmTMB(ExposedRoots ~ GroupedTreeSpecies + Treedata.DBH_cm + Treedata.Tree_Length +
                               Treedata.Tree_Decay +
                               (1|Plot),
                             LogData, family = poisson())

summary(Model2_Roots_Log)

Residuals_Model2_Roots_Log <- simulateResiduals (Model2_Roots_Log)
testOutliers(Residuals_Model2_Roots_Log) 
testDispersion(Residuals_Model2_Roots_Log)
testZeroInflation(Residuals_Model2_Roots_Log)

#Plots for those where length was significant 

#Abundance 
summary(Model2_Abundance_Log)

Predicted_AbundanceLength <- ggpredict(Model2_Abundance_Log, 
                                    terms = c("Treedata.Tree_Length", 
                                              "Treedata.Tree_Decay[Decay stage 1, Decay stage 2]"))

plotABUNDANCELENGTH <- ggplot()+
  geom_smooth (data= Predicted_AbundanceLength, mapping = aes(x = x, y = predicted, colour= group)) +
  geom_ribbon(data = Predicted_AbundanceLength, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  geom_rug (data= LogData,mapping= aes (x= Treedata.Tree_Length, y= Abundance), col="steelblue",alpha=0.1, size=1)+
  xlab("Length (m)") + ylab("TreM Abundance on Downed Deadwood")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=8), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 2" = "#6096ba")) +
  theme(legend.title=element_blank())

plotABUNDANCELENGTH

#Tree injuries
summary(Model2_Injuries_Log)

Predicted_InjuriesLength <- ggpredict(Model2_Injuries_Log, 
                                       terms = c("Treedata.Tree_Length"))

plotINJURIESLENGTH <- ggplot() +
  geom_smooth(data = Predicted_InjuriesLength, 
              mapping = aes(x = x, y = predicted, linetype = group), 
              colour = "black") +  # Set line color to black
  geom_ribbon(data = Predicted_InjuriesLength, 
              mapping = aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = group, colour = NULL),  
              alpha = .05, show.legend = FALSE) +
  geom_rug(data = LogData, 
           mapping = aes(x = Treedata.Tree_Length, y = TreeInjuries), 
           col = "steelblue", alpha = 0.1, size = 1) +
  xlab("Length (m)") + 
  ylab("Tree Injuries on Downed Deadwood") + 
  scale_x_continuous(n.breaks = 5) +
  theme(text = element_text(size = 8),  
        legend.title = element_blank(), 
        plot.margin = margin(0.5, 2, 0.5, 0.5)) +
  scale_linetype(guide = "none") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank())

#Deadwood shelter (without stump structures)
summary(Model2_Shelter_Log)

Predicted_ShelterLength <- ggpredict(Model2_Shelter_Log, 
                                       terms = c("Treedata.Tree_Length", 
                                                 "Treedata.Tree_Decay[Decay stage 1, Decay stage 3, Decay stage 4, Decay stage 5]"))

plotSHELTERLENGTH <- ggplot()+
  geom_smooth (data= Predicted_ShelterLength, mapping = aes(x = x, y = predicted, colour= group)) +
  geom_ribbon(data = Predicted_ShelterLength, mapping = aes(x = x, y = predicted, ymin=conf.low, ymax=conf.high, fill=group, colour=NULL), 
              alpha=.05, show.legend = FALSE)+
  geom_rug (data= LogData,mapping= aes (x= Treedata.Tree_Length, y= DeadwoodShelterForm), col="steelblue",alpha=0.1, size=1)+
  xlab("Length (m)") + ylab("Deadwood shelters on Downed Deadwood")+ 
  scale_x_continuous(n.breaks =5)+
  theme(text = element_text(size=8), 
        legend.title = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5))+
  scale_linetype(guide = "none")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c("Decay stage 1" = "#849324", "Decay stage 3" = "#2f4858", "Decay stage 4" = "#f6ae2d","Decay stage 5" = "#f26419")) +
  scale_fill_manual(values = c("Decay stage 1" = "#849324", "Decay stage 3" = "#2f4858", "Decay stage 4" = "#f6ae2d","Decay stage 5" = "#f26419")) +
  theme(legend.title=element_blank())

plotSHELTERLENGTH

figTremsDBH <- ggarrange(plotABUNDANCELENGTH, plotINJURIESLENGTH, plotSHELTERLENGTH, 
                         ncol = 2, nrow = 2, 
                         common.legend = TRUE, 
                         legend = "bottom",
                         combine = TRUE)  # Add this line to combine legends
figTremsDBH

# Save to PDF
pdf(file = "data/derivatives/FigureTreMsLength.pdf", width = 6.5, height = 4.5)
print(figTremsDBH)
dev.off()

######### PLOT LEVEL ANALYSIS 
#Still need to see what kind of analysis we'll do here - so this section is not very important

TreMs <- TreMs |>
  mutate(ForestType = case_when(
    Plot %in% c(1, 2, 8, 10) ~ "Mixed-coniferous",     # For plots 1, 2, 6, and 9
    TRUE ~ "Mixed-coniferous-broadleaved"                # For all other plots
  ))

TreMs$ForestType <- as.factor(TreMs$ForestType)

plot_abundance_mean <- aggregate(Abundance ~ Plot, data = TreMs, mean, na.rm = TRUE)
print(plot_abundance_mean)
write.table(plot_abundance_mean, file = "data/derivatives/PlotAbundanceMean.csv", sep = ",", quote = FALSE, row.names = F)

plot_richness_mean <- aggregate(Richness ~ Plot, data = TreMs, mean, na.rm = TRUE)
print(plot_richness_mean)
write.table(plot_richness_mean, file = "data/derivatives/PlotRichnessMean.csv", sep = ",", quote = FALSE, row.names = F)

PlotTreMSum <- read.csv("data/PlotData.csv", sep = ",")

PlotTreMMean <- read.csv ("data/PlotDataMean.csv", sep = ",")

#abundance
wilcox.test(Abundance ~ Forest.Type, data=PlotTreMSum)
boxplot(Abundance ~ Forest.Type, data=PlotTreMSum)
t.test(Abundance ~ Forest.Type, data=PlotTreMSum)

wilcox.test(Abundance ~ Forest.type, data=PlotTreMMean)
boxplot(Abundance ~ Forest.type, data=PlotTreMMean)
t.test(Abundance ~ Forest.type, data=PlotTreMMean)

#richness
wilcox.test(Richness ~ Forest.Type, data=PlotTreMSum)
boxplot(Richness ~ Forest.Type, data=PlotTreMSum)
t.test(Richness ~ Forest.Type, data=PlotTreMSum)
  
wilcox.test(Richness ~ Forest.type, data=PlotTreMMean)
boxplot(Richness ~ Forest.type, data=PlotTreMMean)
t.test(Richness ~ Forest.type, data=PlotTreMMean)



levels(TreMs$GroupedTreeSpecies)
class(TreMs$GroupedTreeSpecies)


###LAST DAY STUFF

total_TreMs <- sum(TreMs$Abundance, na.rm = TRUE)

# Step 2: Sum the specific group of TreMs, say columns TreM1, TreM5, and TreM10
new_TreMs <- rowSums(TreMs[, c("ExposedRoots", "WoodyDebris", "LogStructures", "StumpStructures", "DeadwooodShelter", "Other.fungi..corticoid.fungi...others.", "Fungi.cluster.",
                            "group_ti3vx98.Multiple_smaller_cracks_in_the_deadwood", "DecomposedCrack", "group_ti3vx98.Ground_cavity_10_cm_opening","Decomposed.woodpecker.feeding.cavity."  )], na.rm = TRUE)
new_TreMs2 <- sum(new_TreMs)

colnames(TreMs)
# Step 3: Calculate the percentage contribution of the specific group
percentage_contribution <- (new_TreMs2 / total_TreMs) * 100

# Display the result
percentage_contribution
