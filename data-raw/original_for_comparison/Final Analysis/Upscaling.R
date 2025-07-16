#VALIDATION

# Set seed for reproducibility
set.seed(123)

# Get unique plot IDs
unique_plots <- unique(TreMs$Plot)

# Randomly select 18 plot IDs for training set
train_plots <- sample(unique_plots, 18)

# Use set difference to assign the rest to test set
test_plots <- setdiff(unique_plots, train_plots)

# Create training dataset with the selected plots
train_data <- TreMs %>% filter(Plot %in% train_plots)

# Create testing dataset with the remaining plots
test_data <- TreMs %>% filter(Plot %in% test_plots)

# Check if the split worked as expected
table(train_data$Plot)
table(test_data$Plot)

#Model
summary(Model_Abundance)

# Predict on the test data
test_data$Predicted_Abundance <- predict(Model_Abundance, newdata = test_data, type = "response")

plot_abundance_mean <- aggregate(Predicted_Abundance ~ Plot, data = test_data, mean, na.rm = TRUE)
print(plot_abundance_mean)

# View the predicted values along with the actual values
head(test_data[, c("Plot", "Abundance", "Predicted_Abundance")])

mse <- mean((test_data$Abundance - test_data$Predicted_Abundance)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE):", mse, "\n")


#NEW PREDICTIONS

NewPlots <- read.csv ("NewPlots.csv", sep = ",")
NewPlots$GroupedTreeSpecies <- as.factor(NewPlots$GroupedTreeSpecies)
levels(NewPlots$GroupedTreeSpecies)

NewPlots$GroupedTreeSpecies <- factor(NewPlots$GroupedTreeSpecies,
                                        levels = c(2,1),
                                        labels = c("Coniferous spp.", "Deciduous spp."))

NewPlots$Treedata.Tree_Decay <- as.factor(NewPlots$Treedata.Tree_Decay)
NewPlots$Treedata.Tree_Decay <- factor(NewPlots$Treedata.Tree_Decay,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Decay stage 1", "Decay stage 2","Decay stage 3","Decay stage 4","Decay stage 5"))

NewPlots <- NewPlots[NewPlots$Treedata.DBH_cm >= 20, ]

levels(TreMs$GroupedTreeSpecies)  # Adjust for other factors as needed
levels(TreMs$Treedata.Tree_Decay) # Adjust for other factors as needed


NewPlots$Predicted_Abundance <- predict(Model_Abundance2, newdata = NewPlots, type = "response")

NewPlots$Plot <- as.factor(NewPlots$Plot)
levels(NewPlots$Plot)

#NEED TO USE THE SUM!!!
new_plots_abundance_sum <- aggregate(Predicted_Abundance ~ Plot, data = NewPlots, sum, na.rm = TRUE)
write.table(new_plots_abundance_sum, file = "NewPlotsAbundanceSum.csv", sep = ",", quote = FALSE, row.names = F)

PlotTreMSum <- read.csv ("PlotDataAll copy.csv", sep = ",")
PlotTreMSum$foresttype <- as.factor(PlotTreMSum$foresttype)
PlotTreMSum <- PlotTreMSum[!is.na(PlotTreMSum$foresttype), ]
View(PlotTreMSum)
wilcox.test(Predicted_Abundance ~ foresttype, data=PlotTreMSum)
boxplot(Predicted_Abundance ~ foresttype, data=PlotTreMSum)
t.test(Predicted_Abundance ~ foresttype, data=PlotTreMSum)

par(mar = c(5, 6, 4, 2))  # Increase left margin (second number) for y-axis label space

# Create the boxplot
boxplot(Predicted_Abundance ~ foresttype, 
        data = PlotTreMSum,
        col = c("#023047", "#ffb703"),  # Change colors
        xlab = "Forest Type",                       # X-axis label
        ylab = "Predicted Abundance",               # Y-axis label
        cex.axis = 1.5,     # Increase axis tick label size
        cex.lab = 1.8       # Increase axis label size
)


install.packages("ggmap")
library(ggmap)

#MAP ATTEMPT UGHHHHHH

# Load required libraries
install.packages("sf")
install.packages("readr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggspatial")
install.packages("prettymapr")
library(sf)
library(ggplot2)
library(readr) # For reading CSV files
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(prettymapr)

# Load the shapefile (fixed path)
shapefile_path <- "study_area.shp"
study_area <- st_read(shapefile_path)

# Load the CSV file with points (fixed path)
csv_file_path <- "MapData.csv"
points_data <- read_csv(csv_file_path)

# Convert the points data to an sf object (assuming X and Y are longitude and latitude)
points_sf <- st_as_sf(points_data, coords = c("X", "Y"), crs = 4326)

# Load the country borders (medium scale, only for Germany)
germany <- ne_countries(scale = "medium", country = "Germany", 
                        returnclass = "sf")

# Ensure the CRS of both study_area and germany are the same
if (st_crs(study_area) != st_crs(germany)) {
  germany <- st_transform(germany, crs = st_crs(study_area))
}

# Get the bounding box of the study_area
bbox <- st_bbox(study_area)

# Define zoom-out factor by increasing the limits of the plot
zoom_factor <- 0.1  # Increase this value for more zoom-out
xrange <- (bbox["xmax"] - bbox["xmin"]) * zoom_factor
yrange <- (bbox["ymax"] - bbox["ymin"]) * zoom_factor

background_map <- get_stadiamap(bbox = c(left = bbox["xmin"] - xrange,
                                         bottom = bbox["ymin"] - yrange,
                                         right = bbox["xmax"] + xrange,
                                         top = bbox["ymax"] + yrange),
                                zoom = 0, maptype = "stamen_terrain_background")


print(bbox)


# Use coord_sf to adjust the x and y limits for zooming out

ggplot() +
  geom_sf(data = germany, fill = "gray90", color = "black") +  # Country borders as background
geom_sf(data = study_area, fill = "lightblue", color = "darkblue") +  
  # Plot the study area
  geom_sf(data = points_sf, aes(color = Z), size = 2) +  # Plot the points, color by Z
coord_sf(xlim = c(bbox["xmin"] - xrange, bbox["xmax"] + xrange),
         ylim = c(bbox["ymin"] - yrange, bbox["ymax"] + yrange)) +  # Set the zoom level
theme_minimal() +
  labs(title = "Study Area with Points and Country Borders (Zoomed Out)",
       caption = "Source: Shapefile, CSV Data, and Natural Earth",
       color = "Predicted TreM abundance") +
scale_color_gradient(low = "red", high = "yellow")  # Change the colors as desired

ggplot() +
  geom_sf(fill = "gray90", color = "black") +  # Country borders as background
  geom_sf(fill = NA, color = NA) +  
  # Plot the study area
  geom_sf(data = points_sf, aes(color = Z), size = 2) +  # Plot the points, color by Z
  theme_minimal() +
  labs(title = "Study Area with Points and Country Borders (Zoomed Out)",
       caption = "Source: Shapefile, CSV Data, and Natural Earth")


