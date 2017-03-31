## spatial linkage of temperature to SSDs.


# set library and work directory:
.libPaths("D://Stage//Rstudio//Libraries")
setwd("D://Stage//Rstudio//Data//Final_script_linkage")
# load necessary packages:
library(gstat)
library(RgoogleMaps)
library(GISTools)
library(raster)
library(rgdal)
library(sp)
library(shapefiles)
library(mapplots)
library(caTools)
library(fields)


## Overview of the script:
## L28-L102:  PART 1 - SPATIAL POINT MAPS OF TEMPERATURE.
## L105-L193: PART 2 - SPATIAL GRID EXTENTS.
## L196-L282: PART 3 - VARIOGRAPHY AND KRIGING INTERPOLATION.
## L285-L332: PART 4 - COLLECTING INTERPOLATION ACCURACY DATA.
## L335-L431: PART 5 - TRANSLATION OF TEMPERATURE TO PNOF VALUES.


#### PART 1 - SPATIAL POINT MAPS OF TEMPERATURE.


## The first step is to create spatial point maps for temperature values. This requires data from field measurements of temperature, consisting of: logger name, temperature value, x-coordinate (or Longitude), y-coordinate (or Latitude).
## First, we retrieve the data.
# Meuse, first measurement:
Meuse1_10cm  <- read.csv("Meuse1_10cm.csv")
Meuse1_50cm  <- read.csv("Meuse1_50cm.csv")
Meuse1_100cm <- read.csv("Meuse1_100cm.csv")
# Meuse, second measurement:
Meuse2_10cm  <- read.csv("Meuse2_10cm.csv")
Meuse2_50cm  <- read.csv("Meuse2_50cm.csv")
Meuse2_100cm <- read.csv("Meuse2_100cm.csv")
## Note that the 100cm maps have no values for J_100cm, because during the meuse measurements, logger J_100 did not provide accurate measuring results.
# Waal measurements groyne field & shore channel:
Waal_groyne  <- read.csv("Waal_groyne.csv")
Waal_LTD     <- read.csv("Waal_LTD.csv")


## These 8 datasets contain the data for spatial points with temperature values. We can now turn these into spatial points, and afterwards interpolate the values. Before we create the spatial data, we must split each dataset into two groups: one group of 80% and one group of 20%, using random selection of points. The large 80% group is called "Train" and the small 20% group is called "Test". These two groups are necessary to assess the interpolation accuracy, which is explained later on.
# Split randomly into 80% and 20%:
Meuse1_10cm$Train  <- sample.split(Meuse1_10cm$Tmax,  SplitRatio = 32) # 80% = 32 points
Meuse1_50cm$Train  <- sample.split(Meuse1_50cm$Tmax,  SplitRatio = 32) # 80% = 32 points
Meuse1_100cm$Train <- sample.split(Meuse1_100cm$Tmax, SplitRatio = 28) # 80% = 28 points
Meuse2_10cm$Train  <- sample.split(Meuse2_10cm$Tmax,  SplitRatio = 32) # 80% = 32 points
Meuse2_50cm$Train  <- sample.split(Meuse2_50cm$Tmax,  SplitRatio = 32) # 80% = 32 points
Meuse2_100cm$Train <- sample.split(Meuse2_100cm$Tmax, SplitRatio = 28) # 80% = 28 points
Waal_groyne$Train  <- sample.split(Waal_groyne$Tmax,  SplitRatio = 26) # 80% = 26 points
Waal_LTD$Train     <- sample.split(Waal_LTD$Tmax,     SplitRatio = 26) # 80% = 26 points
# Subset Train and Test groups:
Meuse1_10cm_train  <- subset(Meuse1_10cm,  Train=="TRUE",  select = c(1:4))
Meuse1_10cm_test   <- subset(Meuse1_10cm,  Train=="FALSE", select = c(1:4))
Meuse1_50cm_train  <- subset(Meuse1_50cm,  Train=="TRUE",  select = c(1:4))
Meuse1_50cm_test   <- subset(Meuse1_50cm,  Train=="FALSE", select = c(1:4))
Meuse1_100cm_train <- subset(Meuse1_100cm, Train=="TRUE",  select = c(1:4))
Meuse1_100cm_test  <- subset(Meuse1_100cm, Train=="FALSE", select = c(1:4))
Meuse2_10cm_train  <- subset(Meuse2_10cm,  Train=="TRUE",  select = c(1:4))
Meuse2_10cm_test   <- subset(Meuse2_10cm,  Train=="FALSE", select = c(1:4))
Meuse2_50cm_train  <- subset(Meuse2_50cm,  Train=="TRUE",  select = c(1:4))
Meuse2_50cm_test   <- subset(Meuse2_50cm,  Train=="FALSE", select = c(1:4))
Meuse2_100cm_train <- subset(Meuse2_100cm, Train=="TRUE",  select = c(1:4))
Meuse2_100cm_test  <- subset(Meuse2_100cm, Train=="FALSE", select = c(1:4))
Waal_groyne_train  <- subset(Waal_groyne,  Train=="TRUE",  select = c(1:4))
Waal_groyne_test   <- subset(Waal_groyne,  Train=="FALSE", select = c(1:4))
Waal_LTD_train     <- subset(Waal_LTD,     Train=="TRUE",  select = c(1:4))
Waal_LTD_test      <- subset(Waal_LTD,     Train=="FALSE", select = c(1:4))
# Convert everything to spatial data with Long and Lat as x- and y-coordinates:
coordinates(Meuse1_10cm)        = ~Long + Lat
coordinates(Meuse1_10cm_train)  = ~Long + Lat
coordinates(Meuse1_10cm_test)   = ~Long + Lat
coordinates(Meuse1_50cm)        = ~Long + Lat
coordinates(Meuse1_50cm_train)  = ~Long + Lat
coordinates(Meuse1_50cm_test)   = ~Long + Lat
coordinates(Meuse1_100cm)       = ~Long + Lat
coordinates(Meuse1_100cm_train) = ~Long + Lat
coordinates(Meuse1_100cm_test)  = ~Long + Lat
coordinates(Meuse2_10cm)        = ~Long + Lat
coordinates(Meuse2_10cm_train)  = ~Long + Lat
coordinates(Meuse2_10cm_test)   = ~Long + Lat
coordinates(Meuse2_50cm)        = ~Long + Lat
coordinates(Meuse2_50cm_train)  = ~Long + Lat
coordinates(Meuse2_50cm_test)   = ~Long + Lat
coordinates(Meuse2_100cm)       = ~Long + Lat
coordinates(Meuse2_100cm_train) = ~Long + Lat
coordinates(Meuse2_100cm_test)  = ~Long + Lat
coordinates(Waal_groyne)        = ~Long + Lat
coordinates(Waal_groyne_train)  = ~Long + Lat
coordinates(Waal_groyne_test)   = ~Long + Lat
coordinates(Waal_LTD)           = ~Long + Lat
coordinates(Waal_LTD_train)     = ~Long + Lat
coordinates(Waal_LTD_test)      = ~Long + Lat
## Now we have all spatial point maps, and we can now interpolate the temperature values of each map.


#### END OF PART 1 - SPATIAL POINT MAPS OF TEMPERATURE.


#### PART 2 - SPATIAL GRID EXTENTS.


## For interpolation, we need to create underlying grids that serve as extent for the interpolation. We have three different spatial maps: one for the four Meuse 10cm/50cm maps, one for the two Meuse 100cm maps, and one for the two Waal maps. Thus, we need to create three extent grids.
## Lets create the first grid extent, which is the extent of the Meuse 10cm/50cm maps. These maps have the same coordinates, so we can use the coordinates of 1 map and create the grid.
# Retrieve coordinates:
summary(Meuse1_10cm)
min(Meuse1_10cm$Long)
max(Meuse1_10cm$Long)
min(Meuse1_10cm$Lat)
max(Meuse1_10cm$Lat)
## longitude coords (x-values) range from 5.011 to 5.027
## latitude coords (y-values) range from 50.001 to 50.071
## Now we use these values as boundaries to create a new grid, dividing latitude and longitude into pieces with 0.0001 spacing between.
# Longitude: take all x-values and repeat each x-value with the number of y-values:
Meuse_10cm_extent_x = rep(seq(5.011,5.027,0.0001),each=701)
length(Meuse_10cm_extent_x)
# Latitude: take all y-values and repeat them as a whole with the number of x-values:
Meuse_10cm_extent_y = rep(seq(50.001,50.071,0.0001),161)
length(Meuse_10cm_extent_y)
## 161 horizontal (longitudinal) grid cells, repeated 701 times vertically.
## 701 vertical (lateral) grid cells, repeated 161 times horizontally.
## 161*701 = 112861 grid cells in total. 
# Use these long&lat values as coordinates to create a new dataframe:
Meuse_10cm_extent <- data.frame(xc = Meuse_10cm_extent_x, yc = Meuse_10cm_extent_y, z = c(1:112861)) # z-value = total number of grid cells.
coordinates(Meuse_10cm_extent) = ~xc+yc
gridded(Meuse_10cm_extent) = TRUE
Meuse_10cm_extent <- as(Meuse_10cm_extent, "SpatialGridDataFrame")
summary(Meuse_10cm_extent)
image(Meuse_10cm_extent["z"])
## Now we have a grid of 161*701 = 112861 cells across the spatial points, ready to use for interpolation of the temperature values in the Meuse 10cm/50cm maps. Increasing the number of cells will result in more gradual interpolation values, but that will take longer to execute and will result in a larger dataset.


## Now we create the second extent, which is for the Meuse 100cm maps, in the same way.
# Retrieve coordinates:
summary(Meuse1_100cm)
min(Meuse1_100cm$Long)
max(Meuse1_100cm$Long)
min(Meuse1_100cm$Lat)
max(Meuse1_100cm$Lat)
# Longitude: take all x-values and repeat each x-value with the number of y-values:
Meuse_100cm_extent_x = rep(seq(5.011,5.027,0.0001),each=601)
length(Meuse_100cm_extent_x)
# Latitude: take all y-values and repeat them as a whole with the number of x-values:
Meuse_100cm_extent_y = rep(seq(50.001,50.061,0.0001),161)
length(Meuse_100cm_extent_y)
## 161 horizontal (longitudinal) grid cells, repeated 601 times vertically.
## 601 vertical (lateral) grid cells, repeated 161 times horizontally.
## 161*601 = 96761 grid cells in total.
# Use these long&lat values as coordinates to create a new dataframe:
Meuse_100cm_extent <- data.frame(xc = Meuse_100cm_extent_x, yc = Meuse_100cm_extent_y, z = c(1:96761)) # z-value = total number of grid cells.
coordinates(Meuse_100cm_extent) = ~xc+yc
gridded(Meuse_100cm_extent) = TRUE
Meuse_100cm_extent <- as(Meuse_100cm_extent, "SpatialGridDataFrame")
summary(Meuse_100cm_extent)
image(Meuse_100cm_extent["z"])
## Now we have a grid of 161*601 = 96761 cells across the spatial points, ready to use for interpolation of the temperature values in the Meuse 100cm maps.


## Now we create the third extent, which is for the Waal maps, in the same way.
summary(Waal_groyne)
min(Waal_groyne$Long)
max(Waal_groyne$Long)
min(Waal_groyne$Lat)
max(Waal_groyne$Lat)
# Longitude: take all x-values and repeat each x-value with the number of y-values:
Waal_extent_x = rep(seq(5.0011,5.0111,0.00001),each=21)
length(Waal_extent_x)
## latitude: take all y-values and repeat them as a whole with the number of x-values:
Waal_extent_y = rep(seq(50.0001,50.0003,0.00001),1001)
length(Waal_extent_y)
## 1001 horizontal (longitudinal) grid cells, repeated 21 vertically.
## 21 vertical (lateral) grid cells, repeated 1001 times horizontally.
## 1001*21 = 21021 grid cells in total.
## Note that the decimals in this grid extent differs from the Meuse grid extents. This is necessary to generate the right amount of grid cells. However, we use custom coordinates for our spatial maps, so only the number of grid cells and the spatial configuration of the cells are important.
# Use these long&lat values as coordinates to create a new dataframe:
Waal_extent <- data.frame(xc = Waal_extent_x, yc = Waal_extent_y, z = c(1:21021)) # z-value = total number of grid cells.
coordinates(Waal_extent) = ~xc+yc
gridded(Waal_extent) = TRUE
Waal_extent <- as(Waal_extent, "SpatialGridDataFrame")
summary(Waal_extent)
image(Waal_extent["z"])
## Now we have a grid of 1001*21 = 21021 cells across the spatial points, ready to use for interpolation of the temperature values in the Waal maps.


## Now we have 8 spatial point maps and 3 extent grids, which can now be used for interpolation.


#### END OF PART 2 - SPATIAL GRID EXTENTS.


#### PART 3 - VARIOGRAPHY AND KRIGING INTERPOLATION.


## Interpolation will be performed with Kriging, which requires geostatistics/variography. 
## Part 3.1: variography. (Semi)Variogram models contain a nugget, range and sill. At a certain distance, the model flattens (dy=0). The distance (value on the x-axis) where the model first flattens is called the range, and the semivariance at that distance (value on the y-axis) is called the sill. Pairs of locations with separating distances smaller than the range have spatial autocorrelation (increasing model slope, dy/dx>0). The nugget is the semivariance at zero distance, so the intercept with the y-axis.
## First, lets create a variogram model for temperature in one spatial map (Meuse1_10cm). Interpolation will be performed using the Train (80%) group of each map, so we use the Train groups for geostatistics and interpolation.
# Variogram:
Meuse1_10cm_vario  <- variogram(Tmax ~ 1, Meuse1_10cm_train)
plot(Meuse1_10cm_vario)
## The nugget is around 0.001,
## The range is around 0.023, and 
## The sill is around 0.010.
## Now we fit the variogram to a model. Initial values for the fitted model are needed, because fitting the range parameter involves non-linear regression:
Meuse1_10cm_v.fit <- fit.variogram(Meuse1_10cm_vario, vgm(1, "Sph", 0.023, 1))
Meuse1_10cm_v.fit
plot(Meuse1_10cm_vario, model=Meuse1_10cm_v.fit)
## The model is suboptimal to the variogram. Optimization of the parameters requires additional knowledge on geostatistics.
## Now we repeat these variography steps for the other 7 maps.
# Variography Meuse1_50cm:
Meuse1_50cm_vario   <- variogram(Tmax ~ 1, Meuse1_50cm_train)
plot(Meuse1_50cm_vario)
Meuse1_50cm_v.fit <- fit.variogram(Meuse1_50cm_vario, vgm(1, "Sph", 0.024, 1))
Meuse1_50cm_v.fit
plot(Meuse1_50cm_vario, model=Meuse1_50cm_v.fit)
# Variography Meuse1_100cm:
Meuse1_100cm_vario   <- variogram(Tmax ~ 1, Meuse1_100cm_train)
plot(Meuse1_100cm_vario)
Meuse1_100cm_v.fit <- fit.variogram(Meuse1_100cm_vario, vgm(1, "Sph", 0.02, 1))
Meuse1_100cm_v.fit
plot(Meuse1_100cm_vario, model=Meuse1_100cm_v.fit)
# Variography Meuse2_10cm:
Meuse2_10cm_vario   <- variogram(Tmax ~ 1, Meuse2_10cm_train)
plot(Meuse2_10cm_vario)
Meuse2_10cm_v.fit <- fit.variogram(Meuse2_10cm_vario, vgm(1, "Sph", 0.021, 1))
Meuse2_10cm_v.fit
plot(Meuse2_10cm_vario, model=Meuse2_10cm_v.fit)
# Variography Meuse2_50cm:
Meuse2_50cm_vario   <- variogram(Tmax ~ 1, Meuse2_50cm_train)
plot(Meuse2_50cm_vario)
Meuse2_50cm_v.fit <- fit.variogram(Meuse2_50cm_vario, vgm(1, "Sph", 0.024, 1))
Meuse2_50cm_v.fit
plot(Meuse2_50cm_vario, model=Meuse2_50cm_v.fit)
# Variography Meuse2_100cm:
Meuse2_100cm_vario   <- variogram(Tmax ~ 1, Meuse2_100cm_train)
plot(Meuse2_100cm_vario)
Meuse2_100cm_v.fit <- fit.variogram(Meuse2_100cm_vario, vgm(1, "Sph", 0.02, 1))
Meuse2_100cm_v.fit
plot(Meuse2_100cm_vario, model=Meuse2_100cm_v.fit)
# Variography Waal_groyne:
Waal_groyne_vario   <- variogram(Tmax ~ 1, Waal_groyne_train)
plot(Waal_groyne_vario)
Waal_groyne_v.fit <- fit.variogram(Waal_groyne_vario, vgm(1, "Sph", 0.02, 1))
Waal_groyne_v.fit
plot(Waal_groyne_vario, model=Waal_groyne_v.fit)
# Variography Waal_groyne:
Waal_groyne_vario   <- variogram(Tmax ~ 1, Waal_groyne_train)
plot(Waal_groyne_vario)
Waal_groyne_v.fit <- fit.variogram(Waal_groyne_vario, vgm(1, "Sph", 0.02, 1))
Waal_groyne_v.fit
plot(Waal_groyne_vario, model=Waal_groyne_v.fit)
# Variography Waal_LTD:
Waal_LTD_vario   <- variogram(Tmax ~ 1, Waal_LTD_train)
plot(Waal_LTD_vario)
Waal_LTD_v.fit <- fit.variogram(Waal_LTD_vario, vgm(1, "Sph", 0.02, 1))
Waal_LTD_v.fit
plot(Waal_LTD_vario, model=Waal_LTD_v.fit)
## These model are suboptimal, and optimization of the parameters requires additional knowledge. Nevertheless, these models can now be used for kriging interpolation.


## Part 3.2: kriging interpolation. Kriging requires: temperature spatial point data, grid extent spatial data, and the fitted variogram models.
# Kriging interpolation:
Meuse1_10cm_krig  <- krige(Tmax ~ 1, Meuse1_10cm_train,  Meuse_10cm_extent,  Meuse1_10cm_v.fit) # ordinary kriging
Meuse1_50cm_krig  <- krige(Tmax ~ 1, Meuse1_50cm_train,  Meuse_10cm_extent,  Meuse1_50cm_v.fit)
Meuse1_100cm_krig <- krige(Tmax ~ 1, Meuse1_100cm_train, Meuse_100cm_extent, Meuse1_100cm_v.fit)
Meuse2_10cm_krig  <- krige(Tmax ~ 1, Meuse2_10cm_train,  Meuse_10cm_extent,  Meuse2_10cm_v.fit)
Meuse2_50cm_krig  <- krige(Tmax ~ 1, Meuse2_50cm_train,  Meuse_10cm_extent,  Meuse2_50cm_v.fit)
Meuse2_100cm_krig <- krige(Tmax ~ 1, Meuse2_100cm_train, Meuse_100cm_extent, Meuse2_100cm_v.fit)
Waal_groyne_krig  <- krige(Tmax ~ 1, Waal_groyne_train,  Waal_extent,        Waal_groyne_v.fit)
Waal_LTD_krig     <- krige(Tmax ~ 1, Waal_LTD_train,     Waal_extent,        Waal_LTD_v.fit)
as.data.frame(Meuse1_10cm_krig)[1:5,]
## The interpolation values are stored as var1.pred. The interpolation variances are stored as var1.var. These variances are derived from the statistical model and give an indication of the reliability of the estimates.
# Lets see one of the spatial maps:
spplot(Meuse1_10cm_krig, "var1.pred", main = "Meuse1_10cm_krig", colorkey=T) # colorkey=T for continuous values.
## Now we have 8 spatial maps with interpolated temperature data. These temperature maps can now be assessed for accuracy and translated to PNOF values.


#### END OF PART 3 - GEOSTATISTICS AND KRIGING INTERPOLATION.


#### PART 4 - COLLECTING INTERPOLATION ACCURACY DATA.


## For each map, the kriging accuracy is assessed by comparing the Test groups (spatial points) with the kriging values at the same location. Thus, the test values and corresponding interpolation values will be collected and exported.
# Transform kriging spatialGRIDdataframes to kriging rasters:
summary(Meuse1_10cm_krig)
Meuse1_10cm_krig_ras  <- raster(Meuse1_10cm_krig,  layer=1) # layer=1 -> use values of column 1 (interpolation values).
Meuse1_50cm_krig_ras  <- raster(Meuse1_50cm_krig,  layer=1)
Meuse1_100cm_krig_ras <- raster(Meuse1_100cm_krig, layer=1)
Meuse2_10cm_krig_ras  <- raster(Meuse2_10cm_krig,  layer=1)
Meuse2_50cm_krig_ras  <- raster(Meuse2_50cm_krig,  layer=1)
Meuse2_100cm_krig_ras <- raster(Meuse2_100cm_krig, layer=1)
Waal_groyne_krig_ras  <- raster(Waal_groyne_krig,  layer=1)
Waal_LTD_krig_ras     <- raster(Waal_LTD_krig,     layer=1)
# Create new dataframes that include Test coordinates, original Test values and Extract-points-from-raster values:
Meuse1_10cm_test_krig  <- data.frame(coordinates(Meuse1_10cm_test),  Meuse1_10cm_test$Tmax,  extract(Meuse1_10cm_krig_ras, Meuse1_10cm_test))
Meuse1_50cm_test_krig  <- data.frame(coordinates(Meuse1_50cm_test),  Meuse1_50cm_test$Tmax,  extract(Meuse1_50cm_krig_ras, Meuse1_50cm_test))
Meuse1_100cm_test_krig <- data.frame(coordinates(Meuse1_100cm_test), Meuse1_100cm_test$Tmax, extract(Meuse1_100cm_krig_ras, Meuse1_100cm_test))
Meuse2_10cm_test_krig  <- data.frame(coordinates(Meuse2_10cm_test),  Meuse2_10cm_test$Tmax,  extract(Meuse2_10cm_krig_ras, Meuse2_10cm_test))
Meuse2_50cm_test_krig  <- data.frame(coordinates(Meuse2_50cm_test),  Meuse2_50cm_test$Tmax,  extract(Meuse2_50cm_krig_ras, Meuse2_50cm_test))
Meuse2_100cm_test_krig <- data.frame(coordinates(Meuse2_100cm_test), Meuse2_100cm_test$Tmax, extract(Meuse2_100cm_krig_ras, Meuse2_100cm_test))
Waal_groyne_test_krig  <- data.frame(coordinates(Waal_groyne_test),  Waal_groyne_test$Tmax,  extract(Waal_groyne_krig_ras, Waal_groyne_test))
Waal_LTD_test_krig  <- data.frame(coordinates(Waal_LTD_test),  Waal_LTD_test$Tmax,  extract(Waal_LTD_krig_ras, Waal_LTD_test))
# Customize column names:
names(Meuse1_10cm_test_krig)  <- c("xc", "yc", "Test", "Krig")
names(Meuse1_50cm_test_krig)  <- c("xc", "yc", "Test", "Krig")
names(Meuse1_100cm_test_krig) <- c("xc", "yc", "Test", "Krig")
names(Meuse2_10cm_test_krig)  <- c("xc", "yc", "Test", "Krig")
names(Meuse2_50cm_test_krig)  <- c("xc", "yc", "Test", "Krig")
names(Meuse2_100cm_test_krig) <- c("xc", "yc", "Test", "Krig")
names(Waal_groyne_test_krig)  <- c("xc", "yc", "Test", "Krig")
names(Waal_LTD_test_krig)     <- c("xc", "yc", "Test", "Krig")
summary(Meuse1_10cm_test_krig)
nrow(Meuse1_10cm_test_krig)
## xc = Longitude, yc = Latitude, Test = temperature value from not interpolated Test group, Krig = temperature value from interpolated map for the corresponding Test point value. These values can now be exported.
# Export to CSV:
write.csv(Meuse1_10cm_test_krig,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse1_10cm_test_krig.csv")
write.csv(Meuse1_50cm_test_krig,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse1_50cm_test_krig.csv")
write.csv(Meuse1_100cm_test_krig, file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse1_100cm_test_krig.csv")
write.csv(Meuse2_10cm_test_krig,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse2_10cm_test_krig.csv")
write.csv(Meuse2_50cm_test_krig,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse2_50cm_test_krig.csv")
write.csv(Meuse2_100cm_test_krig, file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse2_100cm_test_krig.csv")
write.csv(Waal_groyne_test_krig,  file="D://Stage//Rstudio//Data//Final_script_linkage//Waal_groyne_test_krig.csv")
write.csv(Waal_LTD_test_krig,     file="D://Stage//Rstudio//Data//Final_script_linkage//Waal_LTD_test_krig.csv")
## Now we have exported tables with the temperature test data and the corresponding kriging values.


#### END OF PART 4 - COLLECTING INTERPOLATION ACCURACY DATA.


#### PART 5 - TRANSLATION OF TEMPERATURE TO PNOF VALUES.


## Now we create new dataframes and translate the interpolated temperature values to PNOF values using 2 SSD formulas: one for fish in the Meuse and one for fish in the Waal.
# Extract kriging data to new dataframes:
summary(Meuse1_10cm_krig)
Meuse1_10cm_temp_pnof  <- as.data.frame(Meuse1_10cm_krig)
Meuse1_50cm_temp_pnof  <- as.data.frame(Meuse1_50cm_krig)
Meuse1_100cm_temp_pnof <- as.data.frame(Meuse1_100cm_krig)
Meuse2_10cm_temp_pnof  <- as.data.frame(Meuse2_10cm_krig)
Meuse2_50cm_temp_pnof  <- as.data.frame(Meuse2_50cm_krig)
Meuse2_100cm_temp_pnof <- as.data.frame(Meuse2_100cm_krig)
Waal_groyne_temp_pnof  <- as.data.frame(Waal_groyne_krig)
Waal_LTD_temp_pnof     <- as.data.frame(Waal_LTD_krig)
# Select columns:
summary(Meuse1_10cm_temp_pnof)
Meuse1_10cm_temp_pnof  <- Meuse1_10cm_temp_pnof[,c(3,4,1)]
Meuse1_50cm_temp_pnof  <- Meuse1_50cm_temp_pnof[,c(3,4,1)]
Meuse1_100cm_temp_pnof <- Meuse1_100cm_temp_pnof[,c(3,4,1)]
Meuse2_10cm_temp_pnof  <- Meuse2_10cm_temp_pnof[,c(3,4,1)]
Meuse2_50cm_temp_pnof  <- Meuse2_50cm_temp_pnof[,c(3,4,1)]
Meuse2_100cm_temp_pnof <- Meuse2_100cm_temp_pnof[,c(3,4,1)]
Waal_groyne_temp_pnof  <- Waal_groyne_temp_pnof[,c(3,4,1)]
Waal_LTD_temp_pnof     <- Waal_LTD_temp_pnof[,c(3,4,1)]
# Duplicate temperature columns:
Meuse1_10cm_temp_pnof$Pnof  <- Meuse1_10cm_temp_pnof$var1.pred
Meuse1_50cm_temp_pnof$Pnof  <- Meuse1_50cm_temp_pnof$var1.pred
Meuse1_100cm_temp_pnof$Pnof <- Meuse1_100cm_temp_pnof$var1.pred
Meuse2_10cm_temp_pnof$Pnof  <- Meuse2_10cm_temp_pnof$var1.pred
Meuse2_50cm_temp_pnof$Pnof  <- Meuse2_50cm_temp_pnof$var1.pred
Meuse2_100cm_temp_pnof$Pnof <- Meuse2_100cm_temp_pnof$var1.pred
Waal_groyne_temp_pnof$Pnof  <- Waal_groyne_temp_pnof$var1.pred
Waal_LTD_temp_pnof$Pnof     <- Waal_LTD_temp_pnof$var1.pred
# Change column names:
colnames(Meuse1_10cm_temp_pnof)  <- c("x", "y", "Tmax", "Pnof")
colnames(Meuse1_50cm_temp_pnof)  <- c("x", "y", "Tmax", "Pnof")
colnames(Meuse1_100cm_temp_pnof) <- c("x", "y", "Tmax", "Pnof")
colnames(Meuse2_10cm_temp_pnof)  <- c("x", "y", "Tmax", "Pnof")
colnames(Meuse2_50cm_temp_pnof)  <- c("x", "y", "Tmax", "Pnof")
colnames(Meuse2_100cm_temp_pnof) <- c("x", "y", "Tmax", "Pnof")
colnames(Waal_groyne_temp_pnof)  <- c("x", "y", "Tmax", "Pnof")
colnames(Waal_LTD_temp_pnof)     <- c("x", "y", "Tmax", "Pnof")
summary(Meuse1_10cm_temp_pnof)
## Now we are going to translate the values in the "Pnof" columns by inserting the fish SSD formula. The fish SSD is created through fitting a normal distribution which calculates the PNOF values based on the measured water temperature.
# Insert SSD with pnorm():
Meuse1_10cm_temp_pnof$Pnof  <- pnorm(Meuse1_10cm_temp_pnof$Pnof,  mean=26.6739, sd=6.3318) # Meuse fish SSD
Meuse1_50cm_temp_pnof$Pnof  <- pnorm(Meuse1_50cm_temp_pnof$Pnof,  mean=26.6739, sd=6.3318) # Meuse fish SSD
Meuse1_100cm_temp_pnof$Pnof <- pnorm(Meuse1_100cm_temp_pnof$Pnof, mean=26.6739, sd=6.3318) # Meuse fish SSD
Meuse2_10cm_temp_pnof$Pnof  <- pnorm(Meuse2_10cm_temp_pnof$Pnof,  mean=26.6739, sd=6.3318) # Meuse fish SSD
Meuse2_50cm_temp_pnof$Pnof  <- pnorm(Meuse2_50cm_temp_pnof$Pnof,  mean=26.6739, sd=6.3318) # Meuse fish SSD
Meuse2_100cm_temp_pnof$Pnof <- pnorm(Meuse2_100cm_temp_pnof$Pnof, mean=26.6739, sd=6.3318) # Meuse fish SSD
Waal_groyne_temp_pnof$Pnof  <- pnorm(Waal_groyne_temp_pnof$Pnof,  mean=26.2895, sd=6.5061) # Waal  fish SSD
Waal_LTD_temp_pnof$Pnof     <- pnorm(Waal_LTD_temp_pnof$Pnof,     mean=26.2895, sd=6.5061) # Waal  fish SSD
# Transform to SpatialPointsDataFrame:
coordinates(Meuse1_10cm_temp_pnof)  = ~x + y
coordinates(Meuse1_50cm_temp_pnof)  = ~x + y
coordinates(Meuse1_100cm_temp_pnof) = ~x + y
coordinates(Meuse2_10cm_temp_pnof)  = ~x + y
coordinates(Meuse2_50cm_temp_pnof)  = ~x + y
coordinates(Meuse2_100cm_temp_pnof) = ~x + y
coordinates(Waal_groyne_temp_pnof)  = ~x + y
coordinates(Waal_LTD_temp_pnof)     = ~x + y
# Change spatialPOINTSdataframe to spatialPIXELSdataframe:
Meuse1_10cm_temp_pnof  = as(Meuse1_10cm_temp_pnof,  "SpatialPixelsDataFrame")
Meuse1_50cm_temp_pnof  = as(Meuse1_50cm_temp_pnof,  "SpatialPixelsDataFrame")
Meuse1_100cm_temp_pnof = as(Meuse1_100cm_temp_pnof, "SpatialPixelsDataFrame")
Meuse2_10cm_temp_pnof  = as(Meuse2_10cm_temp_pnof,  "SpatialPixelsDataFrame")
Meuse2_50cm_temp_pnof  = as(Meuse2_50cm_temp_pnof,  "SpatialPixelsDataFrame")
Meuse2_100cm_temp_pnof = as(Meuse2_100cm_temp_pnof, "SpatialPixelsDataFrame")
Waal_groyne_temp_pnof  = as(Waal_groyne_temp_pnof,  "SpatialPixelsDataFrame")
Waal_LTD_temp_pnof     = as(Waal_LTD_temp_pnof,     "SpatialPixelsDataFrame")
# Change spatialPIXELSdataframe to spatialGRIDdataframe:
Meuse1_10cm_temp_pnof  = as(Meuse1_10cm_temp_pnof,  "SpatialGridDataFrame")
Meuse1_50cm_temp_pnof  = as(Meuse1_50cm_temp_pnof,  "SpatialGridDataFrame")
Meuse1_100cm_temp_pnof = as(Meuse1_100cm_temp_pnof, "SpatialGridDataFrame")
Meuse2_10cm_temp_pnof  = as(Meuse2_10cm_temp_pnof,  "SpatialGridDataFrame")
Meuse2_50cm_temp_pnof  = as(Meuse2_50cm_temp_pnof,  "SpatialGridDataFrame")
Meuse2_100cm_temp_pnof = as(Meuse2_100cm_temp_pnof, "SpatialGridDataFrame")
Waal_groyne_temp_pnof  = as(Waal_groyne_temp_pnof,  "SpatialGridDataFrame")
Waal_LTD_temp_pnof     = as(Waal_LTD_temp_pnof,     "SpatialGridDataFrame")
summary(Meuse1_10cm_temp_pnof)
# Lets see one of the spatial maps:
spplot(Meuse1_10cm_temp_pnof, "Tmax", main = "Meuse1_10cm_temp_pnof", at=(920:980)/50) # colorkey=T for continuous values.
spplot(Meuse1_10cm_temp_pnof, "Pnof", main = "Meuse1_10cm_temp_pnof", at=(192:262)/2000)
## Now we have 8 spatial maps with temperature and PNOF values.
# Export to CSV:
write.csv(Meuse1_10cm_temp_pnof,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse1_10cm_temp_pnof.csv")
write.csv(Meuse1_50cm_temp_pnof,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse1_50cm_temp_pnof.csv")
write.csv(Meuse1_100cm_temp_pnof, file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse1_100cm_temp_pnof.csv")
write.csv(Meuse2_10cm_temp_pnof,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse2_10cm_temp_pnof.csv")
write.csv(Meuse2_50cm_temp_pnof,  file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse2_50cm_temp_pnof.csv")
write.csv(Meuse2_100cm_temp_pnof, file="D://Stage//Rstudio//Data//Final_script_linkage//Meuse2_100cm_temp_pnof.csv")
write.csv(Waal_groyne_temp_pnof,  file="D://Stage//Rstudio//Data//Final_script_linkage//Waal_groyne_temp_pnof.csv")
write.csv(Waal_LTD_temp_pnof,     file="D://Stage//Rstudio//Data//Final_script_linkage//Waal_LTD_temp_pnof.csv")
## Now we have exported tables with the temperature and PNOF data for all 8 maps.


#### END OF PART 5 - TRANSLATION OF TEMPERATURE TO PNOF VALUES.


#### END OF THE SPATIAL LINKAGE.