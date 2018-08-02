##time to make some spatial dataframes
##files to work with
#written by Lian Guo

#LA
###LAmripA #2005-2009 mrip sites with landings from data_outputs
###LAmripB #2010-2013 mrip sites with landings from data_outputs
###LAmerc2010sp #2010 mercury data with lat long (no data 2011-2015)
###LAwq2010sp #2010 wq data with lat long (to match mercury data)


#FL
###FLmripA #2005-2010 mrip sites with landings, no 2008, from data_outputs
###FLmripB #2011-2015 mrip sites with landings from data_outputs

#load spatial packages
library(sp)
library(sf)
library(spdep)
library(rgdal)  # for vector work; sp package should always load with rgdal.
library (raster)   # for metadata/attributes- vectors or rasters
library(maptools)
library(ggplot2)
library(grid)
library(maps)

#read in map shapefiles for state boundaries (US_State_Bound) and metropolitan statistical areas (Stat_Area)
US_State_Bound <- readOGR(file.path("~/FUIteam/PydioData/env/raw/cb_2017_us_state_500k"), "cb_2017_us_state_500k") ##readOGR(directory,shapefilename)
plot(US_State_Bound)
crs(US_State_Bound) # +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 

Stat_Area <- readOGR(file.path("~/FUIteam/PydioData/env/raw/cb_2017_us_cbsa_20m"), "cb_2017_us_cbsa_20m") ##readOGR(directory,shapefilename)
plot(Stat_Area)
crs(Stat_Area) # +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 

#subset Stat_Area for New Orleans MSA  (NOLA)
NOLA <- subset(Stat_Area, Stat_Area$GEOID == 35380) #New orleans-metairie area are 35380, based on google search
plot(NOLA,
     col = "yellow")
crs(NOLA)

#readin dataframes for two case study sites for NOLA, sites 222 and 306
LAsite222 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAsite222.csv"), 
                    stringsAsFactors = FALSE)
LAsite306 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAsite306.csv"), 
                      stringsAsFactors = FALSE)
LAsite222
LAsite306

###spatializing process is to go from regular data frame (df) to a spatial data frame (spdf) 
###based on lat/long coordinates, and check crs, assign crs and transform as needed

#convert data.frame to spdf
sp_LAmerc2010 <- SpatialPointsDataFrame(LAmerc2010sp[,14:15], LAmerc2010sp) #long/lat are in columns 14 and 15
sp_LAwq2010 <- SpatialPointsDataFrame(LAwq2010sp[,10:11], LAwq2010sp) #long/lat are in columns 10 and 11
sp_LAsite222 <- SpatialPointsDataFrame(LAsite222[,2:3], LAsite222) #long/lat are in columns 2 and 3
sp_LAsite306 <- SpatialPointsDataFrame(LAsite306[,2:3], LAsite306) #long/lat are in columns 2 and 3

#assign CRS
crs(sp_LAmerc2010) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAwq2010) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAsite222) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAsite306) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#transform from WGS84 to NAD83
re_LAmerc2010 <- spTransform(sp_LAmerc2010, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_LAwq2010 <- spTransform(sp_LAwq2010, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_LAsite222 <- spTransform(sp_LAsite222, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_LAsite306 <- spTransform(sp_LAsite306, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))

crs(re_LAmerc2010)
crs(re_LAwq2010)
crs(re_LAsite222)
crs(re_LAsite306)

#transform from WGS84 to local projection
loc_LAmerc2010 <- spTransform(sp_LAmerc2010, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAwq2010 <- spTransform(sp_LAwq2010, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAsite222 <- spTransform(sp_LAsite222, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAsite306 <- spTransform(sp_LAsite306, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_NOLA <- spTransform(NOLA, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_StateBound <- spTransform(US_State_Bound, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))


plot(NOLA,
     col = "yellow",
     main = "2010 Merc and WQ sites")
plot(US_State_Bound,
     add = TRUE)
plot(re_LAmerc2010,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(re_LAwq2010,
     pch = 5,
     cex = .2,
     col = 'blue',
     add = TRUE)
plot(re_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(re_LAsite306,
     cex = 2,
     col = "red",
     add = TRUE)

ggplot(NOLA,
       col = "yellow",
       main = "2010 Merc and WQ sites") + 
  scalebar(lon = -89.05, 
           lat = 29.0, 
           distanceLon = 200, 
           distanceLat = 100, 
           distanceLegend = 100, 
           dist.unit = "km", 
           orientation = FALSE)
