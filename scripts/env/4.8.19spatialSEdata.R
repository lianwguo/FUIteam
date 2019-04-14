## working to synthesize across environmental and SE data now. need to spatialize and do a lot of steps again,
##hopefully more efficiently this time but we will see

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

LAsites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "seLATop.csv"), 
                      stringsAsFactors = FALSE)
FLsites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "seFLTop.csv"), 
                      stringsAsFactors = FALSE)
str(LAsites)
str(FLsites)

#read in map shapefiles for state boundaries (US_State_Bound) and metropolitan statistical areas (Stat_Area)
US_State_Bound <- readOGR(file.path("~/FUIteam/PydioData/env/raw/cb_2017_us_state_500k"), "cb_2017_us_state_500k") ##readOGR(directory,shapefilename)
plot(US_State_Bound)
crs(US_State_Bound) # +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
LA <- subset(US_State_Bound, US_State_Bound$STUSPS == 'LA')
crs(LA)
plot(LA)

Stat_Area <- readOGR(file.path("~/FUIteam/PydioData/env/raw/cb_2017_us_cbsa_20m"), "cb_2017_us_cbsa_20m") ##readOGR(directory,shapefilename)
plot(Stat_Area)
crs(Stat_Area) # +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 

#subset Stat_Area for New Orleans MSA  (NOLA)
NOLA <- subset(Stat_Area, Stat_Area$GEOID == 35380) #New orleans-metairie area are 35380, based on google search
plot(NOLA,
     col = "yellow")
crs(NOLA)

loc_LA <- spTransform(LA, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_NOLA <- spTransform(NOLA, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))


#all lat/long for mercury sites, and all LA mercury data
LAmercSites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "NOLAmercSites.csv"), 
                        stringsAsFactors = FALSE)
LAmerc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_AllMerc.csv"), 
                        stringsAsFactors = FALSE)
#left join
LAmercLL <- merge(x = LAmerc, y = LAmercSites, by.x = "SiteNum", 
                       by.y = "SITE_Num", all.x = TRUE)
str(LAmercLL)

###spatializing process is to go from regular data frame (df) to a spatial data frame (spdf) 
###based on lat/long coordinates, and check crs, assign crs and transform as needed

#convert data.frame to spdf
sp_LAmerc <- SpatialPointsDataFrame(LAmercLL[,20:21], LAmercLL)
sp_LAmrip <- SpatialPointsDataFrame(LAsites[,3:4], LAsites)

#project based on original stuff
crs(sp_LAmerc) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAmrip) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#transform to NAD83
re_LAmerc <- spTransform(sp_LAmerc, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_LAmrip <- spTransform(sp_LAmrip, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))

#transform to local projection
loc_LAmerc <- spTransform(sp_LAmerc, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAmrip <- spTransform(sp_LAmrip, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))

plot(loc_NOLA,
     col = "yellow",
     main = "TOP mrip and mercury sites")
plot(loc_LA,
     add = TRUE)
plot(loc_LAmerc,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAmrip,
     pch = 5,
     cex = 1,
     col = 'blue',
     add = TRUE)

#going to try to calculate distances

distLA <- spDists(loc_LAmerc, loc_LAmrip, longlat = FALSE) #in m
distLA

which(distLA < 8046.72) #within 5 miles #returns 37, 
which(distLA < 16093.4) #within 10 miles #returns 105
which(distLA < 40233.6) #within 25 miles #returns 3 sites, 44 observations
which(distLA < 80467.2) #within 50 miles #returns 36 sites, 493 observations
which(distLA < 160934.4) #within 100 miles #returns 100 sites, observations

Index50miLA <- which(distLA < 80467.2) #within 50 miles #returns 36 sites, 493 observations
Index100miLA <- which(distLA < 160934.4) #within 100 miles #returns 100 sites, observations

#subset data using index
sub50miLA <- loc_LAmerc[Index50miLA,]
str(sub50miLA)
unique(sub50miLA$Water.Body.Site)
write.csv(subset25miHg222, "~/FUIteam/PydioData/env/data_outputs/subset25miHg222.csv")
unique(subset25miHg222$CollectYear)

for (val in loc_LAmrip) {
  if(val %% 2 == 0)  count = count+1
}
print(count)
