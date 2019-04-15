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

#sites in mrip
#141  150  151  155  222  231  306 3325

#141***
distLA141 <- spDists(loc_LAmrip[1,], loc_LAmerc, longlat = FALSE) #in m
distLA141

which(distLA141 < 8046.72) #within 5 miles #returns 37, 
which(distLA141 < 16093.4) #within 10 miles #returns 105
which(distLA141 < 40233.6) #within 25 miles #returns  sites, 28 observations
which(distLA141 < 80467.2) #within 50 miles #returns  sites, 440 observations
which(distLA141 < 160934.4) #within 100 miles #returns many observations

Ind50miLA141 <- which(distLA141 < 80467.2) #within 50 miles, 32 sites
Ind100miLA141 <- which(distLA141 < 160934.4) #within 100 miles

#subset data using index
sub50miLA141 <- loc_LAmerc[Ind50miLA141,]
str(sub50miLA141)
unique(sub50miLA141$SiteNum)
unique(sub50miLA141$CollectYear) #2003, 2004, 2006-2009

sub100miLA141 <- loc_LAmerc[Ind100miLA141,]
str(sub100miLA141)
unique(sub100miLA141$SiteNum) #128 sites
unique(sub100miLA141$CollectYear) #2003-2009

#150
distLA150 <- spDists(loc_LAmrip[2,], loc_LAmerc, longlat = FALSE) #in m
distLA150

which(distLA150 < 8046.72) #within 5 miles
which(distLA150 < 16093.4) #within 10 miles 
which(distLA150 < 40233.6) #within 25 miles #returns  sites, 28 observations
which(distLA150 < 80467.2) #within 50 miles #returns  sites, 440 observations
which(distLA150 < 160934.4) #within 100 miles #returns many observations

Ind50miLA150 <- which(distLA150 < 80467.2) #within 50 miles, 32 sites
Ind100miLA150 <- which(distLA150 < 160934.4) #within 100 miles

#subset data using index
sub50miLA150 <- loc_LAmerc[Ind50miLA150,]
str(sub50miLA150)
unique(sub50miLA150$SiteNum)
unique(sub50miLA150$CollectYear) #2003, 2004, 2006-2009

sub100miLA150 <- loc_LAmerc[Ind100miLA150,]
str(sub100miLA150)
unique(sub100miLA150$SiteNum) #128 sites
unique(sub100miLA150$CollectYear) #2003-2009

#151
distLA151 <- spDists(loc_LAmrip[3,], loc_LAmerc, longlat = FALSE) #in m
distLA151

which(distLA151 < 8046.72) #within 5 miles
which(distLA151 < 16093.4) #within 10 miles 
which(distLA151 < 40233.6) #within 25 miles #returns  sites, 28 observations
which(distLA151 < 80467.2) #within 50 miles #returns  sites, 440 observations
which(distLA151 < 160934.4) #within 100 miles #returns many observations

Ind50miLA151 <- which(distLA151 < 80467.2) #within 50 miles, 32 sites
Ind100miLA151 <- which(distLA151 < 160934.4) #within 100 miles

#subset data using index
sub50miLA151 <- loc_LAmerc[Ind50miLA151,]
str(sub50miLA151)
unique(sub50miLA151$SiteNum)
unique(sub50miLA151$CollectYear) #2003, 2004, 2006-2009

sub100miLA151 <- loc_LAmerc[Ind100miLA151,]
str(sub100miLA151)
unique(sub100miLA151$SiteNum) #128 sites
unique(sub100miLA151$CollectYear) #2003-2009

#155
distLA155 <- spDists(loc_LAmrip[4,], loc_LAmerc, longlat = FALSE) #in m
distLA155

which(distLA155 < 8046.72) #within 5 miles, 17 sites
which(distLA155 < 16093.4) #within 10 miles, same
which(distLA155 < 40233.6) #within 25 miles #returns  sites, 230 observations
which(distLA155 < 80467.2) #within 50 miles #returns  sites,many observations
which(distLA155 < 160934.4) #within 100 miles #returns many observations

Ind50miLA155 <- which(distLA155 < 80467.2) #within 50 miles, 32 sites
Ind100miLA155 <- which(distLA155 < 160934.4) #within 100 miles

#subset data using index
sub50miLA155 <- loc_LAmerc[Ind50miLA155,]
str(sub50miLA155)
unique(sub50miLA155$SiteNum)
unique(sub50miLA155$CollectYear) #2003-2009

sub100miLA155 <- loc_LAmerc[Ind100miLA155,]
str(sub100miLA155)
unique(sub100miLA155$SiteNum) #128 sites
unique(sub100miLA155$CollectYear) #2003-2010

#222
distLA222 <- spDists(loc_LAmrip[4,], loc_LAmerc, longlat = FALSE) #in m
distLA222

which(distLA222 < 8046.72) #within 5 miles, 17 sites
which(distLA222 < 16093.4) #within 10 miles, same
which(distLA222 < 40233.6) #within 25 miles #returns  sites, 230 observations
which(distLA222 < 80467.2) #within 50 miles #returns  sites,many observations
which(distLA222 < 160934.4) #within 100 miles #returns many observations

Ind50miLA222 <- which(distLA222 < 80467.2) #within 50 miles, 32 sites
Ind100miLA222 <- which(distLA222 < 160934.4) #within 100 miles

#subset data using index
sub50miLA222 <- loc_LAmerc[Ind50miLA222,]
str(sub50miLA222)
unique(sub50miLA222$SiteNum)
unique(sub50miLA222$CollectYear) #2003-2009

sub100miLA222 <- loc_LAmerc[Ind100miLA222,]
str(sub100miLA222)
unique(sub100miLA222$SiteNum) #128 sites
unique(sub100miLA222$CollectYear) #2003-2010
