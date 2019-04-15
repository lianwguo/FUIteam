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
sub50miLA141$mripSite <- 141

sub100miLA141 <- loc_LAmerc[Ind100miLA141,]
str(sub100miLA141)
unique(sub100miLA141$SiteNum) #128 sites
unique(sub100miLA141$CollectYear) #2003-2009
sub100miLA141$mripSite <- 141

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
sub50miLA150$mripSite <- 150

sub100miLA150 <- loc_LAmerc[Ind100miLA150,]
str(sub100miLA150)
unique(sub100miLA150$SiteNum) #128 sites
unique(sub100miLA150$CollectYear) #2003-2009
sub100miLA150$mripSite <- 150

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
sub50miLA151$mripSite <- 151

sub100miLA151 <- loc_LAmerc[Ind100miLA151,]
str(sub100miLA151)
unique(sub100miLA151$SiteNum) #128 sites
unique(sub100miLA151$CollectYear) #2003-2009
sub100miLA151$mripSite <- 151

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
sub50miLA155$mripSite <- 155

sub100miLA155 <- loc_LAmerc[Ind100miLA155,]
str(sub100miLA155)
unique(sub100miLA155$SiteNum) #128 sites
unique(sub100miLA155$CollectYear) #2003-2010
sub100miLA155$mripSite <- 155

#222
distLA222 <- spDists(loc_LAmrip[5,], loc_LAmerc, longlat = FALSE) #in m
distLA222

which(distLA222 < 8046.72) #within 5 miles, 
which(distLA222 < 16093.4) #within 10 miles, 
which(distLA222 < 40233.6) #within 25 miles #returns  sites, 44 observations
which(distLA222 < 80467.2) #within 50 miles #returns  sites,589+ observations
which(distLA222 < 160934.4) #within 100 miles #returns many observations

Ind50miLA222 <- which(distLA222 < 80467.2) #within 50 miles
Ind100miLA222 <- which(distLA222 < 160934.4) #within 100 miles

#subset data using index
sub50miLA222 <- loc_LAmerc[Ind50miLA222,]
str(sub50miLA222)
unique(sub50miLA222$SiteNum)
unique(sub50miLA222$CollectYear) #2003-2009
sub50miLA222$mripSite <- 222

sub100miLA222 <- loc_LAmerc[Ind100miLA222,]
str(sub100miLA222)
unique(sub100miLA222$SiteNum) #128 sites
unique(sub100miLA222$CollectYear) #2003-2009
sub100miLA222$mripSite <- 222

#231
distLA231 <- spDists(loc_LAmrip[6,], loc_LAmerc, longlat = FALSE) #in m
distLA231

which(distLA231 < 8046.72) #within 5 miles, 
which(distLA231 < 16093.4) #within 10 miles, 
which(distLA231 < 40233.6) #within 25 miles #returns  sites, 92 observations
which(distLA231 < 80467.2) #within 50 miles #returns  sites,631+ observations
which(distLA231 < 160934.4) #within 100 miles #returns many observations

Ind50miLA231 <- which(distLA231 < 80467.2) #within 50 miles
Ind100miLA231 <- which(distLA231 < 160934.4) #within 100 miles

#subset data using index
sub50miLA231 <- loc_LAmerc[Ind50miLA231,]
str(sub50miLA231)
unique(sub50miLA231$SiteNum)
unique(sub50miLA231$CollectYear) #2003-2009
sub50miLA231$mripSite <- 231

sub100miLA231 <- loc_LAmerc[Ind100miLA231,]
str(sub100miLA231)
unique(sub100miLA231$SiteNum) 
unique(sub100miLA231$CollectYear) #2003-2009
sub100miLA231$mripSite <- 231

#306
distLA306 <- spDists(loc_LAmrip[7,], loc_LAmerc, longlat = FALSE) #in m
distLA306

which(distLA306 < 8046.72) #within 5 miles, 6 obs
which(distLA306 < 16093.4) #within 10 miles, 17 obs
which(distLA306 < 40233.6) #within 25 miles #returns  sites, 200+ observations
which(distLA306 < 80467.2) #within 50 miles #returns  sites,931+ observations
which(distLA306 < 160934.4) #within 100 miles #returns many observations

Ind50miLA306 <- which(distLA306 < 80467.2) #within 50 miles
Ind100miLA306 <- which(distLA306 < 160934.4) #within 100 miles

#subset data using index
sub50miLA306 <- loc_LAmerc[Ind50miLA306,]
str(sub50miLA306)
unique(sub50miLA306$SiteNum)
unique(sub50miLA306$CollectYear) #2003-2009
sub50miLA306$mripSite <- 306

sub100miLA306 <- loc_LAmerc[Ind100miLA306,]
str(sub100miLA306)
unique(sub100miLA306$SiteNum) 
unique(sub100miLA306$CollectYear) #2003-2010
sub100miLA306$mripSite <- 306

#3325
distLA3325 <- spDists(loc_LAmrip[8,], loc_LAmerc, longlat = FALSE) #in m
distLA3325

which(distLA3325 < 8046.72) #within 5 miles, 14 obs
which(distLA3325 < 16093.4) #within 10 miles, 71 obs
which(distLA3325 < 40233.6) #within 25 miles #returns  sites, 305+ observations
which(distLA3325 < 80467.2) #within 50 miles #returns  sites,780+ observations
which(distLA3325 < 160934.4) #within 100 miles #returns many observations

Ind50miLA3325 <- which(distLA3325 < 80467.2) #within 50 miles
Ind100miLA3325 <- which(distLA3325 < 160934.4) #within 100 miles

#subset data using index
sub50miLA3325 <- loc_LAmerc[Ind50miLA3325,]
str(sub50miLA3325)
unique(sub50miLA3325$SiteNum)
unique(sub50miLA3325$CollectYear) #2003-2009
sub50miLA3325$mripSite <- 3325

sub100miLA3325 <- loc_LAmerc[Ind100miLA3325,]
str(sub100miLA3325)
unique(sub100miLA3325$SiteNum) 
unique(sub100miLA3325$CollectYear) #2003-2010
sub100miLA3325$mripSite <- 3325

#compile all of 50 mi data together into one dataframe, same for 100mi
LA50mi <- rbind(sub50miLA141,sub50miLA150,sub50miLA151,sub50miLA155,
                sub50miLA222,sub50miLA231,sub50miLA306,sub50miLA3325)
nrow(sub50miLA141)
nrow(LA50mi)
unique(LA50mi$spName)
swLA50mi<- subset(LA50mi, LA50mi$spName == 'BLACK DRUM'|LA50mi$spName == 'BLACKFIN TUNA'|
                    LA50mi$spName == 'COBIA'|LA50mi$spName == 'GRAY SNAPPER'|
                    LA50mi$spName == 'GRAY TRIGGERFISH'|LA50mi$spName == 'KING MACKEREL'|
                    LA50mi$spName == 'RED DRUM'|LA50mi$spName == 'RED SNAPPER'|
                    LA50mi$spName == 'SHEEPSHEAD'|LA50mi$spName == 'SOUTHERN FLOUNDER'|
                    LA50mi$spName == 'SPOTTED SEATROUT'|LA50mi$spName == 'STRIPED BASS')
nrow(swLA50mi)
write.csv(swLA50mi, "~/FUIteam/PydioData/env/data_outputs/swLA50mi.csv")

LA100mi <- rbind(sub100miLA141,sub100miLA150,sub100miLA151,sub100miLA155,
                sub100miLA222,sub100miLA231,sub100miLA306,sub100miLA3325)
nrow(sub100miLA141)
nrow(LA100mi)
swLA100mi<- subset(LA100mi, LA100mi$spName == 'BLACK DRUM'|LA100mi$spName == 'BLACKFIN TUNA'|
                    LA100mi$spName == 'COBIA'|LA100mi$spName == 'GRAY SNAPPER'|
                    LA100mi$spName == 'GRAY TRIGGERFISH'|LA100mi$spName == 'KING MACKEREL'|
                    LA100mi$spName == 'RED DRUM'|LA100mi$spName == 'RED SNAPPER'|
                    LA100mi$spName == 'SHEEPSHEAD'|LA100mi$spName == 'SOUTHERN FLOUNDER'|
                    LA100mi$spName == 'SPOTTED SEATROUT'|LA100mi$spName == 'STRIPED BASS')
nrow(swLA100mi)
write.csv(swLA100mi, "~/FUIteam/PydioData/env/data_outputs/swLA100mi.csv")

plot(loc_NOLA,
     col = "yellow",
     main = "TOP mrip and mercury sites")
plot(loc_LA,
     add = TRUE)
plot(swLA100mi,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAmrip,
     pch = 5,
     cex = 1,
     col = 'blue',
     add = TRUE)

dfLA50mi <- as.data.frame(swLA50mi)
spLA50mi <- unique(dfLA50mi[c("spName", "mripSite")])
write.csv(spLA50mi, "~/FUIteam/PydioData/env/data_outputs/spLA50mi.csv")

dfLA100mi <- as.data.frame(swLA100mi)
spLA100mi <- unique(dfLA100mi[c("spName", "mripSite")])
write.csv(spLA100mi, "~/FUIteam/PydioData/env/data_outputs/spLA100mi.csv")

library(dplyr)
tibbLA50mi <- spLA50mi %>%
  group_by(mripSite) %>%
  summarise(n_distinct(spName))
tibbLA100mi <- spLA100mi %>%
  group_by(mripSite) %>%
  summarise(n_distinct(spName))
write.csv(tibbLA50mi, "~/FUIteam/PydioData/env/data_outputs/tibbLA50mi.csv")
write.csv(tibbLA100mi, "~/FUIteam/PydioData/env/data_outputs/tibbLA100mi.csv")

