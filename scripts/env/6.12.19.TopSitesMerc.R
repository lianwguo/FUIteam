### recalculating mercury distances near top sites
library(sp)
library(sf)
library(spdep)
library(rgdal)  # for vector work; sp package should always load with rgdal.
library (raster)   # for metadata/attributes- vectors or rasters
library(maptools)
library(ggplot2)
library(grid)
library(maps)

#read in lat long for different top sites for time periods
LAaSites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAaSites.csv"), 
                    stringsAsFactors = FALSE)
LAbSites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAbSites.csv"), 
                     stringsAsFactors = FALSE)
FLaSites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLaSites.csv"), 
                    stringsAsFactors = FALSE)
FLbSites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLbSites.csv"), 
                     stringsAsFactors = FALSE)

#read in maps 

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
str(LAmercSites)
LAmercSites$SITE_Num <- as.character(LAmercSites$SITE_Num)

LAmerc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_AllMerc.csv"), 
                   stringsAsFactors = FALSE)
str(LAmerc)
LAmerc$SiteNum <- as.character(LAmerc$SiteNum)

#left join
LAmercLL <- left_join(x = LAmerc, y = LAmercSites, by = c("SiteNum" = "SITE_Num"))
str(LAmercLL)
unique(LAmercLL$CollectYear)

LAmercA <- subset(LAmercLL, LAmercLL$CollectYear=="2005" | LAmercLL$CollectYear=="2006" |
                  LAmercLL$CollectYear=="2007" | LAmercLL$CollectYear=="2008" |
                    LAmercLL$CollectYear=="2009")
LAmercB <- subset(LAmercLL, LAmercLL$CollectYear=="2010" | LAmercLL$CollectYear=="2011" |
                    LAmercLL$CollectYear=="2012" | LAmercLL$CollectYear=="2013")

###spatializing process is to go from regular data frame (df) to a spatial data frame (spdf) 
###based on lat/long coordinates, and check crs, assign crs and transform as needed

#convert data.frame to spdf
sp_LAmercA <- SpatialPointsDataFrame(LAmercA[,20:21], LAmercA)
sp_LAmercB <- SpatialPointsDataFrame(LAmercB[,20:21], LAmercB)
sp_LAmripA <- SpatialPointsDataFrame(LAaSites[,2:3], LAaSites)
sp_LAmripB <- SpatialPointsDataFrame(LAbSites[,2:3], LAbSites)
sp_FLmripA <- SpatialPointsDataFrame(FLaSites[,2:3], FLaSites)
sp_FLmripB <- SpatialPointsDataFrame(FLbSites[,2:3], FLbSites)

#project based on original stuff
crs(sp_LAmercA) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAmercB) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAmripA) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAmripB) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLmripA) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLmripB) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


#transform to NAD83
re_LAmercA <- spTransform(sp_LAmercA, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_LAmercB <- spTransform(sp_LAmercB, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_LAmripA <- spTransform(sp_LAmripA, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_LAmripB <- spTransform(sp_LAmripB, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_FLmripA <- spTransform(sp_FLmripA, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
re_FLmripB <- spTransform(sp_FLmripB, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))


#transform to local projection
loc_LAmercA <- spTransform(sp_LAmercA, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAmercB <- spTransform(sp_LAmercB, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAmripA <- spTransform(sp_LAmripA, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAmripB <- spTransform(sp_LAmripB, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))


plot(loc_NOLA,
     col = "yellow",
     main = "TOP mrip and mercury sites")
plot(loc_LA,
     add = TRUE)
plot(loc_LAmercA,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAmercB,
     pch = 4,
     cex = 1,
     col = "orange",
     add = TRUE)
plot(loc_LAmripA,
     pch = 5,
     cex = 1,
     col = 'blue',
     add = TRUE)
plot(loc_LAmripB,
     pch = 5,
     cex = 1,
     col = 'pink',
     add = TRUE)

###
#LA time frame A, top sites = 222, 231, 306, 155, 151

LA222a <- spDists(loc_LAmripA[1,], loc_LAmercA, longlat = FALSE) #in m
LA222a

which(LA222a < 40233.6) #within 25 miles #returns  sites, 44 observations
which(LA222a < 160934.4) #within 100 miles #returns many observations

iLA222a_25mi <- which(LA222a < 40233.6)
iLA222a_100mi <- which(LA222a < 160934.4)

#subset data using index
LA222a_25mi <- loc_LAmercA[iLA222a_25mi,]
str(LA222a_25mi)
unique(LA222a_25mi$SiteNum) # 3 sites
unique(LA222a_25mi$CollectYear) #2007
LA222a_25mi$INTSITE <- 222

LA222a_100mi <- loc_LAmercA[iLA222a_100mi,]
str(LA222a_100mi)
unique(LA222a_100mi$SiteNum) # 100
unique(LA222a_100mi$CollectYear) #2005-2009
LA222a_100mi$INTSITE <- 222


LA231a <- spDists(loc_LAmripA[2,], loc_LAmercA, longlat = FALSE) #in m
LA231a

iLA231a_25mi <- which(LA231a < 40233.6)
iLA231a_100mi <- which(LA231a < 160934.4)

#subset data using index, 
LA231a_25mi <- loc_LAmercA[iLA231a_25mi,]
str(LA231a_25mi)
unique(LA231a_25mi$SiteNum) # 7 sites
unique(LA231a_25mi$CollectYear) #2007, 2008
LA231a_25mi$INTSITE <- 231

LA231a_100mi <- loc_LAmercA[iLA231a_100mi,]
str(LA231a_100mi)
unique(LA231a_100mi$SiteNum) # 109
unique(LA231a_100mi$CollectYear) #2005-2009
LA231a_100mi$INTSITE <- 231

LA306a <- spDists(loc_LAmripA[3,], loc_LAmercA, longlat = FALSE) #in m
LA306a

iLA306a_25mi <- which(LA306a < 40233.6)
iLA306a_100mi <- which(LA306a < 160934.4)

#subset data using index, 
LA306a_25mi <- loc_LAmercA[iLA306a_25mi,]
str(LA306a_25mi)
unique(LA306a_25mi$SiteNum) # 9 sites
unique(LA306a_25mi$CollectYear) #2006,2007
LA306a_25mi$INTSITE <- 306

LA306a_100mi <- loc_LAmercA[iLA306a_100mi,]
str(LA306a_100mi)
unique(LA306a_100mi$SiteNum) # 121
unique(LA306a_100mi$CollectYear) #2005-2009
LA306a_100mi$INTSITE <- 306


LA155a <- spDists(loc_LAmripA[4,], loc_LAmercA, longlat = FALSE) #in m
LA155a

iLA155a_25mi <- which(LA155a < 40233.6)
iLA155a_100mi <- which(LA155a < 160934.4)

#subset data using index, 
LA155a_25mi <- loc_LAmercA[iLA155a_25mi,]
str(LA155a_25mi)
unique(LA155a_25mi$SiteNum) # 10 sites
unique(LA155a_25mi$CollectYear) #2005,2006,2007
LA155a_25mi$INTSITE <- 155

LA155a_100mi <- loc_LAmercA[iLA155a_100mi,]
str(LA155a_100mi)
unique(LA155a_100mi$SiteNum) # 124
unique(LA155a_100mi$CollectYear) #2005-2009
LA155a_100mi$INTSITE <- 155

LA151a <- spDists(loc_LAmripA[5,], loc_LAmercA, longlat = FALSE) #in m
LA151a

iLA151a_25mi <- which(LA151a < 40233.6)
iLA151a_100mi <- which(LA151a < 160934.4)

#subset data using index, 
LA151a_25mi <- loc_LAmercA[iLA151a_25mi,]
str(LA151a_25mi)
unique(LA151a_25mi$SiteNum) # 2 sites
unique(LA151a_25mi$CollectYear) #2007
LA151a_25mi$INTSITE <- 151

LA151a_100mi <- loc_LAmercA[iLA151a_100mi,]
str(LA151a_100mi)
unique(LA151a_100mi$SiteNum) # 95
unique(LA151a_100mi$CollectYear) #2005-2009
LA151a_100mi$INTSITE <- 151

plot(loc_NOLA,
     col = "yellow",
     main = "TOP mrip and mercury sites")
plot(loc_LA,
     add = TRUE)
plot(LA151a_25mi,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(LA151a_100mi,
     pch = 4,
     cex = 1,
     col = "orange",
     add = TRUE)
plot(loc_LAmripA,
     pch = 5,
     cex = 1,
     col = 'blue',
     add = TRUE)
plot(loc_LAmripB,
     pch = 5,
     cex = 1,
     col = 'pink',
     add = TRUE)

###time frame B, top sites = 222, 159, 306, 3325, 151

LA222b <- spDists(loc_LAmripB[1,], loc_LAmercB, longlat = FALSE) #in m
LA222b

iLA222b_25mi <- which(LA222b < 40233.6)
iLA222b_100mi <- which(LA222b < 160934.4)

#with same time frame
LA222b_25mi <- loc_LAmercB[iLA222b_25mi,]
str(LA222b_25mi)
unique(LA222b_25mi$SiteNum) # 0 sites
unique(LA222b_25mi$CollectYear) #0
LA222b_25mi$INTSITE <- 222b

LA222b_100mi <- loc_LAmercB[iLA222b_100mi,]
str(LA222b_100mi)
unique(LA222b_100mi$SiteNum) # 0
unique(LA222b_100mi$CollectYear) 0
LA222b_100mi$INTSITE <- 222b


#with A time frame

LA222ba <- spDists(loc_LAmripB[1,], loc_LAmercA, longlat = FALSE) #in m
LA222ba

iLA222ba_25mi <- which(LA222ba < 40233.6)
iLA222ba_100mi <- which(LA222ba < 160934.4)

LA222ba_25mi <- loc_LAmercA[iLA222ba_25mi,]
str(LA222ba_25mi)
unique(LA222ba_25mi$SiteNum) # 3 sites
unique(LA222ba_25mi$CollectYear) #2007
LA222ba_25mi$INTSITE <- "222ba"

LA222ba_100mi <- loc_LAmercA[iLA222ba_100mi,]
str(LA222ba_100mi)
unique(LA222ba_100mi$SiteNum) # 100
unique(LA222ba_100mi$CollectYear) #2005-2009
LA222ba_100mi$INTSITE <- "222ba"


LA159b <- spDists(loc_LAmripB[2,], loc_LAmercB, longlat = FALSE) #in m
LA159b

iLA159b_25mi <- which(LA159b < 40233.6)
iLA159b_100mi <- which(LA159b < 160934.4)

#with same time frame
LA159b_25mi <- loc_LAmercB[iLA159b_25mi,]
str(LA159b_25mi)
unique(LA159b_25mi$SiteNum) # 0 sites
unique(LA159b_25mi$CollectYear) #0
LA159b_25mi$INTSITE <- "159b"

LA159b_100mi <- loc_LAmercB[iLA159b_100mi,]
str(LA159b_100mi)
unique(LA159b_100mi$SiteNum) # 0
unique(LA159b_100mi$CollectYear) #0
LA159b_100mi$INTSITE <- "159b"

LA159ba <- spDists(loc_LAmripB[2,], loc_LAmercA, longlat = FALSE) #in m
LA159ba

iLA159ba_25mi <- which(LA159ba < 40233.6)
iLA159ba_100mi <- which(LA159ba < 160934.4)

#with A time frame
LA159ba_25mi <- loc_LAmercA[iLA159ba_25mi,]
str(LA159ba_25mi)
unique(LA159ba_25mi$SiteNum) # 3 sites
unique(LA159ba_25mi$CollectYear) #2007
LA159ba_25mi$INTSITE <- "159ba"

LA159ba_100mi <- loc_LAmercA[iLA159ba_100mi,]
str(LA159ba_100mi)
unique(LA159ba_100mi$SiteNum) # 98
unique(LA159ba_100mi$CollectYear) #2005-2009
LA159ba_100mi$INTSITE <- "159ba"


LA306b <- spDists(loc_LAmripB[3,], loc_LAmercB, longlat = FALSE) #in m
LA306b

iLA306b_25mi <- which(LA306b < 40233.6)
iLA306b_100mi <- which(LA306b < 160934.4)

#with same time frame
LA306b_25mi <- loc_LAmercB[iLA306b_25mi,]
str(LA306b_25mi)
unique(LA306b_25mi$SiteNum) # 0 sites
unique(LA306b_25mi$CollectYear) #0
LA306b_25mi$INTSITE <- "306b"

LA306b_100mi <- loc_LAmercB[iLA306b_100mi,]
str(LA306b_100mi)
unique(LA306b_100mi$SiteNum) # 1
unique(LA306b_100mi$CollectYear) #2010
LA306b_100mi$INTSITE <- "306b"

LA306ba <- spDists(loc_LAmripB[3,], loc_LAmercA, longlat = FALSE) #in m
LA306ba

iLA306ba_25mi <- which(LA306ba < 40233.6)
iLA306ba_100mi <- which(LA306ba < 160934.4)

#with A time frame
LA306ba_25mi <- loc_LAmercA[iLA306ba_25mi,]
str(LA306ba_25mi)
unique(LA306ba_25mi$SiteNum) # 9 sites
unique(LA306ba_25mi$CollectYear) #2006,2007
LA306ba_25mi$INTSITE <- "306ba"

LA306ba_100mi <- loc_LAmercA[iLA306ba_100mi,]
str(LA306ba_100mi)
unique(LA306ba_100mi$SiteNum) # 121
unique(LA306ba_100mi$CollectYear) #2005-2009
LA306ba_100mi$INTSITE <- "306ba"

LA3325b <- spDists(loc_LAmripB[4,], loc_LAmercB, longlat = FALSE) #in m
LA3325b

iLA3325b_25mi <- which(LA3325b < 40233.6)
iLA3325b_100mi <- which(LA3325b < 160934.4)

#with same time frame
LA3325b_25mi <- loc_LAmercB[iLA3325b_25mi,]
str(LA3325b_25mi)
unique(LA3325b_25mi$SiteNum) # 0 sites
unique(LA3325b_25mi$CollectYear) #0
LA3325b_25mi$INTSITE <- "3325b"

LA3325b_100mi <- loc_LAmercB[iLA3325b_100mi,]
str(LA3325b_100mi)
unique(LA3325b_100mi$SiteNum) # 1
unique(LA3325b_100mi$CollectYear) #2010
LA3325b_100mi$INTSITE <- "3325b"

LA3325ba <- spDists(loc_LAmripB[4,], loc_LAmercA, longlat = FALSE) #in m
LA3325ba

iLA3325ba_25mi <- which(LA3325ba < 40233.6)
iLA3325ba_100mi <- which(LA3325ba < 160934.4)

#with A time frame
LA3325ba_25mi <- loc_LAmercA[iLA3325ba_25mi,]
str(LA3325ba_25mi)
unique(LA3325ba_25mi$SiteNum) # 9 sites
unique(LA3325ba_25mi$CollectYear) #2005-2009
LA3325ba_25mi$INTSITE <- "3325ba"

LA3325ba_100mi <- loc_LAmercA[iLA3325ba_100mi,]
str(LA3325ba_100mi)
unique(LA3325ba_100mi$SiteNum) # 124
unique(LA3325ba_100mi$CollectYear) #2005-2009
LA3325ba_100mi$INTSITE <- "3325ba"

LA151b <- spDists(loc_LAmripB[5,], loc_LAmercB, longlat = FALSE) #in m
LA151b

iLA151b_25mi <- which(LA151b < 40233.6)
iLA151b_100mi <- which(LA151b < 160934.4)

#with same time frame
LA151b_25mi <- loc_LAmercB[iLA151b_25mi,]
str(LA151b_25mi)
unique(LA151b_25mi$SiteNum) # 0 sites
unique(LA151b_25mi$CollectYear) #0
LA151b_25mi$INTSITE <- "151b"

LA151b_100mi <- loc_LAmercB[iLA151b_100mi,]
str(LA151b_100mi)
unique(LA151b_100mi$SiteNum) # 0
unique(LA151b_100mi$CollectYear) #0
LA151b_100mi$INTSITE <- "151b"

LA151ba <- spDists(loc_LAmripB[5,], loc_LAmercA, longlat = FALSE) #in m
LA151ba

iLA151ba_25mi <- which(LA151ba < 40233.6)
iLA151ba_100mi <- which(LA151ba < 160934.4)

#with A time frame
LA151ba_25mi <- loc_LAmercA[iLA151ba_25mi,]
str(LA151ba_25mi)
unique(LA151ba_25mi$SiteNum) # 2 sites
unique(LA151ba_25mi$CollectYear) #2007
LA151ba_25mi$INTSITE <- "151ba"

LA151ba_100mi <- loc_LAmercA[iLA151ba_100mi,]
str(LA151ba_100mi)
unique(LA151ba_100mi$SiteNum) # 95
unique(LA151ba_100mi$CollectYear) #2005-2009
LA151ba_100mi$INTSITE <- "151ba"

#rbind all data sets together
#LA time frame A, top sites = 222, 231, 306, 155, 151
la25miA <- rbind(LA222a_25mi, LA231a_25mi, LA306a_25mi, LA155a_25mi, LA151a_25mi)
la100miA <- rbind(LA222a_100mi, LA231a_100mi, LA306a_100mi, LA155a_100mi, LA151a_100mi)
###time frame B, top sites = 222, 159, 306, 3325, 151, with merc B
# NO DATA    la25miB <- rbind(LA222b_25mi, LA159b_25mi, LA306b_25mi, LA3325b_25mi, LA151b_25mi)
la100miB <- rbind(LA222b_100mi, LA159b_100mi, LA306b_100mi, LA3325b_100mi, LA151b_100mi)
###time frame B, top sites = 222, 159, 306, 3325, 151, with merc A
la25miBA <- rbind(LA222ba_25mi, LA159ba_25mi, LA306ba_25mi, LA3325ba_25mi, LA151ba_25mi)
la100miBA <- rbind(LA222ba_100mi, LA159ba_100mi, LA306ba_100mi, LA3325ba_100mi, LA151ba_100mi)

unique(la100miA$spName)
unique(la100miB$spName)
unique(la100miBA$spName)

mFishes <- cbind('BLACK DRUM','BLACKFIN TUNA','COBIA','GRAY SNAPPER','GRAY TRIGGERFISH',
                 'KING MACKEREL','RED DRUM','RED SNAPPER','SHEEPSHEAD','SOUTHERN FLOUNDER',
                 'SPOTTED SEATROUT','STRIPED BASS')

la25miAsw <- la25miA[la25miA$spName %in% mFishes,]
la100miAsw <- la100miA[la100miA$spName %in% mFishes,]
###la25miBsw <- la25miB[la25miB$spName %in% mFishes,] ### no marine fishes
###la100miBsw <- la100miB[la100miB$spName %in% mFishes,] ### no marine fishes
la25miBAsw <- la25miBA[la25miBA$spName %in% mFishes,]
la100miBAsw <- la100miBA[la100miBA$spName %in% mFishes,]
unique(la25miAsw$spName)

### Florida's turn!

FLmercLoc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLmercLocUp.csv"), 
                      stringsAsFactors = FALSE)
FLmerc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_mercury.csv"), 
                   stringsAsFactors = FALSE)
str(FLmercLoc)
unique(FLmercSp[,21])
str(FLmerc)

#merge location data together
FLmercSp <- left_join(x = FLmerc, y = FLmercLoc, by = "Bottle", all.x = TRUE)
str(FLmercSp)
head(FLmercSp)
FLmercSp[20:30,]

#removing NA values

unique(FLmercSp[,26])
FLmercSpB <- subset(FLmercSp, !(is.na(FLmercSp[,26])))
head(FLmercSpB)
str(FLmercSpB)
unique(FLmercSp[,34])

#convert to decimal degrees from degrees minutes
FLmercSp$LatDD <- with(FLmercSp, (FLmercSp[,26] + (FLmercSp[,27]/60)))
FLmercSp$LonDD <- with(FLmercSp, (FLmercSp[,29] + (FLmercSp[,30]*-1/60)))

#remove all na lat/longs, not assigned by FWC or Lian
FLmercSpB <- subset(FLmercSp, (!is.na(FLmercSp$LatDD)))
unique(FLmercSpB$LonDD) 
str(FLmercSpB)

#assigning spatial components
sp_FLmripA <- SpatialPointsDataFrame(FLaSites[,2:3], FLaSites)
sp_FLmripB <- SpatialPointsDataFrame(FLbSites[,2:3], FLbSites)
crs(sp_FLmripA) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLmripB) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
loc_FLmripA <- spTransform(sp_FLmripA, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
loc_FLmripB <- spTransform(sp_FLmripB, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
crs(loc_FLmripA)
crs(loc_FLmripB)

#load MSA for Tampa
StatArea <- readOGR("~/FUIteam/PydioData/env/raw/cb_2017_us_cbsa_20m", "cb_2017_us_cbsa_20m") ##readOGR(directory,shapefilename)
head(StatArea)

Tampa <- subset(StatArea, StatArea$GEOID == 45300) #45300. Tampa-St. Petersburg-Clearwater, FL Metro Area
Tampa

crs(Tampa)
loc_Tampa <- spTransform(Tampa, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

which(US_State_Bound$NAME == "Florida")
FL_Bound <- US_State_Bound[51,]
FL_Bound

crs(FL_Bound)
loc_FLbound <- spTransform(FL_Bound, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#spatialize mercury data
unique(FLmercSpC$Year)

sp_FLmerc <- SpatialPointsDataFrame(FLmercSpB[,34:33], FLmercSpB)
crs(sp_FLmerc) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
loc_FLmerc <- spTransform(sp_FLmerc, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
crs(loc_FLmerc)

plot(loc_FLbound,
     main = "FL Mercury Testing Sites")
plot(loc_FLmerc,
     add = TRUE)
plot(loc_FLmripA,
     col = 'blue',
     add = TRUE)
plot(loc_FLmripB,
     col = 'green',
     add = TRUE)

loc_FLmripA <- spTransform(sp_FLmripA, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
loc_FLmripB <- spTransform(sp_FLmripB, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

loc_FLmripA$INTSITE
loc_LAmripA$INTSITE
#Tampa
# time frame A top sites = 770, 769, 632, 615, 742
#all mercury testing outside of time frame

FL770a <- spDists(loc_FLmripA[1,], loc_FLmerc, longlat = FALSE) #in m
FL770a

iFL770a_25mi <- which(FL770a < 40233.6)
iFL770a_100mi <- which(FL770a < 160934.4)

#subset data using index, 
FL770a_25mi <- loc_FLmerc[iFL770a_25mi,]
str(FL770a_25mi)
unique(FL770a_25mi$LatDD) # 10 sites
unique(FL770a_25mi$Year) #2004
FL770a_25mi$INTSITE <- 770

plot(FL770a_100mi,
     col = 'blue')
plot(loc_Tampa, add = TRUE)
plot(loc_FLbound, add = TRUE)
plot(loc_FLmripA[1,],
     col = 'red',
     add = TRUE)
plot(FL770a_25mi,
     col = 'green',
     add = TRUE)

FL770a_100mi <- loc_FLmerc[iFL770a_100mi,]
str(FL770a_100mi)
unique(FL770a_100mi$LatDD) # 52
unique(FL770a_100mi$Year) #2003-2004
FL770a_100mi$INTSITE <- 770


FL769a <- spDists(loc_FLmripA[2,], loc_FLmerc, longlat = FALSE) #in m
FL769a

iFL769a_25mi <- which(FL769a < 40233.6)
iFL769a_100mi <- which(FL769a < 160934.4)

#subset data using index, 
FL769a_25mi <- loc_FLmerc[iFL769a_25mi,]
str(FL769a_25mi)
unique(FL769a_25mi$LatDD) # 10 sites
unique(FL769a_25mi$Year) #2004
FL769a_25mi$INTSITE <- 769

FL769a_100mi <- loc_FLmerc[iFL769a_100mi,]
str(FL769a_100mi)
unique(FL769a_100mi$LatDD) # 52
unique(FL769a_100mi$Year) #2003-2004
FL769a_100mi$INTSITE <- 769


FL632a <- spDists(loc_FLmripA[3,], loc_FLmerc, longlat = FALSE) #in m
FL632a

iFL632a_25mi <- which(FL632a < 40233.6)
iFL632a_100mi <- which(FL632a < 160934.4)

#subset data using index, 
FL632a_25mi <- loc_FLmerc[iFL632a_25mi,]
str(FL632a_25mi)
unique(FL632a_25mi$LatDD) # 8 sites
unique(FL632a_25mi$Year) #2004
FL632a_25mi$INTSITE <- 632

FL632a_100mi <- loc_FLmerc[iFL632a_100mi,]
str(FL632a_100mi)
unique(FL632a_100mi$LatDD) # 52
unique(FL632a_100mi$Year) #2003-2004
FL632a_100mi$INTSITE <- 632


FL615a <- spDists(loc_FLmripA[4,], loc_FLmerc, longlat = FALSE) #in m
FL615a

iFL615a_25mi <- which(FL615a < 40233.6)
iFL615a_100mi <- which(FL615a < 160934.4)

#subset data using index, 
FL615a_25mi <- loc_FLmerc[iFL615a_25mi,]
str(FL615a_25mi)
unique(FL615a_25mi$LatDD) # 9 sites
unique(FL615a_25mi$Year) #2004
FL615a_25mi$INTSITE <- 615

FL615a_100mi <- loc_FLmerc[iFL615a_100mi,]
str(FL615a_100mi)
unique(FL615a_100mi$LatDD) # 85 sites
unique(FL615a_100mi$Year) #2003-2004
FL615a_100mi$INTSITE <- 615


FL742a <- spDists(loc_FLmripA[5,], loc_FLmerc, longlat = FALSE) #in m
FL742a

iFL742a_25mi <- which(FL742a < 40233.6)
iFL742a_100mi <- which(FL742a < 160934.4)

#subset data using index, 
FL742a_25mi <- loc_FLmerc[iFL742a_25mi,]
str(FL742a_25mi)
unique(FL742a_25mi$LatDD) # 7 sites
unique(FL742a_25mi$Year) #2004
FL742a_25mi$INTSITE <- 742

FL742a_100mi <- loc_FLmerc[iFL742a_100mi,]
str(FL742a_100mi)
unique(FL742a_100mi$LatDD) # 100 sites
unique(FL742a_100mi$Year) #2003-2004
FL742a_100mi$INTSITE <- 742


#time frame B
#top sites = 770, 632, 3802, 769, 614

FL770b <- spDists(loc_FLmripB[1,], loc_FLmerc, longlat = FALSE) #in m
FL770b

iFL770b_25mi <- which(FL770b < 40233.6)
iFL770b_100mi <- which(FL770b < 160934.4)

#subset data using index, 
FL770b_25mi <- loc_FLmerc[iFL770b_25mi,]
str(FL770b_25mi)
unique(FL770b_25mi$LatDD) # 10 sites
unique(FL770b_25mi$Year) #2004
FL770b_25mi$INTSITE <- '770b'

FL770b_100mi <- loc_FLmerc[iFL770b_100mi,]
str(FL770b_100mi)
unique(FL770b_100mi$LatDD) # 52
unique(FL770b_100mi$Year) #2003-2004
FL770b_100mi$INTSITE <- '770b'


FL632b <- spDists(loc_FLmripB[2,], loc_FLmerc, longlat = FALSE) #in m
FL632b

iFL632b_25mi <- which(FL632b < 40233.6)
iFL632b_100mi <- which(FL632b < 160934.4)

#subset data using index, 
FL632b_25mi <- loc_FLmerc[iFL632b_25mi,]
str(FL632b_25mi)
unique(FL632b_25mi$LatDD) # 8 sites
unique(FL632b_25mi$Year) #2004
FL632b_25mi$INTSITE <- '632b'

FL632b_100mi <- loc_FLmerc[iFL632b_100mi,]
str(FL632b_100mi)
unique(FL632b_100mi$LatDD) # 52
unique(FL632b_100mi$Year) #2003-2004
FL632b_100mi$INTSITE <- '632b'


FL3802b <- spDists(loc_FLmripB[3,], loc_FLmerc, longlat = FALSE) #in m
FL3802b

iFL3802b_25mi <- which(FL3802b < 40233.6)
iFL3802b_100mi <- which(FL3802b < 160934.4)

#subset data using index, 
FL3802b_25mi <- loc_FLmerc[iFL3802b_25mi,]
str(FL3802b_25mi)
unique(FL3802b_25mi$LatDD) # 10 sites
unique(FL3802b_25mi$Year) #2004
FL3802b_25mi$INTSITE <- '3802b'

FL3802b_100mi <- loc_FLmerc[iFL3802b_100mi,]
str(FL3802b_100mi)
unique(FL3802b_100mi$LatDD) # 52
unique(FL3802b_100mi$Year) #2003-2004
FL3802b_100mi$INTSITE <- '3802b'



FL769b <- spDists(loc_FLmripB[4,], loc_FLmerc, longlat = FALSE) #in m
FL769b

iFL769b_25mi <- which(FL769b < 40233.6)
iFL769b_100mi <- which(FL769b < 160934.4)

#subset data using index, 
FL769b_25mi <- loc_FLmerc[iFL769b_25mi,]
str(FL769b_25mi)
unique(FL769b_25mi$LatDD) # 10 sites
unique(FL769b_25mi$Year) #2004
FL769b_25mi$INTSITE <- '769b'

FL769b_100mi <- loc_FLmerc[iFL769b_100mi,]
str(FL769b_100mi)
unique(FL769b_100mi$LatDD) # 52
unique(FL769b_100mi$Year) #2003-2004
FL769b_100mi$INTSITE <- '769b'


FL614b <- spDists(loc_FLmripB[5,], loc_FLmerc, longlat = FALSE) #in m
FL614b

iFL614b_25mi <- which(FL614b < 40233.6)
iFL614b_100mi <- which(FL614b < 160934.4)

#subset data using index, 
FL614b_25mi <- loc_FLmerc[iFL614b_25mi,]
str(FL614b_25mi)
unique(FL614b_25mi$LatDD) # 9 sites
unique(FL614b_25mi$Year) #2004
FL614b_25mi$INTSITE <- '614b'

FL614b_100mi <- loc_FLmerc[iFL614b_100mi,]
str(FL614b_100mi)
unique(FL614b_100mi$LatDD) # 91
unique(FL614b_100mi$Year) #2003-2004
FL614b_100mi$INTSITE <- '614b'


#rbind all data sets together
#FL time frame A top sites = 770, 769, 632, 615, 742
fl25miA <- rbind(FL770a_25mi, FL769a_25mi, FL632a_25mi, FL615a_25mi, FL742a_25mi)
fl100miA <- rbind(FL770a_100mi, FL769a_100mi, FL632a_100mi, FL615a_100mi, FL742a_100mi)
### time frame B - top sites = 770, 632, 3802, 769, 614
fl25miB <- rbind(FL770b_25mi, FL632b_25mi, FL3802b_25mi, FL769b_25mi, FL614b_25mi)
fl100miB <- rbind(FL770b_100mi, FL632b_100mi, FL3802b_100mi, FL769b_100mi, FL614b_100mi)
###

#mercury data frames
DFfl25miA <- as.data.frame(fl25miA)
DFfl100miA <- as.data.frame(fl100miA)
DFfl25miB <- as.data.frame(fl25miB)
DFfl100miB <- as.data.frame(fl100miB)
DFla25miA <- as.data.frame(la25miAsw)
DFla100miA <- as.data.frame(la100miAsw)
DFla25miBA <- as.data.frame(la25miBAsw)
DFla100miBA <- as.data.frame(la100miBAsw)

#relevant mrip data
landOtimA
landOtimB
landTtimA
landTtimB


