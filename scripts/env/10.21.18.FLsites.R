##FL top landing sites
#Timepoint A - 2005-2010
#769, 770, 742, 615, 712
#Timepoint B - 2011-2015
#770, 3802, 769, 614, 241

###last data available was 2004, which is not within either of our time frames. Should use that 
#information anyway? 

#load
FLmerc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_mercury.csv"), 
                      stringsAsFactors = FALSE)
head(FLmerc)

FLmercLoc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLmercLoc.csv"), 
                   stringsAsFactors = FALSE)
head(FLmercLoc)

#merging location data from FL FWC to Hg data
FLmercSpatial <- merge(x = FLmerc, y = FLmercLoc, by.x = "Bottle", by.y = "Bottle", all.x = TRUE)

head(FLmercSpatial)
tail(FLmercSpatial)
unique(FLmercSpatial$Bottle)

FLmercSpatialB <- FLmercSpatial[27:859,]
head(FLmercSpatialB)

FLmercSpatialC <- FLmercSpatialB
FLmercSpatialC[,19:24] <- NULL
FLmercSpatialC[,16] <- NULL
FLmercSpatialC[,7] <- NULL
FLmercSpatialC[,5] <- NULL
FLmercSpatialC[,3] <- NULL
head(FLmercSpatialC)

#converting Decimal/Minutes/Seconds to Decimal Degrees, and more cleaning of data. simplest form is 
#FLmercSpatialD
FLmercSpatialC$LatDD <- with(FLmercSpatialC, Lat_Deg + (Lat_Min/60))
FLmercSpatialC$LonDDr <- with(FLmercSpatialC, LonDD * -1)
FLmercSpatialC[,23] <- NULL
FLmercSpatialD <- FLmercSpatialC
FLmercSpatialD[,16:21] <- NULL
head(FLmercSpatialD)

sp_FLmerc <- SpatialPointsDataFrame(FLmercSpatialD[,16:17], FLmercSpatialD) #didn't work because NA values
is.na(FLmercSpatialD$LatDD)

write.csv(FLmercSpatialD, "~/FUIteam/PydioData/env/data_outputs/FLmercSpatialD.csv")
#there are a lot of NAs left from samples with no locations. Need to make new dataframe without those

FLmercSpatialE <- subset(FLmercSpatialD, (!is.na(FLmercSpatialD[,16])))
is.na(FLmercSpatialE$LatDD) # hell yeah, it worked!

sp_FLmerc <- SpatialPointsDataFrame(FLmercSpatialE[,17:16], FLmercSpatialE)
crs(sp_FLmerc) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

plot(sp_FLmerc,
         main = "FL Mercury Testing Sites")
plot(US_State_Bound,
     add = TRUE)

loc_FLmerc <- spTransform(sp_FLmerc, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
FL_State <- spTransform(US_State_Bound, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))

plot(loc_FLmerc,
     main = "FL Mercury Testing Sites")
plot(FL_State,
     add = TRUE)

#load MSA for Tampa
StatArea <- readOGR("~/FUIteam/PydioData/env/raw/cb_2017_us_cbsa_20m", "cb_2017_us_cbsa_20m") ##readOGR(directory,shapefilename)
head(StatArea)

Tampa <- subset(StatArea, StatArea$GEOID == 45300) #45300. Tampa-St. Petersburg-Clearwater, FL Metro Area
Tampa

crs(Tampa)
loc_Tampa <- spTransform(Tampa, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))

plot(loc_Tampa,
     col = "pink",
     main = "FL Mercury Testing Sites")
plot(loc_FLmerc,
     add = TRUE)

which(US_State_Bound$NAME == "Florida")
FL_Bound <- US_State_Bound[51,]
FL_Bound

loc_FLbound <- spTransform(FL_Bound, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))

plot(loc_Tampa,
     col = "pink",
     main = "FL Mercury Testing Sites")
plot(loc_FLmerc,
     add = TRUE)
plot(loc_FLbound,
     add = TRUE)

plot(loc_FLbound,
     main = "FL Mercury Testing Sites")
plot(loc_FLmerc,
     add = TRUE)
plot(loc_Tampa,
     col = "yellow",
     add = TRUE)
### everything is transformed to the local projection to west florida, NAD83. 
## there appears to be only one site even close to Tampa...

###work on case study sites (top five for two time blocks)
#readin dataframes for remaining case study sites for both time frames 
FLsite241 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite241.csv"), 
                      stringsAsFactors = FALSE)
FLsite615 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite615.csv"), 
                      stringsAsFactors = FALSE)
FLsite712 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite712.csv"), 
                      stringsAsFactors = FALSE)
FLsite742 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite742.csv"), 
                      stringsAsFactors = FALSE)
FLsite769 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite769.csv"), 
                      stringsAsFactors = FALSE)
FLsite614 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite614.csv"), 
                      stringsAsFactors = FALSE)
FLsite3802 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite3802.csv"), 
                      stringsAsFactors = FALSE)
FLsite770 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLsite770.csv"), 
                       stringsAsFactors = FALSE)
##first time period (2005-2010)
FLsite769
FLsite770
FLsite742
FLsite712
FLsite615

##second time period (2011-2015)
FLsite769
FLsite770
FLsite3802
FLsite241
FLsite614

#convert to spatial dataframe
sp_FLsite241 <- SpatialPointsDataFrame(FLsite241[,2:3], FLsite241)
sp_FLsite769 <- SpatialPointsDataFrame(FLsite769[,2:3], FLsite769)
sp_FLsite770 <- SpatialPointsDataFrame(FLsite770[,2:3], FLsite770)
sp_FLsite742 <- SpatialPointsDataFrame(FLsite742[,2:3], FLsite742)
sp_FLsite712 <- SpatialPointsDataFrame(FLsite712[,2:3], FLsite712)
sp_FLsite615 <- SpatialPointsDataFrame(FLsite615[,2:3], FLsite615)
sp_FLsite614 <- SpatialPointsDataFrame(FLsite614[,2:3], FLsite614)
sp_FLsite3802 <- SpatialPointsDataFrame(FLsite3802[,2:3], FLsite3802)

#assign original crs
crs(sp_FLsite241) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLsite769) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLsite770) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLsite742) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLsite712) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLsite615) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLsite614) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_FLsite3802) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#transform to local projection (Florida NAD83 west)
loc_FLsite241 <- spTransform(sp_FLsite241, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
loc_FLsite769 <- spTransform(sp_FLsite769, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
loc_FLsite770 <- spTransform(sp_FLsite770, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
loc_FLsite742 <- spTransform(sp_FLsite742, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
loc_FLsite712 <- spTransform(sp_FLsite712, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
loc_FLsite615 <- spTransform(sp_FLsite615, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
loc_FLsite614 <- spTransform(sp_FLsite614, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
loc_FLsite3802 <- spTransform(sp_FLsite3802, CRS("+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))

plot(loc_Tampa,
     col = "yellow",
     main = "FL Mercury Testing Sites")
plot(loc_FLmerc,
     add = TRUE)
plot(loc_FLsite241,
     col = "red",
     add = TRUE)
plot(loc_FLsite769,
     col = "red",
     add = TRUE)
plot(loc_FLsite770,
     col = "red",
     add = TRUE)
plot(loc_FLsite614,
     col = "red",
     add = TRUE)
plot(loc_FLsite615,
     col = "red",
     add = TRUE)
plot(loc_FLsite742,
     col = "red",
     add = TRUE)
plot(loc_FLsite712,
     col = "red",
     add = TRUE)
plot(loc_FLsite3802,
     col = "red",
     add = TRUE)
plot(loc_FLbound,
     add = TRUE)

#distances based on local projection, site 769
merc769dist <- spDistsN1(loc_FLmerc, loc_FLsite769, longlat = FALSE) #in ft
merc769dist
str(loc_FLmerc)

unique(merc769dist)
which(merc769dist < 26400) #within 5 miles #returns 0, 
which(merc769dist < 52800) #within 10 miles #returns 1
which(merc769dist < 79200) #within 15 miles #returns 1
which(merc769dist < 132000) #within 25 miles #returns 1
which(merc769dist < 528000) #within 100 miles #returns 13

#make vector of indexes found within given distance in last step
Index10mi769 <- which(merc769dist < 52800)
Index100mi769 <- which(merc769dist < 528000)

#subset sites of interest within 10 miles, using vectors of indexes
subset10miHg769 <- loc_FLmerc[Index10mi769,]
head(subset10miHg769)
unique(subset10miHg769$Bottle)
unique(subset10miHg769$Year)
write.csv(subset10miHg769, "~/FUIteam/PydioData/env/data_outputs/subset10miHg769.csv")

#subset sites of interest within 100 miles, using vectors of indexes
subset100miHg769 <- loc_FLmerc[Index100mi769,]
head(subset100miHg769)
unique(subset100miHg769$Bottle)
unique(subset100miHg769$Year)
write.csv(subset100miHg769, "~/FUIteam/PydioData/env/data_outputs/subset100miHg769.csv")

#distances based on local projection, site 770
merc770dist <- spDistsN1(loc_FLmerc, loc_FLsite770, longlat = FALSE) #in ft
merc770dist

which(merc770dist < 26400) #within 5 miles #returns 0, 
which(merc770dist < 52800) #within 10 miles #returns 1
which(merc770dist < 79200) #within 15 miles #returns 1
which(merc770dist < 132000) #within 25 miles #returns 1
which(merc770dist < 528000) #within 100 miles #returns 0

#make vector of indexes found within given distance in last step
Index10mi770 <- which(merc770dist < 52800)
Index100mi770 <- which(merc770dist < 528000)

#subset sites of interest within 15 miles, using vectors of indexes
subset10miHg770 <- loc_FLmerc[Index10mi770,]
subset100miHg770 <- loc_FLmerc[Index100mi770,]

head(subset10miHg770)
unique(subset10miHg770$Bottle)
unique(subset10miHg770$Year)
write.csv(subset10miHg770, "~/FUIteam/PydioData/env/data_outputs/subset10miHg770.csv")

head(subset100miHg770)
unique(subset100miHg770$Bottle)
unique(subset100miHg770$Year)
write.csv(subset100miHg770, "~/FUIteam/PydioData/env/data_outputs/subset100miHg770.csv")
