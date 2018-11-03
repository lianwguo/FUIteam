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

