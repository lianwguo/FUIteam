#work with files in local projection to calculate
#loc_LAmercA (mercury sites 2005-2009)
#loc_LAsite306
#loc_LAsite222
#loc_NOLA (msa)

#using sp package
#distances based on original projection, site 222
crs(loc_LAmercA)
merc222dist <- spDistsN1(loc_LAmercA, loc_LAsite222, longlat = FALSE) #in m
merc222dist

unique(merc222dist)
which(merc222dist < 8046.72) #within 5 miles #returns 0, 
which(merc222dist < 16093.4) #within 10 miles #returns 0
which(merc222dist < 40233.6) #within 25 miles #returns 3 sites, 44 observations
which(merc222dist < 80467.2) #within 50 miles #returns 36 sites, 493 observations

#make vector of indexes found within given distance in last step
Index25mi <- which(merc222dist < 40233.6)
Index50mi <- which(merc222dist < 80467.2) 

#subset sites of interest within 25 miles, using vectors of indexes
subset25miHg222 <- loc_LAmercA[Index25mi,]
subset25miHg222
unique(subset25miHg222$Water.Body.Site)
write.csv(subset25miHg222, "~/FUIteam/PydioData/env/data_outputs/subset25miHg222.csv")
unique(subset25miHg222$CollectYear)
#only collected from these closest sites in in 2007.
#Test dates are all early in the year (Feb to July)

#trying to get some summary stats - going to just export as csv and look at pivot tables - first change back to dataframe
CloseMerc222 <- as.data.frame(subset25miHg222)
head(CloseMerc222)
str(CloseMerc222)
class(CloseMerc222)
write.csv(CloseMerc222, "~/FUIteam/PydioData/env/data_outputs/CloseMerc222.csv")

#table of sampling frequency for each site over 2005-2009 time period
samplFreq222 <- with(CloseMerc222, table(Water.Body.Site, Collection.Date))
samplFreq222
write.csv(samplFreq222, "~/FUIteam/PydioData/env/data_outputs/samplFreq222.csv")

#species frequency for sites per year
spFreq222 <- with(CloseMerc222, table(Water.Body.Site, Common.Name, CollectYear))
spFreq222
write.csv(spFreq222, "~/FUIteam/PydioData/env/data_outputs/spFreq222.csv")


#now 50 miles
subset50miHg222 <- loc_LAmercA[Index50mi,]
subset50miHg222
unique(subset50miHg222$Water.Body.Site)


#plot - based on files with local projections
plot(loc_NOLA,
     col = "yellow",
     main = "Merc Sites within 25 miles of Site 222")
plot(loc_StateBound,
     add = TRUE)
plot(subset25miHg222,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(subset50miHg222,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)

#IT WORKED! saved images as site222_25miMerc.eps and site222_50miMerc.eps

##Now with site 306
merc306dist <- spDistsN1(loc_LAmercA, loc_LAsite306, longlat = FALSE) #in m
merc306dist
unique(merc306dist)

which(merc306dist < 8046.72) #within 5 miles #returns 0, 
which(merc306dist < 16093.4) #within 10 miles #returns 0
which(merc306dist < 40233.6) #within 25 miles #returns 9 sites, 134 observations
which(merc306dist < 80467.2) #within 50 miles #returns 44 sites, 605 observations

#make vector of indexes found within given distance in last step
Index25mi306 <- which(merc306dist < 40233.6)
Index50mi306 <- which(merc306dist < 80467.2) 

#subset sites of interest within 25 miles, using vectors of indexes
subset25miHg306 <- loc_LAmercA[Index25mi306,]
head(subset25miHg306)
unique(subset25miHg306$Water.Body.Site)
unique(subset25miHg306$Collection.Date)
unique(subset25miHg306$CollectYear)
#only collected from these closest sites in in 2006, 2007.
#Test dates are all early in the year (Feb to July)

#trying to get some summary stats - going to just export as csv and look at pivot tables - first change back to dataframe
CloseMerc306 <- as.data.frame(subset25miHg306)
head(CloseMerc306)
str(CloseMerc306)
class(CloseMerc306)
write.csv(CloseMerc306, "~/FUIteam/PydioData/env/data_outputs/CloseMerc306.csv")

#table of sampling frequency for each site over 2005-2009 time period
samplFreq306 <- with(CloseMerc306, table(Water.Body.Site, Collection.Date))
samplFreq306
write.csv(samplFreq306, "~/FUIteam/PydioData/env/data_outputs/samplFreq306.csv")

#species frequency for sites per year
spFreq306 <- with(CloseMerc306, table(Water.Body.Site, Common.Name, CollectYear))
spFreq306
write.csv(spFreq306, "~/FUIteam/PydioData/env/data_outputs/spFreq306.csv")

#now 50 miles
subset50miHg306 <- loc_LAmercA[Index50mi306,]
subset50miHg306
unique(subset50miHg306$Collection.Date)
#contains data for whole time series. 37 distinct collection dates. 

#plot - based on files with local projections
plot(loc_NOLA,
     col = "yellow",
     main = "Merc Sites within 25 miles of Site 306")
plot(loc_StateBound,
     add = TRUE)
plot(subset25miHg306,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAsite306,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(subset50miHg306,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)

#making maps with rings
##attempt 1 - DIDNT WORK
library(maps)
library(mapdata)#For the worldHires database
library(mapproj)#For the mapproject function

plotCircle <- function(LonDec, LatDec, Km) {#Corrected function
  #LatDec = latitude in decimal degrees of the center of the circle
  #LonDec = longitude in decimal degrees
  #Km = radius of the circle in kilometers
  ER <- 6371000 #Mean Earth radius in meters. Change this to 3959 and you will have your function working in miles.
  AngDeg <- seq(1:360) #angles in degrees 
  Lat1Rad <- LatDec*(pi/180)#Latitude of the center of the circle in radians
  Lon1Rad <- LonDec*(pi/180)#Longitude of the center of the circle in radians
  AngRad <- AngDeg*(pi/180)#angles in radians
  Lat2Rad <-asin(sin(Lat1Rad)*cos(Km/ER)+cos(Lat1Rad)*sin(Km/ER)*cos(AngRad)) #Latitude of each point of the circle rearding to angle in radians
  Lon2Rad <- Lon1Rad+atan2(sin(AngRad)*sin(Km/ER)*cos(Lat1Rad),cos(Km/ER)-sin(Lat1Rad)*sin(Lat2Rad))#Longitude of each point of the circle rearding to angle in radians
  Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  polygon(Lon2Deg,Lat2Deg,lty=2)
}

circle <- plotCircle(-90.17831667,29.15833333,25)#Plot a dashed circle of 25 m arround site 222

plot(loc_NOLA,
         col = "yellow",
         main = "Merc Sites within 25 miles of Site 306")
plot(loc_StateBound,
     add = TRUE)
plot(subset25miHg306,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(circle,
     add = TRUE)

##attempt 2
library(ggplot2)
library(ggmap)
data = data.frame(
  ID = as.numeric(c(1:8)),
  longitude = as.numeric(c(-63.27462, -63.26499, -63.25658, -63.2519, -63.2311, -63.2175, -63.23623, -63.25958)),
  latitude = as.numeric(c(17.6328, 17.64614, 17.64755, 17.64632, 17.64888, 17.63113, 17.61252, 17.62463))
)

#################################################################################
# create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$SiteLat)
  # length per longitude changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$SiteNum, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$SiteLong, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$SiteLat, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

# here is the data frame for all circles
Circles222 <- make_circles(LAsite222, 40.2336)
##################################################################################
head(Circles222)

Circles306 <- make_circles(LAsite306, 40.2336)
head(Circles306)

###mapping with ggplot
# map the points
ggplot() + 
  geom_polygon(data=NOLA, aes(x=long, y=lat), fill="white") +  
  geom_polygon(data = LA, aes(x=long, y=lat, group = group), colour="black", fill=NA) + ##changing group from ID was important to making this plot work
  geom_polygon(data = Circles222, aes(lon, lat, group = ID), color = "red", alpha = 0) +
  geom_point(data=REdfsite222, aes(x=SiteLong, y=SiteLat), color="blue", alpha=1, size = 4, pch=4) +
  geom_point(data=CloseMerc222, aes(x=LONG, y=LAT), color="purple", alpha=1, size = 2, pch=3) +
  geom_polygon(data = Circles306, aes(lon, lat, group = ID), color = "red", alpha = 0) +
  geom_point(data=REdfsite306, aes(x=SiteLong, y=SiteLat), color="orange", alpha=1, size = 4, pch=4) +
  geom_point(data=CloseMerc306, aes(x=LONG, y=LAT), color="purple", alpha=1, size = 2, pch=3) +
  coord_equal(ratio=1) + # square plot to avoid the distortion 
  coord_map(xlim = c(-91.5, -88.5),ylim = c(28.7, 30.8)) +
  labs(x="Longitude", y="Latitude")#labels
  
msaExtent <- extent(NOLA)
msaExtent


REdfsite222 <- data.frame(re_LAsite222)
REdfsite306 <- data.frame(re_LAsite306)
identical(proj4string(loc_NOLA),proj4string(loc_LAsite222))
LA <- subset(US_State_Bound, US_State_Bound$STUSPS == 'LA')
LA
crs(LA)
plot(LA)
