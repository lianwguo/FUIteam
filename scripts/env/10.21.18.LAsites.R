#finish remaining LA sites for time point A, then do time point B

LAmripA #2005-2009
#case study sites: 222, 306, 155, 151, 150

LAmripB #2010-2013
#case study sites: 222, 159, 306, 151, 150

#readin dataframes for remaining case study sites for both time frames (lots of overlap)
LAsite155 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAsite155.csv"), 
                      stringsAsFactors = FALSE)
LAsite150 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAsite150.csv"), 
                      stringsAsFactors = FALSE)
LAsite151 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAsite151.csv"), 
                      stringsAsFactors = FALSE)
LAsite159 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAsite159.csv"), 
                      stringsAsFactors = FALSE)
LAsite155
LAsite150
LAsite151
LAsite159

#convert to spatial data frame
sp_LAsite155 <- SpatialPointsDataFrame(LAsite155[,2:3], LAsite155) #long/lat are in columns 2 and 3
sp_LAsite150 <- SpatialPointsDataFrame(LAsite150[,2:3], LAsite150)
sp_LAsite151 <- SpatialPointsDataFrame(LAsite151[,2:3], LAsite151)
sp_LAsite159 <- SpatialPointsDataFrame(LAsite159[,2:3], LAsite159)

#assign crs from orig data set
crs(sp_LAsite155) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAsite150) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAsite151) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(sp_LAsite159) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#transform to local projection
loc_LAsite155 <- spTransform(sp_LAsite155, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAsite150 <- spTransform(sp_LAsite150, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAsite151 <- spTransform(sp_LAsite151, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
loc_LAsite159 <- spTransform(sp_LAsite159, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))

#plot all points
plot(loc_NOLA,
     col = "yellow",
     main = "top landing sites")
plot(loc_StateBound,
     add = TRUE)
plot(loc_LAsite155,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAsite150,
     pch = 5,
     cex = .2,
     col = 'blue',
     add = TRUE)
plot(loc_LAsite151,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(loc_LAsite159,
     cex = 2,
     col = "red",
     add = TRUE)
plot(loc_LAsite306,
     cex = 2,
     col = "red",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "red",
     add = TRUE)

#start looking at density of sites for time A (2005-2009)
#using sp package
#distances based on local projection, site 155
merc155dist <- spDistsN1(loc_LAmercA, loc_LAsite155, longlat = FALSE) #in m
merc155dist

unique(merc155dist)
which(merc155dist < 8046.72) #within 5 miles #returns 0, 
which(merc155dist < 16093.4) #within 10 miles #returns 0
which(merc155dist < 24140.2) #within 15 miles #returns 3 sites, 50 observations, 2 years
which(merc155dist < 40233.6) #within 25 miles #returns 10 sites, 145 observations, 3 years


#make vector of indexes found within given distance in last step
Index15mi155 <- which(merc155dist < 8046.72)
Index25mi155 <- which(merc155dist < 40233.6)

#subset sites of interest within 15 miles, using vectors of indexes
subset15miHg155 <- loc_LAmercA[Index15mi155,]
head(subset15miHg155)
unique(subset15miHg155$Water.Body.Site)
unique(subset15miHg155$CollectYear)
write.csv(subset15miHg155, "~/FUIteam/PydioData/env/data_outputs/subset15miHg155.csv")

#subset sites of interest within 25 miles, using vectors of indexes
subset25miHg155 <- loc_LAmercA[Index25mi155,]
subset25miHg155
unique(subset25miHg155$Water.Body.Site)
unique(subset25miHg155$CollectYear)
write.csv(subset25miHg155, "~/FUIteam/PydioData/env/data_outputs/subset25miHg155.csv")

#plot all points
plot(loc_NOLA,
     col = "yellow",
     main = "top landing sites")
plot(loc_StateBound,
     add = TRUE)
plot(loc_LAsite155,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(subset15miHg155,
     pch = 5,
     cex = .2,
     col = 'blue',
     add = TRUE)
plot(subset25miHg155,
     pch = 5,
     cex = .2,
     col = 'red',
     add = TRUE)

#distances based on local projection, site 150
merc150dist <- spDistsN1(loc_LAmercA, loc_LAsite150, longlat = FALSE) #in m

which(merc150dist < 8046.72) #within 5 miles #returns 0, 
which(merc150dist < 16093.4) #within 10 miles #returns 0
which(merc150dist < 24140.2) #within 15 miles #returns 0
which(merc150dist < 40233.6) #within 25 miles #returns 2 sites, 28 observations, 1 year

Index25mi150 <- which(merc150dist < 40233.6)

subset25miHg150 <- loc_LAmercA[Index25mi150,]
unique(subset25miHg150$Water.Body.Site)
unique(subset25miHg150$CollectYear)
write.csv(subset25miHg150, "~/FUIteam/PydioData/env/data_outputs/subset25miHg150.csv")

#plot all points
plot(loc_NOLA,
     col = "yellow",
     main = "top landing sites")
plot(loc_StateBound,
     add = TRUE)
plot(loc_LAsite150,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(subset25miHg150,
     pch = 5,
     cex = .2,
     col = 'red',
     add = TRUE)

#distances based on local projection, site 151
merc151dist <- spDistsN1(loc_LAmercA, loc_LAsite151, longlat = FALSE) #in m

which(merc151dist < 8046.72) #within 5 miles #returns 0, 
which(merc151dist < 16093.4) #within 10 miles #returns 0
which(merc151dist < 24140.2) #within 15 miles #returns 0
which(merc151dist < 40233.6) #within 25 miles #returns 2 sites, 28 observations, 1 year

Index25mi151 <- which(merc151dist < 40233.6)

subset25miHg151 <- loc_LAmercA[Index25mi151,]
unique(subset25miHg151$Water.Body.Site)
unique(subset25miHg151$CollectYear)
write.csv(subset25miHg151, "~/FUIteam/PydioData/env/data_outputs/subset25miHg151.csv")

#plot all points
plot(loc_NOLA,
     col = "yellow",
     main = "top landing sites")
plot(loc_StateBound,
     add = TRUE)
plot(loc_LAsite151,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(subset25miHg151,
     pch = 5,
     cex = .2,
     col = 'red',
     add = TRUE)

#distances based on local projection, site 159
merc159dist <- spDistsN1(loc_LAmercB, loc_LAsite159, longlat = FALSE) #in m

which(merc159dist < 8046.72) #within 5 miles #returns 0, 
which(merc159dist < 16093.4) #within 10 miles #returns 0
which(merc159dist < 24140.2) #within 15 miles #returns 0
which(merc159dist < 40233.6) #within 25 miles #returns 3 sites, 44 observations, 1 year

Index25mi159 <- which(merc159dist < 40233.6)

subset25miHg159 <- loc_LAmercA[Index25mi159,]
unique(subset25miHg159$Water.Body.Site)
unique(subset25miHg159$CollectYear)
write.csv(subset25miHg159, "~/FUIteam/PydioData/env/data_outputs/subset25miHg159.csv")

#plot all points
plot(loc_NOLA,
     col = "yellow",
     main = "top landing sites")
plot(loc_StateBound,
     add = TRUE)
plot(loc_LAsite159,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(subset25miHg159,
     pch = 5,
     cex = .2,
     col = 'red',
     add = TRUE)

#######################################
#start looking at density of sites for time A (2010-2013)
LAmripB #2010-2013
#case study sites: 222, 159, 306, 151, 150
#using sp package
#distances based on local projection, site 155
merc222distB <- spDistsN1(loc_LAmerc2010, loc_LAsite222, longlat = FALSE) #in m
merc222distB

which(merc222distB < 8046.72) #within 5 miles #returns 0, 
which(merc222distB < 16093.4) #within 10 miles #returns 0
which(merc222distB < 24140.2) #within 15 miles #returns 0
which(merc222distB < 40233.6) #within 25 miles #returns 0
which(merc222distB < 80467.2) #within 50 miles #returns 0
which(merc222distB < 161000) #within 100 miles #returns 0
which(merc222distB < 321869) #within 200 miles #returns 0, two sites, 25 observations

#make vector of indexes found within given distance in last step
Index200mi222B <- which(merc222distB < 321869)

#subset sites of interest within 15 miles, using vectors of indexes
subset200miHg222B <- loc_LAmerc2010[Index200mi222B,]
subset200miHg222B
unique(subset200miHg222B$Water.Body.Site)
write.csv(subset200miHg222B, "~/FUIteam/PydioData/env/data_outputs/subset200miHg222B.csv")

###site 306
merc306distB <- spDistsN1(loc_LAmerc2010, loc_LAsite306, longlat = FALSE) #in m
merc306distB

which(merc306distB < 8046.72) #within 5 miles #returns 0, 
which(merc306distB < 16093.4) #within 10 miles #returns 0
which(merc306distB < 24140.2) #within 15 miles #returns 0
which(merc306distB < 40233.6) #within 25 miles #returns 0
which(merc306distB < 80467.2) #within 50 miles #returns 0
which(merc306distB < 160934) #within 100 miles #returns 12 observations, 1 site
which(merc306distB < 321869) #within 200 miles #returns 52 observations, 4 sites

#make vector of indexes found within given distance in last step
Index100mi306B <- which(merc306distB < 160934)
Index200mi306B <- which(merc306distB < 321869)

#subset sites of interest within 100 miles, using vectors of indexes
subset100miHg306B <- loc_LAmerc2010[Index100mi306B,]
subset100miHg306B
unique(subset100miHg306B$Water.Body.Site)
write.csv(subset100miHg306B, "~/FUIteam/PydioData/env/data_outputs/subset100miHg306B.csv")

#subset sites of interest within 200 miles, using vectors of indexes
subset200miHg306B <- loc_LAmerc2010[Index200mi306B,]
subset200miHg306B
unique(subset200miHg306B$Water.Body.Site)
write.csv(subset200miHg306B, "~/FUIteam/PydioData/env/data_outputs/subset200miHg306B.csv")

###site 159
merc159distB <- spDistsN1(loc_LAmerc2010, loc_LAsite159, longlat = FALSE) #in m
merc159distB

which(merc159distB < 8046.72) #within 5 miles #returns 0, 
which(merc159distB < 16093.4) #within 10 miles #returns 0
which(merc159distB < 24140.2) #within 15 miles #returns 0
which(merc159distB < 40233.6) #within 25 miles #returns 0
which(merc159distB < 80467.2) #within 50 miles #returns 0
which(merc159distB < 161000) #within 100 miles #returns 0
which(merc159distB < 321869) #within 200 miles #returns two sites, 25 observations

#make vector of indexes found within given distance in last step
Index200mi159B <- which(merc159distB < 321869)

#subset sites of interest within 15 miles, using vectors of indexes
subset200miHg159B <- loc_LAmerc2010[Index200mi159B,]
subset200miHg159B
unique(subset200miHg159B$Water.Body.Site)
write.csv(subset200miHg159B, "~/FUIteam/PydioData/env/data_outputs/subset200miHg159B.csv")


###site 151
merc151distB <- spDistsN1(loc_LAmerc2010, loc_LAsite151, longlat = FALSE) #in m
merc151distB

which(merc151distB < 8046.72) #within 5 miles #returns 0, 
which(merc151distB < 16093.4) #within 10 miles #returns 0
which(merc151distB < 24140.2) #within 15 miles #returns 0
which(merc151distB < 40233.6) #within 25 miles #returns 0
which(merc151distB < 80467.2) #within 50 miles #returns 0
which(merc151distB < 161000) #within 100 miles #returns 0
which(merc151distB < 321869) #within 200 miles #returns two sites, 25 observations

#make vector of indexes found within given distance in last step
Index200mi151B <- which(merc151distB < 321869)

#subset sites of interest within 200 miles, using vectors of indexes
subset200miHg151B <- loc_LAmerc2010[Index200mi151B,]
subset200miHg151B
unique(subset200miHg151B$Water.Body.Site)
write.csv(subset200miHg151B, "~/FUIteam/PydioData/env/data_outputs/subset200miHg151B.csv")

###site 150
merc150distB <- spDistsN1(loc_LAmerc2010, loc_LAsite150, longlat = FALSE) #in m
merc150distB

which(merc150distB < 8046.72) #within 5 miles #returns 0, 
which(merc150distB < 16093.4) #within 10 miles #returns 0
which(merc150distB < 24140.2) #within 15 miles #returns 0
which(merc150distB < 40233.6) #within 25 miles #returns 0
which(merc150distB < 80467.2) #within 50 miles #returns 0
which(merc150distB < 161000) #within 100 miles #returns 0
which(merc150distB < 321869) #within 200 miles #returns two sites, 25 observations

#make vector of indexes found within given distance in last step
Index200mi150B <- which(merc150distB < 321869)

#subset sites of interest within 200 miles, using vectors of indexes
subset200miHg150B <- loc_LAmerc2010[Index200mi150B,]
subset200miHg150B
unique(subset200miHg150B$Water.Body.Site)
write.csv(subset200miHg150B, "~/FUIteam/PydioData/env/data_outputs/subset200miHg150B.csv")

