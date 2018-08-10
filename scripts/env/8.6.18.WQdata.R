###setting up wq data, hopefully in a reasonable, manageable manner

LAwq05 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwq05.csv"), 
                     stringsAsFactors = FALSE)
LAwq06 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwq06.csv"), 
                     stringsAsFactors = FALSE)
LAwq07 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwq07.csv"), 
                     stringsAsFactors = FALSE)
LAwq08 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwq08.csv"), 
                     stringsAsFactors = FALSE)
LAwq09 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwq09.csv"), 
                     stringsAsFactors = FALSE)

#More cleanup as I left NA in the year data. LAwqA$CollectYear[is.na(LAwqA$CollectYear)] <- "2007"

unique(LAwq07$CollectYear)
#combine into one single dataframe
LAwqA <- rbind(LAwq05,LAwq06,LAwq07,LAwq08,LAwq09)
#clean up since it didn't work the first time
str(LAwq09)
LAwq06[,6] <- NULL #forgot to remove MDL column. prevented rbind from happening
str(LAwqA)
tail(LAwqA) #looks like it worked. woot!

#add site lat long to each entry
LAwqAsp <- merge(x = LAwqA, y = LAwqSites, by.x = "SiteNum", by.y = "SITE_ID", all.x = TRUE)
str(LAwqAsp)
#LAwqAsp[,8:9] <- NULL #removing extra stuff

#crs assignment
sp_LAwqA <- SpatialPointsDataFrame(LAwqAsp[,8:9], LAwqAsp) #long/lat are in columns 8 and 9
crs(sp_LAwqA) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
re_LAwqA <- spTransform(sp_LAwqA, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
loc_LAwqA <- spTransform(sp_LAwqA, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
crs(loc_LAwqA)
plot(loc_LAwqA)

plot(loc_NOLA,
     col = "yellow",
     main = "WQ Sites close to NOLA")
plot(loc_StateBound,
     add = TRUE)
plot(loc_LAwqA,
     pch = 4,
     cex = .5,
     col = "green",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(loc_LAsite306,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)

#find closest sites to case study sites
wq306dist <- spDistsN1(loc_LAwqA, loc_LAsite306, longlat = FALSE) #in m
wq306dist
unique(wq306dist)

#vector of distances within 5 miles
WQ5mi306 <- which(wq306dist < 8046.72) #within 5 miles #returns MANY 

#subset sites of interest within 5 miles, using vectors of indexes
subset5miWQ306 <- loc_LAwqA[WQ5mi306,]
head(subset5miWQ306)
unique(subset5miWQ306$SiteNum)
unique(subset5miWQ306$CollectDate)
unique(subset5miWQ306$CollectYear)
#only collected from sites within 5 mi in in 2007
#3 sites total: 305, 306, 1051

#vector of distances within 10 miles
WQ10mi306 <- which(wq306dist < 16093.4) #returns MANY 

#subset sites of interest within 10 miles, using vectors of indexes
subset10miWQ306 <- loc_LAwqA[WQ10mi306,]
head(subset10miWQ306)
unique(subset10miWQ306$SiteNum) #seven sites: 305, 306, 307, 1050, 1051, 1063, 1064
unique(subset10miWQ306$CollectDate) #dates from 2006-2009
unique(subset10miWQ306$CollectYear)

#make dataframe of the selected 
CloseWQ306 <- as.data.frame(subset10miWQ306)
head(CloseWQ306)
str(CloseWQ306)
class(CloseWQ306)
write.csv(CloseWQ306, "~/FUIteam/PydioData/env/data_outputs/CloseWQ306.csv")

plot(loc_NOLA,
     col = "yellow",
     main = "WQ Sites within 10 mi to 306")
plot(loc_StateBound,
     add = TRUE)
plot(subset10miWQ306,
     pch = 4,
     cex = .5,
     col = "green",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(loc_LAsite306,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)

#find closest sites to case study sites - now 222
wq222dist <- spDistsN1(loc_LAwqA, loc_LAsite222, longlat = FALSE) #in m
wq222dist
unique(wq222dist)

#vector of distances within 5 miles
WQ5mi222 <- which(wq222dist < 8046.72) #within 5 miles #returns MANY 

#subset sites of interest within 5 miles, using vectors of indexes
subset5miWQ222 <- loc_LAwqA[WQ5mi222,]
head(subset5miWQ222)
unique(subset5miWQ222$SiteNum)
unique(subset5miWQ222$CollectDate)
unique(subset5miWQ222$CollectYear)
#only collected from sites within 5 mi in in 2007
#2 sites total: 923, 924

#vector of distances within 10 miles
WQ10mi222 <- which(wq222dist < 16093.4) #returns MANY 

#subset sites of interest within 10 miles, using vectors of indexes
subset10miWQ222 <- loc_LAwqA[WQ10mi222,]
head(subset10miWQ222)
unique(subset10miWQ222$SiteNum) #seven sites: 923, 924, 925, 926, 927, 953, 959
unique(subset10miWQ222$CollectDate) #dates from 2007-2009
unique(subset10miWQ222$CollectYear) 

#make dataframe of the selected 
CloseWQ222 <- as.data.frame(subset10miWQ222)
head(CloseWQ222)
str(CloseWQ222)
class(CloseWQ222)
write.csv(CloseWQ222, "~/FUIteam/PydioData/env/data_outputs/CloseWQ222.csv")

plot(loc_NOLA,
     col = "yellow",
     main = "WQ Sites within 10 mi to 222")
plot(loc_StateBound,
     add = TRUE)
plot(subset10miWQ222,
     pch = 4,
     cex = .5,
     col = "green",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(loc_LAsite306,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)

#explore the data? 
#salinity results
#parameters of interest?
unique(CloseWQ222$Parameter) #"TURBIDITY", "TOTAL SUSPENDED SOLIDS", "DISSOLVED OXYGEN", "DISSOLVED OXYGEN PERCENT SATURATION", 
# "TEMPERATURE", "PHOSPHORUS (AS P)", "PH", "AMMONIA NITROGEN", "NITRATE+NITRITE NITROGEN", "NITROGEN, KJELDAHL", "FECAL COLIFORM"             
#
Salt222 <- subset(CloseWQ222, CloseWQ222$Parameter == 'SALINITY')
str(Salt222)
summary(Salt222$Result)
