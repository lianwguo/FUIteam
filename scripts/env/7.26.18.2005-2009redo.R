#focusing on 2005-2009 for LA, as 2010-2015 has no mercury sites really (2010 only)

#read in mercury sites for 2010. there is a data gap in mercury testing until 2016.
LAmercA <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_05to09merc.csv"), 
                       stringsAsFactors = FALSE)
head(LAmercA)

#add lat/long from site register to the mercury data
LAmercAsp <- merge(x = LAmercA, y = LAmercSites, by.x = "Water.Body.Site", by.y = "SITE_Num", all.x = TRUE)
head(LAmercAsp)
str(LAmercAsp)
LAmercAsp[,15:21] <- NULL
write.csv(LAmercAsp, "~/FUIteam/PydioData/env/data_outputs/LAmercAsp.csv")
unique(LAmercAsp$LONG)

#spatialize new sites for 2005-2009 LA
sp_LAmercA <- SpatialPointsDataFrame(LAmercAsp[,15:16], LAmercAsp) #long/lat are in columns 14 and 15
crs(sp_LAmercA) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
re_LAmercA <- spTransform(sp_LAmercA, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
loc_LAmercA <- spTransform(sp_LAmercA, CRS("+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))

#import wq as xls, convert to csv
LAwqA <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwq05to09.csv"), 
                    stringsAsFactors = FALSE)
head(LAwqA)

#plot - based on files spatialized in 7.26.18 file
plot(NOLA,
     col = "yellow",
     main = "2005-2009 Merc and WQ sites")
plot(US_State_Bound,
     add = TRUE)
plot(re_LAmercA,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(re_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(re_LAsite306,
     cex = 2,
     col = "red",
     add = TRUE)

