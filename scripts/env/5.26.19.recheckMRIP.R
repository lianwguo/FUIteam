sourceLand <- read.csv(file.path("~/FUIteam/PydioData/MRIP/data_outputs/clean_MRIP", 
                                 "mrip_species_zip_site_2004_2017_012019.csv"), 
                    stringsAsFactors = FALSE)
str(sourceLand)
unique(sourceLand$YEAR) #2004-2017
unique(sourceLand$MODE_FX) # mode of fishing 1-3
unique(sourceLand$MODE_F) # mode of fishing (old?) all shore, and some NAs
unique(sourceLand$ST) # 12 44 37  9 10 24 51 25 36 23 33 34  1 28 22 45 13 72
#note, more than just LA and FL are currently included. FIPS code for LA is 22, FL is 12

sourceLandB <- subset(sourceLand, sourceLand$ST == "12" | sourceLand$ST == "22")
unique(sourceLandB$ST)

#subset file for columns of interest
sourceLandC <- sourceLandB[,c(1,2,3,4,7,8,10,17,18,25,61,64)]
str(sourceLandC)

#calculate landings from claim + harvest
sourceLandC$landing <- sourceLandC$CLAIM + sourceLandC$HARVEST

#create vector to subset years of interest
years <- cbind(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
str(years)
years <- as.character(years)
sourceLandC$YEAR <- as.character(sourceLandC$YEAR)

#subset years of interest
sourceLandD <- sourceLandC[sourceLandC$YEAR %in% years,]
str(sourceLandD)
unique(sourceLandD$YEAR)
sourceLandD$ST <- as.character(sourceLandD$ST)

#break MRIP file into separate states
#LA is 22, FL is 12
laMRIP <- subset(sourceLandD, sourceLandD$ST == "22")
str(laMRIP)
unique(laMRIP$ST) #worked
unique(laMRIP$YEAR)

#subset for metro zips
LAzip <- read.csv(file.path("~/FUIteam/PydioData/env/raw", 
                                 "LAzip.csv"), 
                       stringsAsFactors = FALSE)
LAzip$ZCTA5 <- as.character(LAzip$ZCTA5)
LAzips <- LAzip[,1]
LAzips <- as.character(LAzips)
str(LAzips)

unique(laMRIP$ZIP)
laMRIP$ZIP <- as.character(laMRIP$ZIP)
str(laMRIP)
str(laMRIPb)
laMRIPb <- laMRIP[laMRIP$ZIP %in% LAzips,] #should only include fishers from metro zips

#subset for fl
flMRIP <- subset(sourceLandD, sourceLandD$ST == "12")
str(flMRIP)
unique(flMRIP$ST) #worked

FLzip <- read.csv(file.path("~/FUIteam/PydioData/env/raw", 
                            "FLzip.csv"), 
                  stringsAsFactors = FALSE)
FLzip$ZCTA5 <- as.character(FLzip$ZCTA5)
FLzips <- FLzip[,1]
FLzips <- as.character(FLzips)
str(FLzips)

unique(flMRIP$ZIP)
flMRIP$ZIP <- as.character(flMRIP$ZIP)
str(flMRIP)
str(flMRIPb)
flMRIPb <- flMRIP[flMRIP$ZIP %in% FLzips,]

#set up time frames based on available years of data for each site

LAtimA <- subset(laMRIPb, laMRIPb$YEAR == "2005" | laMRIPb$YEAR == "2006" |
                   laMRIPb$YEAR == "2007" | laMRIPb$YEAR == "2008" |
                   laMRIPb$YEAR == "2009")
LAtimB <- subset(laMRIPb, laMRIPb$YEAR == "2010" | laMRIPb$YEAR == "2011" |
                   laMRIPb$YEAR == "2012" | laMRIPb$YEAR == "2013")

str(LAtimA)
tail(LAtimA, n=20)
unique(LAtimA$YEAR)
str(LAtimB)
unique(LAtimB$YEAR)

FLtimA <- subset(flMRIPb, flMRIPb$YEAR == "2005" | flMRIPb$YEAR == "2006" |
                   flMRIPb$YEAR == "2007" | flMRIPb$YEAR == "2008" |
                   flMRIPb$YEAR == "2009" | flMRIPb$YEAR == "2010")
FLtimB <- subset(flMRIPb, flMRIPb$YEAR == "2011" | flMRIPb$YEAR == "2012" | 
                   flMRIPb$YEAR == "2013" | flMRIPb$YEAR == "2014" | flMRIPb$YEAR == "2015")

str(FLtimA)
unique(FLtimA$YEAR)
str(FLtimB)
unique(FLtimB$YEAR)

#ID top landing sites

library(dplyr)
laTopLandA <- LAtimA %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(laTopLandA)
print(laTopLandA, n=16) #top sites = 222, 231, 155, 159, 151
laTopLandAordered <- laTopLandA %>% arrange(desc(landings))

laTopLandB <- LAtimB %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(laTopLandB)
print(laTopLandB, n=16) #top sites = 222, 150, 151, 270, 155
laTopLandBordered <- laTopLandB %>% arrange(desc(landings))

library(dplyr)
flTopLandA <- FLtimA %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(flTopLandA)
print(flTopLandA, n=60) #top sites = 632, 770, 742, 769, 615
flTopLandAordered <- flTopLandA %>% arrange(desc(landings))

flTopLandB <- FLtimB %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(flTopLandB)
print(flTopLandB, n=64) #top sites = 632, 3801, 769, 327, 3802
flTopLandBordered <- flTopLandB %>% arrange(desc(landings))

#subset the mrip files for these five top sites
timeAla <- cbind(222, 231, 155, 159, 151)
timeBla <- cbind(222, 150, 151, 270, 155)
timeAfl <- cbind(632, 770, 742, 769, 615)
timeBfl <- cbind(632, 3801, 769, 327, 3802)
topLAtimA <- LAtimA[LAtimA$INTSITE %in% timeAla,]
topLAtimB <- LAtimB[LAtimB$INTSITE %in% timeBla,]
topFLtimA <- FLtimA[FLtimA$INTSITE %in% timeAfl,]
topFLtimB <- FLtimB[FLtimB$INTSITE %in% timeBfl,]

#add species to the files
spList <- read.csv(file.path("~/FUIteam/PydioData/env/raw", "species_list.csv"), 
                  stringsAsFactors = FALSE)
lhKey <- read.csv(file.path("~/FUIteam/PydioData/env/raw", "lhKey.csv"), 
                   stringsAsFactors = FALSE)

spListB <- spList[,1:3]
lhKeyB <- lhKey[,2:11] #wait on life history stuff until later...
str(spListB)
str(lhKeyB)

topLAtimAsp <- left_join(topLAtimA, spListB, by = c("SP_CODE"="sp_code"))
str(topLAtimAsp)
tail(topLAtimAsp)
topLAtimBsp <- left_join(topLAtimB, spListB, by = c("SP_CODE"="sp_code"))
topFLtimAsp <- left_join(topFLtimA, spListB, by = c("SP_CODE"="sp_code"))
topFLtimBsp <- left_join(topFLtimB, spListB, by = c("SP_CODE"="sp_code"))


