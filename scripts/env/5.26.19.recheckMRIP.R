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

unique(topLAtimAsp[c("COMMON_NAME","INTSITE")])
unique(topLAtimAsp[c("COMMON_NAME","INTSITE")]) %>% arrange(desc(INTSITE))

unique(topFLtimAsp[c("COMMON_NAME","INTSITE")])
unique(topFLtimAsp[c("COMMON_NAME","INTSITE")]) %>% arrange(desc(INTSITE))


### no subset for urban zips, to do a comparison

#set up time frames based on available years of data for each site

nuLAtimA <- subset(laMRIP, laMRIP$YEAR == "2005" | laMRIP$YEAR == "2006" |
                   laMRIP$YEAR == "2007" | laMRIP$YEAR == "2008" |
                   laMRIP$YEAR == "2009")
nuLAtimB <- subset(laMRIP, laMRIP$YEAR == "2010" | laMRIP$YEAR == "2011" |
                   laMRIP$YEAR == "2012" | laMRIP$YEAR == "2013")

str(nuLAtimA)
tail(nuLAtimA, n=20)
unique(nuLAtimA$YEAR)
str(nuLAtimB)
unique(nuLAtimB$YEAR)

nuFLtimA <- subset(flMRIP, flMRIP$YEAR == "2005" | flMRIP$YEAR == "2006" |
                   flMRIP$YEAR == "2007" | flMRIP$YEAR == "2008" |
                   flMRIP$YEAR == "2009" | flMRIP$YEAR == "2010")
nuFLtimB <- subset(flMRIP, flMRIP$YEAR == "2011" | flMRIP$YEAR == "2012" | 
                   flMRIP$YEAR == "2013" | flMRIP$YEAR == "2014" | flMRIP$YEAR == "2015")

str(nuFLtimA)
unique(nuFLtimA$YEAR)
str(nuFLtimB)
unique(nuFLtimB$YEAR)

#looking for top sites
library(dplyr)
nuLAtopA <- nuLAtimA %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(nuLAtopA)
print(nuLAtopA, n=16) 
#top sites NONURBAN = 222, 231, 150, 151, 169 Versus URBAN = 222, 231, 155, 159, 151
nuLAtopAordered <- nuLAtopA %>% arrange(desc(landings))

nuLAtopB <- nuLAtimB %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(nuLAtopB)
print(nuLAtopB, n=16) 
#top sites NONURBAN = 222, 151, 150, 1747, 159 Versus URBAN = 222, 150, 151, 270, 155
nuLAtopBordered <- nuLAtopB %>% arrange(desc(landings))

library(dplyr)
nuFLtopA <- nuFLtimA %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(nuFLtopA)
print(nuFLtopA, n=60) 
#top sites NON URBAN = 632, 770, 769, 667, 742 versus urban = 632, 770, 742, 769, 615
nuFLtopAordered <- nuFLtopA %>% arrange(desc(landings))

nuFLtopB <- nuFLtimB %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(nuFLtopB)
print(nuFLtopB, n=64) #top sites = 632, 3801, 769, 327, 3802
nuFLtopBordered <- nuFLtopB %>% arrange(desc(landings))


#################################################################################################
#checking another data frame to try to find where our inconsistencies are
trip <- read.csv(file.path("~/FUIteam/PydioData/MRIP/data_outputs/clean_MRIP", 
                                 "mripTrip_2004_2017.csv"), stringsAsFactors = FALSE)
catch <- read.csv(file.path("~/FUIteam/PydioData/MRIP/data_outputs/clean_MRIP", 
                           "mripCatch_2004_2017.csv"), stringsAsFactors = FALSE)

str(trip) #333430 obs. of  80 variables:
str(catch) #462536 obs. of  10 variables:

unique(trip$YEAR) #2004-2015
unique(trip$MODE_FX) # mode of fishing 5,7,3
unique(trip$MODE_F) # 1,-5, 7-8, NA
unique(trip$ST) #12 and 22. this is different from Meghna's

trip$MODE_FX <- as.character(trip$MODE_FX)
tripB <- subset(trip, trip$MODE_FX == "3")
str(tripB)
unique(tripB$MODE_FX) # mode of fishing 3, shore
unique(tripB$MODE_F) # now only has modes 1-5 :)

tripC <- tripB[,c(3,4,5,6,7,8,9,10,11,25,52)]
str(tripC)
unique(tripC$ST)
tripC$ID_CODE <- as.character(tripC$ID_CODE)
tripC$YEAR <- as.character(tripC$YEAR)
tripC$ST <- as.character(tripC$ST)
tripC$MODE_FX <- as.character(tripC$MODE_FX)

tripD <- subset(tripC, !(tripC$YEAR == "2004" | tripC$YEAR == "2016" | tripC$YEAR == "2017"))
str(tripD)

unique(catch$YEAR) #2004-2017
unique(catch$MODE_FX) # mode of fishing 1-3
unique(catch$MODE_F) # 1-3
unique(catch$ST) #many states

catch$ST <- as.character(catch$ST)
catch$YEAR <- as.character(catch$YEAR)

catchB <- subset(catch, catch$ST == "12" | catch$ST == "22")
unique(catchB$ST)

catchC <- subset(catchB, !(catchB$YEAR == "2004" | catchB$YEAR == "2016" | catchB$YEAR == "2017"))
unique(catchC$YEAR)

str(catchC)
catchC$ID_CODE <- as.character(catchC$ID_CODE)
catchC$MODE_FX <- as.character(catchC$MODE_FX)

#combine catch and trip files
newMRIP <- left_join(tripD,catchC,by = c("ID_CODE","YEAR","ST","MODE_FX"))
str(newMRIP)

#split new files into separate LA and FL files
orleans <- subset(newMRIP, newMRIP$ST == "22")
tampa <- subset(newMRIP, newMRIP$ST == "12")

str(orleans)
unique(orleans$ST)
str(tampa)
unique(tampa$ST)

#subset for urban zips
unique(orleans$ZIP)
orleans$ZIP <- as.character(orleans$ZIP)
str(orleans)
orleansB <- orleans[orleans$ZIP %in% LAzips,]
unique(orleansB$ZIP)
str(orleansB)
tail(orleansB)

unique(tampa$ZIP)
tampa$ZIP <- as.character(tampa$ZIP)
str(tampa)
tampaB <- tampa[tampa$ZIP %in% FLzips,]
unique(tampaB$ZIP)
str(tampaB)
head(tampaB)

#break into time frames
orleanstimA <- subset(orleansB, orleansB$YEAR == "2005" | orleansB$YEAR == "2006" |
                     orleansB$YEAR == "2007" | orleansB$YEAR == "2008" |
                     orleansB$YEAR == "2009")
orleanstimB <- subset(orleansB, orleansB$YEAR == "2010" | orleansB$YEAR == "2011" |
                     orleansB$YEAR == "2012" | orleansB$YEAR == "2013")

unique(orleanstimA$YEAR)
unique(orleanstimB$YEAR)

tampatimA <- subset(tampaB, tampaB$YEAR == "2005" | tampaB$YEAR == "2006" |
                        tampaB$YEAR == "2007" | tampaB$YEAR == "2008" |
                        tampaB$YEAR == "2009" | tampaB$YEAR == "2010")
tampatimB <- subset(tampaB, tampaB$YEAR == "2011" | tampaB$YEAR == "2012" | 
                      tampaB$YEAR == "2013"| tampaB$YEAR == "2014" | tampaB$YEAR == "2015")

unique(tampatimA$YEAR)
unique(tampatimB$YEAR)

#finding top sites for each time frame

orleansTopA <- orleanstimA %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(orleansTopA)
orleansTopAord <- orleansTopA %>% arrange(desc(landings))
#top sites = 222, 231, 306, 155, 151
oTopA <- cbind(222, 231, 306, 155, 151)
oTopA <- as.character(oTopA)
str(oTopA)

subOtimA <- orleanstimA[orleanstimA$INTSITE %in% oTopA,]
unique(subOtimA$INTSITE)
landOtimA <- subset(subOtimA, subOtimA$landing > 0)

orleansTopB <- orleanstimB %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(orleansTopB)
orleansTopBord <- orleansTopB %>% arrange(desc(landings))
#top sites = 222, 159, 306, 3325, 151
oTopB <- cbind(222, 159, 306, 3325, 151)
oTopB <- as.character(oTopB)
str(oTopB)

subOtimB <- orleanstimB[orleanstimB$INTSITE %in% oTopB,]
unique(subOtimB$INTSITE)
landOtimB <- subset(subOtimB, subOtimB$landing > 0)

tampaTopA <- tampatimA %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(tampaTopA)
tampaTopAord <- tampaTopA %>% arrange(desc(landings))
#top sites = 770, 769, 632, 615, 742
tTopA <- cbind(770, 769, 632, 615, 742)
tTopA <- as.character(tTopA)
str(tTopA)

subTtimA <- tampatimA[tampatimA$INTSITE %in% tTopA,]
unique(subTtimA$INTSITE)
landTtimA <- subset(subTtimA, subTtimA$landing > 0)

tampaTopB <- tampatimB %>% 
  group_by(INTSITE) %>% 
  summarise(landings = sum(landing))
str(tampaTopB)
tampaTopBord <- tampaTopB %>% arrange(desc(landings))
#top sites = 770, 632, 3802, 769, 614
tTopB <- cbind(770, 632, 3802, 769, 614)
tTopB <- as.character(tTopB)
str(tTopB)

subTtimB <- tampatimB[tampatimB$INTSITE %in% tTopB,]
unique(subTtimB$INTSITE)
landTtimB <- subset(subTtimB, subTtimB$landing > 0)

#playing around looking at species
orleansSpA <- landOtimA %>% 
  group_by(INTSITE,common) %>% 
  summarise(landings = sum(landing), biomass = sum(wgt_ab1))
str(orleansSpA)
orleansSpA

orleansSpB <- landOtimB %>% 
  group_by(INTSITE,common) %>% 
  summarise(landings = sum(landing), biomass = sum(wgt_ab1))
str(orleansSpB)
orleansSpB

tampaSpA <- landTtimA %>% 
  group_by(INTSITE,common) %>% 
  summarise(landings = sum(landing), biomass = sum(wgt_ab1))
str(tampaSpA)
tampaSpA

tampaSpB <- landTtimB %>% 
  group_by(INTSITE,common) %>% 
  summarise(landings = sum(landing), biomass = sum(wgt_ab1))
str(tampaSpB)
tampaSpB

###












###random stuff, rooting around in files

otherTrip <- read.csv(file.path("~/FUIteam/PydioData/MRIP/raw/MRIP_trip_ZIP/original", 
                           "mrip_survey_trip_2004_2015.csv"), stringsAsFactors = FALSE)
str(otherTrip)

unique(otherTrip$YEAR) #2004-2015
unique(otherTrip$MODE_FX) # mode of fishing 5,7,3
unique(otherTrip$MODE_F) 
unique(otherTrip$ST) #FL only, but has CA counties? looks like a bad file

otherTripB <- read.csv(file.path("~/FUIteam/PydioData/MRIP/raw/Old_MRIP_Archive/MRIP_2004-2015_LA", 
                                "mrip_survey_trip_2004_2015.csv"), stringsAsFactors = FALSE)
str(otherTripB)

unique(otherTripB$YEAR) #2004-2013
unique(otherTripB$MODE_FX) # mode of fishing 5,7,3
unique(otherTripB$MODE_F) 
unique(otherTripB$ST) #LA only

otherTripC <- read.csv(file.path("~/FUIteam/PydioData/MRIP/raw/Old_MRIP_Archive/MRIP_2004-2015_FL&LA", 
                                 "mrip_survey_trip_2004_2015.csv"), stringsAsFactors = FALSE)

str(otherTripC)
unique(otherTripC$YEAR) #2004-2015
unique(otherTripC$MODE_FX) # mode of fishing 5,7,3
unique(otherTripC$MODE_F) 
unique(otherTripC$ST) #LA, FL, and two others

otherTripD <- read.csv(file.path("~/FUIteam/PydioData/MRIP/raw/Old_MRIP_Archive/MRIP_2004-2015_FL&LA", 
                                 "GC_NEW_mrip_survey_trip_2004_2015.csv"), stringsAsFactors = FALSE)

str(otherTripD)
unique(otherTripD$YEAR) #2004-2015
unique(otherTripD$MODE_FX) # mode of fishing 5,7,3
unique(otherTripD$MODE_F) 
unique(otherTripD$ST) #LA, FL, and two others
