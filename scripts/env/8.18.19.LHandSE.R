##trophic level and SE comparisons

lhKey <- read.csv(file.path("~/FUIteam/PydioData/env/raw", "lhKey.csv"), 
                  stringsAsFactors = FALSE)
str(lhKey)

#average trophic level difference

#florida time B
##top sites = 770, 632, 3802, 769, 614
Bwgt614mrip <- subset(sizeAddTb,sizeAddTb$INTSITE == '614')
str(Bwgt614mrip)

B614mrip <- Bwgt614mrip[,c(21:22,26:29)]
B614mrip$site <- '614b'
colnames(B614mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
unique(B614mrip$spName)
str(B614mrip)

Bwgt770mrip <- subset(sizeAddTb,sizeAddTb$INTSITE == '770')
B770mrip <- Bwgt770mrip[,c(21:22,26:29)]
B770mrip$site <- '770b'
colnames(B770mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(B770mrip)

Bwgt632mrip <- subset(sizeAddTb,sizeAddTb$INTSITE == '632')
B632mrip <- Bwgt632mrip[,c(21:22,26:29)]
B632mrip$site <- '632b'
colnames(B632mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(B632mrip)

Bwgt3802mrip <- subset(sizeAddTb,sizeAddTb$INTSITE == '3802')
B3802mrip <- Bwgt3802mrip[,c(21:22,26:29)]
B3802mrip$site <- '3802b'
colnames(B3802mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(B3802mrip)

Bwgt769mrip <- subset(sizeAddTb,sizeAddTb$INTSITE == '769')
B769mrip <- Bwgt769mrip[,c(21:22,26:29)]
B769mrip$site <- '769b'
colnames(B769mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(B769mrip)

allFLb <- rbind(B769mrip, B3802mrip, B632mrip, B770mrip, B614mrip)
str(allFLb)
str(lhKeyB)

LHflB <- left_join(x = allFLb,y = lhKeyB, by = c("spName" = "Common.Name"))
str(LHflB)
head(LHflB)


##se data for both states
laSE <- read.csv(file.path("~/FUIteam/PydioData/env/raw", "la_updated_income.csv"), 
                 stringsAsFactors = FALSE)
flSE <- read.csv(file.path("~/FUIteam/PydioData/env/raw", "fl_updated_income.csv"), 
                 stringsAsFactors = FALSE)

str(laSE)
laSE$zcta <- as.character(laSE$zcta)
flSE$zcta <- as.character(flSE$zcta)

#combine together
LHSEflB <- left_join(x = LHflB,y = flSE, by = c("ZIP" = "zcta"))
str(LHSEflB)
head(LHSEflB)
View(LHSEflB)

#ADD IN MISSING TROPHIC LEVELS
LHSEflB$trophicCode[LHSEflB$spName == "SEATROUT GENUS"] <- 4.0
LHSEflB$trophicCode[LHSEflB$spName == "STINGRAY GENUS"] <- 3.5
LHSEflB$trophicCode[LHSEflB$spName == "MULLET GENUS"] <- 2
LHSEflB$trophicCode[LHSEflB$spName == "LEFTEYE FLOUNDER FAMILY"] <- 3.5
LHSEflB$trophicCode[LHSEflB$spName == "MOJARRA FAMILY"] <- 2.3



##aggregate to have a look at the data
library(dplyr)
detach(package:plyr)

View(test)

sumLHSEflB <- LHSEflB %>% 
  group_by(testStatus100mi) %>% 
  summarise(nSpecies = n_distinct(Scientific.Name), 
            aveLEN = mean(LEN_mm, na.rm = TRUE), 
            aveWT = mean(WT_kg, na.rm = TRUE), 
            aveTrophic = mean(trophicCode, na.rm = TRUE), 
            aveBenthic=mean(benthCode, na.rm = TRUE), 
            aveSed = mean(sedCode, na.rm = TRUE), 
            aveMinority = mean(racial_minority_percent_pop, na.rm = TRUE),
            aveForeign = mean(foreign_born_percent_pop, na.rm = TRUE), 
            avePoverty = mean(poverty_percent_famil, na.rm = TRUE),
            aveIncome = mean(median_income_dollars_hhlds_percent_scaled, na.rm = TRUE), 
            aveEducation = mean(education_HS_GED_percent_pop, na.rm = TRUE), 
            aveNOveh = mean(no_vehicles_percent_hhlds, na.rm = TRUE),
            aveONEveh = mean(one_vehicle_percent_hhlds, na.rm = TRUE), 
            aveStamp = mean(food_stamp_percent_hhlds, na.rm = TRUE))

library(ggpubr)
ggplot(LHSEflB, aes(testStatus100mi, trophicCode)) + geom_boxplot() +
  stat_compare_means()
#untested species tend to be lower on the food chain
ggplot(LHSEflB, aes(testStatus100mi, racial_minority_percent_pop)) + geom_boxplot() +
  stat_compare_means()
ggplot(LHSEflB, aes(testStatus100mi, foreign_born_percent_pop)) + geom_boxplot()+
  stat_compare_means()
ggplot(LHSEflB, aes(testStatus100mi, poverty_percent_famil)) + geom_boxplot()+
  stat_compare_means()
ggplot(LHSEflB, aes(testStatus100mi, median_income_dollars_hhlds_percent_scaled)) + geom_boxplot()
ggplot(LHSEflB, aes(testStatus100mi, education_HS_GED_percent_pop)) + geom_boxplot()
ggplot(LHSEflB, aes(testStatus100mi, no_vehicles_percent_hhlds)) + geom_boxplot()
ggplot(LHSEflB, aes(testStatus100mi, one_vehicle_percent_hhlds)) + geom_boxplot()
ggplot(LHSEflB, aes(testStatus100mi, food_stamp_percent_hhlds)) + geom_boxplot()

ggplot(LHSEflB, aes(racial_minority_percent_pop, trophicCode)) + geom_point() 
ggplot(LHSEflB, aes(foreign_born_percent_pop, trophicCode)) + geom_point()
ggplot(LHSEflB, aes(poverty_percent_famil, trophicCode)) + geom_point() 
ggplot(LHSEflB, aes(median_income_dollars_hhlds_percent_scaled, trophicCode)) + geom_point()
ggplot(LHSEflB, aes(education_HS_GED_percent_pop, trophicCode)) + geom_point() 
ggplot(LHSEflB, aes(no_vehicles_percent_hhlds, trophicCode)) + geom_point()
ggplot(LHSEflB, aes(one_vehicle_percent_hhlds, trophicCode)) + geom_point()
ggplot(LHSEflB, aes(food_stamp_percent_hhlds, trophicCode)) + geom_point()

ggplot(LHSEflB, aes(racial_minority_percent_pop, WT_kg)) + geom_point()
ggplot(LHSEflB, aes(racial_minority_percent_pop, LEN_mm)) + geom_point()
  
ggplot(subset(LHSEflB,spName %in% "RED DRUM")) + 
    geom_boxplot(aes(testStatus100mi, trophicCode))

##FL time a
#770, 769, 632, 615, 742
Awgt770mrip <- subset(sizeAddTa,sizeAddTa$INTSITE == '770')
A770mrip <- Awgt770mrip[,c(21:22,26:29)]
A770mrip$site <- '770a'
colnames(A770mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(A770mrip)

Awgt769mrip <- subset(sizeAddTa,sizeAddTa$INTSITE == '769')
A769mrip <- Awgt769mrip[,c(21:22,26:29)]
A769mrip$site <- '769a'
colnames(A769mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(A769mrip)

Awgt632mrip <- subset(sizeAddTa,sizeAddTa$INTSITE == '632')
A632mrip <- Awgt632mrip[,c(21:22,26:29)]
A632mrip$site <- '632a'
colnames(A632mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(A632mrip)

Awgt615mrip <- subset(sizeAddTa,sizeAddTa$INTSITE == '615')
A615mrip <- Awgt615mrip[,c(21:22,26:29)]
A615mrip$site <- '615a'
colnames(A615mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(A615mrip)

Awgt742mrip <- subset(sizeAddTa,sizeAddTa$INTSITE == '742')
A742mrip <- Awgt742mrip[,c(21:22,26:29)]
A742mrip$site <- '742a'
colnames(A742mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_kg","LEN_mm", "INTSITE")
str(A742mrip)

allFLa <- rbind(A770mrip, A769mrip, A632mrip, A615mrip, A742mrip)
str(allFLa)

LHflA <- left_join(x = allFLa,y = lhKeyB, by = c("spName" = "Common.Name"))
str(LHflA)

#combine together
LHSEflA <- left_join(x = LHflA,y = flSE, by = c("ZIP" = "zcta"))
str(LHSEflA)
head(LHSEflA)
View(LHSEflA)

#ADD IN MISSING TROPHIC LEVELS
LHSEflA$trophicCode[LHSEflA$spName == "KINGFISH GENUS"] <- 3.6
LHSEflA$trophicCode[LHSEflA$spName == "MOJARRA FAMILY"] <- 2.3
LHSEflA$trophicCode[LHSEflA$spName == "HERRING FAMILY"] <- 3.7
LHSEflA$trophicCode[LHSEflA$spName == "STINGRAY GENUS"] <- 3.5
LHSEflA$trophicCode[LHSEflA$spName == "LIZARDFISH GENUS"] <- 4.35
LHSEflA$trophicCode[LHSEflA$spName == "ORANGE FILEFISH"] <- 2.0
LHSEflA$trophicCode[LHSEflA$spName == "GRUNT GENUS"] <- 3.43
LHSEflA$trophicCode[LHSEflA$spName == "MACKEREL GENUS"] <- 4.47
LHSEflA$trophicCode[LHSEflA$spName == "SEATROUT GENUS"] <- 4.0
LHSEflA$trophicCode[LHSEflA$spName == "SNOOK"] <- 4.2
LHSEflA$trophicCode[LHSEflA$spName == "LEFTEYE FLOUNDER FAMILY"] <- 3.5
LHSEflA$trophicCode[LHSEflA$spName == "ANCHOVY FAMILY"] <- 3.4
LHSEflA$trophicCode[LHSEflA$spName == "MULLET GENUS"] <- 2


ggplot(LHSEflA, aes(testStatus100mi, trophicCode)) + geom_boxplot() +
  stat_compare_means()

##aggregate to have a look at the data
sumLHSEflA <- LHSEflA %>% 
  group_by(testStatus100mi) %>% 
  summarise(nSpecies = n_distinct(Scientific.Name), 
            aveLEN = mean(LEN_mm, na.rm = TRUE), 
            aveWT = mean(WT_kg, na.rm = TRUE), 
            aveTrophic = mean(trophicCode, na.rm = TRUE), 
            aveBenthic=mean(benthCode, na.rm = TRUE), 
            aveSed = mean(sedCode, na.rm = TRUE), 
            aveMinority = mean(racial_minority_percent_pop, na.rm = TRUE),
            aveForeign = mean(foreign_born_percent_pop, na.rm = TRUE), 
            avePoverty = mean(poverty_percent_famil, na.rm = TRUE),
            aveIncome = mean(median_income_dollars_hhlds_percent_scaled, na.rm = TRUE), 
            aveEducation = mean(education_HS_GED_percent_pop, na.rm = TRUE), 
            aveNOveh = mean(no_vehicles_percent_hhlds, na.rm = TRUE),
            aveONEveh = mean(one_vehicle_percent_hhlds, na.rm = TRUE), 
            aveStamp = mean(food_stamp_percent_hhlds, na.rm = TRUE))
View(sumLHSEflA)

#LA time A, copied from other .Rmd

#222, 231, 306, 155, 151,

Awgt222mrip <- subset(sizeAddLAa,sizeAddLAa$INTSITE == '222')
A222mrip <- Awgt222mrip[,c(21:22,27:30)]
A222mrip$site <- '222a'
colnames(A222mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm", "INTSITE")
str(A222mrip)

Awgt231mrip <- subset(sizeAddLAa,sizeAddLAa$INTSITE == '231')
A231mrip <- Awgt231mrip[,c(21:22,27:30)]
A231mrip$site <- '231a'
colnames(A231mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm", "INTSITE")
str(A231mrip)

Awgt306mrip <- subset(sizeAddLAa,sizeAddLAa$INTSITE == '306')
A306mrip <- Awgt306mrip[,c(21:22,27:30)]
A306mrip$site <- '306a'
colnames(A306mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm","INTSITE")
str(A306mrip)

Awgt155mrip <- subset(sizeAddLAa,sizeAddLAa$INTSITE == '155')
A155mrip <- Awgt155mrip[,c(21:22,27:30)]
A155mrip$site <- '155a'
colnames(A155mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm","INTSITE")
str(A155mrip)

Awgt151mrip <- subset(sizeAddLAa,sizeAddLAa$INTSITE == '151')
A151mrip <- Awgt151mrip[,c(21:22,27:30)]
A151mrip$site <- '151a'
colnames(A151mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm","INTSITE")
str(A151mrip)

allLAa <- rbind(A222mrip, A231mrip, A306mrip, A155mrip, A151mrip)
str(allLAa)

LHlaA <- left_join(x = allLAa,y = lhKeyB, by = c("spName" = "Common.Name"))
str(LHlaA)
str(laSE)

#combine together
LHSElaB <- left_join(x = LHlaA,y = laSE, by = c("ZIP" = "zcta"))
str(LHSElaA)
head(LHSElaA)
View(LHSElaA)

#add in missing trophic levels for species we didn't catch the first time around
LHSElaA$trophicCode[LHSElaA$spName == "ATLANTIC STINGRAY"] <- 3.5
LHSElaA$trophicCode[LHSElaA$spName == "GULF MENHADEN"] <- 2.2
LHSElaA$trophicCode[LHSElaA$spName == "HARDHEAD CATFISH"] <- 3.2
LHSElaA$trophicCode[LHSElaA$spName == "KINGFISH GENUS"] <- 3.6
LHSElaA$trophicCode[LHSElaA$spName == "BLUE CATFISH"] <- 3.4
LHSElaA$trophicCode[LHSElaA$spName == "HARDHEAD CATFISH"] <- 3.2
LHSElaA$trophicCode[LHSElaA$spName == "SEATROUT GENUS"] <- 4.0
LHSElaA$trophicCode[LHSElaA$spName == "SOUTHERN FLOUNDER"] <- 3.5
LHSElaA$trophicCode[LHSElaA$spName == "SPOT"] <- 3.2
#remove freshwater fish
finLHSElaA <- subset(LHSElaA, !LHSElaA$spName%in%c("CHANNEL CATFISH", "FRESHWATER DRUM", "LARGEMOUTH BASS"))
View(finLHSElaA)


ggplot(finLHSElaA, aes(testStatus100mi, trophicCode)) + geom_boxplot() +
  stat_compare_means()

##aggregate to have a look at the daOa
sumLHSElaA <- LHSElaA %>% 
  group_by(testStatus100mi) %>% 
  summarise(nSpecies = n_distinct(Scientific.Name), 
            aveLEN = mean(LEN_cm, na.rm = TRUE), 
            aveWT = mean(WT_g, na.rm = TRUE), 
            aveTrophic = mean(trophicCode, na.rm = TRUE), 
            aveBenthic=mean(benthCode, na.rm = TRUE), 
            aveSed = mean(sedCode, na.rm = TRUE), 
            aveMinority = mean(racial_minority_percent_pop, na.rm = TRUE),
            aveForeign = mean(foreign_born_percent_pop, na.rm = TRUE), 
            avePoverty = mean(poverty_percent_famil, na.rm = TRUE),
            aveIncome = mean(median_income_dollars_hhlds_percent_scaled, na.rm = TRUE), 
            aveEducation = mean(education_HS_GED_percent_pop, na.rm = TRUE), 
            aveNOveh = mean(no_vehicles_percent_hhlds, na.rm = TRUE),
            aveONEveh = mean(one_vehicle_percent_hhlds, na.rm = TRUE), 
            aveSOamp = mean(food_stamp_percent_hhlds, na.rm = TRUE))
View(sumLHSElaA)

#LA time B, copied from other .Rmd
#222, 159, 306, 3325, 151
Bwgt222mrip <- subset(sizeAddLAb,sizeAddLAb$INTSITE == '222')
B222mrip <- Bwgt222mrip[,c(21:22,26:29)]
B222mrip$site <- '222b'
colnames(B222mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm", "INTSITE")
str(B222mrip)

Bwgt159mrip <- subset(sizeAddLAb,sizeAddLAb$INTSITE == '159')
B159mrip <- Bwgt159mrip[,c(21:22,26:29)]
B159mrip$site <- '159b'
colnames(B159mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm", "INTSITE")
str(B159mrip)

Bwgt306mrip <- subset(sizeAddLAb,sizeAddLAb$INTSITE == '306')
B306mrip <- Bwgt306mrip[,c(21:22,26:29)]
B306mrip$site <- '306b'
colnames(B306mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm","INTSITE")
str(B306mrip)

Bwgt3325mrip <- subset(sizeAddLAb,sizeAddLAb$INTSITE == '3325')
B3325mrip <- Bwgt3325mrip[,c(21:22,26:29)]
B3325mrip$site <- '3325b'
colnames(B3325mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm","INTSITE")
str(B3325mrip)

Bwgt151mrip <- subset(sizeAddLAb,sizeAddLAb$INTSITE == '151')
B151mrip <- Bwgt151mrip[,c(21:22,26:29)]
B151mrip$site <- '151b'
colnames(B151mrip) <- c("ZIP", "spName", "testStatus25mi", "testStatus100mi","WT_g","LEN_cm","INTSITE")
str(B151mrip)

allLAb <- rbind(B222mrip, B159mrip, B306mrip, B3325mrip, B151mrip)

str(allLAb)

LHlaB <- left_join(x = allLAb,y = lhKeyB, by = c("spName" = "Common.Name"))
str(LHlaB)
str(laSE)

#combine together
LHSElaB <- left_join(x = LHlaB,y = laSE, by = c("ZIP" = "zcta"))
str(LHSElaB)
head(LHSElaB)
View(LHSElaB)

#add in missing trophic levels for species we didn't catch the first time around
LHSElaB$trophicCode[LHSElaB$spName == "SKIPJACK HERRING"] <- 3.9
LHSElaB$trophicCode[LHSElaB$spName == "HARDHEAD CATFISH"] <- 3.2
LHSElaB$trophicCode[LHSElaB$spName == "BLUE CATFISH"] <- 3.4
LHSElaB$trophicCode[LHSElaB$spName == "HARDHEAD CATFISH"] <- 3.2
LHSElaB$trophicCode[LHSElaB$spName == "SOUTHERN FLOUNDER"] <- 3.5

#remove freshwater fish
finLHSElaB <- subset(LHSElaB, !LHSElaB$spName%in%c("BLUEGILL"))
View(finLHSElaB)

ggplot(finLHSElaB, aes(testStatus100mi, trophicCode)) + geom_boxplot() +
  stat_compare_means()

##aggregate to have a look at the daOa
sumLHSElaB <- LHSElaB %>% 
  group_by(testStatus100mi) %>% 
  summarise(nSpecies = n_distinct(Scientific.Name), 
            aveLEN = mean(LEN_cm, na.rm = TRUE), 
            aveWT = mean(WT_g, na.rm = TRUE), 
            aveTrophic = mean(trophicCode, na.rm = TRUE), 
            aveBenthic=mean(benthCode, na.rm = TRUE), 
            aveSed = mean(sedCode, na.rm = TRUE), 
            aveMinority = mean(racial_minority_percent_pop, na.rm = TRUE),
            aveForeign = mean(foreign_born_percent_pop, na.rm = TRUE), 
            avePoverty = mean(poverty_percent_famil, na.rm = TRUE),
            aveIncome = mean(median_income_dollars_hhlds_percent_scaled, na.rm = TRUE), 
            aveEducation = mean(education_HS_GED_percent_pop, na.rm = TRUE), 
            aveNOveh = mean(no_vehicles_percent_hhlds, na.rm = TRUE),
            aveONEveh = mean(one_vehicle_percent_hhlds, na.rm = TRUE), 
            aveSOamp = mean(food_stamp_percent_hhlds, na.rm = TRUE))
View(sumLHSElaB)

## exporting files
write.csv(sumLHSElaA, "~/FUIteam/PydioData/env/data_outputs/sumLHSElaA.csv")
write.csv(sumLHSElaB, "~/FUIteam/PydioData/env/data_outputs/sumLHSElaB.csv")
write.csv(sumLHSEflA, "~/FUIteam/PydioData/env/data_outputs/sumLHSEflA.csv")
write.csv(sumLHSEflB, "~/FUIteam/PydioData/env/data_outputs/sumLHSEflB.csv")


#top species by zip code? or other?
seFact <- read.csv(file.path("~/FUIteam/PydioData/env/data_outputs", 
                             "zip_FactorAnalysis_PrinComponents_ScoresCoords.csv"), 
                   stringsAsFactors = FALSE)
unique(seFact$ZIP)
str(seFact)
seFact$ZIP <- as.character(seFact$ZIP)
LHSElaA$ZIP <- as.character(LHSElaA$ZIP)
LHSElaB$ZIP <- as.character(LHSElaB$ZIP)
LHSEflA$ZIP <- as.character(LHSEflA$ZIP)
LHSEflB$ZIP <- as.character(LHSEflB$ZIP)

faLHSElaA <- left_join(LHSElaA, seFact)
faLHSElaB <- left_join(LHSElaB, seFact)
faLHSEflA <- left_join(LHSEflA, seFact)
faLHSEflB <- left_join(LHSEflB, seFact)

#see if the top fish species differ by SE status, using factor? analysis dimensions
LAaSPbyDIM <- faLHSElaA %>% 
  group_by(FA_dim1_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_g), aveWT = mean(WT_g), aveLEN = mean(LEN_cm),
            aveTrophic = mean(trophicCode))
str(LAaSPbyDIM)
print(LAaSPbyDIM, n=55) 
LAaSPbyDIMord <- LAaSPbyDIM %>% arrange(desc(FA_dim1_quantile),desc(records))
print(LAaSPbyDIMord, n=55)

LAbSPbyDIM <- faLHSElaB %>% 
  group_by(FA_dim1_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_g), aveWT = mean(WT_g), aveLEN = mean(LEN_cm),
            aveTrophic = mean(trophicCode))
str(LAbSPbyDIM)
LAbSPbyDIMord <- LAbSPbyDIM %>% arrange(desc(FA_dim1_quantile),desc(records))
print(LAbSPbyDIMord, n=36)

FLaSPbyDIM <- faLHSEflA %>% 
  group_by(FA_dim1_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_kg), aveWT = mean(WT_kg), aveLEN = mean(LEN_mm),
            aveTrophic = mean(trophicCode))
str(FLaSPbyDIM)
FLaSPbyDIMord <- FLaSPbyDIM %>% arrange(desc(FA_dim1_quantile),desc(records))
print(FLaSPbyDIMord, n=136)

FLbSPbyDIM <- faLHSEflB %>% 
  group_by(FA_dim1_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_kg), aveWT = mean(WT_kg), aveLEN = mean(LEN_mm),
            aveTrophic = mean(trophicCode))
str(FLbSPbyDIM)
FLbSPbyDIMord <- FLbSPbyDIM %>% arrange(desc(FA_dim1_quantile),desc(records))
print(FLbSPbyDIMord, n=96)

## exporting files
write.csv(LAaSPbyDIMord, "~/FUIteam/PydioData/env/data_outputs/LAaSPbyDIMord.csv")
write.csv(LAbSPbyDIMord, "~/FUIteam/PydioData/env/data_outputs/LAbSPbyDIMord.csv")
write.csv(FLaSPbyDIMord, "~/FUIteam/PydioData/env/data_outputs/FLaSPbyDIMord.csv")
write.csv(FLbSPbyDIMord, "~/FUIteam/PydioData/env/data_outputs/FLbSPbyDIMord.csv")

#unique sp and tested
unique(LHSElaA$spName) & unique(LHSElaA$testStatus100mi)
# distinct
LHSElaA %>% 
  distinct(spName, testStatus100mi)
LHSElaB %>% 
  distinct(spName, testStatus100mi)
LHSEflA %>% 
  distinct(spName, testStatus100mi)
LHSEflB %>% 
  distinct(spName, testStatus100mi)


#DIM 2
#see if the top fish species differ by SE status, using factor? analysis dimensions
LAaSPbyDIM2 <- faLHSElaA %>% 
  group_by(FA_dim2_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_g), aveWT = mean(WT_g), aveLEN = mean(LEN_cm),
            aveTrophic = mean(trophicCode))
str(LAaSPbyDIM2)
LAaSPbyDIM2ord <- LAaSPbyDIM2 %>% arrange(desc(FA_dim2_quantile),desc(records))
print(LAaSPbyDIM2ord, n=58)

LAbSPbyDIM2 <- faLHSElaB %>% 
  group_by(FA_dim2_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_g), aveWT = mean(WT_g), aveLEN = mean(LEN_cm),
            aveTrophic = mean(trophicCode))
str(LAbSPbyDIM2)
LAbSPbyDIM2ord <- LAbSPbyDIM2 %>% arrange(desc(FA_dim2_quantile),desc(records))
print(LAbSPbyDIM2ord, n=37)

FLaSPbyDIM2 <- faLHSEflA %>% 
  group_by(FA_dim2_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_kg), aveWT = mean(WT_kg), aveLEN = mean(LEN_mm),
            aveTrophic = mean(trophicCode))
str(FLaSPbyDIM2)
FLaSPbyDIM2ord <- FLaSPbyDIM2 %>% arrange(desc(FA_dim2_quantile),desc(records))
print(FLaSPbyDIM2ord, n=133)

FLbSPbyDIM2 <- faLHSEflB %>% 
  group_by(FA_dim2_quantile, spName) %>% 
  summarise(records = n(), biomass = sum(WT_kg), aveWT = mean(WT_kg), aveLEN = mean(LEN_mm),
            aveTrophic = mean(trophicCode))
str(FLbSPbyDIM2)
FLbSPbyDIM2ord <- FLbSPbyDIM2 %>% arrange(desc(FA_dim2_quantile),desc(records))
print(FLbSPbyDIM2ord, n=96)

## exporting files
write.csv(LAaSPbyDIM2ord, "~/FUIteam/PydioData/env/data_outputs/LAaSPbyDIM2ord.csv")
write.csv(LAbSPbyDIM2ord, "~/FUIteam/PydioData/env/data_outputs/LAbSPbyDIM2ord.csv")
write.csv(FLaSPbyDIM2ord, "~/FUIteam/PydioData/env/data_outputs/FLaSPbyDIM2ord.csv")
write.csv(FLbSPbyDIM2ord, "~/FUIteam/PydioData/env/data_outputs/FLbSPbyDIM2ord.csv")

#unique sp and tested
unique(LHSElaA$spName) & unique(LHSElaA$testStatus100mi)
# distinct
LHSElaA %>% 
  distinct(spName, testStatus100mi)
LHSElaB %>% 
  distinct(spName, testStatus100mi)
LHSEflA %>% 
  distinct(spName, testStatus100mi)
LHSEflB %>% 
  distinct(spName, testStatus100mi)

### trying to get together stuff for visualization
finLHSElaA$time <- "A"
finLHSElaB$time <- "B"
LHSEflA$time <- "A"
LHSEflB$time <- "B"

tcLAsum <- rbind(finLHSElaA, finLHSElaB)
tcFLsum <- rbind(LHSEflA, LHSEflB)

ggplot(tcLAsum, aes(testStatus100mi, trophicCode, )) + geom_boxplot() +
  stat_compare_means(method = t.test)
