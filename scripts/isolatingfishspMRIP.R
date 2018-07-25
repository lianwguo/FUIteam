#=================================================================
# Cleaning MRIP data ==for fish species!
# For catch, takes the dozens of raw data sheets and aggregates 
# extracts for specific variables for all years and survey waves
# For survey, takes the 4 of raw data sheets and aggregates them
# outputs both as aggregated csv files
# 
# Name(email): J. Zachary Koehn (zkoehn@uw.edu)
# For: SESYNC Graduate Pursuit
# Date started: 02/23/2018
# Revised: 07/24/2018
#==================================================================
#testing differences

# global settings to make things less painful
options(scipen=500) #this helps with importing the very long coding sequences used by NOAA that R will otherwise parse them in scientific notation (which is useless)
options(stringsAsFactors=FALSE) #so this doesn't have to be repeated anytime a new dataframe is created


#____________________________________________________
# bringing together the MRIP trip survey data


# arguments used in loops
# variables that will be extracted to save computation time
variablesToIndex <- c(
  c("NAME",
    "AREA_X",
    "CLAIM",
    "COMMON",
    "HARV_D3",
    "HARV_D4",
    "HARV_D5",
    "HARV_D6",
    "HARV_D7",
    "HARVEST",
    "HARVEST_UNADJ",
    "ID_CODE",
    "LANDING",
    "REGION",
    "REL_D1",
    "REL_D2",
    "RELEASE",
    "RELEASE_UNADJ",
    "SP_CODE",
    "TOT_CAT",
    "TOT_LEN",
    "TOT_LEN_A",
    "TOT_LEN_B1",
    "WGT_A",
    "WGT_AB1",
    "WGT_B1",
    "WP_CATCH",
    "WP_INT",
    "YEAR",
    "ST",
    "YEAR",
    "WAVE",
    "MODE_FX")
)
years <- seq(2004,2017,by=1)
wave <- seq(1,6,by=1)


# first loop just to get things started
csv <- paste0("PydioData/MRIP/raw/MRIP_catch_dispo/catch_dispo_",years[1],wave[1],".csv",sep="")
new <- read.csv(csv,header=TRUE)
new <- new[which( new$MODE_FX < 4 ),]

nameVec <- names(new) %in% variablesToIndex
fullBind <- new[nameVec]

# loops through first wave
for(w in 2:6) {
  # loops through second wave to set things up 
  csv <- paste0("PydioData/MRIP/raw/MRIP_catch_dispo/catch_dispo_",years[1],wave[w],".csv",sep="")
  
  new <- read.csv(csv,header=TRUE)
  new <- new[which( new$MODE_FX < 4 ),] #subset only relevant fishing modes
  nameVec <- names(new) %in% variablesToIndex #extract names
  new <- new[nameVec] #subset only variables specified above
  fullBind <- rbind(fullBind,new) #bind new information to all other information
}

# second nested loop runs through each subsequent year and wave
for(y in 2:length(years)) {
  for(w in 1:length(wave)) {
    # loops through each year after the first year and all of each wave
    csv <- paste0("PydioData/MRIP/raw/MRIP_catch_dispo/catch_dispo_",years[y],wave[w],".csv",sep="")
    
    new <- read.csv(csv,header=TRUE)
    new <- new[which( new$MODE_FX < 4 ),] #subset only relevant fishing modes
    nameVec <- names(new) %in% variablesToIndex #extract names
    new <- new[nameVec] #subset only variables specified above
    fullBind <- rbind(fullBind,new) #bind new information to all other information
  }
}
str(fullBind)

write.csv(fullBind,"PydioData/MRIP/data_outputs/clean_MRIP/mripCatch_species_2004_2017.csv",row.names=FALSE)


# we need to site, zipcode, landing by species, weight of species, length of species, etc
# basically each line of code will be a site zip species combination

# link expanded catch database to trip database by ID_CODE should do it.

datCatch <- read.csv("PydioData/MRIP/data_outputs/clean_MRIP/mripCatch_species_2004_2017.csv",header=TRUE) #test
datTrip <- read.csv("PydioData/MRIP/data_outputs/clean_MRIP/mripTrip_2004_2017.csv",header=TRUE) #test
metrozips <- read.csv("PydioData/MRIP/data_outputs/clean_MRIP/metropolitan_ZCTA.csv",header = TRUE) # ZCTAs metro areas from Sarita

datCatch$ID_CODE <- as.character(datCatch$ID_CODE) #convert to character so it merges long strings of numbers with no issues


#clean MRIP Trip file so merge doesn't have duplicates
intersect(names(datTrip),names(datCatch)) #drop all these *except* ID_CODE used for merge
dropDups <- c("YEAR","ST","MODE_FX","AREA_X","WAVE") #identified using intersect
datTrip <- datTrip[ , !(names(datTrip) %in% dropDups)] #drops duplicates using above objects

names(datTrip)

# merge together data onto new species-specific dataset 
dat_all <- merge(datCatch,datTrip,by="ID_CODE", all.x = TRUE) #this merges trip to catch and ensures that all catch remains even if there is no match on trip


write.csv(dat_all,"PydioData/MRIP/data_outputs/clean_MRIP/mrip_species_zip_site_2004_2017.csv",row.names=FALSE)
