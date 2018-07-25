#=================================================================
# Cleaning MRIP data 
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
  "ID_CODE",
  "ST",
  "YEAR",
  "WAVE",
  "MODE_FX",
  "harv_d3",
  "landing",
  "wgt_ab1",
  "common",
  "SP_CODE"
  )
years <- seq(2004,2017,by=1)
wave <- seq(1,6,by=1)


# first loop just to get things started
csv <- paste0("PydioData/MRIP/MRIP_catch_dispo/catch_dispo_",years[1],wave[1],".csv",sep="")
new <- read.csv(csv,header=TRUE)
new <- new[which( new$MODE_FX < 4 ),]

nameVec <- names(new) %in% variablesToIndex
fullBind <- new[nameVec]

# loops through first wave
for(w in 2:6) {
	# loops through second wave to set things up 
	csv <- paste0("PydioData/MRIP/MRIP_catch_dispo/catch_dispo_",years[1],wave[w],".csv",sep="")
	
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
		csv <- paste0("PydioData/MRIP/MRIP_catch_dispo/catch_dispo_",years[y],wave[w],".csv",sep="")
		
		new <- read.csv(csv,header=TRUE)
		new <- new[which( new$MODE_FX < 4 ),] #subset only relevant fishing modes
		nameVec <- names(new) %in% variablesToIndex #extract names
		new <- new[nameVec] #subset only variables specified above
		fullBind <- rbind(fullBind,new) #bind new information to all other information
 	}
 }

write.csv(fullBind,"PydioData/MRIP/mripCatch_2004_2017.csv",row.names=FALSE)

#____________________________________________________
# bringing together the MRIP trip survey data

# Florida all in one file
FL_trip_2004_2015 <- read.csv("PydioData/MRIP/MRIP_trip_ZIP/original/mrip_survey_trip_2004_2015.csv") # all of florida
# Louisiana was broken up
LA_trip_2004 <- read.csv("PydioData/MRIP/MRIP_trip_ZIP/original/LA_trip_2004_2004_zipcode.csv")
LA_trip_2005_2010 <- read.csv("PydioData/MRIP/MRIP_trip_ZIP/original/LA_mrip_survey_trip_zipcode_2005_2010.csv") #this one seems wrong
LA_trip_2010_2015 <- read.csv("PydioData/MRIP/MRIP_trip_ZIP/original/mrip_survey_trip_2010_2015.csv") # part of Louisiana
# overlap in 2005-2010 and 2010 to 2015 data, so remove 2010 from the 2005 - 2010 dataset
LA_trip_2005_2009 <- subset(LA_trip_2005_2010,LA_trip_2005_2010$YEAR!=2010)

# sight check to make sure each row has the same name and can be easily bound
cbind(names(FL_trip_2004_2015),
	names(LA_trip_2004),
	names(LA_trip_2005_2009),
	names(LA_trip_2010_2015)
	)



# bind by all rows
trip_all <- rbind(
	FL_trip_2004_2015,
	LA_trip_2004,
	LA_trip_2005_2009,
	LA_trip_2010_2015
	)

# will need to amend this, as the raw trip
write.csv(trip_all,"PydioData/MRIP/mripTrip_2004_2017.csv",row.names=FALSE)



