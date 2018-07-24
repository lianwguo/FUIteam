
#===============================================================================
# MRIP data subset and visualization for metropolitan zip code tabulation areas
# 
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@uw.edu
# For: SESYNC Graduate Pursuit
# Date started: 05/22/2018
# Revised: 05/26/2018
#===============================================================================

# next step link top fishing sites with zip codes 

# NOTE best way to do this is most likley with the existing lists... 
# and then putting it in a long format dataframe under columns (LA/TB, Year, ZipCode,Site,Quantity)

setwd("/Users/zachkoehn/UW/FoodFishHappy/SESYNC.Grad/ScriptData")

# useful custom fucntions
source("UrbanFishFunctions.R")

# global settings to make things less painful
options(scipen=500) #this helps with importing the very long coding sequences, R will otherwise parse them in scientific notation (which is useless)
options(stringsAsFactors=FALSE) #so this doesn't have to be repeated anytime a new dataframe is created


# load spreadsheets
metrozips <- read.csv("data/metropolitan_ZCTA.csv",header = TRUE) # ZCTAs metro areas from Sarita

datCatch <- read.csv("data/aggregatedMRIP/mripCatch_2004_2017.csv")  # MRIP catch data
datTrip <- read.csv("data/aggregatedMRIP/mripTrip_2004_2017.csv") #MRIP trip data

summary(as.factor(datCatch$MODE_F))

#####################################################
# ranks top zip codes by landing
#####################################################


# years for each list, to beused repeatedly
LAyearsVec <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013) # LA missing 2014,2015 for catch
FLyearsVec <- c(2005,2006,2007,2009,2010,2011,2012,2013,2014,2015) # 2008 has missing information
howMany=50

#______________________________________________________ 
# Florida

FLTopZipLanding <- lapply(FLyearsVec, function(x) 
	rankZipCodes(yearVec=x,harvestType="landing",state="Florida", howMany=howMany,METRO=TRUE,metrozips=metrozips,datCatch=datCatch,datTrip=datTrip)
	)
names(FLTopZipLanding) <- FLyearsVec


FLZipLanding_df <- as.data.frame(
	cbind(
		as.vector(unlist(unname(sapply(FLTopZipLanding, `[[`, 1)))),
		as.vector(unlist(unname(sapply(FLTopZipLanding, `[[`, 2)))),
		as.vector(unlist(unname(sapply(FLTopZipLanding, `[[`, 4))))	
		)
	)

names(FLZipLanding_df) <- c("zipcode","landed","year")
FLZipLanding_df$state <- "Florida"


#______________________________________________________ 
# Louisiana
LATopZipLanding <- lapply(LAyearsVec, function(x) 
	rankZipCodes(yearVec=x,harvestType="landing",state="Louisiana", howMany=howMany,METRO=TRUE,metrozips=metrozips,datCatch=datCatch,datTrip=datTrip)
	)
names(LATopZipLanding) <- LAyearsVec


LAZipLanding_df <- as.data.frame(
	cbind(
		unlist(unname(sapply(LATopZipLanding, `[[`, 1))),
		unlist(unname(sapply(LATopZipLanding, `[[`, 2))),
		unlist(unname(sapply(LATopZipLanding, `[[`, 4)))	
		)
	)


names(LAZipLanding_df) <- c("zipcode","landed","year")
LAZipLanding_df$state <- "Louisiana"

# where are the zips (plug in to google maps)
paste(unique(LAZipLanding_df$zipcode), collapse=", ") 
paste(unique(FLZipLanding_df$zipcode), collapse=", ") 
#______________________________________________________ 
# bring together Louisiana and Florida results
MetroZipLandingYearState <- rbind(LAZipLanding_df,FLZipLanding_df)



#####################################################
# Creates dataframe for FL and LA for year, ID_CODE,County, Zip, Site, Landing
#####################################################

# years for each list, to beused repeatedly
LAyearsVec <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013) # LA missing 2014,2015 for catch
FLyearsVec <- c(2005,2006,2007,2009,2010,2011,2012,2013,2014,2015) # 2008 has missing information
howMany=50

# # Florida
# create first year as dataframe to bind looped years to
LA_zipSitesLanding <- zipSitesLanding(yearVec=LAyearsVec[1],harvestType="landing",state="Louisiana", howMany=howMany,METRO=TRUE,metrozips=metrozips,datCatch=datCatch,datTrip=datTrip)	
for(y in 2:length(LAyearsVec)) { #and loop through
	toRowBind <- zipSitesLanding(yearVec=LAyearsVec[y],harvestType="landing",state="Louisiana", howMany=howMany,METRO=TRUE,metrozips=metrozips,datCatch=datCatch,datTrip=datTrip)	
	toRowBind
	LA_zipSitesLanding <- rbind(LA_zipSitesLanding,toRowBind)
}

# # Louisiana
# create first year as dataframe to bind looped years to
FL_zipSitesLanding <- zipSitesLanding(yearVec=FLyearsVec[1],harvestType="landing",state="Florida", howMany=howMany,METRO=TRUE,metrozips=metrozips,datCatch=datCatch,datTrip=datTrip)	
for(y in 2:length(FLyearsVec)) { #and loop through
	toRowBind <- zipSitesLanding(yearVec=FLyearsVec[y],harvestType="landing",state="Florida", howMany=howMany,METRO=TRUE,metrozips=metrozips,datCatch=datCatch,datTrip=datTrip)	
	FL_zipSitesLanding <- rbind(FL_zipSitesLanding,toRowBind)
}

# and export both to new folder
write.csv(LA_zipSitesLanding,"data/metro_ZipSiteLanding/metro_ZipSiteLanding_LA.csv",row.names=FALSE)
write.csv(FL_zipSitesLanding,"data/metro_ZipSiteLanding/metro_ZipSiteLanding_FL.csv",row.names=FALSE)

str(LA_zipSitesLanding)

par(mfrow=c(2,1))
plot(density(LA_zipSitesLanding$landing),col="deepskyblue4",main="NOLA kernel density landings")
abline(v=median(LA_zipSitesLanding$landing),col="firebrick4")
mtext(
	paste(
		"Median:", median(LA_zipSitesLanding$landing)
	)
)

plot(density(FL_zipSitesLanding$landing),col="deepskyblue4", "Tampa Bay kernel density landings")
abline(v=median(FL_zipSitesLanding$landing),col="firebrick4")
mtext(
	paste(
		"Median:", median(FL_zipSitesLanding$landing)
	)
)


# plots zips 

# \learning how to plot zip codes in r using choroplethr package
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(ggplot2)



pdf("plots/diagnosticPlots/LouisianaLanding_MetroAreaZips.pdf")
zipPlot(1,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(2,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(3,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(4,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(5,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(6,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(7,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(8,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
zipPlot(9,Mat= MetroZipLandingYearState, state="Louisiana",harvestType="Landing")
dev.off()
	

pdf("plots/diagnosticPlots/FloridaLanding_MetroAreaZips.pdf")
zipPlot(1,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(2,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(3,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(4,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(5,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(6,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(7,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(8,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(9,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
zipPlot(10,Mat= MetroZipLandingYearState, state="Florida",harvestType="Landing")
dev.off()


