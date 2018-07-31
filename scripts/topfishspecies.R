# Top 10 fish species 
# Based on the top sites in each sampling year, this will isolate the most commonly 
#caught and harvested species during each year and at each site. 
#This file is a test for the AFS presentation, focus on sites 222 and site 306 in New Orleans. 
# 
# Name(email): Meghna Marjadi (mmarjadi@umass.edu)
# For: SESYNC Graduate Pursuit
# Date started: 07/31/2018

# global settings to make things less painful
options(scipen=500) #this helps with importing the very long coding sequences used by NOAA that R will otherwise parse them in scientific notation (which is useless)
options(stringsAsFactors=FALSE) #so this doesn't have to be repeated anytime a new dataframe is created

#read in data set 
fishdata<-read.csv("PydioData/MRIP/data_outputs/clean_MRIP/mrip_species_zip_site_2004_2017.csv")
str(fishdata)
str(fishdata$INTSITE)

#For the afs presentation, we are focusing on sites 222 and 306 in NoLA, so I will create a subset 
#Site 222
#First isolate the data for site 222
fish222<-subset(fishdata, INTSITE == 222)
summary(fish222$INTSITE) #check (should all be 222)
#subset by state
#state is the variable ST. FIPS code 22 = LA, 12 =FL
fish222<-subset(fish222, ST == 22)
summary(fish222$ST) #check (should all be 22)

#to consider one year, subset by year
fish222_2010<-subset(fish222,YEAR == 2010)
summary(fish222_2010$YEAR)

#For 2010:
table(fish222_2010$SP_CODE, fish222_2010$HARVEST)
table(fish222_2010$SP_CODE, fish222_2010$CLAIM)
#adding a column for landing, which is harvest + claim
fish222_2010$LANDING<-(fish222_2010$HARVEST + fish222_2010$CLAIM)

##
table(fish222_2010$SP_CODE, fish222_2010$LANDING)
