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
#add a column for landing, which is harvest + claim
fishdata$LANDING<-(fishdata$HARVEST + fishdata$CLAIM)

#use aggregate to sum the landing, harvest and claim data by SP_CODE
#separately for each year and fishing site 
L<-aggregate(LANDING ~SP_CODE + YEAR + INTSITE, data = fishdata, sum)
H<-aggregate(HARVEST~SP_CODE + YEAR + INTSITE, data = fishdata, sum)
C<-aggregate(CLAIM~SP_CODE + YEAR + INTSITE, data = fishdata, sum)

species<-rbind(L, H, C)
#merge these files together

#For the afs presentation, we are focusing on sites 222 and 306 in NoLA, 
L222<-subset(L, INTSITE == 222)
L222_2010<-subset(L222, YEAR==2010)

summary(L222$YEAR)

