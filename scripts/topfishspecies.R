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
require(dplyr)

#read in data set 
fishdata<-read.csv("PydioData/MRIP/data_outputs/clean_MRIP/mrip_species_zip_site_2004_2017.csv")
str(fishdata)
str(fishdata$INTSITE)
#add a column for landing, which is harvest + claim
fishdata$LANDING<-(fishdata$HARVEST + fishdata$CLAIM)

#use aggregate to sum the landing, harvest and claim data by SP_CODE
#separately for each year and fishing site 
L<-aggregate(LANDING ~SP_CODE + YEAR + INTSITE + ST, data = fishdata, sum)
H<-aggregate(HARVEST~SP_CODE + YEAR + INTSITE, data = fishdata, sum)
C<-aggregate(CLAIM~SP_CODE + YEAR + INTSITE, data = fishdata, sum)

##mergespecies ID with species codes to decode meaning
speciesid<-read.csv("PydioData/MRIP/raw/MRIP_support_docs/species_list.csv")
head(speciesid)

Lfull<-merge(x = L, y = speciesid, by.x="SP_CODE", by.y="sp_code", all=FALSE)


#For the afs presentation, we are focusing on sites 222 and 306 in NoLA, 
L222<-subset(Lfull, INTSITE == 222)
L222_2010<-subset(L222, YEAR==2010)
L222<-subset(Lfull, INTSITE == 222)
L222_2011<-subset(L222, YEAR==2010)

L306<-subset(Lfull, INTSITE == 306)

#WRITE A CSV for top species in 2010 for site 222
write.csv(L222_2010, "PydioData/env/data_outputs/top_species/nolasite222_2010.csv")
write.csv(L222, "PydioData/env/data_outputs/top_species/nolasite222sp.csv")
#write csv ofr overall merged landings file
write.csv(Lfull, "PydioData/env/data_outputs/top_species/landings_species_allsites.csv")

#same for site 306
write.csv(L306, "PydioData/env/data_outputs/top_species/nolasite306sp.csv")
