##Merging landing sites with lat/long from site registry for each state
##written by Lian Guo

# Read the NOLA sites with landings for time A (2005-2009).csv file
LAmripA <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_SiteByLandingTimeA.csv"), 
                    stringsAsFactors = FALSE)
LAmripA

# Read the NOLA sites with landings for time B (2010-2013).csv file
LAmripB <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_SiteByLandingTimeB.csv"), 
                    stringsAsFactors = FALSE)
LAmripB

# Read the FL sites with landings for time A (2005-2010, not 2008).csv file
FLmripA <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_SiteByLandingTimeA.csv"), 
                    stringsAsFactors = FALSE)
FLmripA

# Read the FL sites with landings for time B (2011-2015).csv file
FLmripB <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_SiteByLandingTimeB.csv"), 
                    stringsAsFactors = FALSE)
FLmripB

# Read in site register for florida (based on cleaned up version, not raw)
FLsiteReg <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_CutSiteReg.csv"), 
                      stringsAsFactors = FALSE)
str(FLsiteReg)

# Read in site register for louisiana (based on cleaned up version, not raw)
LAsiteReg <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_CutSiteReg.csv"), 
                      stringsAsFactors = FALSE)
str(LAsiteReg)

#left join to add lat long to top landing sites for florida (keeps all of x, adds other columns of y)
FLmripAfinal <- merge(x = FLmripA, y = FLsiteReg, by.x = "SiteNum", by.y = "SiteNum", all.x = TRUE)
FLmripBfinal <- merge(x = FLmripB, y = FLsiteReg, by.x = "SiteNum", by.y = "SiteNum", all.x = TRUE)

write.csv(FLmripAfinal, "~/FUIteam/PydioData/env/data_outputs/FLmripA.csv") #write landing sites through years with latlongs
write.csv(FLmripBfinal, "~/FUIteam/PydioData/env/data_outputs/FLmripB.csv") 

#left join to add lat long to top landing sites for louisiana (keeps all of x, adds other columns of y)
LAmripAfinal <- merge(x = LAmripA, y = LAsiteReg, by.x = "SiteNum", by.y = "SiteNum", all.x = TRUE)
LAmripBfinal <- merge(x = LAmripB, y = LAsiteReg, by.x = "SiteNum", by.y = "SiteNum", all.x = TRUE)

write.csv(LAmripAfinal, "~/FUIteam/PydioData/env/data_outputs/LAmripA.csv") #write landing sites through years with latlongs
write.csv(LAmripBfinal, "~/FUIteam/PydioData/env/data_outputs/LAmripB.csv")

#case study sites for AFS will be 
#29.15833333, -90.17831667 site 222
#30.03153333, -90.03693333 site 306

#read in mercury sites for 2010. there is a data gap in mercury testing until 2016.
LAmerc2010 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAmerc2010.csv"), 
                      stringsAsFactors = FALSE)
head(LAmerc2010)
unique(LAmerc2010$Water.Body.Site)

#read in mercury site register from Al Hendrichs, includes lat/long
LAmercSites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "NOLAmercSites.csv"), 
                       stringsAsFactors = FALSE)
head(LAmercSites)

#add lat/long from site register to the mercury data
LAmerc2010sp <- merge(x = LAmerc2010, y = LAmercSites, by.x = "Water.Body.Site", by.y = "SITE_Num", all.x = TRUE)
head(LAmerc2010sp)
str(LAmerc2010sp)
LAmerc2010sp[,14:20] <- NULL
write.csv(LAmerc2010sp, "~/FUIteam/PydioData/env/data_outputs/LAmerc2010sp.csv")

#read in wq sites for 2010. 
LAwq2010 <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwq2010.csv"), 
                       stringsAsFactors = FALSE)
head(LAwq2010)

#read in water quality site register, includes lat/long
LAwqSites <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LAwqSiteReg.csv"), 
                        stringsAsFactors = FALSE)
head(LAwqSites)
str(LAwqSites)
LAwqSites[,6:11] <- NULL

#add lat/long from site register to the wq data
LAwq2010sp <- merge(x = LAwq2010, y = LAwqSites, by.x = "Site.", by.y = "SITE_ID", all.x = TRUE)
head(LAwq2010sp)
str(LAwq2010sp)
write.csv(LAwq2010sp, "~/FUIteam/PydioData/env/data_outputs/LAwq2010sp.csv")

