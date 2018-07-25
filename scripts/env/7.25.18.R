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
