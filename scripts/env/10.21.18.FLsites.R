##FL top landing sites
#Timepoint A - 2005-2010
#769, 770, 742, 615, 712
#Timepoint B - 2011-2015
#770, 3802, 769, 614, 241

###last data available was 2004, which is not within either of our time frames. Should use that 
#information anyway? 

#load
FLmerc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_mercury.csv"), 
                      stringsAsFactors = FALSE)
head(FLmerc)

FLmercLoc <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FLmercLoc.csv"), 
                   stringsAsFactors = FALSE)
head(FLmercLoc)
