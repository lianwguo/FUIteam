#FL 25 miles mercury

FL25mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_25mi.csv"), 
                   stringsAsFactors = FALSE)
LA25mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_25mi.csv"), 
                   stringsAsFactors = FALSE)
FL100mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_100mi.csv"), 
                   stringsAsFactors = FALSE)
LA100mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_100mi.csv"), 
                   stringsAsFactors = FALSE)
LA25miSW <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_25miSW.csv"), 
                   stringsAsFactors = FALSE)
LA100miSW <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_100miSW.csv"), 
                    stringsAsFactors = FALSE)

head(FL25mi)
str(FL25mi)

#looking at how many sites each spatial scale has
uniLA25miSW <- LA25miSW[!duplicated(LA25miSW[1:2]),]
uniLA100miSW <- LA100miSW[!duplicated(LA100miSW[1:2]),]
aggregate(Water.Body.Site~Site, uniLA100miSW, FUN=function(x) c(mean=mean(x), count=length(x)))
subset(uniLA100miSW, uniLA100miSW$Site == 150)

#narrows down duplicated sites
uniFL25mi <- ddply(FL25mi, c("Site","LatDD","LonDDr"), head, 1)
uniFL100mi <- ddply(FL100mi, c("Site","LatDD","LonDDr"), head, 1)
#subset by each site to see how many testing sites there are for each fishing site
str(subset(uniFL100mi, uniFL100mi$Site == 3802))

#aggregating mercury results #note that sites with same nearby mercury results are represented multiple times, may affect average
LA25AveHgSp <- aggregate(Mercury.Results~Common.Name, data=LA25miSW, FUN=function(x) c(mean=mean(x), count=length(x)))
LA100AveHgSp <- aggregate(Mercury.Results~Common.Name, data=LA100miSW, FUN=function(x) c(mean=mean(x), count=length(x)))
LA100AveHgSp
write.csv(LA25AveHgSp, "~/FUIteam/PydioData/env/data_outputs/LA25AveHgSp.csv")
write.csv(LA100AveHgSp, "~/FUIteam/PydioData/env/data_outputs/LA100AveHgSp.csv")

FL25AveHgSp <- aggregate(Hg~Species, data=FL25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
FL100AveHgSp <- aggregate(Hg~Species, data=FL100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
FL100AveHgSp
write.csv(FL25AveHgSp, "~/FUIteam/PydioData/env/data_outputs/FL25AveHgSp.csv")
write.csv(FL100AveHgSp, "~/FUIteam/PydioData/env/data_outputs/FL100AveHgSp.csv")

#parring down samples to remove pseudoreplication
noDupLA25mi <- ddply(LA25miSW, c("Water.Body.Site","Collection.Date","Common.Name", "Average.Fish.Weight..grams."), head, 1)
noDupLA100mi <- ddply(LA100miSW, c("Water.Body.Site","Collection.Date","Common.Name", "Average.Fish.Weight..grams."), head, 1)
#average mercury
noDupLA25AveHgSp <- aggregate(Mercury.Results~Common.Name, data=noDupLA25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
noDupLA100AveHgSp <- aggregate(Mercury.Results~Common.Name, data=noDupLA100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
write.csv(noDupLA25AveHgSp, "~/FUIteam/PydioData/env/data_outputs/noDupLA25AveHgSp.csv")
write.csv(noDupLA100AveHgSp, "~/FUIteam/PydioData/env/data_outputs/noDupLA100AveHgSp.csv")
#average size
LA25AveLengthSp <- aggregate(Average.Fish.Length..cm.~Common.Name, data=noDupLA25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
LA100AveLengthSp <- aggregate(Average.Fish.Length..cm.~Common.Name, data=noDupLA100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
write.csv(LA25AveLengthSp, "~/FUIteam/PydioData/env/data_outputs/LA25AveLengthSp.csv")
write.csv(LA100AveLengthSp, "~/FUIteam/PydioData/env/data_outputs/LA100AveLengthSp.csv")
#average weight
LA25AveWtSp <- aggregate(Average.Fish.Weight..grams.~Common.Name, data=noDupLA25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
LA100AveWtSp <- aggregate(Average.Fish.Weight..grams.~Common.Name, data=noDupLA100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
write.csv(LA25AveWtSp, "~/FUIteam/PydioData/env/data_outputs/LA25AveWtSp.csv")
write.csv(LA100AveWtSp, "~/FUIteam/PydioData/env/data_outputs/LA100AveWtSp.csv")
#now florida
noDupFL25mi <- ddply(FL25mi, c("New_date","Species","SL"), head, 1) # didn't include bottle in original file FL25mi but there was only one fish anyway
noDupFL100mi <- ddply(FL100mi, c("Bottle", "New_date","Species"), head, 1)
noDupFL25AveHgSp <- aggregate(Hg~Species, data=noDupFL25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
noDupFL100AveHgSp <- aggregate(Hg~Species, data=noDupFL100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
write.csv(noDupFL25AveHgSp, "~/FUIteam/PydioData/env/data_outputs/noDupFL25AveHgSp.csv")
write.csv(noDupFL100AveHgSp, "~/FUIteam/PydioData/env/data_outputs/noDupFL100AveHgSp.csv")
# by length
FL25AveLengthSp <- aggregate(TL~Species, data=noDupFL25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
FL100AveLengthSp <- aggregate(TL~Species, data=noDupFL100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
write.csv(FL25AveLengthSp, "~/FUIteam/PydioData/env/data_outputs/FL25AveLengthSp.csv")
write.csv(FL100AveLengthSp, "~/FUIteam/PydioData/env/data_outputs/FL100AveLengthSp.csv")
# no weights now but if somehow this comes up later...
#FL25AveWtSp <- aggregate(Whole_weight~Species, data=noDupFL25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
#FL100AveWtSp <- aggregate(Whole_weight~Species, data=noDupFL100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
#write.csv(FL25AveWtSp, "~/FUIteam/PydioData/env/data_outputs/FL25AveWtSp.csv")
#write.csv(FL100AveWtSp, "~/FUIteam/PydioData/env/data_outputs/FL100AveWtSp.csv")

###by length
ggplot(data = LA25miSW, aes(x = Average.Fish.Length..cm., y = Mercury.Results, color = Common.Name)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

###BY WEIGHT
ggplot(data = LA25miSW, aes(x = Average.Fish.Weight..grams., y = Mercury.Results, color = Common.Name)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

ggplot(data = LA100miSW, aes(x = Average.Fish.Length..cm., y = Mercury.Results, color = Common.Name)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

head(FL25mi)
FL25AveHgSp <- aggregate(Hg~Species, data=FL25mi, FUN=function(x) c(mean=mean(x), count=length(x)))
FL100AveHgSp <- aggregate(Hg~Species, data=FL100mi, FUN=function(x) c(mean=mean(x), count=length(x)))
write.csv(FL25AveHgSp, "~/FUIteam/PydioData/env/data_outputs/FL25AveHgSp.csv")
write.csv(FL100AveHgSp, "~/FUIteam/PydioData/env/data_outputs/FL100AveHgSp.csv")


ggplot(data = FL25mi, aes(x = TL, y = Hg, color = Species)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

ggplot(data = FL100mi, aes(x = TL, y = Hg, color = Species)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

