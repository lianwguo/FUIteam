#work with files in local projection to calculate
#loc_LAmercA (mercury sites 2005-2009)
#loc_LAsite306
#loc_LAsite222
#loc_NOLA (msa)

#using sp package
#distances based on original projection, site 222
crs(loc_LAmercA)
merc222dist <- spDistsN1(loc_LAmercA, loc_LAsite222, longlat = FALSE) #in m
merc222dist

unique(merc222dist)
which(merc222dist < 8046.72) #within 5 miles #returns 0, 
which(merc222dist < 16093.4) #within 10 miles #returns 0
which(merc222dist < 40233.6) #within 25 miles #returns 3 sites, 44 observations
which(merc222dist < 80467.2) #within 50 miles #returns 36 sites, 493 observations

#make vector of indexes found within given distance in last step
Index25mi <- which(merc222dist < 40233.6)
Index50mi <- which(merc222dist < 80467.2) 

#subset sites of interest within 25 miles, using vectors of indexes
subset25miHg222 <- loc_LAmercA[Index25mi,]
subset25miHg222
unique(subset25miHg222$Water.Body.Site)
write.csv(subset25miHg222, "~/FUIteam/PydioData/env/data_outputs/subset25miHg222.csv")
unique(subset25miHg222$CollectYear)
#only collected from these closest sites in in 2007.
#Test dates are all early in the year (Feb to July)

#trying to get some summary stats - going to just export as csv and look at pivot tables - first change back to dataframe
CloseMerc222 <- as.data.frame(subset25miHg222)
head(CloseMerc222)
str(CloseMerc222)
class(CloseMerc222)
write.csv(CloseMerc222, "~/FUIteam/PydioData/env/data_outputs/CloseMerc222.csv")

#table of sampling frequency for each site over 2005-2009 time period
samplFreq222 <- with(CloseMerc222, table(Water.Body.Site, Collection.Date))
samplFreq222
write.csv(samplFreq222, "~/FUIteam/PydioData/env/data_outputs/samplFreq222.csv")

#species frequency for sites per year
spFreq222 <- with(CloseMerc222, table(Water.Body.Site, Common.Name, CollectYear))
spFreq222
write.csv(spFreq222, "~/FUIteam/PydioData/env/data_outputs/spFreq222.csv")


#now 50 miles
subset50miHg222 <- loc_LAmercA[Index50mi,]
subset50miHg222
unique(subset50miHg222$Water.Body.Site)


#plot - based on files with local projections
plot(loc_NOLA,
     col = "yellow",
     main = "Merc Sites within 25 miles of Site 222")
plot(loc_StateBound,
     add = TRUE)
plot(subset25miHg222,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(subset50miHg222,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)

#IT WORKED! saved images as site222_25miMerc.eps and site222_50miMerc.eps

##Now with site 306
merc306dist <- spDistsN1(loc_LAmercA, loc_LAsite306, longlat = FALSE) #in m
merc306dist
unique(merc306dist)

which(merc306dist < 8046.72) #within 5 miles #returns 0, 
which(merc306dist < 16093.4) #within 10 miles #returns 0
which(merc306dist < 40233.6) #within 25 miles #returns 9 sites, 134 observations
which(merc306dist < 80467.2) #within 50 miles #returns 44 sites, 605 observations

#make vector of indexes found within given distance in last step
Index25mi306 <- which(merc306dist < 40233.6)
Index50mi306 <- which(merc306dist < 80467.2) 

#subset sites of interest within 25 miles, using vectors of indexes
subset25miHg306 <- loc_LAmercA[Index25mi306,]
head(subset25miHg306)
unique(subset25miHg306$Water.Body.Site)
unique(subset25miHg306$Collection.Date)
unique(subset25miHg306$CollectYear)
#only collected from these closest sites in in 2006, 2007.
#Test dates are all early in the year (Feb to July)

#trying to get some summary stats - going to just export as csv and look at pivot tables - first change back to dataframe
CloseMerc306 <- as.data.frame(subset25miHg306)
head(CloseMerc306)
str(CloseMerc306)
class(CloseMerc306)
write.csv(CloseMerc306, "~/FUIteam/PydioData/env/data_outputs/CloseMerc306.csv")

#table of sampling frequency for each site over 2005-2009 time period
samplFreq306 <- with(CloseMerc306, table(Water.Body.Site, Collection.Date))
samplFreq306
write.csv(samplFreq306, "~/FUIteam/PydioData/env/data_outputs/samplFreq306.csv")

#species frequency for sites per year
spFreq306 <- with(CloseMerc306, table(Water.Body.Site, Common.Name, CollectYear))
spFreq306
write.csv(spFreq306, "~/FUIteam/PydioData/env/data_outputs/spFreq306.csv")

#now 50 miles
subset50miHg306 <- loc_LAmercA[Index50mi306,]
subset50miHg306
unique(subset50miHg306$Collection.Date)
#contains data for whole time series. 37 distinct collection dates. 

#plot - based on files with local projections
plot(loc_NOLA,
     col = "yellow",
     main = "Merc Sites within 25 miles of Site 306")
plot(loc_StateBound,
     add = TRUE)
plot(subset25miHg306,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAsite306,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(subset50miHg306,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)
