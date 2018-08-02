#work with files in local projection to calculate
#loc_LAmercA (mercury sites 2005-2009)
#loc_LAsite306
#loc_LAsite222
#loc_NOLA (msa)

#using sp package
merc222dist <- spDistsN1(loc_LAmercA, loc_LAsite222, longlat = FALSE) #in ft
merc222dist
unique(merc222dist)
which(merc222dist < 26400) #returns 0
which(merc222dist < 52800) #returns a decent number
Index10mi <- which(merc222dist < 52800)
#subset sites of interest within 10 miles
subset10miHg <- loc_LAmercA[Index10mi,]
subset10miHg

#try 25 miles
Index25mi <- which(merc222dist < 132000)
subset25miHg <- loc_LAmercA[Index25mi,]
subset25miHg
#883 observations (mercury measurements)
unique(subset25miHg$Water.Body.Site)
#65 unique sites

#plot - based on files spatialized in 7.26.18 file
plot(loc_NOLA,
     col = "yellow",
     main = "Merc Sites within 10 miles of Site 222")
plot(loc_StateBound,
     add = TRUE)
plot(subset10miHg,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)
plot(loc_LAsite222,
     cex = 2,
     col = "purple",
     add = TRUE)
plot(subset25miHg,
     pch = 3,
     cex = 1,
     col = "blue",
     add = TRUE)

#IT WORKED! saves as site222_10mile
unique(subset10miHg$Water.Body.Site)
# 9 sites in this time frame in the region, within 10 miles
#118 independent observations for these sites in the time frame

#checking out real map, seems like the distances may not be correct. Will need to check projections and metrics 

