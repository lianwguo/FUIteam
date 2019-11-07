###11/7/19 making visualizations for ESP manuscript
###L. Guo

testSum <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", 
                                 "testedSummary.csv"), 
                       stringsAsFactors = FALSE)
str(testSum)
testSum$dist <- as.factor(testSum$dist)
testSum$fishSite <- as.character(testSum$fishSite)

testSum$dist = factor(testSum$dist, levels=levels(testSum$dist)[c(2,1)])
?factor
library(ggplot2)

#boxplot
ggplot(testSum, aes(x=city, y=percSpTested, fill = dist))  + 
  geom_boxplot() + 
  xlab("City") + 
  ylab("Percent Species Tested") +
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) + 
  facet_wrap(~ timeframe) + theme_light()

#bubbleplot
ggplot(testSum, aes(x=city, y=percSpTested, col = dist, size = numSp)) +
  geom_point(alpha=0.7, shape=21, stroke = 1) +
  xlab("City") + 
  ylab("Percent Species Tested") +
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) +
  scale_size(range = c(.1, 10), name="Total Species Landed") +
  facet_wrap(~ timeframe) + theme_light()

ggplot(testSum, aes(x=city, y=percLandTested, col = dist, size = totRelLandings)) +
  geom_point(alpha=0.7, shape=21) +
  xlab("City") + 
  ylab("Percent Landings Tested") +
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) +
  scale_size(range = c(.1, 10), name="Total Relative Landings") +
  facet_wrap(~ timeframe)

ggplot(testSum, aes(x=city, y=percBiomassTested, col = dist, size = totRelBiomass)) +
  geom_point(alpha=0.7, shape=21) +
  xlab("City") + 
  ylab("Percent Biomass Tested") +
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) +
  scale_size(range = c(.1, 10), name="Total Biomass Landed") +
  facet_wrap(~ timeframe)
