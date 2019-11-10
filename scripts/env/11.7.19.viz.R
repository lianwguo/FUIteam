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
library(RColorBrewer)
library(wesanderson)

#boxplot
ggplot(testSum, aes(x=city, y=percSpTested, col = dist))  + 
  geom_boxplot() + 
  ylim(0,100) +
  xlab("City") + 
  ylab("Percent Species Tested") + 
  facet_wrap(~ timeframe) + 
  theme_light() + 
  scale_color_manual(values=c("#E69F00", "#56B4E9")) + 
  labs(col='Distance') 


#bubbleplot
ggplot(testSum, aes(x=city, y=percSpTested, col = dist, size = numSp)) +
  geom_point(alpha=0.7, shape=21, stroke = 1) +
  ylim(0,100) +
  xlab("City") + 
  ylab("Percent Species Tested") +
  scale_size(range = c(.1, 10), name="Total Species Landed") +
  facet_wrap(~ timeframe) + 
  theme_light() + 
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                                      name="Distance",
                                      breaks=c("25mi", "100mi"),
                                      labels=c("within 40 km", "within 161 km")) +
  scale_x_discrete(breaks=c("NOLA", "TFL"),
                     labels=c("New Orleans", "Tampa")) +
  labs(col='Distance') 

ggplot(testSum, aes(x=city, y=percLandTested, col = dist, size = totRelLandings)) +
  geom_point(alpha=0.7, shape=21, stroke = 1) +
  ylim(0,100) +
  xlab("City") + 
  ylab("Percent Landings Tested") +
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) +
  scale_size(range = c(.1, 10), name="Total Relative Landings") +
  facet_wrap(~ timeframe) + theme_light()+ 
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Distance",
                     breaks=c("25mi", "100mi"),
                     labels=c("within 40 km", "within 161 km")) +
  scale_x_discrete(breaks=c("NOLA", "TFL"),
                   labels=c("New Orleans", "Tampa")) +
  labs(col='Distance') 

ggplot(testSum, aes(x=city, y=percBiomassTested, col = dist, size = totRelBiomass)) +
  geom_point(alpha=0.7, shape=21, stroke = 1) +
  ylim(0,100) +
  xlab("City") + 
  ylab("Percent Biomass Tested") +
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) +
  scale_size(range = c(.1, 10), name="Total Biomass Landed") +
  facet_wrap(~ timeframe) + theme_light() + 
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Distance",
                     breaks=c("25mi", "100mi"),
                     labels=c("within 40 km", "within 161 km")) +
  scale_x_discrete(breaks=c("NOLA", "TFL"),
                   labels=c("New Orleans", "Tampa")) +
  labs(col='Distance') 

#size

ggplot(compLAa, aes(x=spName, y=aveWt_kg, col = source))  + 
  geom_boxplot() + 
  xlab("Species") + 
  ylab("Average Weight (kg)") +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) + coord_flip() + 
  theme_light() + 
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Source",
                     breaks=c("MERC", "MRIP"),
                     labels=c("Tested Fish", "Landed Fish")) +
  theme(legend.position=c(.75,.8))
compLAa$time <- "A"

results.LAa <-ddply(compLAa,.(spName),function(x) summarize(x,
                                                            Pvalue=t.test(aveWt_kg~source,data=x,na.rm=TRUE,paired=F)$p.value,
                                                            Estimate.MERC=t.test(aveWt_kg~source,data=x,na.rm=TRUE,paired=F)$estimate[1],
                                                            Estimate.MRIP=t.test(aveWt_kg~source,data=x,na.rm=TRUE,paired=F)$estimate[2]))
results.LAa$adjP <- p.adjust(results.LAa$Pvalue, method = 'fdr', n = length(results.LAa$Pvalue))
results.LAa$meanDiff <- (results.LAa$Estimate.MERC - results.LAa$Estimate.MRIP)
results.LAa$site <- "NOLA"
results.LAa$time <- "A"


ggplot(compLAb, aes(x=spName, y=aveWt_kg, col = source))  + 
  geom_boxplot() + 
  xlab("Species") + 
  ylab("Average Weight (kg)") +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) + coord_flip() + 
  theme_light() + 
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Source",
                     breaks=c("MERC", "MRIP"),
                     labels=c("Tested Fish", "Landed Fish")) +
  theme(legend.position=c(.75,.8)) 
compLAb$time <- "B"

results.LAb <-ddply(compLAb,.(spName),function(x) summarize(x,
                                                            Pvalue=t.test(aveWt_kg~source,data=x,na.rm=TRUE,paired=F)$p.value,
                                                            Estimate.MERC=t.test(aveWt_kg~source,data=x,na.rm=TRUE,paired=F)$estimate[1],
                                                            Estimate.MRIP=t.test(aveWt_kg~source,data=x,na.rm=TRUE,paired=F)$estimate[2]))
results.LAb$adjP <- p.adjust(results.LAb$Pvalue, method = 'fdr', n = length(results.LAa$Pvalue))
results.LAb$meanDiff <- (results.LAb$Estimate.MERC - results.LAb$Estimate.MRIP)
results.LAb$site <- "NOLA"
results.LAb$time <- "B"

compLAbS <- left_join(compLAb,results.LAb, by = "spName", all.x = TRUE)


comboSizeLA <- rbind(compLAa, compLAb)
str(comboSizeLA)

ggplot(comboSizeLA, aes(x=spName, y=aveWt_kg, col = source)) +
  geom_boxplot() +
  xlab("Species") + 
  ylab("Average Weight (kg)") +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.x = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain")) + 
  theme(legend.text=element_text(size = 12, face = "plain"),
        legend.title=element_text(size = 14, face = "bold")) + coord_flip() + 
  theme_light() + 
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Source",
                     breaks=c("MERC", "MRIP"),
                     labels=c("Tested Fish", "Landed Fish")) +
  facet_wrap(~ time)

##trophic levels

ggplot(tcLAsum, aes(testStatus100mi, trophicCode, col = testStatus100mi)) + 
  geom_boxplot() + 
  ylab("Trophic Level") +
  scale_y_continuous(breaks = c(2, 2.5, 3, 3.5, 4, 4.5), limits = c(2, 4.6)) +
  theme_light() + theme(axis.title.x = element_blank()) + 
  facet_wrap(~ time) + 
  stat_compare_means(label.x.npc = .4, label.y.npc = 1, label = "p.signif",
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) +  
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Testing Status",
                     breaks=c("Not Tested", "Tested"))

ggplot(tcFLsum, aes(testStatus100mi, trophicCode, col = testStatus100mi)) + 
  geom_boxplot() + 
  ylab("Trophic Level") +
  scale_y_continuous(breaks = c(2, 2.5, 3, 3.5, 4, 4.5), limits = c(2, 4.6)) +
  theme_light() + theme(axis.title.x = element_blank()) + 
  facet_wrap(~ time) + 
  stat_compare_means(label.x.npc = .4, label.y.npc = 1, label = "p.signif",
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) + 
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Testing Status",
                     breaks=c("Not Tested", "Tested"))

#### SE

str(tcFLsum)
ggplot(tcFLsum, aes(racial_minority_percent_pop, trophicCode, col = testStatus100mi)) + 
  geom_point()
