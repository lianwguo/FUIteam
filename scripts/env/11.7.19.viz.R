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

## subset trophic levels

ggplot(subset(tcLAsum,INTSITE %in% c("306a")), aes(testStatus100mi, trophicCode, col = testStatus100mi)) + 
  geom_boxplot() + 
  ylab("Trophic Level") +
  scale_y_continuous(breaks = c(2, 2.5, 3, 3.5, 4, 4.5), limits = c(2, 4.6)) +
  theme_light() + theme(axis.title.x = element_blank()) + 
  facet_wrap(~ INTSITE) + 
  stat_compare_means(label.x.npc = .4, label.y.npc = 1, label = "p.signif",
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) +  
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Testing Status",
                     breaks=c("Not Tested", "Tested"))

ggplot(subset(tcFLsum,INTSITE %in% c("769a")), aes(testStatus100mi, trophicCode, col = testStatus100mi)) + 
  geom_boxplot() + 
  ylab("Trophic Level") +
  scale_y_continuous(breaks = c(2, 2.5, 3, 3.5, 4, 4.5), limits = c(2, 4.6)) +
  theme_light() + theme(axis.title.x = element_blank()) + 
  facet_wrap(~ INTSITE) + 
  stat_compare_means(label.x.npc = .4, label.y.npc = 1, label = "p.signif",
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) +  
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                     name="Testing Status",
                     breaks=c("Not Tested", "Tested"))

#### SE

View(tcLAsum)
View(tcFLsum)
str(finLHSElaA)
finLHSElaB
LHSEflA
LHSEflB

###focus on the results of the SE team's analysis

#florida
#variables on levels of racial minority, high school educational attainment, 
#households with one vehicle only, and households on SNAP were significantly 
#associated with fishing levels at the 0.05 level. Median income and households 
#with no vehicles were significant at p < 0.08

ggplot(LHSEflA, aes(racial_minority_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 50, label.y = 4.25) +
  stat_regline_equation(label.x = 60, label.y = 4.22) 
##more racial minority = lower trophic level *
ggplot(LHSEflB, aes(racial_minority_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 50, label.y = 4.15) +
  stat_regline_equation(label.x = 60, label.y = 4.12)
#more racial minority = higher trophic level ***

ggplot(LHSEflA, aes(education_HS_GED_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 30, label.y = 4.25) +
  stat_regline_equation(label.x = 30, label.y = 4.22) 
##higher education = lower trophic level
ggplot(LHSEflB, aes(education_HS_GED_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 30, label.y = 4.25) +
  stat_regline_equation(label.x = 30, label.y = 4.22)
#higher education = lower trophic level

ggplot(LHSEflA, aes(one_vehicle_percent_hhlds,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 30, label.y = 4.25) +
  stat_regline_equation(label.x = 30, label.y = 4.22) 
##more households with 1 car = lower trophic level ***
ggplot(LHSEflB, aes(one_vehicle_percent_hhlds,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 30, label.y = 4.25) +
  stat_regline_equation(label.x = 30, label.y = 4.22)
#more households with 1 car = lower trophic level ***

ggplot(LHSEflA, aes(food_stamp_percent_hhlds,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 15, label.y = 4.25) +
  stat_regline_equation(label.x = 15, label.y = 4.22) 
##more food stamps = higher trophic level
ggplot(LHSEflB, aes(food_stamp_percent_hhlds,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 15, label.y = 4.25) +
  stat_regline_equation(label.x = 15, label.y = 4.22)
#flat


##louisiana
#foreign-born people, high school educational attainment, and households 
#with no vehicles were significantly related to fishing levels at the 0.05 level

ggplot(finLHSElaA, aes(foreign_born_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 15, label.y = 4.25) +
  stat_regline_equation(label.x = 15, label.y = 4.2) 
#more foreign born = lower trophic level ***
ggplot(finLHSElaB, aes(foreign_born_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 10, label.y = 4.25) +
  stat_regline_equation(label.x = 10, label.y = 4.2) 
#more foreign born = lower trophic level **

ggplot(finLHSElaA, aes(education_HS_GED_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 30, label.y = 4.25) +
  stat_regline_equation(label.x = 30, label.y = 4.20) 
##higher education = higher trophic level
ggplot(finLHSElaB, aes(education_HS_GED_percent_pop,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 30, label.y = 4.25) +
  stat_regline_equation(label.x = 30, label.y = 4.22)
#higher education = higher trophic level

ggplot(finLHSElaA, aes(no_vehicles_percent_hhlds,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 30, label.y = 4.25) +
  stat_regline_equation(label.x = 30, label.y = 4.20) 
##less vehicles = higher trophic level **
ggplot(finLHSElaB, aes(no_vehicles_percent_hhlds,trophicCode)) +
  geom_smooth(method='lm') +
  stat_cor(label.x = 20, label.y = 4.25) +
  stat_regline_equation(label.x = 20, label.y = 4.22)
#less vehicles = higher trophic level *










## stepwise linear regression

# Specify a null model with no predictors
null_modelFLA <- glm(trophicCode ~ 1, data = LHSEflA, family = gaussian)

# Specify the full model using all of the potential predictors
full_modelFLA <- glm(trophicCode ~ racial_minority_percent_pop + foreign_born_percent_pop +
                       poverty_percent_famil + median_income_dollars_hhlds_percent_scaled +
                       no_vehicles_percent_hhlds + one_vehicle_percent_hhlds +
                       food_stamp_percent_hhlds + education_HS_GED_percent_pop, 
                     data = LHSEflA, family = gaussian)

# Use a forward stepwise algorithm to build a parsimonious model
step_modelFLA <- step(null_modelFLA, scope = list(lower = null_modelFLA, upper = full_modelFLA), 
                         direction = "both")

# Estimate the stepwise donation probability
step_prob <- step_modelFLA

# Specify a null model with no predictors
null_modelFLB <- glm(trophicCode ~ 1, data = LHSEflB, family = gaussian)

# Specify the full model using all of the potential predictors
full_modelFLB <- glm(trophicCode ~ racial_minority_percent_pop + foreign_born_percent_pop +
                       poverty_percent_famil + median_income_dollars_hhlds_percent_scaled +
                       no_vehicles_percent_hhlds + one_vehicle_percent_hhlds +
                       food_stamp_percent_hhlds + education_HS_GED_percent_pop, 
                     data = LHSEflB, family = gaussian)

# Use a forward stepwise algorithm to build a parsimonious model
step_modelFLB <- step(null_modelFLB, scope = list(lower = null_modelFLB, upper = full_modelFLB), 
                      direction = "both")

# Estimate the stepwise donation probability
step_prob <- step_modelFLB

# Specify a null model with no predictors
null_modelLAA <- glm(trophicCode ~ 1, data = finLHSElaA, family = gaussian)

# Specify the full model using all of the potential predictors
full_modelLAA <- glm(trophicCode ~ racial_minority_percent_pop + foreign_born_percent_pop +
                       poverty_percent_famil + median_income_dollars_hhlds_percent_scaled +
                       no_vehicles_percent_hhlds + one_vehicle_percent_hhlds +
                       food_stamp_percent_hhlds + education_HS_GED_percent_pop, 
                     data = LHSElaA, family = gaussian)

# Use a forward stepwise algorithm to build a parsimonious model
step_modelLAA <- step(null_modelLAA, scope = list(lower = null_modelLAA, upper = full_modelLAA), 
                      direction = "both")

# Estimate the stepwise donation probability
step_prob <- step_modelLAA

# Specify a null model with no predictors
null_modelLAB <- glm(trophicCode ~ 1, data = finLHSElaB, family = gaussian)

# Specify the full model using all of the potential predictors
full_modelLAB <- glm(trophicCode ~ racial_minority_percent_pop + foreign_born_percent_pop +
                       poverty_percent_famil + median_income_dollars_hhlds_percent_scaled +
                       no_vehicles_percent_hhlds + one_vehicle_percent_hhlds +
                       food_stamp_percent_hhlds + education_HS_GED_percent_pop, 
                     data = LHSElaB, family = gaussian)

# Use a forward stepwise algorithm to build a parsimonious model
step_modelLAB <- step(null_modelLAB, scope = list(lower = null_modelLAB, upper = full_modelLAB), 
                      direction = "both")

# Estimate the stepwise donation probability
step_prob <- step_modelLAB

##########################
#map visualizations

plot(loc_NOLA,
     col = "yellow",
     main = "TOP MRIP sites, 2005 - 2009")
plot(loc_LA,
     add = TRUE)
plot(loc_LAmripA,
     pch = 4,
     cex = 1,
     col = "green",
     add = TRUE)

