
library(dplyr)
library(data.table)
library(plyr)

detach(package:plyr)

res.FLa <- compFLa3 %>% 
  group_by(spName, source) %>%
  summarise(medWt_kg = median(aveWt_kg), n = n())
res.FLa <- as.data.frame(res.FLa)
dFL <- dcast(setDT(res.FLa), spName ~ source, value.var = c('medWt_kg','n'))
dFL$medDiff <- dFL$medWt_kg_MERC - dFL$medWt_kg_MRIP

mean(subset(compFLa3, compFLa3$spName == "GRAY SNAPPER" & 
              compFLa3$source == "MRIP")$aveWt_kg)
median(subset(compFLa3, compFLa3$spName == "GRAY SNAPPER" & 
                compFLa3$source == "MRIP")$aveWt_kg)

results.FLa <-ddply(compFLa3,.(spName),
                    function(x) summarize(x,
                    Pvalue=wilcox.test(aveWt_kg~source,data=x,na.rm=TRUE,
                                       correct=FALSE,paired=F, exact=T)$p.value, 
                    W=wilcox.test(aveWt_kg~source,data=x,na.rm=TRUE,
                                       correct=FALSE,paired=F, exact=T)$statistic))

results.FLa$adjP <- p.adjust(results.FLa$Pvalue, method = 'fdr', n = length(results.FLa$Pvalue))
results.FLa <- left_join(results.FLa,dFL)
results.FLa$site <- "TFL"
results.FLa$time <- "A"
results.FLa$sig <- results.FLa$adjP <= 0.05
results.FLa$Z <- qnorm(results.FLa$adjP/2)

write.csv(results.FLa,"~/FUIteam/PydioData/env/data_outputs/statsFLa.csv")

library(dplyr)

#trophic levels
flWilcox <- wilcox.test(trophicCode ~ testStatus100mi, data = tcFLsum,
                        exact = FALSE)
flWilcox$p.value
flWilcox$statistic
qnorm(flWilcox$p.value/2)

tcTable <- tcFLsum %>% 
  group_by(testStatus100mi) %>%
  summarise(medTC = median(trophicCode), n = n())



laWilcox <- wilcox.test(trophicCode ~ testStatus100mi, data = tcLAsum,
                        exact = FALSE)
laWilcox$p.value
laWilcox$statistic
qnorm(laWilcox$p.value/2)

tcTable2 <- tcLAsum %>% 
  group_by(testStatus100mi) %>%
  summarise(medTC = median(trophicCode), n = n())

