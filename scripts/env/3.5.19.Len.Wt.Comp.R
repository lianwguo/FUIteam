#reading in master data file with lengths and weights from landed fish and mercury tested fish
sizeComp <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "SizeComparisons.csv"), 
                   stringsAsFactors = FALSE)
str(sizeComp)


#subset data based on fishes that were both landed and tested - to do length and weight comparisons
?subset
nolaOverlap <- subset(sizeComp, sizeComp$caseStudy == 'NOLA' & sizeComp$species == 'BLACK DRUM' | 
         sizeComp$caseStudy == 'NOLA' & sizeComp$species == 'GRAY SNAPPER' | 
         sizeComp$caseStudy == 'NOLA' & sizeComp$species == 'RED DRUM' | 
         sizeComp$caseStudy == 'NOLA' & sizeComp$species == 'SHEEPSHEAD' | 
         sizeComp$caseStudy == 'NOLA' & sizeComp$species == 'SOUTHERN FLOUNDER' | 
         sizeComp$caseStudy == 'NOLA' & sizeComp$species == 'SPOTTED SEATROUT')

str(nolaOverlap)
write.csv(nolaOverlap, "~/FUIteam/PydioData/env/data_outputs/nolaOverlap.csv")

# NOLA histograms
ggplot(nolaOverlap, aes(x=aveLen, color=source)) +
  geom_bar(bins = 50, fill="white") +
  facet_wrap(~ facet) 

ggplot(nolaOverlap, aes(x=aveWt, color=source)) +
  geom_histogram(bins = 50, fill="white") + 
  facet_wrap(~ facet) 

## aveWt was saved as a character before. nolaOverlap$aveWt <- as.numeric(nolaOverlap$aveWt)
tail(nolaOverlap)
#new column for reordering facet wrap
nolaOverlap$facet = factor(nolaOverlap$species, levels = c("BLACK DRUM", "GRAY SNAPPER", 
                                                           "RED DRUM", "SHEEPSHEAD", "SOUTHERN FLOUNDER",
                                                           "SPOTTED SEATROUT"))

## now florida
tflOverlap <- subset(sizeComp, sizeComp$caseStudy == 'TFL' & sizeComp$species == 'ATLANTIC SHARPNOSE SHARK' | 
                        sizeComp$caseStudy == 'TFL' & sizeComp$species == 'BLACKTIP SHARK' | 
                        sizeComp$caseStudy == 'TFL' & sizeComp$species == 'BONNETHEAD' | 
                        sizeComp$caseStudy == 'TFL' & sizeComp$species == 'CREVALLE JACK' | 
                        sizeComp$caseStudy == 'TFL' & sizeComp$species == 'FLORIDA POMPANO' | 
                        sizeComp$caseStudy == 'TFL' & sizeComp$species == 'GULF FLOUNDER' | 
                       sizeComp$caseStudy == 'TFL' & sizeComp$species == 'RED DRUM' | 
                       sizeComp$caseStudy == 'TFL' & sizeComp$species == 'SOUTHERN KINGFISH' | 
                       sizeComp$caseStudy == 'TFL' & sizeComp$species == 'SPANISH MACKEREL' | 
                       sizeComp$caseStudy == 'TFL' & sizeComp$species == 'SPOTTED SEATROUT')
str(tflOverlap)
tail(tflOverlap)
tflOverlap$aveWt <- as.numeric(tflOverlap$aveWt)
write.csv(tflOverlap, "~/FUIteam/PydioData/env/data_outputs/tflOverlap.csv")

tflOverlapTL <- read.csv(file.path("~/FUIteam/PydioData/env/data_outputs/", "tflOverlapTL.csv"))
tail(tflOverlapTL)
str(tflOverlapTL)
#new column for reordering facet wrap
#tflOverlapTL$facet = factor(tflOverlap$species, levels = c("ATLANTIC SHARPNOSE SHARK", "BLACKTIP SHARK", 
#                                                           "BONNETHEAD", "CREVALLE JACK", 
#                                                         "FLORIDA POMPANO", "GULF FLOUNDER","RED DRUM",
#                                                         "SOUTHERN KINGFISH", "SPANISH MACKEREL",
#                                                         "SPOTTED SEATROUT"), order)

# FLORIDA histograms
ggplot(tflOverlap, aes(x=aveLen, color=source)) +
  geom_bar(bins = 100, fill="white") +
  facet_wrap(~ facet) 

ggplot(tflOverlap, aes(x=aveWt, color=source)) +
  geom_histogram(bins = 100, fill="white") + 
  facet_wrap(~ facet) 

unique(nolaOverlap$collectYear)
unique(tflOverlap$collectYear)

#nola boxplot
boxplot(aveLen ~ source*species,
        col=c("white","lightgray"), ylim = c(0, 120), nolaOverlap)
boxplot(aveWt ~ source*species,
        col=c("white","lightgray"), ylim = c(0, 11000), nolaOverlap)
g <- ggplot(nolaOverlap, aes(x=species, y=aveLen, col = source)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 100)) 
g + theme(axis.text.x=element_text(angle=90, hjust=1))
gW <- ggplot(nolaOverlap, aes(x=species, y=aveWt, col = source)) + 
  geom_boxplot() 
  coord_cartesian(ylim = c(0, 100)) 
gW + theme(axis.text.x=element_text(angle=90, hjust=1))


#tfl boxplot
boxplot(aveLen ~ source*species,
        col=c("white","lightgray"), ylim = c(0, 100), tflOverlap)
boxplot(aveWt ~ source*species,
        col=c("white","lightgray"), ylim = c(0, 6000), tflOverlap)
p<- ggplot(tflOverlapTL, aes(x=species, y=aveLen, col = source)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 100)) 
p + theme(axis.text.x=element_text(angle=90, hjust=1))
  

#try some t-tests for NOLA
redDrumNO <- subset(nolaOverlap, nolaOverlap$species == 'RED DRUM')
t.test(aveLen~source, data = redDrumNO) ##sign p-value = 0.0008624
t.test(aveWt~source, data = redDrumNO) ##sign [p = 0.0003868]

write.csv(blackDrumNO, "~/FUIteam/PydioData/env/data_outputs/blackDrumNO.csv")
#edited - they had flipped weight and length
blackDrumNO <- read.csv(file.path("~/FUIteam/PydioData/env/data_outputs/", "blackDrumNO.csv"))

#blackDrumNO <- subset(nolaOverlap, nolaOverlap$species == 'BLACK DRUM')
t.test(aveLen~source, data = blackDrumNO) ##sign, p = .00001658
t.test(aveWt~source, data = blackDrumNO) ## sign, p = 0.0001157

ggplot(snapperNO,aes(x=source, y=aveWt)) +
  geom_boxplot()

sheepsNO <- subset(nolaOverlap, nolaOverlap$species == 'SHEEPSHEAD')
t.test(aveLen~source, data = sheepsNO)  ##NS
t.test(aveWt~source, data = sheepsNO) #sign, p = 0.005218

sFlounderNO <- subset(nolaOverlap, nolaOverlap$species == 'SOUTHERN FLOUNDER')
t.test(aveLen~source, data = sFlounderNO)  ##borderline sign p = 0.05421
t.test(aveWt~source, data = sFlounderNO) ##NS, p = 0.1103

seatroutNO <- subset(nolaOverlap, nolaOverlap$species == 'SPOTTED SEATROUT')
t.test(aveLen~source, data = seatroutNO)  ##sign p = 0.043
t.test(aveWt~source, data = seatroutNO)  ##sign, p = 0.01426

snapperNO <- subset(nolaOverlap, nolaOverlap$species == 'GRAY SNAPPER')
t.test(aveLen~source, data = snapperNO)  ##NS, but low sample size
t.test(aveWt~source, data = snapperNO) ##NS, still low sample size, 0.6805

unique(nolaOverlap$species)

#and t-tests for florida
ggplot(atlSharkT,aes(x=aveLen, y=aveWt, color=source)) +
  geom_point()
ggplot(seatroutT,aes(x=source, y=aveLen)) +
  geom_point()
ggplot(seatroutT,aes(x=source, y=aveWt)) +
  geom_point()
crevalleT

tail(tflOverlapTL)
atlSharkT <- subset(tflOverlapTL, tflOverlapTL$species == 'ATLANTIC SHARPNOSE SHARK')
t.test(aveLen~source, data = atlSharkT) ##can't run, too small of sample

blackSharkT <- subset(tflOverlapTL, tflOverlapTL$species == 'BLACKTIP SHARK')
t.test(aveLen~source, data = blackSharkT) ##can't run, too small of sample

bonnetT <- subset(tflOverlapTL, tflOverlapTL$species == 'BONNETHEAD')
#two weights missing
t.test(aveLen~source, data = bonnetT) ##NS, small sample
t.test(aveWt~source, data = bonnetT) #borderline, p=0.0575, small sample

crevalleT <- subset(tflOverlapTL, tflOverlapTL$species == 'CREVALLE JACK')
t.test(aveLen~source, data = crevalleT) ##sign, p = 0.04437, low sample
#missing weights
#t.test(aveWt~source, data = crevalleT) ##NS, low sample

flPompT <- subset(tflOverlapTL, tflOverlapTL$species == 'FLORIDA POMPANO')
#weights missing
t.test(aveLen~source, data = flPompT) ##sign, p=0.02953
t.test(aveWt~source, data = flPompT) ##NS, p = 0.4473, low sample

#some weird stuff going on with this one too
floundT <- subset(tflOverlapTL, tflOverlapTL$species == 'GULF FLOUNDER')
t.test(aveLen~source, data = floundT) ##NS, small sample size (p=0.144)
t.test(aveWt~source, data = floundT) #NS, small sample size (p=0.2014)

redDrumT <- subset(tflOverlapTL, tflOverlapTL$species == 'RED DRUM')
t.test(aveLen~source, data = redDrumT) ##sign, p=-0.02384
#weights missing
t.test(aveWt~source, data = redDrumT) #sign, p=0.04128

kingT <- subset(tflOverlapTL, tflOverlapTL$species == 'SOUTHERN KINGFISH')
t.test(aveLen~source, data = kingT) ##NS, p=0.3186, low sample
t.test(aveWt~source, data = kingT) #sign, p=0.02422, low sample

mackT <- subset(tflOverlapTL, tflOverlapTL$species == 'SPANISH MACKEREL')
t.test(aveLen~source, data = mackT) ##NS, close though, p=0.07758
t.test(aveWt~source, data = mackT) #NS like almost exactly the same!

seatroutT <- subset(tflOverlapTL, tflOverlapTL$species == 'SPOTTED SEATROUT')
t.test(aveLen~source, data = seatroutT) ## can't run, not enough observations
t.test(aveWt~source, data = seatroutT) 


### merging to look at trophic level and sediment/benthic codes between tested/landed sp

lhKey <- read.csv(file.path("~/FUIteam/PydioData/env/raw/","lhKey.csv"))
str(lhKey)
lhKey[,1] <- NULL

str(tflOverlapTL)
tail(tflOverlapTL)

tflOverlapTL$species <- as.character(tflOverlapTL$species)
nolaOverlap$species <- as.character(nolaOverlap$species)
lhKey$Common.Name <- as.character(lhKey$Common.Name)

tflLH <- left_join(tflOverlapTL, lhKey, by = c("species" = "Common.Name"))
tail(tflLH)

ggplot(tflLH,aes(x=source, y=trophicCode, color=source)) +
  geom_boxplot()
ggplot(tflLH,aes(x=source, y=Avg.size..cm., color=source)) +
  geom_boxplot()
ggplot(tflLH,aes(x=source, y=benthCode, color=source)) +
  geom_point()
ggplot(tflLH,aes(x=source, y=sedCode, color=source)) +
  geom_point()

t.test(trophicCode ~ source, data = tflLH) # t = -6.1542, df = 321.92, p-value = 2.241e-09
t.test(Avg.size..cm. ~ source, data = tflLH) # t = 9.1161, df = 296.85, p-value < 2.2e-16

tail(nolaOverlap)

nolaLH <- left_join(nolaOverlap, lhKey, by = c("species" = "Common.Name"))
tail(nolaLH)

ggplot(nolaLH,aes(x=source, y=trophicCode, color=source)) +
  geom_boxplot()
ggplot(nolaLH,aes(x=source, y=Avg.size..cm., color=source)) +
  geom_boxplot()
ggplot(nolaLH,aes(x=source, y=benthCode, color=source)) +
  geom_point()
ggplot(nolaLH,aes(x=source, y=sedCode, color=source)) +
  geom_point()

t.test(trophicCode ~ source, data = nolaLH) # NOT SIG
t.test(Avg.size..cm. ~ source, data = nolaLH) # t = 4.5307, df = 268.55, p-value = 8.855e-06
