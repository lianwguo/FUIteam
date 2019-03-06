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

# Change histogram plot line colors by groups
ggplot(nolaOverlap, aes(x=aveLen, color=source)) +
  geom_histogram(bins = 50)+ 
  facet_wrap(~ facet) 

ggplot(nolaOverlap, aes(x=aveWt, color=source)) +
  geom_histogram(bins = 50)+ 
  facet_wrap(~ facet) 
as.numeric(nolaOverlap$aveWt)

#new column for reordering facet wrap
nolaOverlap$facet = factor(nolaOverlap$species, levels = c("BLACK DRUM", "GRAY SNAPPER", 
                                                           "RED DRUM", "SHEEPSHEAD", "SOUTHERN FLOUNDER",
                                                           "SPOTTED SEATROUT"))
