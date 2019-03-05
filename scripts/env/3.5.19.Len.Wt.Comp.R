#reading in master data file with lengths and weights from landed fish and mercury tested fish
sizeComp <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "SizeComparisons.csv"), 
                   stringsAsFactors = FALSE)
str(sizeComp)

# Change histogram plot line colors by groups
ggplot(sizeComp, aes(x=aveLen, color=species)) +
  geom_histogram(fill="white")
