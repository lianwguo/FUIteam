#reading in data for life history of two case study sites
lhComp <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "allLH.csv"), 
                     stringsAsFactors = FALSE)
str(lhComp)

#run PCA
lh.pca <- prcomp(lhComp[,c(6:9)], center = TRUE,scale. = TRUE)

summary(lh.pca)
str(lh.pca)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(lh.pca)

ggbiplot(lh.pca)

#try different package
install.packages(c("FactoMineR", "factoextra"))

library("FactoMineR")
res.pca <- PCA(decathlon2.active, graph = FALSE)
