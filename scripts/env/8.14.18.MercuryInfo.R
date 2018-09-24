#looking at average mercury content

head(CloseMerc306)
str(CloseMerc306)
unique(CloseMerc306$Common.Name)

#aggregate mercury results and fish weight by common name and year
agg306 <- aggregate(cbind(Mercury.Results, Average.Fish.Weight..grams.)~Common.Name, data=CloseMerc306, mean)
agg306
write.csv(agg306, "~/FUIteam/PydioData/env/data_outputs/aggHg306.csv")
aggYr306 <- aggregate(cbind(Mercury.Results, Average.Fish.Weight..grams.)~Common.Name + CollectYear, data=CloseMerc306, mean)
aggYr306
write.csv(aggYr306, "~/FUIteam/PydioData/env/data_outputs/aggHgYr306.csv")

agg222 <- aggregate(cbind(Mercury.Results, Average.Fish.Weight..grams.)~Common.Name, data=CloseMerc222, mean)
agg222
write.csv(agg222, "~/FUIteam/PydioData/env/data_outputs/aggHg222.csv")
aggYr222 <- aggregate(cbind(Mercury.Results, Average.Fish.Weight..grams.)~Common.Name + CollectYear, data=CloseMerc222, mean)
aggYr222
write.csv(aggYr222, "~/FUIteam/PydioData/env/data_outputs/aggHgYr222.csv")


ggplot(data = CloseMerc306, aes(x = Average.Fish.Length..cm., y = Mercury.Results, color = Common.Name)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

ggplot(data = CloseMerc222, aes(x = Average.Fish.Length..cm., y = Mercury.Results, color = Common.Name)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)


