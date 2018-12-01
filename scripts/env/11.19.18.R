#FL 25 miles mercury

FL25mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_25mi.csv"), 
                   stringsAsFactors = FALSE)
LA25mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_25mi.csv"), 
                   stringsAsFactors = FALSE)
FL100mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "FL_100mi.csv"), 
                   stringsAsFactors = FALSE)
LA100mi <- read.csv(file.path("~/FUIteam/PydioData/env/raw/", "LA_100mi.csv"), 
                   stringsAsFactors = FALSE)

head(LA25mi)

ggplot(data = LA25mi, aes(x = Average.Fish.Length..cm., y = Mercury.Results, color = Common.Name)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

ggplot(data = LA100mi, aes(x = Average.Fish.Length..cm., y = Mercury.Results, color = Common.Name)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

head(FL25mi)

ggplot(data = FL25mi, aes(x = TL, y = Hg, color = Species)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

ggplot(data = FL100mi, aes(x = TL, y = Hg, color = Species)) + 
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  labs(x="Total Length (cm)", y="Methylmercury content (ppm)") + #labels
  font("xylab", size = 14, face = "bold") +
  font("axis.text", size = 14) +
  font("legend.title", size = 14, face = "bold") +
  font("legend.text", size = 10)

