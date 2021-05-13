library(tidyverse)
library(MASS)
library(optmatch)

# Read in data and view summary statistics
data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw5/wine-training-data.csv", header = TRUE)
head(data)
summary(data)

data <- data[2:16]
data <- fill.NAs(data)

par(mfrow = c(4,4))
for(i in 2:15) {
  hist(data[,i], main = names(data)[i])
}

p1 <- glm(TARGET ~., family = "poisson", data = data)
summary(p1)

p2 <- glm(TARGET ~ Alcohol + ResidualSugar, family = "poisson", data = data)
summary(p2)

nb1 <- MASS::glm.nb(TARGET ~., data = data)
summary(nb1)

nb2 <- MASS::glm.nb(TARGET ~ Alcohol + ResidualSugar, data = data)
summary(nb2)

ml1 <- lm(TARGET ~., data = data)
summary(ml1)

ml2 <- lm(TARGET ~ Alcohol + ResidualSugar, data = data)
summary(ml2)

eval <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw5/wine-evaluation-data.csv", header = TRUE)
eval <- eval[2:16]
eval <- fill.NAs(eval)

eval_pred <- predict(p1, eval, type = "response")
