library(ggplot2)
library(tidyverse)
library(mice)
library(corrplot)

# Read in the data
mlb <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/moneyball-training-data.csv")

# Plot a histogram for each statistic to get a sense of the distributions
ggplot(gather(mlb, cols, value), aes(x = value)) + 
  geom_histogram(bins = 100) +
  facet_wrap(.~cols) +
  xlim(0,2000)

# The data has widely different scales, so here is a function to normalize the data
# on a 0 to 1 scale.
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

mlb_norm <- as.data.frame(lapply(mlb[1:17], normalize))

# View the correlation between variables before any manipulation is done
mlb_cor <- cor(mlb_norm)
corrplot(mlb_cor, method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)

# Use the mice package (predictive mean matching method) to impute any missing
# values. Then normalize the dataset and plot a series of histograms.
mlb_temp <- mice(mlb,
                 m = 5,
                 maxit = 10,
                 method = "pmm",
                 seed = 1234)
complete_mlb <- complete(mlb_temp,1)
mlb_norm <- as.data.frame(lapply(complete_mlb[1:17], normalize))

ggplot(gather(mlb_norm, cols, value), aes(x = value)) + 
  geom_histogram(bins = 100) +
  facet_wrap(.~cols)

# Create a new variable:
# ON BASE
complete_mlb <- complete_mlb %>%
  mutate(OB = TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_HBP) %>%
  mutate(TEAM_PITCHING_H_LOG = log(TEAM_PITCHING_H))

mlb_norm <- as.data.frame(lapply(complete_mlb[1:19], normalize))
ggplot(gather(mlb_norm, cols, value), aes(x = value)) + 
  geom_histogram(bins = 100) +
  facet_wrap(.~cols)

# Checking the normality of the new variable
ggplot(complete_mlb, aes(x = OB)) + 
  geom_histogram(bins = 50)

# View the correlation with the new created variables included
mlb_cor <- cor(complete_mlb)
corrplot(mlb_cor, method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)

###############################################################################
# Model with all features included
mod1 <- lm(TARGET_WINS ~ TEAM_BASERUN_CS + TEAM_BASERUN_SB
           + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_BB + TEAM_BATTING_HR
           + TEAM_BATTING_HBP + TEAM_BATTING_H + TEAM_BATTING_SO
           + TEAM_FIELDING_DP + TEAM_FIELDING_E
           + TEAM_PITCHING_BB + TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_SO
           + OB, data = complete_mlb)
summary(mod1)

# Model with features removed
mod2 <- lm(TARGET_WINS ~ OB + TEAM_BATTING_H
           + TEAM_FIELDING_E, data = complete_mlb)
summary(mod2)

# Predicting on evaluation data
eval <- read.csv("moneyball-evaluation-data.csv")
eval_temp <- mice(eval,
                 m = 5,
                 maxit = 10,
                 method = "pmm",
                 seed = 1234)
complete_eval <- complete(eval_temp,1)

complete_eval <- complete_eval %>%
  mutate(OB = TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_HBP)

pred1 <- predict(mod1,complete_eval)
pred2 <- predict(mod2,complete_eval)
prediction <- cbind(pred1,pred2)

write.csv(prediction, "prediction.csv")
