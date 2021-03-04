library(ggplot2)
library(tidyverse)
library(mice)
library(corrplot)

# Read in the data
mlb <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw1/moneyball-training-data.csv")
summary(mlb)

# Plot a histogram for each statistic to get a sense of the distributions
mlb %>%
  gather(-INDEX, key = "var", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 100) +
  facet_wrap(~var, scales = "free") +
  xlab("Category Value") +
  ylab("Count")

# Plot a scatterplot for each predictor to get a sense of relationships to TARGET_WINS
mlb %>%
  gather(-TARGET_WINS, -INDEX, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET_WINS)) +
  geom_point() +
  facet_wrap(~var, scales = "free") +
  xlab("Category Value") +
  ylab("Wins")

# Plot a boxplot for each statistic to get a sense of the outliers
ggplot(stack(mlb), aes(x = ind, y = values)) +
  geom_boxplot() +
  coord_flip()

# Zoom in on the boxplot
ggplot(stack(mlb), aes(x = ind, y = values)) +
  geom_boxplot() +
  coord_flip() +
  ylim(0,5000)

# Show the correlation between the predictors and the target
corrplot(cor(mlb), method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)

# Fix the skew of the variable by doing a square root transformation
mlb <- mlb %>%
  mutate(TEAM_BATTING_3B_SQRT = sqrt(TEAM_BATTING_3B)) %>%
  mutate(TEAM_BATTING_HR_SQRT = sqrt(TEAM_BATTING_HR)) %>%
  mutate(TEAM_FIELDING_E_SQRT = sqrt(TEAM_FIELDING_E)) %>%
  mutate(TEAM_BASERUN_CS_SQRT = sqrt(TEAM_BASERUN_CS)) %>%
  mutate(TEAM_BASERUN_SB_SQRT = sqrt(TEAM_BASERUN_SB)) %>%
  mutate(TEAM_PITCHING_HR_SQRT = sqrt(TEAM_PITCHING_HR))

ggplot(gather(mlb[18:23], cols, value), aes(x = value)) + 
  geom_histogram(bins = 100) +
  facet_wrap(~cols, scales = "free")

mlb <- mlb %>%
  mutate(TEAM_FIELDING_E_LOG = log(TEAM_FIELDING_E))

ggplot(gather(mlb[24], cols, value), aes(x = value)) + 
  geom_histogram(bins = 100) +
  facet_wrap(~cols, scales = "free")

# Use the mice package (predictive mean matching method) to impute any missing
# values. Then normalize the data set and plot a series of histograms.
mlb_temp <- mice(mlb,
                 m = 5,
                 maxit = 10,
                 method = "pmm",
                 seed = 1234)
complete_mlb <- complete(mlb_temp,1)

complete_mlb %>%
  gather(-INDEX, key = "var", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 100) +
  facet_wrap(~var, scales = "free") +
  xlab("Category Value") +
  ylab("Count")

complete_mlb %>%
  gather(-TARGET_WINS, -INDEX, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = TARGET_WINS)) +
  geom_point() +
  facet_wrap(~var, scales = "free") +
  xlab("Category Value") +
  ylab("Wins")

# Create a new variable: ON BASE
complete_mlb <- complete_mlb %>%
  mutate(TEAM_BATTING_OB = TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_HBP) %>%
  filter(TEAM_PITCHING_H < 7500) %>%
  filter(TEAM_PITCHING_BB < 1500) %>%
  filter(TEAM_PITCHING_SO < 2000) %>%
  filter(TEAM_PITCHING_SO > 0) %>%
  filter(TEAM_PITCHING_HR_SQRT > 0) %>%
  filter(TEAM_BATTING_HR_SQRT > 0) %>%
  filter(TEAM_BATTING_SO > 0)

# Model with all features included
mod1 <- lm(TARGET_WINS ~ TEAM_BASERUN_CS_SQRT + TEAM_BASERUN_SB_SQRT
           + TEAM_BATTING_2B + TEAM_BATTING_3B_SQRT + TEAM_BATTING_BB + TEAM_BATTING_HR_SQRT
           + TEAM_BATTING_HBP + TEAM_BATTING_H + TEAM_BATTING_SO
           + TEAM_FIELDING_DP + TEAM_FIELDING_E_LOG
           + TEAM_PITCHING_BB + TEAM_PITCHING_H + TEAM_PITCHING_HR_SQRT + TEAM_PITCHING_SO
           + TEAM_BATTING_OB, data = complete_mlb)
summary(mod1)

par(mfrow=c(2,2))
hist(mod1$residuals)
plot(mod1$residuals ~ mod1$fitted.values)
qqnorm(mod1$residuals)
qqline(mod1$residuals)

# Model based on removing high p-value predictors until best r-squared is reached
mod2 <- lm(TARGET_WINS ~ TEAM_BASERUN_SB_SQRT
           + TEAM_BATTING_2B + TEAM_BATTING_3B_SQRT + TEAM_BATTING_BB + TEAM_BATTING_HR_SQRT
           + TEAM_BATTING_SO
           + TEAM_FIELDING_DP + TEAM_FIELDING_E_LOG
           + TEAM_PITCHING_BB + TEAM_PITCHING_H + TEAM_PITCHING_HR_SQRT + TEAM_PITCHING_SO
           + TEAM_BATTING_OB, data = complete_mlb)
summary(mod2)

par(mfrow=c(2,2))
hist(mod2$residuals)
plot(mod2$residuals ~ mod2$fitted.values)
qqnorm(mod2$residuals)
qqline(mod2$residuals)

# Model based on features with high correlation to TARGET_WINS
mod3 <- lm(TARGET_WINS ~ TEAM_BATTING_OB + TEAM_BATTING_H
           + TEAM_FIELDING_E_LOG + TEAM_PITCHING_H, data = complete_mlb)
summary(mod3)

par(mfrow=c(2,2))
hist(mod3$residuals)
plot(mod3$residuals ~ mod3$fitted.values)
qqnorm(mod3$residuals)
qqline(mod3$residuals)

# Predicting on evaluation data using model 2
eval <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw1/moneyball-evaluation-data.csv")

eval <- eval %>%
  mutate(TEAM_BATTING_3B_SQRT = sqrt(TEAM_BATTING_3B)) %>%
  mutate(TEAM_BATTING_HR_SQRT = sqrt(TEAM_BATTING_HR)) %>%
  mutate(TEAM_FIELDING_E_LOG = log(TEAM_FIELDING_E)) %>%
  mutate(TEAM_BASERUN_CS_SQRT = sqrt(TEAM_BASERUN_CS)) %>%
  mutate(TEAM_BASERUN_SB_SQRT = sqrt(TEAM_BASERUN_SB)) %>%
  mutate(TEAM_PITCHING_HR_SQRT = sqrt(TEAM_PITCHING_HR))

eval_temp <- mice(eval,
                 m = 5,
                 maxit = 10,
                 method = "pmm",
                 seed = 1234)
complete_eval <- complete(eval_temp,1)

complete_eval <- complete_eval %>%
  mutate(TEAM_BATTING_OB = TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_HBP) %>%
  filter(TEAM_PITCHING_H < 7500) %>%
  filter(TEAM_PITCHING_BB < 1500) %>%
  filter(TEAM_PITCHING_SO < 2000) %>%
  filter(TEAM_PITCHING_SO > 0) %>%
  filter(TEAM_PITCHING_HR_SQRT > 0) %>%
  filter(TEAM_BATTING_HR_SQRT > 0) %>%
  filter(TEAM_BATTING_SO > 0)

pred <- predict(mod2,complete_eval)
