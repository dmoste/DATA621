library(tidyverse)
library(caret)
library(pROC)
library(corrplot)
library(mice)

# Read in data and view summary statistics
data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw4/insurance_training_data.csv", header = TRUE)
head(data)
summary(data)

length(data$TARGET_FLAG[data$TARGET_FLAG == 0])/length(data$TARGET_FLAG)

my_transform <- function(data){
  data <- data[-c(1)]
  data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
  data[data == ""] <- NA
  
  data$TARGET_AMT <- as.numeric(data$TARGET_AMT)
  data$INCOME <- as.numeric(str_remove_all(data$INCOME, "[\\$,]"))
  data$HOME_VAL <- as.numeric(str_remove_all(data$HOME_VAL, "[\\$,]"))
  data$BLUEBOOK <- as.numeric(str_remove_all(data$BLUEBOOK, "[\\$,]"))
  data$OLDCLAIM <- as.numeric(str_remove_all(data$OLDCLAIM, "[\\$,]"))
  data$CAR_AGE <- abs(data$CAR_AGE)
  
  data <- data %>%
    mutate_if(is.character, as.factor)
  
  data$INCOME <- log(data$INCOME + 1)
  data$HOME_VAL <- log(data$HOME_VAL + 1)
  data$BLUEBOOK <- log(data$BLUEBOOK + 1)
  data$TIF <- log(data$TIF + 1)
  data$MVR_PTS <- log(data$MVR_PTS + 1)
  data$CAR_AGE <- log(data$CAR_AGE + 1)
  
  return(data)
}

t_data <- my_transform(data)

temp <- mice(t_data,
             m = 5,
             maxit = 10,
             method = "pmm",
             seed = 1234)
t_data <- complete(temp,1)

numeric_data <- data %>%
  select_if(is.numeric)

par(mfrow = c(4,4))
for(i in 3:15) {
  hist(numeric_data[,i], main = names(numeric_data)[i])
}

for(i in 3:15) {
  plot(x = numeric_data[,i], y = numeric_data[,2], main = names(numeric_data)[i])
}

corrplot(cor(numeric_data), method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)

accident_prob_1 <- glm(TARGET_FLAG ~ . - TARGET_AMT,
                      data = t_data, family = binomial)
summary(accident_prob_1)

accident_prob_2 <- glm(TARGET_FLAG ~ AGE + INCOME + HOME_VAL + SEX +
                        EDUCATION + TRAVTIME + CAR_TYPE + OLDCLAIM +
                        CLM_FREQ,
                      data = t_data, family = binomial)
summary(accident_prob_2)

accident_prob_3 <- glm(TARGET_FLAG ~ AGE + MVR_PTS + TRAVTIME,
                       data = t_data, family = binomial)
summary(accident_prob_3)

accident_prob <- predict(accident_prob_1, t_data, type = "response")
accident_prob_pred <- as.factor(ifelse(accident_prob > 0.5, 1, 0))
accident_prob_data <- cbind(t_data, accident_prob, accident_prob_pred)

caret::confusionMatrix(data = accident_prob_data$accident_prob_pred,
                       reference = accident_prob_data$TARGET_FLAG,
                       mode = 'everything')


############
cost_mod_1 <- lm(TARGET_AMT ~ .,
                     data = t_data)
summary(cost_mod_1)

cost_mod_2 <- lm(TARGET_AMT ~ AGE + INCOME + HOME_VAL +
                       SEX + EDUCATION + TRAVTIME + CAR_TYPE +
                       OLDCLAIM + CLM_FREQ,
                     data = t_data)
summary(cost_mod_2)

amount_pred <- predict(cost_mod_1,t_data)
plot(data$TARGET_AMT, amount_pred)

####
eval <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw4/insurance-evaluation-data.csv", header = TRUE)

t_eval <- my_transform(eval)

temp <- mice(t_eval,
             m = 5,
             maxit = 10,
             method = "pmm",
             seed = 1234)
t_eval <- complete(temp,1)

eval_prob_pred <- predict(accident_prob_1, t_eval, type = "response")
eval_prob_pred <- ifelse(eval_prob_pred > 0.5, 1, 0)
eval_amount_pred <- predict(cost_mod_1,t_eval)
