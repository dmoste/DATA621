library(tidyverse)
library(caret)
library(pROC)

# Read in data and view summary statistics
data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw3/crime-training-data_modified.csv", header = TRUE)
head(data)
summary(data)

par(mfrow = c(3,4))
# View histogram for all data
for(i in 1:12) {
  hist(data[,i], main = names(data)[i])
}

# View scatter plot for all data
for(i in 1:12) {
  plot(x = data[,i], y = data[,13], main = names(data)[i])
}

# View boxplot for all data
for(i in 1:12) {
  boxplot(x = data[,i], y = data[,13], main = names(data)[i])
}

# Transformation function
my_transform <- function(data){
  data$zn_log <- log(data$zn + 1)
  data$dis_log <- log(data$dis + 1)
  data$lstat_log <- log(data$lstat + 1)
  data$nox_log <- log(data$nox + 1)
  data$age_cr <- (data$age)**(1/3)
  
  return(data)
}

# Transform data
t_data <- my_transform(data)

# Create first model using all parameters
fit1 <- glm(target ~ ., data = t_data, family = binomial)
summary(fit1)

# Create second model using backward elimination
fit2 <- glm(target ~ . - indus - zn_log - chas - zn - lstat - lstat_log,
            data = t_data, family = binomial)
summary(fit2)

# Create third model with hand selection
fit3 <- glm(target ~ zn + rm +age + dis + rad + tax + ptratio + medv,
            data = t_data, family = binomial)
summary(fit3)

# Make predictions from training data and add to the transformed data
likely <- predict(fit1, t_data, type = "response")
pred <- ifelse(likely > 0.5, 1, 0)
t_data <- cbind(t_data, likely, pred)

# Convert class and scored.class into factors for use with caret
t_data$target <- as.factor(t_data$target)
t_data$pred <- as.factor(t_data$pred)

# Calculate the precision with caret
caret::precision(data = t_data$pred,
                   reference = t_data$target,
                   positive = 1)

# Calculate the sensitivity with caret
caret::sensitivity(data = t_data$pred,
                   reference = t_data$target,
                   positive = 1)

# Calculate the specificity with caret
caret::specificity(data = t_data$pred,
                   reference = t_data$target,
                   negative = 0)

# Calculate the F1 with caret
caret::F_meas(data = t_data$pred,
                   reference = t_data$target,
                   negative = 0)

# Use pROC to obtain an roc curve
rocCurve <- pROC::roc(response = t_data$target,
                      predictor = t_data$likely)
auc(rocCurve)

# Produce a confusion matrix with caret
caret::confusionMatrix(data = t_data$pred,
                       reference = t_data$target,
                       mode = 'everything')

# Read in evaluation data and transform in the same way as training data
eval <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw3/crime-evaluation-data_modified.csv", header = TRUE)
t_eval <- my_transform(eval)

# Make predictions on the evaluation data
eval_pred <- predict(fit1, t_eval, type = "response")
eval_pred <- ifelse(eval_pred > 0.5, 1, 0)
