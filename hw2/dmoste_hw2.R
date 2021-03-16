library(tidyverse)
library(caret)
library(pROC)

# Read in the data
data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA621/master/hw2/classification-output-data.csv")

# Obtain the rows required to use the table function
table_data <- data[9:10]
table(table_data)

# Accuracy function
accuracy <- function(x, class, predicted){
  x_mutated <- x %>%
    mutate(result = ifelse(class == 1 & predicted == 1, "TP",
                           ifelse(class == 0 & predicted == 0, "TN",
                                  ifelse(class == 0 & predicted == 1, "FP", "FN"))))
  
  TP <- sum(x_mutated$result == "TP")
  TN <- sum(x_mutated$result == "TN")
  FP <- sum(x_mutated$result == "FP")
  FN <- sum(x_mutated$result == "FN")
  
  y <- (TP + TN)/(TP + FP + TN + FN)
  
  return(y)
}

# Classification error rate function
error_rate <- function(x, class, predicted){
  x_mutated <- x %>%
    mutate(result = ifelse(class == 1 & predicted == 1, "TP",
                           ifelse(class == 0 & predicted == 0, "TN",
                                  ifelse(class == 0 & predicted == 1, "FP", "FN"))))
  
  TP <- sum(x_mutated$result == "TP")
  TN <- sum(x_mutated$result == "TN")
  FP <- sum(x_mutated$result == "FP")
  FN <- sum(x_mutated$result == "FN")
  
  y <- (FP + FN)/(TP + FP + TN + FN)
  
  return(y)
}

# Precision function
precision <- function(x, class, predicted){
  x_mutated <- x %>%
    mutate(result = ifelse(class == 1 & predicted == 1, "TP",
                           ifelse(class == 0 & predicted == 0, "TN",
                                  ifelse(class == 0 & predicted == 1, "FP", "FN"))))
  
  TP <- sum(x_mutated$result == "TP")
  TN <- sum(x_mutated$result == "TN")
  FP <- sum(x_mutated$result == "FP")
  FN <- sum(x_mutated$result == "FN")
  
  y <- TP/(TP + FP)
  
  return(y)
}

# Sensitivity function
sensitivity <- function(x, class, predicted){
  x_mutated <- x %>%
    mutate(result = ifelse(class == 1 & predicted == 1, "TP",
                           ifelse(class == 0 & predicted == 0, "TN",
                                  ifelse(class == 0 & predicted == 1, "FP", "FN"))))
  
  TP <- sum(x_mutated$result == "TP")
  TN <- sum(x_mutated$result == "TN")
  FP <- sum(x_mutated$result == "FP")
  FN <- sum(x_mutated$result == "FN")

  y <- TP/(TP + FN)
  
  return(y)
}

# Specificity function
specificity <- function(x, class, predicted){
  x_mutated <- x %>%
    mutate(result = ifelse(class == 1 & predicted == 1, "TP",
                           ifelse(class == 0 & predicted == 0, "TN",
                                  ifelse(class == 0 & predicted == 1, "FP", "FN"))))
  
  TP <- sum(x_mutated$result == "TP")
  TN <- sum(x_mutated$result == "TN")
  FP <- sum(x_mutated$result == "FP")
  FN <- sum(x_mutated$result == "FN")
  
  y <- TN/(TN + FP)
  
  return(y)
}

# F1 score function
f1_score <- function(x, class, predicted){
  x_mutated <- x %>%
    mutate(result = ifelse(class == 1 & predicted == 1, "TP",
                           ifelse(class == 0 & predicted == 0, "TN",
                                  ifelse(class == 0 & predicted == 1, "FP", "FN"))))
  
  TP <- sum(x_mutated$result == "TP")
  TN <- sum(x_mutated$result == "TN")
  FP <- sum(x_mutated$result == "FP")
  FN <- sum(x_mutated$result == "FN")
  
  prec <- TP/(TP + FP)
  sens <- TP/(TP + FN)
  y <- (2*prec*sens)/(prec+sens)
  
  return(y)
}

# ROC function
roc <- function(x, class, prob){
  spec_data <- c()
  sens_data <- c()
  
  for(thresh in seq(0, 1, by = 0.01)){
    x_mutated <- x %>%
      mutate(predicted = ifelse(prob >= thresh, 1, 0)) %>%
      mutate(result = ifelse(class == 1 & predicted == 1, "TP",
                             ifelse(class == 0 & predicted == 0, "TN",
                                    ifelse(class == 0 & predicted == 1, "FP", "FN"))))
    
    TP <- sum(x_mutated$result == "TP")
    TN <- sum(x_mutated$result == "TN")
    FP <- sum(x_mutated$result == "FP")
    FN <- sum(x_mutated$result == "FN")
    
    spec <- TN/(TN + FP)
    sens <- TP/(TP + FN)
    
    spec_data <- c(1-spec, spec_data)
    sens_data <- c(sens, sens_data)
  }
  
  roc_data <- data.frame(spec_data, sens_data)
  
  roc_curve <- ggplot(roc_data, aes(x = spec_data, y = sens_data)) +
    geom_line() +
    geom_point() +
    labs(title = "ROC Curve",
         x = "1-Specificity",
         y = "Sensitivity")
  
  auc <- c()
  for(i in 1:length(roc_data$spec_data)){
    delta_spec <- roc_data$spec_data[i] - roc_data$spec_data[i-1]
    auc <- c(auc,(roc_data$sens_data[i]*delta_spec))
  }
  
  auc <- sum(auc)
  
  return(list(roc_curve,auc))
}

#  Obtain classification metrics using user-created functions
acc <- accuracy(data, data$class, data$scored.class)
err <- error_rate(data, data$class, data$scored.class)
pre <- precision(data, data$class, data$scored.class)
sens <- sensitivity(data, data$class, data$scored.class)
spec <- specificity(data, data$class, data$scored.class)
f1 <- f1_score(data, data$class, data$scored.class)
roc(data, data$class, data$scored.probability)

# Convert class and scored.class into factors for use with caret
table_data$class <- as.factor(table_data$class)
table_data$scored.class <- as.factor(table_data$scored.class)

# Produce a confusion matrix with caret
caret::confusionMatrix(data = table_data$scored.class,
                       reference = table_data$class,
                       positive = 1)

# Calculate the sensitivity with caret
caret::sensitivity(data = table_data$scored.class,
                   reference = table_data$class,
                   positive = 1)

# Calculate the specificity with caret
caret::specificity(data = table_data$scored.class,
                   reference = table_data$class,
                   negative = 0)

# Use pROC to obtain an roc curve
rocCurve <- pROC::roc(response = data$class,
                      predictor = data$scored.probability)
auc(rocCurve)
plot(rocCurve)
