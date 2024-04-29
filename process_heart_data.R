library(tidyverse)
library(plotly)
library(widgetframe)
library(tidytext)

### load heart data ###
heart <- read_csv("data/heart.csv")
heart$Sex <- factor(heart$Sex)
heart$ExerciseAngina <- factor(heart$ExerciseAngina)
heart$RestingECG <- factor(heart$RestingECG)
heart$ST_Slope <- factor(heart$ST_Slope)

### EDA ###

# summary(heart)
# unique(heart$Sex)
# unique(heart$ExerciseAngina)
# unique(heart$RestingECG)
# unique(heart$ST_Slope)
# unique(heart$HeartDisease)

# heart |>
#   ggplot() +
#   geom_bar(aes(x=HeartDisease))

# numeric_var <- sapply(heart, is.numeric)
# numeric_var <- names(heart[numeric_var])
# heart_numeric <- heart[numeric_var]
# pairs(heart_numeric)

### split 80-20 training testing ###

set.seed(1007829637)
train_indices <- sample(1:nrow(heart), nrow(heart) * 0.8, replace = FALSE)

heart_train <- heart[train_indices,]
heart_test <- heart[-train_indices,]

### train and test logistic model ###
full_model <- glm(data = heart_train, HeartDisease ~., family = binomial(link = "logit"))
reduced_model <- step(full_model, direction = "both", trace = FALSE)
pred <- predict(reduced_model, newdata=heart_test, type = "response")
roc_logit <- roc(heart_test$HeartDisease, pred)
TPR <- roc_logit$sensitivities
FPR <- 1 - roc_logit$specificities

logistic_mse <- sum((pred - heart_test$HeartDisease)^2)