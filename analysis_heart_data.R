library(tidyverse)
library(plotly)
library(widgetframe)
library(tidytext)
library(randomForest)
library(xgboost)
library(caret)
library(pROC)
library(xgboost)



### load heart data ###
heart <- read_csv("data/heart.csv")
heart$Sex <- factor(heart$Sex)
heart$ExerciseAngina <- factor(heart$ExerciseAngina)
heart$RestingECG <- factor(heart$RestingECG)
heart$ST_Slope <- factor(heart$ST_Slope)
chol_median <- median(heart[heart$Cholesterol != 0,]$Cholesterol)
heart[heart$Cholesterol == 0,]$Cholesterol <- chol_median

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

pred <- round(pred)
logistic_por <- mean((pred - heart_test$HeartDisease)^2) # accuracy
auc_logit <- roc_logit$auc
auc_value <- as.numeric(stringr::str_extract(auc_logit, "[0-9]+\\.[0-9]+"))

### train and test random forest ###
heart_rf <- randomForest(
  formula = HeartDisease~.,
  data = heart_train,
  mtry = 11,
  ntree = 1000
)
importance_df <- data.frame(
  Variables = rownames(as.data.frame(heart_rf$importance)),
  Importance = as.data.frame(importance(heart_rf))$IncNodePurity
)
importance_df <- importance_df %>%
                    arrange(-Importance)
importance_df$Variables <- factor(importance_df$Variables, 
                                  levels = importance_df$Variables)
pred_rf <- predict(heart_rf, newdata = heart_test, type = "response")
pred_rf <- round(pred_rf)
rf_por <- mean((pred_rf - heart_test$HeartDisease)^2) # accuracy
rf_por

### train and test extreme boosting ### 
heart_train$HeartDisease <- factor(heart_train$HeartDisease)

tune_grid<-  expand.grid(max_depth = 6,
                         nrounds = 100,
                         eta = c(10,4,2,1,0.5,0.1,0.01,0.001,0.0001),
                         gamma = seq(0, 2, by = 0.1),
                         subsample = 1,
                         min_child_weight = 1,
                         colsample_bytree = 0.6
)

heart_xgb<-caret::train(
  form=HeartDisease~.,
  method="xgbTree",
  data=heart_train,
  tuneGrid=tune_grid,
  verbosity = 0
)

pred_xgb <- predict(heart_xgb, heart_test)
pred_xgb <- as.numeric(pred_xgb) - 1
xgb_por <- mean((pred_xgb - heart_test$HeartDisease)^2)
var_Imp <- varImp(heart_xgb)
ximportance_df <- data.frame(
  Variables = row.names(var_Imp$importance),
  Importance = var_Imp$importance$Overall
)
ximportance_df <- ximportance_df %>%
  arrange(-Importance)
ximportance_df <- ximportance_df %>%
                    filter(!Variables %in% c("ChestPainTypeTA", "ChestPainTypeNAP","ST_SlopeFlat","RestingECGST"))
ximportance_df[ximportance_df$Variables == "ChestPainTypeATA",1] <- "ChestPainType"
ximportance_df[ximportance_df$Variables == "SexM",1] <- "Sex"
ximportance_df[ximportance_df$Variables == "RestingECGNormal",1] <- "RestingECG"
ximportance_df[ximportance_df$Variables == "ST_SlopeUp",1] <- "ST_Slope"
ximportance_df$Variables <- factor(ximportance_df$Variables, 
                                  levels = ximportance_df$Variables)





