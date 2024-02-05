library(tidyverse)
library(caret)
library(pROC)
library(randomForest)
library(ranger)
library(here)
set.seed(101195)

# We use the transformed 'data', we don't need scaling
train_dataRF <- trainr

## Formula
response_variable <- "salary_binary"
predictor_columns <- setdiff(names(train_dataRF), c("id", response_variable))
formulaRF <- as.formula(paste(response_variable, "~", paste(predictor_columns, collapse = " + ")))

# x:          formula of the model
# data:       data frame with observations
# ntree:      number of trees (default = 500)
# mtry:       number of predictors to use (default = sqrt(number of all predictors))
# replace:    TRUE for sampling with replacement 
# sampsize:   size of the sample used to create trees (drawn from full sample) 
#             (for replace = TRUE the default value = nrow(x))
# importance: TRUE if we want to save measures of predictors importance 
#             to the result object 

RF_model <- randomForest(formulaRF, data = train_dataRF, 
                         ntree = 1000,
                         mtry = sqrt(ncol(train_dataRF) - 2),  # Exclude id and response_variable
                         replace = TRUE, 
                         sampsize = floor(0.5 * nrow(train_dataRF)),  # Use 50% of the data
                         importance = TRUE)

saveRDS(RF_model, file = here("output", "RF_model.rds"))

# Check the results
print(RF_model)

# The estimate OOB Error is 14% 
# The prediction error is significantly lower for 0 than for 1

plot(RF_model)
# The plot above helps determine the appropriate number of trees.
# The dark solid line presents the total OOB error, the red line presents 
# the prediction error for 1 response, while the green line for 0 response.
# As the number of trees is higher, the OOB error is smaller, and 
# converges to approx. 100 trees.

RF_model2 <- 
  randomForest(formulaRF,
               data = train_dataRF,
               ntree = 200,
               sampsize = nrow(train_dataRF),
               mtry = 19,
               # minimum number of obs in the terminal nodes
               nodesize = 100,
               # we also generate predictors importance measures,
               importance = TRUE)
print(RF_model2)
saveRDS(object = RF_model2, file = here("output", "RF_model2.rds"))

# Plot
plot(RF_model2)

# maybe with 25 trees?
# Cross validation to find better parameters:
# We have 19 predictors, we can try between 5 and 17
parameters_rf <- expand.grid(mtry = 5:17)
ctrl_cv <- trainControl(method = "cv", number = 7)

set.seed(101195)
RF_model3 <- train(formulaRF,
                   data = train_dataRF,
                   method = "rf",
                   ntree = 25,
                   nodesize = 150,
                   tuneGrid = parameters_rf,
                   trControl = ctrl_cv,
                   importance = TRUE)

# saving the object to the external file
saveRDS(object = RF_model3, file = here("output", "RF_model3.rds"))

# loading the object from the external file
RF_model3 <- readRDS(here("output", "RF_model3.rds"))

# Check highest accuracy
RF_model3

# the final value used for the model was mtry = 17

plot(RF_model3$results$mtry,
     RF_model3$results$Accuracy, type = "b")

plot(RF_model3$results$mtry,
     RF_model3$results$Kappa, type = "b")
# The optimal value for mtry = 17
plot(RF_model3)
# According to the plot, the optimal value of mtry is 17.

### Comparison of the 3 tests. Now we will compare between test and predict
test_dataRF <- testr
test_dataRF$salary_binary <- as.factor(test_dataRF$salary_binary)
str(test_dataRF)
# RF_model
pred_train_RF <- predict(RF_model, 
                         train_dataRF, 
                         type = "prob")[, "1"]
ROC_trainRF  <- roc(as.numeric(train_dataRF$salary_binary == "1"), 
                     pred_train_RF)
pred_test_RF  <- predict(RF_model, 
                         test_dataRF, 
                         type = "prob")[, "1"]
ROC_test_RF   <- roc(as.numeric(test_dataRF$salary_binary == "1"), 
                     pred_test_RF)
# RF_model2
pred_train_RF2 <- predict(RF_model2, 
                          train_dataRF, 
                          type = "prob")[, "1"]
ROC_trainRF2  <- roc(as.numeric(train_dataRF$salary_binary == "1"), 
                     pred_train_RF2)
pred_test_RF2  <- predict(RF_model2, 
                          test_dataRF, 
                          type = "prob")[, "1"]
ROC_test_RF2   <- roc(as.numeric(test_dataRF$salary_binary == "1"), 
                      pred_test_RF2)
# RF_model3
pred_train_RF3 <- predict(RF_model3, 
                          train_dataRF, 
                          type = "prob")[, "1"]
ROC_trainRF3  <- roc(as.numeric(train_dataRF$salary_binary == "1"), 
                     pred_train_RF3)
pred_test_RF3  <- predict(RF_model3, 
                          test_dataRF, 
                          type = "prob")[, "1"]
ROC_test_RF3   <- roc(as.numeric(test_dataRF$salary_binary == "1"), 
                      pred_test_RF3)

# plot and compare
RF_results <- 
  list(
    ROC_trainRF = ROC_trainRF,
    ROC_test_RF = ROC_test_RF,
    ROC_trainRF2 = ROC_trainRF2,
    ROC_test_RF2 = ROC_test_RF2,
    ROC_trainRF3 = ROC_trainRF3,
    ROC_test_RF3 = ROC_test_RF3
  ) %>%
    pROC::ggroc(alpha = 0.5, linetype = 1, size = 1) + 
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color = "grey", 
                 linetype = "dashed") +
    labs(title = paste0("Gini TEST: ",
                        "RF_model = ", 
                        round(100 * (2 * auc(ROC_test_RF) - 1), 1), "%, ",
                        "RF_model2 = ", 
                        round(100 * (2 * auc(ROC_test_RF2) - 1), 1), "%, ",
                        "RF_model3 = ",
                        round(100 * (2 * auc(ROC_test_RF3) - 1), 1), "%, "),
  
         subtitle =  paste0("Gini TRAIN: ",
                            "RF_model = ", 
                            round(100 * (2 * auc(ROC_trainRF) - 1), 1), "%, ",
                            "RF_model2 = ", 
                            round(100 * (2 * auc(ROC_trainRF2) - 1), 1), "%, ",
                            "RF_model3 = ", 
                            round(100 * (2 * auc(ROC_trainRF3) - 1), 1), "%, ")) +
    theme_bw() + coord_fixed() +
    scale_color_brewer(palette = "Paired")

