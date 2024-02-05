set.seed(101195)

# Loading package 
library(caret)
library(xgboost)
library(doParallel)
library(foreach)

train_XGB <- trainr
train_XGB <- subset(train_XGB, select = -id)

train_XGB$salary_binary <- factor(train_XGB$salary_binary,
                                  levels = c("1", "0"),
                                  labels = c("Positive", "Negative"))

## Formula
response_variable <- "salary_binary"
predictor_columns <- setdiff(names(train_XGB), c(response_variable))
formulaXGB <- as.formula(paste(response_variable, "~", paste(predictor_columns, collapse = " + ")))

# hyperparameters definition
parameters_xgb <- expand.grid(nrounds = seq(20, 120, 20),
                              max_depth = seq(8, 16, 2),
                              eta = c(0.25), 
                              gamma = 1,
                              colsample_bytree = seq(0.1, 0.8, 0.1),
                              min_child_weight = seq(50, 300, 50),
                              subsample = 0.8)

ctrl_cv3 <- trainControl(method = "cv", 
                         number = 3,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

XGB1 <- caret::train(formulaXGB,
                     data = train_XGB,
                     method = "xgbTree",
                     trControl = ctrl_cv3,
                     tuneGrid  = parameters_xgb)

saveRDS(XGB1, file = here("output", "XBG1.rds"))
XGB1 <- readRDS(here("output", "XBG1.rds"))
# nrounds = 40, max_depth = 8, colsample_bytree = 0.6, min_child_weight = 50

# we will try with nrounds from 20 till 100, max depth lower, from 4 till 12, min_child weight from 20 till 300 by 20
# we will add subsample c(0.6, 0.7, 0.75, 0.85, 0.9)
parameters_xgb2 <- expand.grid(nrounds = seq(20, 80, 20),
                              max_depth = seq(4, 10, 2),
                              eta = c(0.25), 
                              gamma = 1,
                              colsample_bytree = seq(0.3, 0.9, 0.1),
                              min_child_weight = seq(20, 200, 20),
                              subsample = c(0.6, 0.7, 0.8))
XGB2 <- caret::train(formulaXGB,
                     data = train_XGB,
                     method = "xgbTree",
                     trControl = ctrl_cv3,
                     tuneGrid  = parameters_xgb2)
XGB2
saveRDS(XGB2, file = here("output", "XBG2.rds"))
# The final values used for the model were nrounds = 80, max_depth = 4, eta =
# 0.25, gamma = 1, colsample_bytree = 0.3, min_child_weight = 20 and subsample
# = 0.8.

# third model
parameters_xgb3 <- expand.grid(nrounds = 80,
                               max_depth = 4,
                               eta = 0.25, 
                               gamma = 1,
                               colsample_bytree = 0.3,
                               min_child_weight = 20,
                               subsample = c(0.6, 0.7, 0.75, 0.8, 0.85, 0.9))
XGB3 <- caret::train(formulaXGB,
                     data = train_XGB,
                     method = "xgbTree",
                     trControl = ctrl_cv3,
                     tuneGrid  = parameters_xgb3)
XGB3
saveRDS(XGB3, file = here("output", "XBG3.rds"))
# forth model
parameters_xgb4 <- expand.grid(nrounds = 160,
                               max_depth = 4,
                               eta = 0.12, 
                               gamma = 1,
                               colsample_bytree = 0.3,
                               min_child_weight = 20,
                               subsample = c(0.9))
XGB4 <- caret::train(formulaXGB,
                     data = train_XGB,
                     method = "xgbTree",
                     trControl = ctrl_cv3,
                     tuneGrid  = parameters_xgb4)
XGB4
saveRDS(XGB4, file = here("output", "XBG4.rds"))

# Fifth model
parameters_xgb5 <- expand.grid(nrounds = 320,
                               max_depth = 4,
                               eta = 0.06, 
                               gamma = 1,
                               colsample_bytree = 0.3,
                               min_child_weight = 20,
                               subsample = c(0.9))
XGB5 <- caret::train(formulaXGB,
                     data = train_XGB,
                     method = "xgbTree",
                     trControl = ctrl_cv3,
                     tuneGrid  = parameters_xgb5)
XGB5
saveRDS(XGB5, file = here("output", "XBG5.rds"))

## comparison
## results training set
(models <- c("1":"5"))
results_trainXGB <- sapply(paste0("XGB", models),
                           function(x) getAccuracyAndGini(model = get(x),
                                                          data = train_XGB,
                                                          target_variable = "salary_binary",
                                                          predicted_class = "Positive")
)
## results on the test set
test_XGB <- testr
test_XGB$salary_binary <- factor(test_XGB$salary_binary,
                                  levels = c("1", "0"),
                                  labels = c("Positive", "Negative"))
results_testXGB <- sapply(paste0("XGB", models),
                          function(x) getAccuracyAndGini(model = get(x),
                                                         data = test_XGB,
                                                         target_variable = "salary_binary",
                                                         predicted_class = "Positive")
)

# ROCs comparison
# train
ROC_trainXGB1 <- pROC::roc(train_XGB$salary_binary, 
                       predict(XGB1,
                               train_XGB, 
                               type = "prob",
                               n.trees = 500)[, "Positive"])

ROC_trainXGB2 <- pROC::roc(train_XGB$salary_binary, 
                           predict(XGB2,
                                   train_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])
ROC_trainXGB3 <- pROC::roc(train_XGB$salary_binary, 
                           predict(XGB3,
                                   train_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])
ROC_trainXGB4 <- pROC::roc(train_XGB$salary_binary, 
                           predict(XGB4,
                                   train_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])
ROC_trainXGB5 <- pROC::roc(train_XGB$salary_binary, 
                           predict(XGB5,
                                   train_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])

# test
ROC_testXGB1 <- pROC::roc(test_XGB$salary_binary, 
                           predict(XGB1,
                                   test_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])

ROC_testXGB2 <- pROC::roc(test_XGB$salary_binary, 
                           predict(XGB2,
                                   test_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])
ROC_testXGB3 <- pROC::roc(test_XGB$salary_binary, 
                           predict(XGB3,
                                   test_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])
ROC_testXGB4 <- pROC::roc(test_XGB$salary_binary, 
                           predict(XGB4,
                                   test_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])
ROC_testXGB5 <- pROC::roc(test_XGB$salary_binary, 
                           predict(XGB5,
                                   test_XGB, type = "prob",
                                   n.trees = 500)[, "Positive"])
# plot and compare
XGB_results <- 
  list(
    ROC_trainXGB1 = ROC_trainXGB1,
    ROC_testXGB1 = ROC_testXGB1,
    ROC_trainXGB2 = ROC_trainXGB2,
    ROC_testXGB2 = ROC_testXGB2,
    ROC_trainXGB3 = ROC_trainXGB3,
    ROC_testXGB3 = ROC_testXGB3,
    ROC_trainXGB4 = ROC_trainXGB4,
    ROC_testXGB4 = ROC_testXGB4,
    ROC_trainXGB5 = ROC_trainXGB5,
    ROC_testXGB5 = ROC_testXGB5
  ) %>%
  pROC::ggroc(alpha = 0.5, linetype = 1, size = 1) + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color = "grey", 
               linetype = "dashed") +
  labs(subtitle = paste0("Gini TEST: ",
                      "XGB_model = ", 
                      round(100 * (2 * auc(ROC_testXGB1) - 1), 1), "%, ",
                      "XGB_model2 = ", 
                      round(100 * (2 * auc(ROC_testXGB2) - 1), 1), "%, ",
                      "XGB_model3 = ",
                      round(100 * (2 * auc(ROC_testXGB3) - 1), 1), "%, ",
                      "XGB_model4 = ", 
                      round(100 * (2 * auc(ROC_testXGB4) - 1), 1), "%, ",
                      "XGB_model5 = ",
                      round(100 * (2 * auc(ROC_testXGB5) - 1), 1), "%, ",
                      "Gini TRAIN: ",
                      "XGB_model = ", 
                      round(100 * (2 * auc(ROC_trainXGB1) - 1), 1), "%, ",
                      "XGB_model2 = ", 
                      round(100 * (2 * auc(ROC_trainXGB2) - 1), 1), "%, ",
                      "XGB_model3 = ", 
                      round(100 * (2 * auc(ROC_trainXGB3) - 1), 1), "%, ",
                      "XGB_model4 = ", 
                      round(100 * (2 * auc(ROC_trainXGB4) - 1), 1), "%, ",
                      "XGB_model5 = ", 
                      round(100 * (2 * auc(ROC_trainXGB5) - 1), 1), "%, ")) +
  theme_bw() + coord_fixed() +
  scale_color_brewer(palette = "Paired")
