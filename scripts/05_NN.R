set.seed(101195)
library(tidyverse)
library(caret)
library(neuralnet)
library(pROC)
library(here)
library(keras)
library(tensorflow)

train_dataNN <- scaled_trainr
test_dataNN <- scaled_testr

## Formula
predictor_columns <- setdiff(names(train_dataNN), c("id", "salary_binary"))
formulaNN <- as.formula(paste("salary_binary", "~", paste(predictor_columns, collapse = " + ")))

# encoding categorical variables
train_dataNN_mtx <- model.matrix(object = formulaNN, data = train_dataNN)

dim(train_dataNN_mtx)

# check column names
colnames(train_dataNN_mtx)

#manually correct colnames
colnames(train_dataNN_mtx) <- gsub(" ", "_",  colnames(train_dataNN_mtx))
colnames(train_dataNN_mtx) <- gsub(",", "_",  colnames(train_dataNN_mtx))
colnames(train_dataNN_mtx) <- gsub("/", "",   colnames(train_dataNN_mtx))
colnames(train_dataNN_mtx) <- gsub("-", "_",  colnames(train_dataNN_mtx))
colnames(train_dataNN_mtx) <- gsub("'", "",   colnames(train_dataNN_mtx))
colnames(train_dataNN_mtx) <- gsub("\\+", "", colnames(train_dataNN_mtx))
colnames(train_dataNN_mtx) <- gsub("\\^", "", colnames(train_dataNN_mtx))
colnames(train_dataNN_mtx)

col_list <- paste(c(colnames(train_dataNN_mtx[, -1])), collapse = "+")
col_list <- paste(c("salary_binary_1 ~ ", col_list), collapse = "")
(FormulaNN_2 <- formula(col_list))

# training the model:
NN_model1 <- 
  data.frame(train_dataNN_mtx,
             salary_binary_1 = as.numeric(train_dataNN$salary_binary == "1")) %>%
  sample_n(1000) %>%
  neuralnet(FormulaNN_2,
            data = data.frame(train_dataNN_mtx,
                              salary_binary_1 = as.numeric(train_dataNN$salary_binary == "1")),
            hidden = c(5, 3), # number of neurons in hidden layers
            linear.output = FALSE, # T for regression, F for classification
            stepmax = 1e7,
            rep = 2,
            learningrate.limit = NULL,
            learningrate.factor = list(minus = 0.5, plus = 1.2),
            algorithm = "rprop+",
            threshold = 0.01)
NN_model1$result.matrix

saveRDS(object = NN_model1, file = here("output", "NN_model1.rds"))

NN1_plot <- plot(NN_model1, rep = "best")

########### testing the model
test_dataNN_mtx <- model.matrix(object = formulaNN, data = test_dataNN)

dim(test_dataNN_mtx)

# check column names
colnames(test_dataNN_mtx)

#manually correct colnames
colnames(test_dataNN_mtx) <- gsub(" ", "_",  colnames(test_dataNN_mtx))
colnames(test_dataNN_mtx) <- gsub(",", "_",  colnames(test_dataNN_mtx))
colnames(test_dataNN_mtx) <- gsub("/", "",   colnames(test_dataNN_mtx))
colnames(test_dataNN_mtx) <- gsub("-", "_",  colnames(test_dataNN_mtx))
colnames(test_dataNN_mtx) <- gsub("'", "",   colnames(test_dataNN_mtx))
colnames(test_dataNN_mtx) <- gsub("\\+", "", colnames(test_dataNN_mtx))
colnames(test_dataNN_mtx) <- gsub("\\^", "", colnames(test_dataNN_mtx))
colnames(test_dataNN_mtx)

df2 <-data.frame(test_dataNN_mtx[, -1], 
                 salary_binary_1 = as.numeric(test_dataNN$salary_binary == "1"))
pred_NN_model1 <- compute(NN_model1, df2)
pred_NN_model1$net.result %>% head(10)

# confusion matrix
(CM_NN_model1 <-
    confusionMatrix(as.numeric((pred_NN_model1$net.result > 0.5)) %>% as.factor(),
                    as.factor(ifelse(test_dataNN$salary_binary == "1", 1, 0))) )

# ROC curves
ROC_train_NN1 <- 
  pROC::roc(as.numeric(train_dataNN$salary_binary == "1"), 
            compute(NN_model1, train_dataNN_mtx)$net.result[, 1])
ROC_test_NN1  <- 
  pROC::roc(as.numeric(test_dataNN$salary_binary == "1"), 
            compute(NN_model1, df2)$net.result[, 1])

NN_Results <- list(
  ROC_train_NN1 = ROC_train_NN1,
  ROC_test_NN1  = ROC_test_NN1
) %>%
  pROC::ggroc(alpha = 0.5, linetype = 1, size = 1) + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color = "grey", 
               linetype = "dashed") +
  labs(subtitle = paste0("Gini TRAIN: ",
                         "nn = ", 
                         round(100*(2 * auc(ROC_train_NN1) - 1), 1), "%, ",
                         "Gini TEST: ",
                         "nn = ", 
                         round(100*(2 * auc(ROC_test_NN1) - 1), 1), "%, "
  )) +
  theme_bw() + coord_fixed() +
  scale_color_brewer(palette = "Paired")
