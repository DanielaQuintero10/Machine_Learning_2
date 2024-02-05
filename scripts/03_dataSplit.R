# Before splitting the data we will scale the numeric variables for
# better performance of NN and KNN
library(dplyr)
library(caret)

set.seed(101195)
data_r <- data1
str(data_r)


# Salary to factor
data_r$salary_binary <- factor(data_r$salary_binary, levels = c(1, 0))

# data split 80% train, 20% test
trainrIndex <- createDataPartition(data_r$salary_binary, 
                                  times=1, 
                                  p = .8, 
                                  list = FALSE)
trainr <- data_r[trainrIndex, ]
testr <- data_r[-trainrIndex, ]

str(trainr)
str(testr)
write.csv(trainr, "data/train_RF.csv", row.names = FALSE)
write.csv(testr, "data/test_RF.csv", row.names = FALSE)

# Define columns to exclude from scaling
exclude_columns <- c("id", "salary_binary")

# Identify numeric columns to scale
numeric_columns <- sapply(trainr, is.numeric) & !(names(trainr) %in% exclude_columns)
numeric_columns2 <- sapply(testr, is.numeric) & !(names(testr) %in% exclude_columns)

# Scale numeric columns
scaled_trainr <- trainr %>%
  mutate(across(all_of(names(trainr)[numeric_columns]), scale))
scaled_testr <- testr %>%
  mutate(across(all_of(names(testr)[numeric_columns2]), scale))

# View the scaled data
str(scaled_trainr)
str(scaled_testr)

# Save to CSV
write.csv(scaled_trainr, "data/train_scaled.csv", row.names = FALSE)
write.csv(scaled_testr, "data/test_scaled.csv", row.names = FALSE)
