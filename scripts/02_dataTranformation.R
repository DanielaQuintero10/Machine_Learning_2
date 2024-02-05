set.seed(101195)
data <- read.csv("data/c3.csv")
str(data)
summary(data)
# some categories have '?' value, we will replace with 'other'
library(dplyr)
library(corrplot)

data <- data %>%
  mutate_all(~ ifelse(. == '?', 'other', .))

data[data$occupation == "?", ] # no more '?' :)

# Age as categorical variabel
data <- data %>%
  mutate(age_group = ((age - 15) %/% 5) + 1)
data$age_group <- as.factor(data$age_group)
str(data)
data <- data %>% select(-age)

# we will group education level a little bit more, to represent better the 
# educational levels.
# We will group into: no-PS, no-SS, no-HS, HS-grad, Some_college, Associate_degree,
# bachelors, Master, Post-Graduate studies (Doctorate+prof-school)
data <- data %>%
  mutate(educ_levels = case_when(
    education.num %in% c(1, 2) ~ 1,
    education.num %in% c(3, 4) ~ 2,
    education.num %in% c(5, 6, 7, 8) ~ 3,
    education.num %in% c(9) ~ 4,
    education.num %in% c(10) ~ 5,
    education.num %in% c(11, 12) ~ 6,
    education.num %in% c(13) ~ 7,
    education.num %in% c(14) ~ 8,
    education.num %in% c(15, 16) ~ 9,
    TRUE ~ NA_integer_
  ))
data$educ_levels <- as.factor(data$educ_levels)
data <- data %>% select(-education)
data <- data %>% select(-education.num)
str(data)
# labels
data$age_group <- factor(data$age_group, levels = 1:16, labels = c("15-19 years", "20-24 years",
                                                                   "25-29 years", "30-34 years",
                                                                   "35-39 years", "40-44 years",
                                                                   "45-49 years", "50-54 years",
                                                                   "55-59 years", "60-64 years",
                                                                   "65-69 years", "70-74 years",
                                                                   "75-79 years", "80-84 years",
                                                                   "85-89 years", "90-94 years"
                                                                   ))

data$educ_levels <- factor(data$educ_levels, levels = 1:9, labels = c("no-PS", "no-SS", "no-HS", 
                                                                      "HS-grad", "Some-college", "Assoc-degree",
                                                                      "Bachelors", "Masters", "Post-grad"))

# we need to transform working hours per week into a range
data <- data %>%
  mutate(hours_week = case_when(
    hours.per.week < 35 ~ "less than 35 hours",
    between(hours.per.week, 35, 50) ~ "between 35 and 50 hours",
    hours.per.week > 50 ~ "more than 50 hours",
    TRUE ~ NA_character_
  ))
data$hours_week <- as.factor(data$hours_week)
data <- data %>% select(-hours.per.week)

# country, occupation,workclass as factor:
data$native.country <- as.factor(data$native.country)
data$occupation <- as.factor(data$occupation)
data$workclass <- as.factor(data$workclass)

# Since we have the column fnlwgt avoiding biases, we will drop biasing columns
# marital.status, relationship, race, sex
data1 <- select(data, -marital.status, -relationship, -race, -sex)

# Salary needs to be binary variable:
# Salary needs to be binary variable:
data1 <- data1 %>%
  mutate(salary_binary = ifelse(salary == ">50K", 1, 0))
data1 <- data1 %>% select(-salary)

write.csv(data1, "data/data_ready.csv", row.names = FALSE)


################################ Function from ML2 class, Prof Sakowski:
getAccuracyAndGini <- function(model, 
                               data, 
                               target_variable = "salary_binary",
                               predicted_class = "1") {
  
  # funkcja dla modelu zapisanego jako wynik 
  # funkcji train() obliczy 
  # accuracy, specificity, sensitivity i Gini = 2 * AUC - 1
  
  require(pROC)
  data <- data %>% as.data.frame()
  
  # wygeneruj prawdopodobieństwa poziomu "predicted_class"
  forecasts_p <- predict(model, data,
                         type = "prob")[, predicted_class]
  
  # i samą przewidywaną kategorię
  if (any(class(model) == "train")) {
    forecasts_c <- predict(model, data) 
  } else forecasts_c <- predict(model, data, type = "class")
  
  # wartości rzeczywiste - pull() zamienia obiekt tibble w wektor
  real <- data[, target_variable]
  
  # pole pod wykresem ROC
  AUC <- roc(predictor = forecasts_p,
             response = (real == predicted_class),
             quiet = T)
  
  # tabela klasyfikacji i miary na niej oparte
  table <- confusionMatrix(forecasts_c,
                           real,
                           predicted_class) 
  # zbieramy w ostateczny wynik
  result <- c(table$overall[1], # cccuracy
              table$byClass[1:2], # sens, spec
              Gini = 2 * AUC$auc - 1)
  
  return(result)
  
}

