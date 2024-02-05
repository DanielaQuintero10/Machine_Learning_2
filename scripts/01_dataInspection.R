data <- read.csv("data/c3.csv")
head(data)
str(data)
summary(data)
unique(data$education)
unique(data$marital.status)
unique(data$native.country)
unique(data$occupation)
unique(data$race)
unique(data$relationship)
unique(data$sex)
unique(data$workclass)
# Load necessary libraries
library(ggplot2)
library(scales)
library(gridExtra)

# Distribution of Gender and Salary
bar_plot_combined <- ggplot(data, aes(x = sex, fill = salary)) +
  geom_bar(position = "stack") +  # Use position = "stack" to stack bars
  labs(title = "Distribution of Gender and Salary") +
  facet_grid(scales = "free_y") +  # Facet by gender with free y-axis scales
  theme(axis.text.x = element_blank())  # Remove x-axis labels for better presentation

# Show the percentage labels on top of the bars
gender_salary <- bar_plot_combined + geom_text(aes(label = sprintf("%.1f%%", ..count../sum(..count..)*100)),
                                                   position = position_stack(vjust = 0.5),
                                                   stat = "count", color = "white")


# From our data we see that men tend to earn more than women.
# Our data also contains more information about men (2/3) than woman (1/3).
# Drop gender

# Bar plot % of <=50k and >50k in our data
bar_plot_salary <- ggplot(data, aes(x = salary, fill = salary)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Salary in the Data") +
  theme(axis.text.x = element_blank()) +
  geom_text(stat = "count", aes(label = sprintf("%.1f%%", after_stat(count/sum(count)*100))),
            color = "black")

# Countries frequency
country_counts <- table(data$native.country)
country_percentages <- prop.table(country_counts) * 100
df <- data.frame(country = names(country_percentages), percentage = country_percentages)
df <- df[order(-df$percentage.Freq), ]

# Plot salary distribution by native country
country_salary <- ggplot(data, aes(x = native.country, fill = salary)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Salary by Native Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot salary distribution by occupation
ggplot(data, aes(x = salary, fill = occupation)) +
  geom_bar(position = "stack") +
  labs(title = "Occupation's frequency in salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bar_plot_combined2 <- ggplot(data, aes(x = occupation, fill = salary)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Occupation and Salary") +
  facet_grid(scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the percentage labels on top of the bars
Salary_job <- bar_plot_combined2 + geom_text(aes(label = sprintf("%.1f%%", ..count../sum(..count..)*100)),
                              position = position_stack(vjust = 0.5),
                              stat = "count", color = "black")

# Plot salary distribution by race
p <- ggplot(data, aes(x = race, fill = salary)) +
  geom_bar(position = "stack" ) +
  labs(title = "Distribution of Race by Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Show the percentage labels on top of the bars
race <- p + geom_text(aes(label = sprintf("%.1f%%", ..count../sum(..count..)*100)),
                               position = position_stack(vjust = 0.5),
                               stat = "count", color = "black")


salary_race <- ggplot(data, aes(x = salary, fill = race)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Salary by Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# drop race

# Plot salary distribution by marital status
marital <- ggplot(data, aes(x = marital.status, fill = salary)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Salary by Civil Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

relationship <- ggplot(data, aes(x = relationship, fill = salary)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Salary by Relationship") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

marital_salary <- ggplot(data, aes(x = salary, fill = marital.status)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Salary by Civil Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# drop marital.status
# drop column - relationship.

## Plot age distribution 
age <- ggplot(data, aes(x = age, fill = salary)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of age and salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# group age into ranges
# histogram age
# Plot histogram of age variable
hist_age <- ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age Variable",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

## Plot working hours 
wh <- ggplot(data, aes(x = hours.per.week, fill = salary)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of working hours and salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# group also into ranges less than 35, 35-50, more than 50.

## Plot education distribution 
educ <- ggplot(data, aes(x = education, fill = salary)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of education and salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salary_educ <- ggplot(data, aes(x = education, fill = salary)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of education and salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


