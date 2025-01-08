library(readxl)
library(dplyr)
library(ggplot2)
library(naniar)

data <- read_excel("Midterm_Dataset_Section(C).xlsx")
print("Data loaded:")
print(head(data))

summary(data)

total_missing <- sum(is.na(data))
print(paste("Total missing values across the dataset:", total_missing))

missing_counts <- sapply(data, function(x) sum(is.na(x)))
print("Missing values per column:")
print(missing_counts)

get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

income_lower_bound <- 50000
income_upper_bound <- 500000
data <- data %>%
  filter(person_income >= income_lower_bound & person_income <= income_upper_bound)
print("Data after filtering by income range:")
print(head(data))

age_lower_bound <- 20
age_upper_bound <- 80
data <- data %>%
  filter(person_age >= age_lower_bound & person_age <= age_upper_bound)
print("Data after filtering by Age:")
print(head(data))

data$person_age[is.na(data$person_age)] <- mean(data$person_age, na.rm = TRUE)
data$person_income[is.na(data$person_income)] <- median(data$person_income, na.rm = TRUE)
print("Data after imputing numeric columns:")
print(head(data))

mode_education <- get_mode(data$person_education)
data$person_education[is.na(data$person_education)] <- mode_education
data$person_education <- as.factor(data$person_education)

mode_loan_status <- get_mode(data$loan_status)
data$loan_status[is.na(data$loan_status)] <- mode_loan_status
data$loan_status <- as.factor(data$loan_status)

mode_gender <- get_mode(data$person_gender)
data$person_gender[is.na(data$person_gender)] <- mode_gender
data$person_gender <- as.factor(data$person_gender)

print("Data after imputing and converting categorical columns:")
print(head(data))

data <- data %>%
  distinct()
print("Data after removing duplicates:")
print(head(data))

data$normalized_person_income <- (data$person_income - min(data$person_income)) / (max(data$person_income) - min(data$person_income))
print("Data after normalization of person_income:")
print(head(data))

summary_stats <- summary(data)
print("Summary statistics of dataset:")
print(summary_stats)

numeric_columns <- sapply(data, is.numeric)
std_devs <- sapply(data[, numeric_columns, drop = FALSE], sd, na.rm = TRUE)
print("Standard deviations of numeric columns:")
print(std_devs)

data <- na.omit(data)
print(head(data))

std_data <- data.frame(Variable = names(std_devs), StdDev = std_devs)
ggplot(std_data, aes(x = Variable, y = StdDev)) +
  geom_col(fill = "blue") +
  theme_minimal() +
  labs(title = "Standard Deviation of Numeric Columns", x = "Variable", y = "Standard Deviation")

Q1 <- quantile(data$person_income, 0.25, na.rm = TRUE)
Q3 <- quantile(data$person_income, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- data$person_income < lower_bound | data$person_income > upper_bound

print("Visualizing outliers in person_income:")
ggplot(data, aes(x = person_income)) +
  geom_histogram(fill = "blue", color = "black", binwidth = 500) +
  geom_vline(xintercept = c(lower_bound, upper_bound), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Person Income with Outlier Thresholds")

data <- data[!outliers, ]
print("Data after removing outliers:")
print(head(data))

ggplot(data, aes(x = normalized_person_income)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Normalized Person Income", x = "Normalized Income", y = "Frequency")

print("Final cleaned data:")
print(head(data))

write.csv(data, "cleaned_dataset.csv", row.names = FALSE)

print("Cleaned dataset exported to 'cleaned_dataset.csv' successfully!")

