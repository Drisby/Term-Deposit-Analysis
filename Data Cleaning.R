library(readxl)
library(tidyverse)
library(psych)

# Read data from Excel
data <- read_excel("term.xlsx")

# Calculate percentage of missing values for each variable
data_NA <- colSums(is.na(data)) * 100 / nrow(data)
data_NA <- data.frame(Variable = names(data_NA), Missing_Percentage = data_NA) %>%
  arrange(desc(Missing_Percentage))


# Removing 'credit_default' due to imbalanced data
data <- select(data, -credit_default, - poutcome)

# Age cleaning
data$age[data$age > 150] <- NA
data <- data[complete.cases(data$age), ]

# day of week cleaning
table(data$day_of_week)
data$day_of_week[data$day_of_week == "tues"] <- "tue"
data <- data[complete.cases(data$day_of_week), ]

# Visualize 'pdays' using histogram excluding '999'
filtered_data <- filter(data, pdays != 999)
ggplot(filtered_data, aes(x = pdays)) +
  geom_histogram(bins = 20, color = 'black', fill = 'skyblue', alpha = 0.7) +
  labs(x = 'pday', y = 'Frequency', title = 'Histogram of pday excluding 999')

# Categorize 'pdays' into 'Contacted' and 'Not contacted'
data$pdays <- ifelse(data$pdays == 999, "Not Contacted", "Contacted")

# Handling outliers in 'contact_duration'
data$contact_duration[data$contact_duration > 1000] <- NA
data <- data[complete.cases(data$contact_duration), ]

# Factorize relevant variables and handle 'unknown' values
data <- data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(~all(. == "unknown"), ~NA) %>%
  mutate_at(vars(housing_loan, personal_loan, marital_status, occupation), ~as.factor(replace(., . == "unknown", NA)))

# Drop unused levels in specific factor columns
data <- data %>%
  mutate(across(c(housing_loan, personal_loan, marital_status, occupation), ~droplevels(.)))

# Remove rows with NA values in 'marital_status'
data <- data %>%
  filter(!is.na(marital_status))


columns_to_impute <- c("housing_loan", "personal_loan", "occupation")
data <- impute_specific_variables(data, columns_to_impute)

# Summary of the cleaned dataset
summary(data)
