#libraries Used
library(dplyr)
library(ggplot2)

ggplot(data, aes(x = subscribed)) +
  geom_bar(aes(y = (..count..), fill = subscribed), show.legend = FALSE) +
  geom_text(
    aes(label = ..count.., y = ..count..),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    title = "Count of Subscribed (Yes/No)",
    x = "Subscribed",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the counts of 'Housing Loan' and 'Subscription' categories
housing_subscribed <- table(data$housing_loan, data$subscribed)

# Convert the table to a data frame for ggplot
housing_subscribed_df <- as.data.frame(housing_subscribed)
names(housing_subscribed_df) <- c("HousingLoan", "Subscription", "Count")

# Create a grouped bar plot for 'Housing Loan' and 'Subscription'
ggplot(housing_subscribed_df, aes(x = HousingLoan, y = Count, fill = Subscription)) +
  geom_bar(stat = "identity", position = "dodge") +  # Plotting bars
  labs(x = "Housing Loan", y = "Count", fill = "Subscription Status") +  # Labels for axes and legend
  scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("No Subscription", "Subscription")) +  # Fill colors and labels
  ggtitle("Counts by Housing Loan and Subscription Status") +  # Title for the plot
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a horizontal violin plot for age distribution by subscribed status
ggplot(data, aes(x = subscribed, y = age, fill = subscribed)) +
  geom_violin(alpha = 0.7, scale = "width", trim = FALSE) +
  labs(x = "Age", y = "Result", title = "Comparison of Age by Result") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

# Categorize 'emp_var_rate' into intervals or ranges
data$emp_var_rate_category <- cut(data$emp_var_rate, breaks = c(-Inf, -3, -2, -1, 0, 1, Inf))

# Calculate the proportion of subscriptions within each 'emp_var_rate' category
subscription_prop <- data %>%
  group_by(emp_var_rate_category) %>%
  summarise(subscription_rate = mean(subscribed == "yes"))

# Create a bar plot for the proportion of subscriptions across 'emp_var_rate' categories
ggplot(subscription_prop, aes(x = emp_var_rate_category, y = subscription_rate)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Employment Variation Rate Category", y = "Yes Propotion", title = "Proportion of Yes Subscriptions based on emp_var_rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate counts of subscriptions per occupation
occupation_counts <- data %>%
  group_by(occupation, subscribed) %>%
  summarise(count = n()) %>%
  filter(subscribed %in% c("yes", "no")) %>%
  group_by(occupation) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))# Arrange by total count from highest to lowest

# Reorder 'occupation' factor levels based on total count
data$occupation <- factor(data$occupation, levels = occupation_counts$occupation)

# Create a bar plot for count of subscriptions ('yes' and 'no') across occupations
ggplot(data, aes(x = occupation, fill = subscribed)) +
  geom_bar() +
  labs(x = "Job Role", y = "Count", title = "Subscriptions by Job Role") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate counts of subscriptions per education level
education_counts <- data %>%
  group_by(education_level, subscribed) %>%  # Group by education level and subscription status
  summarise(count = n()) %>%  # Count occurrences
  filter(subscribed %in% c("yes", "no")) %>%  # Filter for 'yes' and 'no' subscriptions
  group_by(education_level) %>%  # Regroup by education level
  summarise(total_count = sum(count)) %>%  # Calculate total counts
  arrange(desc(total_count))  # Arrange by total count from highest to lowest

# Reorder 'education_level' factor levels based on total count
data$education_level <- factor(data$education_level, levels = education_counts$education_level)

# Create a bar plot for count of subscriptions ('yes' and 'no') across education levels
ggplot(data, aes(x = education_level, fill = subscribed)) +  # Set x as education level and fill as subscribed
  geom_bar() +  # Plot bars
  labs(x = "Education Level", y = "Count", title = "Subscriptions by Education Level") +  # Labels for axes and title
  scale_fill_manual(values = c("lightblue", "lightgreen")) +  # Fill colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))