# Libraries Used
library(caret)
library(stargazer)
set.seed(402652)

index <- createDataPartition(data$subscribed, times = 1, p = 0.8, list = FALSE)

train <- data[index, ] #80% of the data
test <- data[-index, ] #20% of the data

formula1 <- subscribed ~ age + housing_loan + emp_var_rate + occupation + education_level
model1 <- glm(formula1, data = train, family = "binomial")

formula2 <- subscribed ~ age + occupation + contact_method + month + campaign + pdays + emp_var_rate + cons_price_idx + cons_conf_idx + month + education_level
model2 <- glm(formula2, data = train, family = "binomial")



