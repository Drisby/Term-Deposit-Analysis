#Age
age_t_test <- t.test(data$age ~ data$subscribed)
age_t_test

#housing loan
housing_loan_cross_tab <- table(data$housing_loan, data$subscribed)
housing_loan_chi_square_test <- chisq.test(housing_loan_cross_tab)
housing_loan_chi_square_test

#occupation
occupation_cross_tab <- table(data$occupation, data$subscribed)
occupation_chi_square_test <- chisq.test(occupation_cross_tab)
occupation_chi_square_test

#education
education_cross_tab <- table(data$education_level, data$subscribed)
education_fisher_test <- fisher.test(education_cross_tab, workspace = 2e8, hybrid = TRUE, control = list(max = 1e5))
education_fisher_test

#emp_var_rate
emp_var_rate_t_test <- t.test(data$emp_var_rate ~ data$subscribed)
emp_var_rate_t_test

#Multicollinearity check
vif(model2)