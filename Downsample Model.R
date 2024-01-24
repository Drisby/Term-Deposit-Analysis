library(caret)

#downsampling to balance classes
traindown <- downSample(x = train[, -ncol(train)], y = train$subscribed)

#Downsampling distribution
table(traindown$Class)

formulad <- subscribed ~ age + occupation + contact_method + month + campaign + pdays + emp_var_rate + cons_price_idx + cons_conf_idx + month + education_level
modeld <- glm(formulad, data = traindown, family = "binomial")