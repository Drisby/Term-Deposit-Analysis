library(pROC)
library(car)

prediction1 <- predict(model1, test, type="response")
class_pred1 <- as.factor(ifelse(prediction1 >0.5, "yes", "no"))
postResample(class_pred1, test$subscribed)
confusionMatrix(data = class_pred1, test$subscribed)

prediction2 <- predict(model2, test, type="response")
class_pred2 <- as.factor(ifelse(prediction2 >0.5, "yes", "no"))
postResample(class_pred2, test$subscribed)
confusionMatrix(data = class_pred2, test$subscribed)


predictiond <- predict(modeld, test, type="response")
class_predd <- as.factor(ifelse(predictiond >0.5, "yes", "no"))
postResample(class_predd, test$subscribed)
confusionMatrix(data = class_predd, test$subscribed)

# Convert "Yes" and "No" to 1 and 0 directly
test$subscribed_binary <- ifelse(test$subscribed == "Yes", 1, 0)

# Check the levels in test$subscribed_binary
table(test$subscribed_binary)

# Predict probabilities on the test data using model2
predictions_model2 <- predict(model2, newdata = test, type = "response")

# Creating the ROC curve
roc_curve_model2 <- roc(test$subscribed, predictions_model2)

# Plotting the ROC curve
plot(roc_curve_model2, main = "ROC Curve - Model 2", col = "blue")

# Adding labels and legend
legend("bottomright", legend = c("ROC Curve"), col = "blue", lty = 1)

plot(model2)
