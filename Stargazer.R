library(stargazer)

model_list <- list(model1, model2, modeld)
stargazer(model_list, type = "html", out = "LRmodel.html", title = "Logistic Regression Models")