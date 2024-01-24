#libraries Used
library(mice)

# Function to impute NA values for specified variables using mice
impute_specific_variables <- function(data, vars_to_impute) {
  # Subset the dataset to include only the specified variables
  data_to_impute <- data[, vars_to_impute]
  
  imp <- mice(data_to_impute, method = 'pmm', seed = 40265478)
  
  imp_completed <- complete(imp)
  
  # Replace original columns in the dataset with imputed values
  for (col in vars_to_impute) {
    data[[col]] <- imp_completed[[col]]
  }
  
  return(data)
}