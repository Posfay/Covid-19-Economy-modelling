library(leaps)
library(plyr)
library(dplyr)
library(glmnet)
library(magrittr)
library(tidyverse)
library(caret)
library(olsrr)
library(broom)
library(scales)
library(withr)

# Initializing data
init_data = function() {
  read_data = read.csv(file = "monthly-narrowed-merged-data.csv", stringsAsFactors = FALSE)
  # covid_data = monthly.narrowed.merged.data
  final_data = subset(read_data, select = -c(iso_code, location, date, human_development_index, population))
  npreds = ncol(final_data) - 1
  return(list(read_data, final_data, npreds))
}

# Training & Testing data separation
create_train_test = function(all_data, train_percent) {
  train = all_data %>% sample_frac(train_percent)
  test = all_data %>% setdiff(train)
  return(list(train, test))
}

# ----------- Best subset regression ---------------------
best_subs_reg = function(traindata, testdata) {
  npreds = ncol(traindata) - 1
  # Performing best subset regression on training data
  best_subs_model = regsubsets(oecd_cli_index~., data = traindata, nvmax = npreds)
  best_subs_model_summary = summary(best_subs_model)
  
  # Analyzing best subset regression
  par(mfrow = c(2,2))
  plot(best_subs_model_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  plot(best_subs_model_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
  adj_r2_max = which.max(best_subs_model_summary$adjr2)
  points(adj_r2_max, best_subs_model_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
  plot(best_subs_model_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
  cp_min = which.min(best_subs_model_summary$cp) # 10
  points(cp_min, best_subs_model_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
  plot(best_subs_model_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  bic_min = which.min(best_subs_model_summary$bic) # 6
  points(bic_min, best_subs_model_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
  
  # Measuring accuracy of model on test data (MSE)
  test_mat = model.matrix(oecd_cli_index~., data = testdata)
  val_errors = rep(NA, npreds)
  for(i in 1:npreds) {
    coefi = coef(best_subs_model, id = i)
    pred = test_mat[,names(coefi)] %*% coefi
    # Calculate the MSE
    val_errors[i] = mean((testdata$oecd_cli_index - pred)^2)
  }
  
  # Analyzing model MSE
  par(mfrow = c(1,1))
  min = which.min(val_errors)
  plot(val_errors, type = 'b')
  points(min, val_errors[min][1], col = "red", cex = 2, pch = 20)
}

# --------------  K-folds validation  ------------------------
# Predict function
predict.regsubsets = function(object, newdata, id,...) {
  # form = as.formula(object$call[[2]])   # Extract the formula used when we called regsubsets()
  mat = model.matrix(oecd_cli_index~., newdata)     # Build the model matrix
  coefi = coef(object, id = id)         # Extract the coefficients of the ith model
  xvars = names(coefi)                  # Pull out the names of the predictors used in the ith model
  mat[,xvars] %*% coefi                 # Make predictions using matrix multiplication
}

kfolds_validation = function(all_data, k) {
  npreds = ncol(all_data) - 1
  folds = sample(1:k, nrow(all_data), replace = TRUE)
  cv_errors = matrix(NA, k, npreds, dimnames = list(NULL, paste(1:npreds)))
  
  for(i in 1:k){
    # Performing best subset selection on the full dataset, minus the ith fold
    best_fit = regsubsets(oecd_cli_index~., data = all_data[folds!=i,], nvmax = npreds)
    
    for(j in 1:npreds){
      # Predict the values of the current fold from the best subset model on i predictors
      pred = predict(best_fit, all_data[folds==i,], id = j)
      # Calculate the MSE
      cv_errors[i,j] = mean((all_data$oecd_cli_index[folds==i] - pred)^2)
    }
  }
  
  # Take the mean of over all folds for each model size
  mean_cv_errors = apply(cv_errors, 2, mean)
  min_err = which.min(mean_cv_errors)
  min_err_val = mean_cv_errors[min_err]
  
  # Plot the cross-validation error for each model size, highlight the min
  par(mfrow = c(1,1))
  plot(mean_cv_errors, type='b')
  points(min_err, mean_cv_errors[min_err][1], col = "red", cex = 2, pch = 20)
}

# --------------  K-folds validation repeated ------------------------
kfolds_validation_repeat = function(all_data, k, repetitions, random_seed) {
  if (random_seed == TRUE) {
    set.seed(NULL)
  }
  model_errors = c()
  models = list()
  npreds = ncol(all_data) - 1
  for (cyc in 1:repetitions) {
    if (random_seed == FALSE) {
      set.seed(cyc)
    }
    
    folds = sample(1:k, nrow(all_data), replace = TRUE)
    cv_errors = matrix(NA, k, npreds, dimnames = list(NULL, paste(1:npreds)))
    
    for(i in 1:k){
      # Performing best subset selection on the full dataset, minus the ith fold
      best_fit = regsubsets(oecd_cli_index~., data = all_data[folds!=i,], nvmax = npreds)
      
      for(j in 1:npreds){
        # Predict the values of the current fold from the best subset model on i predictors
        pred = predict(best_fit, all_data[folds==i,], id = j)
        # Calculate the MSE
        cv_errors[i,j] = mean((all_data$oecd_cli_index[folds==i] - pred)^2)
      }
    }
    
    # Take the mean of over all folds for each model size
    mean_cv_errors = apply(cv_errors, 2, mean)
    min_err = which.min(mean_cv_errors)
    min_err_val = mean_cv_errors[min_err]
    model_errors = append(model_errors, min_err_val)
    models[[cyc]] = best_fit
    
    print(paste(cyc, "/", repetitions, " - ", min_err_val))
  }
  
  # Plot the cross-validation error for each model size, highlight the min
  plot(model_errors, type='b')
  min_model_err = which.min(model_errors)
  points(min_model_err, model_errors[min_model_err][1], col = "red", cex = 2, pch = 20)
  paste("Min MSE: ", model_errors[min_model_err])
}

# --------------  Polynomial regression  ------------------------
polynomial_reg = function(traindata, testdata, ex) {
  # Multiple polynomial regression on training data
  lm_fit = lm(oecd_cli_index~
                poly(total_deaths,ex)+
                poly(total_cases,ex)+
                poly(new_deaths,ex)+
                poly(new_cases,ex)+
                poly(reproduction_rate,ex)+
                poly(median_age,ex)+
                poly(gdp_per_capita,ex)+
                poly(female_smokers,ex)+
                poly(male_smokers,ex), data = traindata)
  lm_fit_sum = summary(lm_fit)
  # lm_fit_sum
  # plot(lm_fit)
  
  # Making Predictions
  lm_fit_pred = lm_fit %>% predict(testdata)
  
  # Checking performance by calculating R2 ,RMSE and MAE
  result = data.frame( R2 = R2(lm_fit_pred, testdata$oecd_cli_index),
              MSE = RMSE(lm_fit_pred, testdata$oecd_cli_index)^2,
              RMSE = RMSE(lm_fit_pred, testdata$oecd_cli_index),
              MAE = MAE(lm_fit_pred, testdata$oecd_cli_index))
  return(result)
}

# -------------- START OF ANALYSIS --------------------------

# Init data
res = init_data()
read_data = res[[1]]
final_data = res[[2]]
npreds = res[[3]]

# Train & test data
res = create_train_test(final_data, 0.7)
train_data = res[[1]]
test_data = res[[2]]

best_subs_reg(train_data, test_data)

kfolds_validation(final_data, 25)

polynomial_reg(train_data, test_data, 2)

final_data_modified = final_data
final_data_modified_col = lapply(final_data_modified$oecd_cli_index, 
                                 function(x) mean(final_data_modified$oecd_cli_index) - x)
final_data_modified$oecd_cli_index = unlist(final_data_modified_col)

# Train & test data
res = create_train_test(final_data_modified, 0.7)
train_data_mod = res[[1]]
test_data_mod = res[[2]]

best_subs_reg(train_data_mod, test_data_mod)

kfolds_validation(final_data_modified, 25)

set.seed()
res = polynomial_reg(train_data_mod, test_data_mod, 1)
res

