library(leaps)
library(plyr)
library(dplyr)
library(glmnet)
library(magrittr)
library(tidyverse)
library(caret)
library(olsrr)
library(car)
library(broom)
library(scales)
library(withr)
library(Hmisc)
library(corrplot)

set.seed(39)

# Initializing the data
read_data = read.csv(file = "monthly-narrowed-merged-data.csv", stringsAsFactors = FALSE)
final_data = subset(read_data, select = -c(iso_code, location, date))
npreds = ncol(final_data) - 1

# Creating training & testing data
train = final_data %>% sample_frac(0.7)
test = final_data %>% setdiff(train)

# --------------- Linear regressions ----------------------------

par(mfrow = c(2,2))
plot(final_data$human_development_index, final_data$oecd_cli_index, pch = 16, cex = 1.3,
     col = "blue", xlab = "HDI", ylab = "OECD CLI Index")
abline(lm(final_data$oecd_cli_index ~ final_data$human_development_index), lwd = 2, col = "red")
plot(final_data$new_cases_per_million, final_data$oecd_cli_index, pch = 16, cex = 1.3,
     col = "blue", xlab = "New cases per million", ylab = "OECD CLI Index")
abline(lm(final_data$oecd_cli_index ~ final_data$new_cases_per_million), lwd = 2, col = "red")
plot(final_data$stringency_index, final_data$oecd_cli_index, pch = 16, cex = 1.3,
     col = "blue", xlab = "Stringency index", ylab = "OECD CLI Index")
abline(lm(final_data$oecd_cli_index ~ final_data$stringency_index), lwd = 2, col = "red")
plot(final_data$reproduction_rate, final_data$oecd_cli_index, pch = 16, cex = 1.3,
     col = "blue", xlab = "Reproduction Rate", ylab = "OECD CLI Index")
abline(lm(final_data$oecd_cli_index ~ final_data$reproduction_rate), lwd = 2, col = "red")

# --------------- Best subset regression ------------------------

set.seed(1)

# Performing best subset regression on training data
best_subs_model = regsubsets(oecd_cli_index~., data = train, nvmax = npreds)
best_subs_model_summary = summary(best_subs_model)

# Analyzing best subset regression
par(mfrow = c(1,3))
plot(best_subs_model_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq",
     type = "l", lwd = 2, col = "blue")
adj_r2_max = which.max(best_subs_model_summary$adjr2)
points(adj_r2_max, best_subs_model_summary$adjr2[adj_r2_max], col ="red", cex = 3, pch = 20)

# Measuring accuracy of model on test data (MSE, MAE)
test_mat = model.matrix(oecd_cli_index~., data = test)
val_errors = rep(NA, npreds)
val_errors_mae = rep(NA, npreds)
for(i in 1:npreds) {
  coefi = coef(best_subs_model, id = i)
  pred = test_mat[,names(coefi)] %*% coefi
  # Calculate the MSE
  val_errors[i] = mean((test$oecd_cli_index - pred)^2)
  # Calculate the MAE
  val_errors_mae[i] = mean(abs(test$oecd_cli_index - pred))
}

# Analyzing model MSE, MAE
min = which.min(val_errors)
points(min, best_subs_model_summary$adjr2[min], col = "orange", cex = 2.5, pch = 20)
min_mae = which.min(val_errors_mae)
data.frame( adjR2 = best_subs_model_summary$adjr2[min],
            MSE = val_errors[min],
            MAE = val_errors_mae[min])
coef(best_subs_model, min)
plot(val_errors, xlab = "Number of Variables", ylab = "MSE", type = "l", lwd = 2, col = "blue")
points(min, val_errors[min][1], col = "orange", cex = 2.5, pch = 20)
plot(val_errors_mae, xlab = "Number of Variables", ylab = "MAE", type = "l", lwd = 2, col = "blue")
points(min_mae, val_errors_mae[min_mae][1], col = "red", cex = 3, pch = 20)
points(min, val_errors_mae[min][1], col = "orange", cex = 2.5, pch = 20)

# Calculating weighted coefficients
mean(final_data$total_cases)*coef(best_subs_model, min)[[2]]
mean(final_data$new_cases)*coef(best_subs_model, min)[[3]]
mean(final_data$total_deaths)*coef(best_subs_model, min)[[4]]
mean(final_data$new_deaths)*coef(best_subs_model, min)[[5]]
mean(final_data$new_deaths_smoothed)*coef(best_subs_model, min)[[6]]
mean(final_data$total_deaths_per_million)*coef(best_subs_model, min)[[7]]
mean(final_data$reproduction_rate)*coef(best_subs_model, min)[[8]]
mean(final_data$population)*coef(best_subs_model, min)[[9]]
mean(final_data$median_age)*coef(best_subs_model, min)[[10]]
mean(final_data$female_smokers)*coef(best_subs_model, min)[[11]]
mean(final_data$male_smokers)*coef(best_subs_model, min)[[12]]
mean(final_data$hospital_beds_per_thousand)*coef(best_subs_model, min)[[13]]
mean(final_data$human_development_index)*coef(best_subs_model, min)[[14]]

# --------------- Multiple linear regression ----------------

set.seed(39)

# Creating training & testing data
train = final_data %>% sample_frac(0.7)
test = final_data %>% setdiff(train)

# Excluding outliers
train = train[-c(159, 146, 96), ]

# Fitting the model
lm_fit = lm(oecd_cli_index~
              log(total_cases)+
              total_deaths+
              reproduction_rate+
              median_age+
              female_smokers+
              male_smokers+
              human_development_index, 
            data = train)
lm_fit_sum = summary(lm_fit)
lm_fit_sum
plot(lm_fit)

# Making Predictions
lm_fit_pred = lm_fit %>% predict(test)

# Checking performance by calculating R2, MSE, RMSE and MAE
data.frame( R2 = R2(lm_fit_pred, test$oecd_cli_index),
            MSE = RMSE(lm_fit_pred, test$oecd_cli_index)^2,
            RMSE = RMSE(lm_fit_pred, test$oecd_cli_index),
            MAE = MAE(lm_fit_pred, test$oecd_cli_index))

# Creating correlation plot
par(mfrow = c(1,1))
corr_matrix = cor(vcov(lm_fit))
corrplot(corr_matrix)

# Creating VIF barplot
vif_values = vif(lm_fit)
barplot(vif_values, main = "VIF Values",col = 'blue', ylim = c(0, 4))
bad_vif = 3.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
