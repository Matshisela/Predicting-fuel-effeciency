#loading packages
library(tidyverse)
library(rsample)

#loading data
cars2020 <- read.csv(file.choose())
glimpse(cars2020)
View(cars2020)

#Plotting the histogram
ggplot(cars2020, aes(x= comb08)) +
  geom_histogram(bins = 25) +
  labs(x_lab = "Fuel effeciency (MPG)", 
       y_lab = "Number of cars")

#Need to transform

#Drive, trany, make, year

#selecting data
car_var <- cars2020 %>%
  select(displ, cylinders, trany, comb08, drive, make, year, fuelType)

#Linear models
car_lim <- lm(comb08 ~., data = car_var)
summary(car_lim) #79.73%

car_var1 <- na.omit(car_var)

library(rsample)
set.seed(1234)
car_split <- car_var1 %>%
  initial_split(prop = 0.8,
                strata = "make")

car_training <- training(car_split)
car_testing <- testing(car_split)


# Load caret
library(caret)

# Train a linear regression model
fit_lm <- train(log(comb08) ~ ., 
                method = "lm", 
                data = car_training,
                trControl = trainControl(method = "none"))

# Print the model object
fit_lm


# Load caret
library(randomForest)

# Train a random forest model
fit_rf <- train(log(comb08) ~ ., 
                method = "rf", 
                data = car_training,
                trControl = trainControl(method = "none"))

# Print the model object
fit_rf


# Load yardstick
library(yardstick)


# Create the new columns
results <- car_training %>%
  mutate(MPG = log(comb08),
         `Linear regression` = predict(fit_lm, car_training),
         `Random forest` = predict(fit_rf, car_training))


# Evaluate the performance
metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate = `Random forest`)

# Create the new columns
results <- car_testing %>%
  mutate(MPG = log(comb08),
         `Linear regression` = predict(fit_lm, car_testing),
         `Random forest` = predict(fit_rf, car_testing))

# Evaluate the performance
metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate = `Random forest`)


#Bootstrapping
# Fit the models with bootstrap resampling
cars_lm_bt <- train(log(comb08) ~ ., 
                    method = "lm", 
                    data = car_training,
                    trControl = trainControl(method = "boot"))

cars_rf_bt <- train(log(comb08) ~ ., 
                    method = "rf", 
                    data = car_training,
                    trControl = trainControl(method = "boot"))

# Quick look at the models
cars_lm_bt
cars_rf_bt

#Testing
results <- car_testing %>%
  mutate(MPG = log(comb08),
         `Linear regression` = predict(cars_lm_bt, car_testing),
         `Random forest` = predict(cars_rf_bt, car_testing))

metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate =`Random forest`)

#visualizing
results <- car_test %>%
  mutate(MPG = log(MPG),
         `Linear regression` = predict(cars_lm_bt, car_test),
         `Random forest` = predict(cars_rf_bt, car_test))

results %>%
  gather(Method, Result, `Linear regression`:`Random forest`) %>%
  ggplot(aes(MPG, Result, color = Method)) +
  geom_point(size = 1.5, alpha = 0.5) +
  facet_wrap(~Method) +
  geom_abline(lty = 2, color = "gray50") +
  geom_smooth(method = "lm")
