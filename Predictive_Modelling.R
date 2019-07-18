
# importing the population data into population data frame
population <- read.csv("population.csv", header = TRUE)
str(population)

# importing the propert_price data into price data frame
price <- read.csv("property_price.csv", header = TRUE)
str(price)

# merging the data frames
merged_data <- merge(population, price)
str(merged_data)

# creating linear model for population and price
simple_linear_model <- lm(price ~ population, data = merged_data)
simple_linear_model

par(mfrow = c(1,1))

# plotting both the attributes
plot(merged_data$population,merged_data$price,
     xlab="Population",
     ylab="Property price",
     main = "Plot showing population versus property price")

# drawing a reference line with respect to linear model
abline(simple_linear_model)
# summary of the linear model
summary(simple_linear_model)

# finding the correlation between both the variables
cor(merged_data$population, merged_data$price)
# the correlation co-efficient value is 0.5322

# Examining the 95% confidence intervals of the model
confint(simple_linear_model)

# examining the goodness of fit of the model
summary(simple_linear_model)

# using scatter smooth plot to visualize the linear relationship between the two variables
scatter.smooth(x = merged_data$population,
               y = merged_data$price,
               main = "Population vs price",
               xlab = "population",
               ylab = "price")

# dividing the graph area into two columns
par(mfrow = c(1,2))

# using box plot to identify the outliers in population
boxplot(merged_data$population, main = "Population", 
        sub = paste("Outlier rows: ", boxplot.stats(merged_data$population)$out))

# using box plot to identify the outliers in price
boxplot(merged_data$price, main = "price", 
        sub = paste("Outlier rows: ", boxplot.stats(merged_data$price)$out))

# Using "e1071" library to determine the outliers
install.packages("e1071")
library(e1071)
par(mfrow = c(1,2))

# Plotting density plot to determine the skewness for population variable
plot(density(merged_data$population), main = "Density Plot: Population", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(merged_data$population), 2)))

# Filling the plot area with blue
polygon(density(merged_data$population), col = "blue")


# Plotting density plot to determine the skewness for price variable
plot(density(merged_data$price), main = "Density Plot: Price", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(merged_data$price), 2)))

# Filling the plot area with red
polygon(density(merged_data$price), col = "red")


#Finding the correlation between population and price
cor(merged_data$population, merged_data$price)

# Building linear regression model on full data
linearModel <- lm(price ~ population, data = merged_data)
print(linearModel)

# Summary of the built linear regression model
summary_model <- summary(linearModel)
summary_model

# Calculating t-statistc and p-value

# storing model co-efficient in seperate variable
model_coefficient <- summary_model$coefficients
model_coefficient

# calculating beta estimate for population
beta.estimate <- model_coefficient["population", "Estimate"]

# calculating standard error for population
standard_error <- model_coefficient["population", "Std. Error"] 

# Claculating t-statistic 
t_value <- beta.estimate / standard_error
t_value
# calculating p-value 
p_value <- 2 * pt(-abs(t_value), df = nrow(merged_data) - ncol(merged_data))
p_value
#calculating f-statistic  
f_statistic <- linearModel$fstatistic[1]
# parameters for model p-value calculations
f <- summary(linearModel)$fstatistic
# calculating model's p-value
model_p <- pf(f[1], f[2], f[3], lower = FALSE)
model_p

# Determining AIC and BIC values for linear regression model
AIC(linearModel)
BIC(linearModel)

# Creating training and testing datasets 

# Taking 80% of the dataset as training dataset 
number_of_records <- sample(1:nrow(merged_data), 0.8 * nrow(merged_data))
training_data <- merged_data[number_of_records,]
head(training_data)

# Taking the remaining 20% of the dataset as testing dataset
testing_data <- merged_data[-number_of_records,]
head(testing_data)

# Building the linear regression model on the training_data
lnr_model <- lm(price ~ population, data = training_data)
# checking the summary of the training linear model
summary(lnr_model)

# predicting the price from testing data using linear regression model
prediction <- predict(lnr_model, testing_data)

# combining the original testing data and predicted data
actual_prediction <- data.frame(cbind(actuals = testing_data$price, predicted = prediction))
head(actual_prediction)

# Checking the correlation accuracy
correlation_accuracy <- cor(actual_prediction)
correlation_accuracy

# Calculating the minimum and maximum accuracy
min_max_accuracy <- mean(apply(actual_prediction, 1, min) / apply(actual_prediction, 1, max))
min_max_accuracy

# calculating Mape
mape <- mean(abs((actual_prediction$predicted - actual_prediction$actuals)) / actual_prediction$actuals)
mape

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearModel)
summary(gvmodel)

# Polynomial Regression ----------------------------------------------
par(mfrow = c(2,2))

# Building a Polynomial regression model
poly_fit <- lm(price ~ population + I(population ^ 2), data = merged_data)
plot(poly_fit)

#summary
summary(poly_fit)

# Plotting the datapoints to check the fit
plot(merged_data$population, merged_data$price, xlab = "Population", ylab = "Price")
lines(merged_data$population, fitted(poly_fit))

# Determining the AIC and BIC for the polynomial regression model
AIC(poly_fit)
BIC(poly_fit)

# training the polynomial regression model with training dataset
poly_model <- lm(price ~ population + I(population ^ 2), data = training_data)

# predicting price using polynomial regression model
prediction_2 <- predict(poly_model, testing_data)

# combining the actual and predicted values
actual_prediction_poly <- data.frame(cbind(actuals = testing_data$price, predicted = prediction_2))
head(actual_prediction_poly)

# calculating the correlation accuracy
correlation_accuracy2 <- cor(actual_prediction_poly)
correlation_accuracy2

# Calculating the minimum and maximum accuracy
min_max_accuracy_poly <- mean(apply(actual_prediction_poly, 1, min) / apply(actual_prediction_poly, 1, max))
min_max_accuracy_poly

# calculating Mape
mape_poly <- mean(abs((actual_prediction_poly$predicted - actual_prediction_poly$actuals)) / actual_prediction_poly$actuals)
mape_poly

# Checking the summary
summary(prediction_2)

# Global validation of polynomial regression model assumption
library(gvlma)
gvmodel <- gvlma(poly_fit)
summary(gvmodel)

# Multi-linear --------------------------------

# Building a Multi-linear regression model
multi_linear_fit <- lm(price ~ population + year, data = merged_data)
par(mfrow = c(2,2))
plot(multi_linear_fit)

summary(multi_linear_fit)

# Determining the AIC and BIC for the Multi-linear regression model
AIC(multi_linear_fit)
BIC(multi_linear_fit)

# training the polynomial regression model with training dataset
multi_linear_model <- lm(price ~ population + year, data = training_data)

# predicting price using multi-linear regression model
multi_linear_prediction <- predict(multi_linear_model, testing_data)


# combining the actual and predicted values
actual_prediction_multilinear <- data.frame(cbind(actuals = testing_data$price, 
                                                  predicted = multi_linear_prediction))
head(actual_prediction_multilinear)

# calculating the correlation accuracy
correlation_accuracy3 <- cor(actual_prediction_multilinear)
correlation_accuracy3

# Calculating the minimum and maximum accuracy
min_max_accuracy_multilinear <- mean(apply(actual_prediction_multilinear, 1, min) / 
                                       apply(actual_prediction_multilinear, 1, max))
min_max_accuracy_multilinear

# Calculating Mape
mape_multilinear <- mean(abs((actual_prediction_multilinear$predicted - actual_prediction_multilinear$actuals)) / 
                           actual_prediction_multilinear$actuals)
mape_multilinear

# Global validation of linear model assumption
par(mar= c(1,1,1,1))
library(gvlma)
gvmodel1 <- gvlma(multi_linear_fit)
summary(gvmodel1)

#Dublin------------------------------------------------------------

# Considering the records based only on Dublin
dublin <- subset(merged_data, grepl("Dublin", county))
dublin

par(mfrow = c(1,1))

# plotting both the attributes
plot(dublin$population,dublin$price,
     xlab="Population",
     ylab="Property price",
     main = "Plot showing population versus property price")

# drawing a reference line with respect to linear model
abline(simple_linear_model_1)
# summary of the linear model
summary(simple_linear_model_1)

# finding the correlation between both the variables
cor(dublin$population, dublin$price)

# Examining the 95% confidence intervals of the model
confint(simple_linear_model_1)

# examining the goodness of fit of the model
summary(simple_linear_model_1)

# using scatter plot to visualize the linear relationship between the two variables
scatter.smooth(x = dublin$population,
               y = dublin$price,
               main = "Population vs price",
               xlab = "population",
               ylab = "price")

par(mfrow = c(1,2))

# using box plot to identify the outliers in population
boxplot(dublin$population, main = "Population", 
        sub = paste("Outlier rows: ", boxplot.stats(dublin$population)$out))

# using box plot to identify the outliers in price
boxplot(dublin$price, main = "price", 
        sub = paste("Outlier rows: ", boxplot.stats(dublin$price)$out))


par(mfrow = c(1,2))
# Plotting density plot to determine the skewness for population variable
plot(density(dublin$population), main = "Density Plot: Population", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(dublin$population), 2)))

# Filling the plot area with blue
polygon(density(dublin$population), col = "blue")

# Plotting density plot to determine the skewness for price variable
plot(density(dublin$price), main = "Density Plot: Price", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(dublin$price), 2)))

# Filling the plot area with red
polygon(density(dublin$price), col = "red")

#Finding the correlation between population and price
cor(dublin$population, dublin$price)

# Building linear regression model on full data
linearModel1 <- lm(price ~ population, data = dublin)
print(linearModel1)


# Summary of the built linear regression model
summary_model1 <- summary(linearModel1)
summary_model1

# Calculating t-statistc and p-value

# storing model co-efficient in seperate variable
model_coefficient1 <- summary_model1$coefficients
model_coefficient1

# calculating beta estimate for population
beta.estimate <- model_coefficient1["population", "Estimate"]

# calculating standard error for population
standard_error <- model_coefficient1["population", "Std. Error"] 

# Claculating t-statistic 
t_value <- beta.estimate / standard_error
# calculating p-value 
p_value <- 2 * pt(-abs(t_value), df = nrow(dublin) - ncol(dublin))
#calculating f-statistic  
f_statistic <- linearModel1$fstatistic[1]
# parameters for model p-value calculations
f <- summary(linearModel1)$fstatistic
# calculating model's p-value
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

# Determining AIC and BIC values for linear regression model
AIC(linearModel1)
BIC(linearModel1)

# Creating training and testing datasets 

# Taking 80% of the dataset as training dataset 
number_of_records <- sample(1:nrow(dublin), 0.8 * nrow(dublin))
training_data <- dublin[number_of_records,]
training_data

# Taking the remaining 20% of the dataset as testing dataset
testing_data <- dublin[-number_of_records,]
testing_data

# Building the linear regression model on the training_data
lnr_model <- lm(price ~ population, data = training_data)
# checking the summary of the training linear model
summary(lnr_model)

# predicting the price from testing data using linear regression model
prediction <- predict(lnr_model, testing_data)

# combining the original testing data and predicted data
actual_prediction <- data.frame(cbind(actuals = testing_data$price, predicted = prediction))
head(actual_prediction)

# Checking the correlation accuracy
correlation_accuracy <- cor(actual_prediction)
correlation_accuracy

# Calculating the minimum and maximum accuracy
min_max_accuracy <- mean(apply(actual_prediction, 1, min) / apply(actual_prediction, 1, max))
min_max_accuracy

# calculating Mape
mape <- mean(abs((actual_prediction$predicted - actual_prediction$actuals)) / actual_prediction$actuals)
mape

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearModel1)
summary(gvmodel)

# Polynomial Regression ----------------------------------------------

# Building a Polynomial regression model
poly_fit <- lm(price ~ population + I(population ^ 2), data = dublin)

#summary
summary(poly_fit)

# Plotting the datapoints to check the fit
plot(dublin$population, dublin$price, xlab = "Population", ylab = "Price")
lines(dublin$population, fitted(poly_fit))

# Determining the AIC and BIC for the polynomial regression model
AIC(poly_fit)
BIC(poly_fit)

# training the polynomial regression model with training dataset
poly_model <- lm(price ~ population + I(population ^ 2), data = training_data)

# predicting price using polynomial regression model
prediction_2 <- predict(poly_model, testing_data)
prediction_2

# combining the actual and predicted values
actual_prediction_poly <- data.frame(cbind(actuals = testing_data$price, predicted = prediction_2))
actual_prediction_poly

# calculating the correlation accuracy
correlation_accuracy2 <- cor(actual_prediction_poly)
correlation_accuracy2

# Calculating the minimum and maximum accuracy
min_max_accuracy_poly <- mean(apply(actual_prediction_poly, 1, min) / apply(actual_prediction_poly, 1, max))
min_max_accuracy_poly

# calculating Mape
mape_poly <- mean(abs((actual_prediction_poly$predicted - actual_prediction_poly$actuals)) / actual_prediction_poly$actuals)
mape_poly

# Checking the summary
summary(prediction_2)

# Global validation of polynomial regression model assumption
library(gvlma)
gvmodel <- gvlma(poly_fit)
summary(gvmodel)

# Multi-linear --------------------------------

# Building a Multi-linear regression model
multi_linear_fit <- lm(price ~ population + year, data = dublin)
par(mfrow = c(2,2))
plot(multi_linear_fit)

# Determining the AIC and BIC for the Multi-linear regression model
AIC(multi_linear_fit)
BIC(multi_linear_fit)

# training the polynomial regression model with training dataset
multi_linear_model <- lm(price ~ population + year, data = training_data)

# predicting price using multi-linear regression model
multi_linear_prediction <- predict(multi_linear_model, testing_data)
summary(multi_linear_prediction)

# combining the actual and predicted values
actual_prediction_multilinear <- data.frame(cbind(actuals = testing_data$price, 
                                                  predicted = multi_linear_prediction))
head(actual_prediction_multilinear)

# calculating the correlation accuracy
correlation_accuracy3 <- cor(actual_prediction_multilinear)
correlation_accuracy3

# Calculating the minimum and maximum accuracy
min_max_accuracy_multilinear <- mean(apply(actual_prediction_multilinear, 1, min) / 
                                       apply(actual_prediction_multilinear, 1, max))
min_max_accuracy_multilinear

# Calculating Mape
mape_multilinear <- mean(abs((actual_prediction_multilinear$predicted - actual_prediction_multilinear$actuals)) / 
                           actual_prediction_multilinear$actuals)
mape_multilinear

# Global validation of linear model assumption
par(mar= c(1,1,1,1))
library(gvlma)
gvmodel1 <- gvlma(multi_linear_fit)
summary(gvmodel1)



