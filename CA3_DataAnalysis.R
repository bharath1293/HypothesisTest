
# The data from the property price is read and loaded into a dataframe
price <- read.csv("property_price.csv")
#price
# Structure of the dataframe is viewed
str(price)

# Data from population dataset is read and loaded into another dataframe
population <- read.csv("population.csv")
#population
# Structure of the data is viewed
str(population)

# Modifying the price & population attributes to numeric
price$price <- as.numeric(price$price)
population$population <- as.numeric(population$population)
# Modifying the county attribute to categorizing factor in both the dataframes
price$county <- as.factor(price$county)
population$county <- as.factor(population$county)

# Merging both the datasets
merged_data <- merge(price,population)
str(merged_data)

# Plotting price and population attributes in histogram to view the distribution
library(lattice)
histogram(~price | county, data=merged_data)
histogram(~population | county, data=merged_data)

# plotting QQPlot to compare quantiles of both the attributes and to check the normality
with(merged_data,
     qqplot(price, population,
            main = "Comparing price and population",
            xlab = "property price",
            ylab = "county population"))

# Using QQNorm to plot the price attribute over normal distribution
with(merged_data, {
  qqnorm(price,
         main = "price")
  qqline(price)
})
# Using QQNorm to plot the population attribute over normal distribution
with(merged_data, {
  qqnorm(population,
         main = "Population")
  qqline(population)
})


# Shapiro-wilks is used to check the normality distribution since using histogram and qqplot, the normality cannot be concluded

# Using shapiro test for price attribute
normality_test <- shapiro.test(merged_data$price)
normality_test
# Checking the p-value
normality_test$p.value

# Using shapiro test for population attribute
normality_test <- shapiro.test(merged_data$population)
normality_test
# Checking the p-value
normality_test$p.value

# Hypothesis test is performed to check the p-value and to prove either null hypothesis or alternative hypothesis
# In our case null hypothesis states that there is no relation between property price and population
# Alternative hypothesis states there is a relation between property price and poulation

# Both pearson and spearman methods are used here, since the two varibales are continuous 
# and data distribution is normal for one variable and data distribution is not normal for another variable 
# in our case

# Pearson method
hypothesis_test <- cor.test(x = merged_data$price, y = merged_data$population, method = 'pearson')
hypothesis_test

# Spearman method
hypothesis_test1 <- cor.test(x = merged_data$price, y = merged_data$population, method = 'spearman', 
                             exact = FALSE)
hypothesis_test1

# since the p-value is lesser than 0.05, the null hypothesis is rejected and alternative hypothesis is proved 


# to perform power analysis and to determine the effect and sample size, "pwr" library is used
library(pwr)
# cohen.ES is used to set the statistical test of interest and ES size
effect_size <- cohen.ES(test = "r", size = "large")
effect_size

# "pwr.r.test" is performed here since both the variables in analysis are continuous 
sample_size <- pwr.r.test(r = effect_size$effect.size, 
                          sig.level = 0.05, power = 0.90, 
                          alternative = "two.sided")
sample_size
# plotting the results
plot(sample_size)
# From the results, we can see that sample size to 38 by setting the significance lever and power

# generating 38 samples from the merged dataset
library(dplyr)
samples <- sample_n(merged_data,38)
#samples
# verifying the number of sample records
nrow(samples)

# Performing correlation test again to verify the hypothesis
cor.test(samples$price, samples$population)