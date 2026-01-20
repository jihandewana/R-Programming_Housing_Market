#Part 1 - Data analysis
# Load required libraries
# optional installation step, if you don't have the libraries installed:
install.packages(c("ggplot2", "dplyr","car"))
library(ggplot2)
library(dplyr)
library(car)

# 1. Load the data
#data <- read.csv("D:\WORK\TRAINING ONLINE\365 DATA SCIENCE\17. Housing Market Data Analysis in R Project (portfolio)/housing_data.csv",header=TRUE)

#convert the csv to excel first
library(readxl)
housing_data <- read_excel("D:/WORK/TRAINING ONLINE/365 DATA SCIENCE/17. Housing Market Data Analysis in R Project (portfolio)/housing_data.xlsx")
View(housing_data)

# 2. Summary statistics
print(summary(housing_data))

# 3. Calculate mean, mode and median, as well as the standard deviation for each variable
# Mode function
# Mode function
mode_func <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

means <- sapply(housing_data, mean, na.rm = TRUE)  # Calculate means
modes <- sapply(housing_data, mode_func)  # Calculate modes
medians <- sapply(housing_data, median, na.rm = TRUE)  # Calculate medians
sds <- sapply(housing_data, sd) 

medians
# Print results
print(data.frame(Variable = names(housing_data), Mean = means, Mode = modes, Median = medians, SDT = sds))

# 4. Compute the correlation between each pair of variables
correlation_matrix <- cor(housing_data, use = "complete.obs")
print(correlation_matrix)
max(correlation_matrix[correlation_matrix < 1])

means <- sapply(housing_data, mean)  # Calculate means
sds <- sapply(housing_data, sd)      # Calculate standard deviations
print(data.frame(Variable = names(housing_data), Mean = means, Std_Deviation = sds))

# Fill in missing values with the mean for each variable
#data <- housing_data %>%
#  mutate_all(~ifelse(is.na(.), mean(., na.rm=TRUE), .))

#syntax above = failed

#Fill in missing values with the mean for each variable
names(housing_data)
#[1] "Crime Rate"             
#[2] "Average Rooms"          
#[3] "Public Transport Access"
#[4] "Number of Schools"      
#[5] "Median Home Value"    
housing_data$`Crime Rate`[is.na(housing_data$`Crime Rate`)] <- mean(housing_data$`Crime Rate`,na.rm = TRUE)
housing_data$`Average Rooms`[is.na(housing_data$`Average Rooms`)] <- mean(housing_data$`Average Rooms`,na.rm = TRUE)
housing_data$`Public Transport Access`[is.na(housing_data$`Public Transport Access`)] <- mean(housing_data$`Public Transport Access`,na.rm = TRUE)
housing_data$`Number of Schools`[is.na(housing_data$`Number of Schools`)] <- mean(housing_data$`Number of Schools`,na.rm = TRUE)
housing_data$`Median Home Value`[is.na(housing_data$`Median Home Value`)] <- mean(housing_data$`Median Home Value`,na.rm = TRUE)

print(housing_data, n=506)
summary (housing_data)

# Part 2 - Data visualization
# Create a histogram of house prices with styling and 8 bins
hist(housing_data$`Crime Rate`, main= "Crime Rate",col = "navyblue", border = "white")
hist(housing_data$`Median Home Value`, main= "Median Home Value",col = "slateblue", border = "white")
hist(housing_data$`Average Rooms`, main= "Average Rooms",col = "deepskyblue", border = "white")
hist(housing_data$`Public Transport Access`, main= "Public Transport Access",col ="thistle2", border = "white")
hist(housing_data$`Number of Schools`, main= "Number of School",col = "violet", border = "white")

#Part 3 - Hypothesis testing
# 1. Define "high" and "low" crime rates based on the median of the Crime.Rate variable
# Categorize crime rate into two levels: Low and High
housing_data$crime_category <- ifelse(housing_data$`Crime Rate` < median(housing_data$`Crime Rate`), "Low", "High")
housing_data$crime_category

unique(housing_data$crime_category)
levels(housing_data$crime_category)

View(housing_data)

# Simple Bar Plot - Crime Category
counts <- table(housing_data$crime_category)
barplot(counts, main="Crime Category",
        col="cadetblue3",
        xlab="Number of Crime Category")
counts

#levene_test (berhasil)
library(car)
levene_test #semua variabel

# 2. Additional: Check for assumptions
# For a t-test, it's essential to check for normality & homogeneity of variance.
shapiro_test_high <- shapiro.test(housing_data$`Median Home Value`[housing_data$crime_category == "High"])
shapiro_test_low <- shapiro.test(housing_data$`Median Home Value`[housing_data$crime_category == "Low"])
shapiro_test_high
shapiro_test_low

print(paste("Shapiro Test p-value for High Crime Rate:", shapiro_test_high$p.value))
print(paste("Shapiro Test p-value for Low Crime Rate:", shapiro_test_low$p.value))

# If the p-value is < 0.05 for the Shapiro test, then the data is not normally distributed.
# Check for homogeneity of variance using Levene's Test (need to install & load the 'car' package)
# If the p-value is < 0.05 for the Levene's test, then variances between the groups are not equal.


# 3. Conduct an independent two-sample t-test
t_test <- t.test(`Median Home Value`~ crime_category, data = housing_data, var.equal = TRUE)
t_test

alpha <- 0.05
df <- nrow(housing_data) - 1  # where n is your sample size

# For a two-tailed test, we take the alpha value and divide by 2 for the upper tail. 
# We use 1 - (alpha/2) because qt() gives the left tail by default, and subtracting from 1 gives us the right tail.
critical_t <- qt(1 - (alpha/2), df)
print(critical_t)

# Part 4 - Linear regression
# for predicting the median home value with the average rooms variable
model <- lm(`Median Home Value`~ `Average Rooms`+ `Crime Rate` + `Public Transport Access` + `Number of Schools`
            , data = housing_data)
print(summary(model))

#scatter plot
plot(housing_data$`Average Rooms`,housing_data$`Median Home Value`, 
     main='Average Rooms vs Median Home Value', 
     xlab='Average Room', ylab='Median Home Value')

# plot a regression line 
abline(lm(`Median Home Value`~ `Average Rooms`, data = data),col='red') 


