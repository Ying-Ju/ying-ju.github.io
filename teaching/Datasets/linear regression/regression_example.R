# Prepare packages

if(!require("DataExplorer")) install.packages("DataExplorer")
if(!require("ggplot2")) install.packages("ggplot2")

library(DataExplorer)
library(ggplot2)

# Obtain the URL for the dataset
URL <- "https://raw.githubusercontent.com/Ying-Ju/ying-ju.github.io/main/teaching/Datasets/linear%20regression/Boston%20House%20Prices/boston.csv"

# Step 1: read your dataset
df1 <- read.csv(URL)

# plot basic information for input data
plot_intro(df1)

# create a correlation heatmap for continuous variables
plot_correlation(df1, type="continuous")

# create scatterplot for all features fixing on a selected variable
plot_scatterplot(df1, by="MEDV")

# We don't have to create scatterplot for all features, 
# we can pick several variables for this.
plot_scatterplot(df1[,c("AGE", "CRIM", "RM", "LSTAT", "MEDV")], 
                 by="MEDV", ncol=2, nrow=2)

# read the first a few lines of the data
head(df1)

# Step 2: EDA
### Find summary statistics for each variable
summary(df1)

### Scatter plots for RM, LSTAT, and MEDV
### Names of variables and title should be changed based on the data

ggplot(df1, aes(x = RM, y = MEDV)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Relation between RM and MEDV")

ggplot(df1, aes(x = LSTAT, y = MEDV)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Relation between LSTAT and MEDV")

# Step 3: Fit the linear model using RM and LSTAT
## If we want to use three predictors v1, v2, v3 in the data to predict y,
## we can use lm(y ~ v1 + v2 + v3, data = df1)
m1 <- lm(MEDV ~ RM + LSTAT, data = df1)

## Summary of the model
summary(m1)

## conduct a regression analysis using all predictors in the data
m2 <- lm(MEDV~., data = df1)

## Summary of the model
summary(m2)

# Step 4: Visualize the model
## Plotting residuals
plot(m1, which = 1, main = "Residuals vs Fitted", col = "blue")
abline(h = 0, col = "red")

## QQ plot of residuals
qqnorm(residuals(m1))
qqline(residuals(m1), col = "red")

## Actual vs Predicted Plot
### The name for the response variable should be adjusted based on the data
predicted_values <- predict(m1)
actual_values <- df1$MEDV

# Plotting Actual vs Predicted
plot(actual_values, predicted_values, 
     main = "Actual vs Predicted", 
     xlab = "Actual Values", ylab = "Predicted Values", 
     pch = 19, col = "blue")
abline(0, 1, col = "red")  # Adding a line y=x for reference

