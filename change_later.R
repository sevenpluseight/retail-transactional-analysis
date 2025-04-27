# NOTE: 
# 1. include good programming practices such as comments, variable naming conventions and indentation
# 2. validation should be done for each entry from the users to avoid logical errors
# 3. perform additional research to further understand the information on the given dataset during evaluation of data
# 4. the analysis should be meaningful and effective in providing the information for decision-making

# data import
dataFinal1 <- read.csv("final_data_with_imputed_Customer_ID.csv")
View(dataFinal1)

# required libraries: dplyr, ggplot2
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
# -----------------------------------------------------------------------
# Elysha Sophia binti Ridzuan - TP071162
# total amount spend, positive feedback likelihood
# To investigate how the total amount spent influences positive feedback
# Is there a significant relationship between the total amount spent and the likelihood of receiving positive feedback?
# Fit the linear regression model
linear_model <- lm(Feedback_Num ~ Total_Amount, data = dataFinal1)

# View the summary of the regression model
summary(linear_model)
library(ggplot2)

# Scatter plot with regression line
ggplot(dataFinal1, aes(x = Total_Amount, y = Feedback_Num)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Linear Regression: Total Amount vs Feedback",
       x = "Total Amount Spent",
       y = "Feedback (Numerical)") +
  theme_minimal()
# Residual plots
plot(linear_model)
-------------------------------------------------------------------------
# Frequency table for Feedback
table(dataFinal1$Feedback)

# Bar plot for Feedback distribution
barplot(table(dataFinal1$Feedback), main = "Feedback Distribution", 
        col = c("gold", "skyblue", "lightgreen", "lightcoral"), 
        xlab = "Feedback Types", ylab = "Count")

# Perform ANOVA (Analysis of Variance)
anova_result <- aov(Total_Amount ~ Feedback, data = dataFinal1)
summary(anova_result)

# Boxplot for visualization
boxplot(Total_Amount ~ Feedback, data = dataFinal1, 
        main = "Total Amount by Feedback Type", 
        col = c("gold", "skyblue", "lightgreen", "lightcoral"),
        xlab = "Feedback Type", ylab = "Total Amount")

# Predicting Feedback Likelihood
library(nnet)  # Load the package for multinomial logistic regression

# Multinomial logistic regression model
multi_log_model <- multinom(Feedback ~ Total_Amount, data = dataFinal1)

# Summary of the model
summary(multi_log_model)

# Predicted probabilities for feedback types
predicted_feedback <- predict(multi_log_model, newdata = dataFinal1, type = "probs")
head(predicted_feedback)  # View the predicted probabilities

# Explore Spending Thresholds
# Define thresholds as mean or median (both have different results)
threshold <- mean(dataFinal1$Total_Amount, na.rm = TRUE)  # Define threshold as mean
# or
threshold <- median(dataFinal1$Total_Amount, na.rm = TRUE)  # Define threshold as median

# Create a binary variable for high spending
dataFinal1$High_Spending <- ifelse(dataFinal1$Total_Amount > threshold, "Above Threshold", "Below Threshold")

# Test feedback distribution across spending categories
table(dataFinal1$High_Spending, dataFinal1$Feedback)

# Visualization
barplot(table(dataFinal1$High_Spending, dataFinal1$Feedback), beside = TRUE, 
        col = c("lightgreen", "lightblue"), 
        main = "Feedback Distribution by Spending Threshold", 
        xlab = "Spending Category", legend = rownames(table(dataFinal1$High_Spending, dataFinal1$Feedback)))

# Create spending tiers
dataFinal1$Spending_Tier <- cut(dataFinal1$Total_Amount,
                                breaks = c(-Inf, 500, 1000, Inf), 
                                labels = c("Low", "Medium", "High"))

# Table of feedback by spending tiers
table(dataFinal1$Spending_Tier, dataFinal1$Feedback)

# Visualization
barplot(table(dataFinal1$Spending_Tier, dataFinal1$Feedback), beside = TRUE,
        col = c("lightgreen", "lightblue", "gold"),
        main = "Feedback Distribution by Spending Tier",
        xlab = "Spending Tier", legend = rownames(table(dataFinal1$Spending_Tier, dataFinal1$Feedback)))
-----------------------------------

# How accurately can positive feedback likelihood be predicted based on the total amount spent?
  # Convert Feedback into ordered factor or numeric levels
  dataFinal1$Feedback_Num <- as.numeric(factor(dataFinal1$Feedback, 
                                               levels = c("bad", "average", "good", "excellent")))

# Convert Feedback into binary format
dataFinal1$Positive_Feedback <- ifelse(dataFinal1$Feedback == "excellent", 1, 0)

#split data
library(caret)

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(dataFinal1$Positive_Feedback, p = 0.8, list = FALSE)
train_data <- dataFinal1[trainIndex, ]
test_data <- dataFinal1[-trainIndex, ]

# Logistic Regression for Binary Feedback
log_model <- glm(Positive_Feedback ~ Total_Amount, data = train_data, family = binomial)

# Summary of the model
summary(log_model)

# Predict probabilities on the test set
log_predictions <- predict(log_model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions (e.g., threshold = 0.5)
log_predictions_binary <- ifelse(log_predictions > 0.5, 1, 0)

# Multinomial Logistic Regression for Categorical Feedback
library(nnet)
multi_log_model <- multinom(Feedback_Num ~ Total_Amount, data = train_data)

# Predict probabilities on the test set
multi_predictions <- predict(multi_log_model, newdata = test_data, type = "class")

-------------------------------
# Does spending above a specific threshold significantly increase the chances of receiving positive feedback?
#Define the Spending Threshold
threshold <- mean(dataFinal1$Total_Amount, na.rm = TRUE)  # or median()
print(threshold)

# Create a binary column for spending
dataFinal1$Spending_Category <- ifelse(dataFinal1$Total_Amount > threshold, "Above Threshold", "Below Threshold")

# Table of Feedback by Spending Category
table(dataFinal1$Spending_Category, dataFinal1$Feedback)

# Proportion of positive feedback (e.g., excellent) in each spending group
prop.table(table(dataFinal1$Spending_Category, dataFinal1$Feedback), margin = 1)

# Perform Chi-Square Test
chi_sq_test <- chisq.test(table(dataFinal1$Spending_Category, dataFinal1$Feedback))
print(chi_sq_test)

# Logistic regression
log_model <- glm(Positive_Feedback ~ Spending_Category, data = dataFinal1, family = binomial)

# Summary of the model
summary(log_model)

# Odds ratio interpretation
exp(coef(log_model))

# Bar plot of Feedback proportions by Spending Category
library(ggplot2)
ggplot(dataFinal1, aes(x = Spending_Category, fill = Feedback)) +
  geom_bar(position = "fill") +
  ggtitle("Feedback Distribution by Spending Category") +
  xlab("Spending Category") +
  ylab("Proportion of Feedback") +
  scale_fill_brewer(palette = "Set2")
------------------------------------
# Do customers in different spending tiers (low, medium, high) show varying levels of positive feedback likelihood?
# Convert Income and Subscription into factors
dataFinal1$Income <- factor(dataFinal1$Income, levels = c("low", "medium", "high"))
dataFinal1$Subscription <- factor(dataFinal1$Subscription, levels = c("new", "regular", "premium"))

# Cross-tabulation for Income and Feedback
table(dataFinal1$Income, dataFinal1$Feedback)

# Cross-tabulation for Subscription and Feedback
table(dataFinal1$Subscription, dataFinal1$Feedback)

# Proportions for Income categories
prop.table(table(dataFinal1$Income, dataFinal1$Feedback), margin = 1)

# Proportions for Subscription categories
prop.table(table(dataFinal1$Subscription, dataFinal1$Feedback), margin = 1)

# Chi-Square Test for Income
chisq_income <- chisq.test(table(dataFinal1$Income, dataFinal1$Feedback))
print(chisq_income)

# Chi-Square Test for Subscription
chisq_subscription <- chisq.test(table(dataFinal1$Subscription, dataFinal1$Feedback))
print(chisq_subscription)

library(ggplot2)

# Feedback by Income
ggplot(dataFinal1, aes(x = Income, fill = Feedback)) +
  geom_bar(position = "fill") +
  ggtitle("Feedback Distribution by Income Tier") +
  xlab("Income Tier") +
  ylab("Proportion of Feedback") +
  scale_fill_brewer(palette = "Set2")

# Feedback by Subscription
ggplot(dataFinal1, aes(x = Subscription, fill = Feedback)) +
  geom_bar(position = "fill") +
  ggtitle("Feedback Distribution by Subscription Tier") +
  xlab("Subscription Type") +
  ylab("Proportion of Feedback") +
  scale_fill_brewer(palette = "Set2")
---------------------------------------------
# extra - Segmentation analysis – group customers into spending tiers (low, medium, high) and compare their feedback likelihood
-
-------------------------------------------------------------------------------------------------------------------------------------------
# Hew Pik Rou - TP071195
# product brand, average product ratings
# To analyze the effect of product brand on average product rating
# Do average product ratings significantly differ across various product brands?

# How does the distribution of product ratings vary for different brands?

# Can product brands be grouped based on similarities in their rating patterns?

# What insights can be derived by analyzing customer review sentiments for each product brand?

# extra - Sentiment analysis – extract and analyze customer reviews for each brand to correlate sentiments with ratings

# Irfan bin Ismail - TP070616
# product type, loyalty or subscription enrollment
# To assess how product type influences loyalty or subscription enrollment
# Which product types are most associated with higher loyalty or subscription enrollment rates?

# Can the likelihood of loyalty enrollment be predicted based on the type of product purchased?

# Are there identifiable clusters of product types with similar loyalty enrollment patterns?

# How has loyalty or subscription enrollment for specific product types changed over time?

# extra - Trend analysis – explore shifts in loyalty enrollment for different product types over time

# Keeranraj a/l Gunasegaran- TP070672
# shopping frequency, future total purchase amount
# To evaluate the relationship between shopping frequency and future total purchase amount
# How does shopping frequency correlate with the total purchase amount in the future?

# Can future total purchase amounts be forecasted based on shopping frequency trends?

# Does shopping frequency vary among customer groups with different purchase behaviors over time?

# How does shopping frequency predict the likelihood of a decline in future purchases (churn)?

# extra - Churn analysis – investigate how shopping frequency predicts a decline in future purchases.
