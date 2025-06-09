# NOTE: 
# 1. include good programming practices such as comments, variable naming conventions and indentation
# 2. validation should be done for each entry from the users to avoid logical errors
# 3. perform additional research to further understand the information on the given dataset during evaluation of data
# 4. the analysis should be meaningful and effective in providing the information for decision-making

# data import (cleaned)
dataFinal1 <- read.csv("cleaned_data.csv")
View(dataFinal1)

# required libraries: dplyr, ggplot2, tidyr
library(dplyr)
library(ggplot2)
library(tidyr)
-----------------------------------------------------------------------
# Elysha Sophia binti Ridzuan - TP071162
  # customer income levels, ratings
  # To Investigate the relationship between customer income levels and product ratings to determine if income impacts customer satisfaction 
  # Do income levels (low, medium, high) and country influence customer satisfaction, as reflected in product ratings?
  # Contingency table for Income, Country, and Ratings(add 2 more variables)
  # Summarize how Income Levels, Countries, and Ratings are distributed
  income_country_ratings_table <- table(dataFinal1$Income, dataFinal1$Country, dataFinal1$Ratings)

# Summarize distribution across income levels and countries
dataFinal1 %>%
  group_by(Income, Country) %>%
  summarize(Count = n())

# Checks if Income Level significantly affects Product Ratings
# Perform chi-square test on Income and Ratings with Country as context
chi_income_country_ratings <- chisq.test(table(dataFinal1$Income, dataFinal1$Ratings))
chi_income_country_ratings

# deep insights
chi_income_country_ratings$expected
chi_income_country_ratings$observed

# Create Count (preserve original data structure)
library(dplyr)
dataFinal1 <- dataFinal1 %>%
  group_by(Income, Ratings) %>%
  mutate(Count = n())

# visualize
install.packages("pdp")
library(vcd)
mosaic(~ Income + Country + Ratings, data = dataFinal1, shade = TRUE)

library(ggplot2)

# Residuals calculation
residuals_df <- as.data.frame(chi_income_country_ratings$residuals)
colnames(residuals_df) <- c("Income", "Ratings", "Residuals")

# Heatmap visualization
ggplot(residuals_df, aes(x = Ratings, y = Income, fill = Residuals)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap of Chi-Squared Residuals",
       x = "Ratings",
       y = "Income Level",
       fill = "Residuals Strength")

# Proof for assumptions
# Conducts multiple comparisons while adjusting for error.
pairwise.prop.test(table(dataFinal1$Income, dataFinal1$Ratings), p.adjust.method = "bonferroni")

library(plotly)

# Convert categorical variables to numeric
dataFinal1$Country_Num <- as.numeric(factor(dataFinal1$Country, levels = unique(dataFinal1$Country)))
dataFinal1$Income_Num <- as.numeric(factor(dataFinal1$Income, levels = c("low", "medium", "high")))

# 3D scatter plot with correct labeling
plot_ly(dataFinal1, x = ~Country_Num, y = ~Income_Num, z = ~Count, 
        type = "scatter3d", mode = "markers", color = ~Income, text = ~Country) %>%
  layout(title = "3D Scatter Plot of Income Groups by Country",
         scene = list(
           xaxis = list(title = "Country", tickmode = "array", 
                        tickvals = unique(dataFinal1$Country_Num), 
                        ticktext = unique(dataFinal1$Country)),
           yaxis = list(title = "Income Level"),
           zaxis = list(title = "Count")
         ))

# Install & Load Libraries
install.packages("ALEPlot")
library(nnet)
library(pscl)  # Load pscl early for pR2()

# Ensure categorical variables are factors
dataFinal1$Income <- factor(dataFinal1$Income, levels = c("low", "medium", "high"))
dataFinal1$Country <- factor(dataFinal1$Country)  # Convert if needed
dataFinal1$Ratings <- factor(dataFinal1$Ratings)  # Convert Ratings if needed

# Build Multinomial Model
glm_model_multi <- multinom(Ratings ~ Income * Country, data = dataFinal1)

# Model Summary & Coefficients Assessment
summary(glm_model_multi)
z_values <- summary(glm_model_multi)$coefficients / summary(glm_model_multi)$standard.errors
print(z_values)

# Assess Model Fit Using McFadden’s Pseudo R²
pseudo_r2 <- pR2(glm_model_multi)
print(pseudo_r2)

# Test Predictions on New Income & Country Values
new_data <- data.frame(Income = factor(c("low", "high"), levels = c("low", "medium", "high")),
                       Country = factor(c("uk", "germany"), levels = unique(dataFinal1$Country)))

predicted_ratings <- predict(glm_model_multi, new_data)
print(predicted_ratings)

# Probability Estimates for Predictions
probs <- predict(glm_model_multi, new_data, type = "probs")
print(probs)
#done
----------------------------------
# Does customer income level, country, order status, and feedback shape product ratings, revealing patterns in satisfaction trends?
# prepare rating_num values as categorical
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                   ifelse(dataFinal1$Ratings == "high", 2,NA ))
# Check rating imbalance
table(dataFinal1$Ratings_Num)  # Check counts of each class
prop.table(table(dataFinal1$Ratings_Num))  # Check percentage distribution

# Downsampling the rating
library(dplyr)

# Separate majority and minority classes
minority_class <- dataFinal1 %>% filter(Ratings_Num == "1")
majority_class <- dataFinal1 %>% filter(Ratings_Num == "2")

# Randomly select a subset of majority class to match minority class size
set.seed(42)  # For reproducibility
majority_downsampled <- majority_class %>% sample_n(nrow(minority_class))

# Combine balanced dataset
data_downsampled <- rbind(majority_downsampled, minority_class)

# Check balance
table(data_downsampled$Ratings_Num)

# Check the current changes 
summary(data_downsampled)  # Ensure there are no unexpected missing values

# Train a Classification Model
data_downsampled$Ratings_Num <- factor(data_downsampled$Ratings_Num, levels = c("1", "2"))

# Train model using corrected factor variable
library(randomForest)
rf_model <- randomForest(Ratings_Num ~ Income + Country + Order_Status + Feedback, 
                         data = data_downsampled, 
                         ntree = 500,  
                         mtry = 3,  
                         importance = TRUE)

# Evaluate feature importance
importance(rf_model)

# Performance evaluation
library(caret)

# Define actual vs predicted values
predictions <- predict(rf_model, data_downsampled)

conf_matrix <- confusionMatrix(predictions, data_downsampled$Ratings_Num)

# Print model accuracy & performance metrics
print(conf_matrix)

# Incorporate Time-Series Insights
# must have declare "ds"
library(dplyr)

# Convert Month names to proper case (first letter uppercase)
dataFinal1 <- dataFinal1 %>%
  mutate(Month = tolower(Month)) %>%  # Convert everything to lowercase for consistency
  mutate(Month = case_when(
    Month == "january" ~ 1,
    Month == "february" ~ 2,
    Month == "march" ~ 3,
    Month == "april" ~ 4,
    Month == "may" ~ 5,
    Month == "june" ~ 6,
    Month == "july" ~ 7,
    Month == "august" ~ 8,
    Month == "september" ~ 9,
    Month == "october" ~ 10,
    Month == "november" ~ 11,
    Month == "december" ~ 12,
    TRUE ~ NA_real_  # Assign NA if unexpected value is found
  ))

# Verify conversion
table(dataFinal1$Month)

# Recreate the month_binary column
dataFinal1 <- dataFinal1 %>%
  mutate(Month_Binary = ifelse(Month %in% c(1, 2, 3, 4, 5, 6), 1, 0))

# Verify results
table(dataFinal1$Month_Binary)

# Create synthetic date using Year & Month (set to first day of each month)
dataFinal1 <- dataFinal1 %>%
  mutate(ds = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))

# Ensure there are no NA values in ds
dataFinal1 <- dataFinal1 %>% filter(!is.na(ds))

# Format dataset for Prophet
prophet_data <- dataFinal1[, c("ds", "Ratings_Num")]
colnames(prophet_data) <- c("ds", "y")  # Prophet requires 'ds' and 'y'

# Train Prophet model
library(prophet)
model <- prophet(prophet_data)

# Forecast next 12 months
future <- make_future_dataframe(model, periods = 12, freq = "month")  # Predict next 12 months
forecast <- predict(model, future)

# Visualize predictions
plot(model, forecast)

#model XGBoost
library(xgboost)

# Convert data to matrix format for XGBoost
data_matrix <- model.matrix(Ratings_Num ~ Income + Country + Order_Status + Feedback, data = data_downsampled)[,-1]
labels <- as.numeric(data_downsampled$Ratings_Num) - 1  # Convert to binary (0,1)

dtrain <- xgb.DMatrix(data = data_matrix, label = labels)

# Train XGBoost model
xgb_model <- xgboost(data = dtrain, nrounds = 500, objective = "binary:logistic")

# Predictions
xgb_preds <- predict(xgb_model, dtrain)
xgb_conf_matrix <- confusionMatrix(factor(round(xgb_preds)), factor(labels))
print(xgb_conf_matrix)

# model linear regression
# Train logistic regression model first
log_model <- glm(Ratings_Num ~ Income + Country + Order_Status + Feedback, data = data_downsampled, family = binomial)

# Generate predictions
log_preds <- predict(log_model, data_downsampled, type = "response")

# Ensure Ratings_Num is properly set as a factor before comparison
data_downsampled$Ratings_Num <- factor(data_downsampled$Ratings_Num, levels = c(1, 2))

# Round predictions and match factor levels
log_preds_rounded <- factor(round(log_preds), levels = c(1, 2))

# Compare levels (now this won't error)
levels(factor(data_downsampled$Ratings_Num))  # Check levels in actual data
levels(log_preds_rounded)  # Check levels in predictions

# Predictions
log_preds <- predict(log_model, data_downsampled, type = "response")
log_conf_matrix <- confusionMatrix(log_preds_rounded, data_downsampled$Ratings_Num)
print(log_conf_matrix)

# Compare model performance
model_performance <- data.frame(
  Model = c("Random Forest", "XGBoost", "Logistic Regression"),
  Accuracy = c(conf_matrix$overall["Accuracy"], 
               xgb_conf_matrix$overall["Accuracy"], 
               log_conf_matrix$overall["Accuracy"])
)

# Print comparison
print(model_performance)

# feature importance analysis
varImpPlot(rf_model)  # Visualize feature importance

# If Income & Feedback jointly affect satisfaction rather than treating them independently
rf_model_interaction <- randomForest(Ratings_Num ~ Income * Feedback + Country + Order_Status, data = data_downsampled, ntree = 500)
#check model summary
print(rf_model_interaction)
# feature importance analysis
importance(rf_model_interaction)
varImpPlot(rf_model_interaction)  # Visualize importance
# make predictions
rf_preds <- predict(rf_model_interaction, data_downsampled)
#model accuracy
library(caret)

conf_matrix_interaction <- confusionMatrix(rf_preds, data_downsampled$Ratings_Num)
print(conf_matrix_interaction)

# partial dependence plots
summary(data_downsampled$Income)
str(data_downsampled$Income)
data_downsampled <- data_downsampled %>%
  mutate(Income = as.numeric(factor(Income, levels = c("low", "medium", "high"), ordered = TRUE)))
# Ensure no NAs remain
data_downsampled <- data_downsampled %>% filter(!is.na(Income))

# visual
library(pdp)
partialPlot(rf_model_interaction, data_downsampled, Income)
partialPlot(rf_model_interaction, data_downsampled, Feedback)

# track how it changes over months
library(ggplot2)
# If the plot looks noisy, add a smooth trend line to highlight overall satisfaction patterns
ggplot(dataFinal1, aes(x = ds, y = Ratings_Num, color = Income)) +
  geom_line() +
  theme_minimal()

# Since you're analyzing satisfaction trends over time, add facet_wrap() to split trends by month
ggplot(dataFinal1, aes(x = ds, y = Ratings_Num, color = Income)) +
  geom_line() +
  facet_wrap(~ Month) +  # Separates trends by month
  theme_minimal()

# Compare Rating Trends by Country
ggplot(dataFinal1, aes(x = ds, y = Ratings_Num, color = Country)) +
  geom_line() +
  theme_minimal()

#done
---------------------------------------------------------------------------------------------
# What specific products show the strongest income-level effects? (analysis question)
# Helps identify how different income levels rate various product categories
table(dataFinal1$Income, dataFinal1$Product_Category, dataFinal1$Ratings)

dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 5,NA ))

dataFinal1$Ratings_Binary <- ifelse(dataFinal1$Ratings == "low", 0, 
                                    ifelse(dataFinal1$Ratings == "high", 1,NA ))

# This allows us to see which product types have the highest/lowest ratings across different income groups
dataFinal1 %>%
  group_by(Income, Product_Type) %>%
  summarize(Average_Rating = mean(Ratings_Num))
#If significant, we can focus on specific product types
income_product_ratings_table <- table(dataFinal1$Income, dataFinal1$Product_Type)

chi_income_product <- chisq.test(income_product_ratings_table)
chi_income_product

#This chart will show how income groups distribute ratings across product types
ggplot(dataFinal1, aes(x = Product_Type, fill = Income)) +
  geom_bar(position = "fill") +
  coord_flip() +  # Flip to improve readability
  labs(title = "Income-Level Effects on Product Ratings", x = "Product Type", y = "Proportion") +
  theme_minimal()

# Coefficients tell us how Income, Country, and Product_Type influence ratings
model <- glm(Ratings_Binary ~ Income + Country + Product_Type, data = dataFinal1, family = binomial)
summary(model)

# To understand the likelihood of high ratings across income levels
exp(coef(model))  # Convert log-odds to odds ratios

# Measures overall correctness (35.11%)
library(caret)
predicted_ratings <- ifelse(predict(model, type = "response") > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_ratings), as.factor(dataFinal1$Ratings_Num))

# Visualize results
ggplot(dataFinal1, aes(x = Income, fill = Country)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Product_Type) +
  labs(title = "Density of Income Levels Across Product Types", x = "Income Level", y = "Density") +
  theme_minimal()

# Must use other model, XGBoost (65.02%)
#Convert categorical variables into numeric representations.
#XGBoost requires input as matrix format
library(dplyr)

# Convert categorical variables to factors, then to numeric
dataFinal1 <- dataFinal1 %>%
  mutate(
    Ratings_Num = ifelse(Ratings == "high", 1, 0),  
    Income_Num = as.numeric(factor(Income, levels = c("low", "medium", "high"))),
    Country_Num = as.numeric(factor(Country)),
    Product_Type_Num = as.numeric(factor(Product_Type))
  )

# Prepare data matrix
X <- as.matrix(select(dataFinal1, Income_Num, Country_Num, Product_Type_Num))
y <- dataFinal1$Ratings_Num

# Create XGBoost Model
library(xgboost)
xgb_data <- xgb.DMatrix(data = X, label = y)

# Define parameters
params <- list(
  objective = "binary:logistic",  # Classification task
  eval_metric = "logloss",  # Loss metric for binary classification
  eta = 0.1,  # Learning rate
  max_depth = 6,  # Tree depth
  subsample = 0.8,  # Fraction of data used for training
  colsample_bytree = 0.8  # Feature sampling per tree
)

# Train model
xgb_model <- xgb.train(params, xgb_data, nrounds = 100, verbose = TRUE)

# Checks if predictions align with actual ratings
predictions <- predict(xgb_model, X)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

library(caret)
confusionMatrix(as.factor(predicted_labels), as.factor(y))

# Feature Engineering – Introduce New Predictors
dataFinal1 <- dataFinal1 %>%
  mutate(
    Payment_Num = as.numeric(factor(Payment_Method)),
    Shipping_Num = as.numeric(factor(Shipping_Method))
  )

# LightGBM is faster and handles categorical features better than XGBoost
install.packages("lightgbm")
library(lightgbm)

lgb_data <- lgb.Dataset(data = X, label = y)

lgb_params <- list(objective = "binary", metric = "logloss", learning_rate = 0.05, num_leaves = 31)
lgb_model <- lgb.train(params = lgb_params, data = lgb_data, nrounds = 200)

predictions_lgb <- predict(lgb_model, X)
confusionMatrix(as.factor(ifelse(predictions_lgb > 0.5, 1, 0)), as.factor(y))

# Some relationships may be complex—explicitly adding interactions like
dataFinal1 <- dataFinal1 %>%
  mutate(Income_Product_Interaction = Income_Num * Product_Type_Num,
         Country_Payment_Interaction = Country_Num * Payment_Num)

# Adjust Probability Threshold Dynamically
# This shows how different classification thresholds affect True Positive Rate (Sensitivity) vs. False Positive Rate (1 - Specificity)
library(pROC)
roc_curve <- roc(y, predictions_lgb)

# Plot ROC Curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Threshold Analysis")

# Add Optimal Threshold Marker
optimal_threshold <- coords(roc_curve, "best", ret = "threshold")
abline(v = optimal_threshold[1], col = "red", lwd = 2, lty = 2)  
text(optimal_threshold[1], 0.5, labels = paste("Opt. Threshold:", round(optimal_threshold[1], 2)), col = "red", pos = 4)

# This is useful when high ratings dominate, ensuring the model doesn't favor majority classes 
library(PRROC)

# Create Precision-Recall Curve
pr_curve <- pr.curve(scores.class0 = predictions_lgb, weights.class0 = y, curve = TRUE)

# Plot Precision-Recall Curve
plot(pr_curve, col = "green", lwd = 2, main = "Precision-Recall Curve for Threshold Tuning")

# Visualizes how performance changes as the threshold shifts
thresholds <- seq(0.1, 0.9, by = 0.05)
results <- data.frame()

for (t in thresholds) {
  predicted_labels <- ifelse(predictions_lgb > t, 1, 0)
  cm <- confusionMatrix(as.factor(predicted_labels), as.factor(y))
  
  results <- rbind(results, data.frame(
    Threshold = t,
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"]
  ))
}

ggplot(results, aes(x = Threshold)) +
  geom_line(aes(y = Accuracy, color = "Accuracy")) +
  geom_line(aes(y = Sensitivity, color = "Sensitivity")) +
  geom_line(aes(y = Specificity, color = "Specificity")) +
  labs(title = "Threshold Optimization", x = "Threshold", y = "Metric Value") +
  scale_color_manual(name = "Metrics", values = c("Accuracy" = "blue", "Sensitivity" = "red", "Specificity" = "green")) +
  theme_minimal()

# Simplifies the model, reducing overfitting risk
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(X))
print(importance_matrix)  # View top contributing features

# Select only the strongest features
X_refined <- as.matrix(select(dataFinal1, Income_Num, Product_Type_Num, Country_Num, Payment_Num))
xgb_data_refined <- xgb.DMatrix(data = X_refined, label = y)

# Model Evaluation Beyond Accuracy
library(pROC)
roc_curve <- roc(y, predictions_lgb)
auc(roc_curve)  # Evaluate AUC for classification quality

# feature refinment
dataFinal1 <- dataFinal1 %>%
  mutate(Income_Product_Interaction = Income_Num * Product_Type_Num)

# train again with updated parameters
params <- list(eta = 0.05, max_depth = 8, subsample = 0.9, colsample_bytree = 0.7)
X_updated <- as.matrix(select(dataFinal1, Income_Num, Product_Type_Num, Country_Num, Payment_Num, Income_Product_Interaction))
y <- dataFinal1$Ratings_Num
xgb_data_updated <- xgb.DMatrix(data = X_updated, label = y)
xgb_model_updated <- xgb.train(params, xgb_data_updated, nrounds = 100, verbose = TRUE)

#evaluate performance
predictions_updated <- predict(xgb_model_updated, X_updated)
predicted_labels_updated <- ifelse(predictions_updated > 0.5, 1, 0)

library(caret)
confusionMatrix(as.factor(predicted_labels_updated), as.factor(y))

# feature importance analysis
importance_matrix_updated <- xgb.importance(model = xgb_model_updated, feature_names = colnames(X_updated))
print(importance_matrix_updated)

#AUC-ROC Validation
library(pROC)
roc_curve_updated <- roc(y, predictions_updated)
auc(roc_curve_updated)

# Visualizing the AUC-ROC Difference
library(pROC)

# Original ROC Curve
roc_curve_original <- roc(y, predictions)
auc_original <- auc(roc_curve_original)

# Updated ROC Curve
roc_curve_updated <- roc(y, predictions_updated)
auc_updated <- auc(roc_curve_updated)

# Plot ROC Curves
plot(roc_curve_original, col = "red", lwd = 2, main = "AUC-ROC Comparison")
lines(roc_curve_updated, col = "blue", lwd = 2)

# Add Legend
legend("bottomright", legend = c(
  paste("Original AUC:", round(auc_original, 3)), 
  paste("Updated AUC:", round(auc_updated, 3))
), col = c("red", "blue"), lwd = 2)
#done
---------------------------------------------------------------------------------------------------
  # Is there a relationship between customers income and spending tier, their satisfaction levels and month, as shown by product ratings? (analysis question)
  # Prepare spending tier column
  dataFinal1 <- dataFinal1 %>%
  mutate(Spending_Tier = case_when(
    Total_Amount < 500 ~ "Low",
    Total_Amount >= 500 & Total_Amount < 1000 ~ "Medium",
    Total_Amount >= 1000 ~ "High"
  ))

dataFinal1$Spending_Tier <- factor(dataFinal1$Spending_Tier, levels = c("Low", "Medium", "High"))

dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 5,NA ))

# This will count how many people gave high vs. low ratings, categorized by Spending Tier
dataFinal1 <- dataFinal1 %>%
  mutate(Rating_Category = ifelse(Ratings_Num == 1, "High", "Low"))

# Count ratings per group
rating_counts <- dataFinal1 %>%
  group_by(Income, Spending_Tier, Rating_Category) %>%
  summarize(Count = n()) %>%
  ungroup()

print(rating_counts)

# Visualization
ggplot(rating_counts, aes(x = Spending_Tier, y = Count, fill = Rating_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Income) +  # Separate charts per income level
  scale_fill_manual(values = c("pink", "lightblue")) +  
  labs(title = "High vs. Low Ratings Across Spending Tiers",
       x = "Spending Tier", y = "Number of Ratings", fill = "Rating Level") +
  theme_minimal()

# Not enough difference
chi_spending_ratings <- chisq.test(table(dataFinal1$Spending_Tier, dataFinal1$Rating_Category))
print(chi_spending_ratings)

# ANOVA tests whether any significant rating differences exist between income and spending tiers
anova_model <- aov(Ratings_Num ~ Income * Spending_Tier, data = dataFinal1)
summary(anova_model)
# Linear regression quantifies how much each factor influences ratings numerically
reg_model <- lm(Ratings_Num ~ Income + Spending_Tier, data = dataFinal1)
summary(reg_model)

dataFinal1$Ratings_Binary <- ifelse(dataFinal1$Ratings_Num == 1, 0, 
                                    ifelse(dataFinal1$Ratings_Num == 5, 1, NA))

# Logistic regression focuses on binary satisfaction (high vs. low ratings)
log_model <- glm(Ratings_Binary ~ Income + Spending_Tier, family = binomial, data = dataFinal1)
summary(log_model)

# This counts high vs. low ratings per month, revealing seasonal shifts
rating_month_counts <- dataFinal1 %>%
  group_by(Month, Rating_Category) %>%
  summarize(Count = n()) %>%
  ungroup()

print(rating_month_counts)

# Visualizing Monthly Rating Trends
ggplot(rating_month_counts, aes(x = Month, y = Count, fill = Rating_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("brown", "lightblue")) +  
  labs(title = "Monthly Distribution of High vs. Low Ratings",
       x = "Month", y = "Number of Ratings", fill = "Rating Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# If ratings fluctuate over time, check whether satisfaction is significantly different across months
chi_monthly_ratings <- chisq.test(table(dataFinal1$Month, dataFinal1$Rating_Category))
print(chi_monthly_ratings)

# Reveals seasonal rating shifts across different spending groups.
anova_model_month <- aov(Ratings_Num ~ Income * Spending_Tier * Month, data = dataFinal1)
summary(anova_model_month)

#assumptions 1
aggregate(Ratings_Num ~ Income, data = dataFinal1, mean)

aov_income <- aov(Ratings_Num ~ Income, data = dataFinal1)
summary(aov_income)

TukeyHSD(aov_income)

ggplot(dataFinal1, aes(x = Income, fill = Ratings)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of High VS Low Ratings by Income Level",
       x = "Income Level", y = "Proportion") +
  theme_minimal()

chisq.test(table(dataFinal1$Income, dataFinal1$Ratings_Num))

# add clustering to see any changes
-------------------------------
  # Which spending tier , payment method and total amount receive the highest ratings, and how does this reflect customer preferences? (analysis question) 
  library(cluster)
library(factoextra)

dataFinal1$Income <- as.numeric(as.factor(dataFinal1$Income))
dataFinal1$Spending_Tier <- as.numeric(as.factor(dataFinal1$Spending_Tier))
dataFinal1$Payment_Method <- as.numeric(as.factor(dataFinal1$Payment_Method))  # Convert categorical variable to numeric

# Select relevant variables for clustering
data_cluster <- dataFinal1 %>%
  select(Income, Spending_Tier, Ratings_Num, Total_Amount, Payment_Method) %>%
  scale()  # Standardize variables for better clustering

# Run K-Means with optimal clusters
set.seed(123)
kmeans_result <- kmeans(data_cluster, centers = 5)  # Adjust centers based on patterns

# Add cluster labels to dataset
dataFinal1$Cluster <- factor(kmeans_result$cluster)

# Visualize clusters
fviz_cluster(kmeans_result, data = data_cluster, geom = "point")

# This tells you the average values of Income, Spending_Tier, Ratings_Num, and Total_Amount within each cluster
cluster_centroids <- aggregate(data_cluster, by = list(Cluster = kmeans_result$cluster), FUN = mean)
print(cluster_centroids)

# Identifies which cluster contains the largest group of customers.
table(dataFinal1$Cluster, dataFinal1$Payment_Method)

# Check if Income, Spending_Tier, Ratings_Num, and Total_Amount are significantly different across clusters
anova_income <- aov(Income ~ Cluster, data = dataFinal1)
summary(anova_income)

anova_spending <- aov(Spending_Tier ~ Cluster, data = dataFinal1)
summary(anova_spending)

anova_ratings <- aov(Ratings_Num ~ Cluster, data = dataFinal1)
summary(anova_ratings)

anova_ratings <- aov(Total_Amount ~ Cluster, data = dataFinal1)
summary(anova_ratings)

# Aggregate total spending per cluster
cluster_spending <- dataFinal1 %>%
  group_by(Cluster, Ratings) %>%
  summarize(Total_Spent = sum(Total_Amount), Count = n()) %>%
  ungroup()

print(cluster_spending)

# Helps identify whether certain clusters spend more while rating poorly or highly
ggplot(dataFinal1, aes(x = Cluster, fill = factor(Payment_Method))) +
  geom_bar(position = "fill") +  # Stacked proportions
  scale_fill_manual(values = c("#FFABAB", "#85E3FF", "#A3A3F3", "#FFD166")) +
  labs(title = "Payment Method Preference Across Clusters",
       x = "Cluster", y = "Proportion", fill = "Payment Method") +
  theme_minimal()

#deep insight
dataFinal1$Payment_Method <- factor(dataFinal1$Payment_Method)  # Convert numeric values to categorical factors

payment_spending <- dataFinal1 %>%
  group_by(Spending_Tier, Payment_Method) %>%
  summarize(Average_Spend = mean(Total_Amount), Count = n()) %>%
  ungroup()

#declare new column
payment_usage <- dataFinal1 %>%
  group_by(Spending_Tier, Payment_Method) %>%
  summarize(Count = n()) %>%
  ungroup()

print(payment_usage)

ggplot(payment_usage, aes(x = Spending_Tier, y = Count, fill = factor(Payment_Method))) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge for comparison across tiers
  scale_fill_manual(values = c("#FF6F61", "#6A5ACD", "#FFD166", "#85E3FF")) +
  labs(title = "Number of Users by Payment Method Across Spending Tiers",
       x = "Spending Tier", y = "Number of Users", fill = "Payment Method") +
  theme_minimal()


chi_payment_spending <- chisq.test(table(dataFinal1$Payment_Method, dataFinal1$Spending_Tier))
print(chi_payment_spending)

anova_payment_ratings <- aov(Ratings_Num ~ Payment_Method * Spending_Tier, data = dataFinal1)
summary(anova_payment_ratings)

library(caret)
log_model_refined <- glm(Ratings_Binary ~ Payment_Method + Spending_Tier, family = binomial, data = dataFinal1)
summary(log_model_refined)

# Check updated accuracy
predictions_refined <- ifelse(predict(log_model_refined, type = "response") > 0.5, 1, 0)
confusionMatrix(as.factor(predictions_refined), as.factor(dataFinal1$Ratings_Binary))

#machine learning
#prepare dataset
set.seed(123)

# Create 90%-10% split
trainIndex <- sample(1:nrow(dataFinal1), size = round(0.9 * nrow(dataFinal1)))  
trainData <- dataFinal1[trainIndex, ]
testData <- dataFinal1[-trainIndex, ]

summary(trainData[, c("Spending_Tier", "PaymentMethod_Cash", "PaymentMethod_PayPal",
                      "PaymentMethod_DebitCard", "PaymentMethod_CreditCard")])

library(caret)

# Convert categorical Payment_Method into dummy variables
dummy_vars <- dummyVars(~ Payment_Method, data = dataFinal1)
payment_encoded <- predict(dummy_vars, dataFinal1)

# Combine with original dataset
dataFinal1 <- cbind(dataFinal1, payment_encoded)

set.seed(123)

# Sample 50% from 30K (15000 rows)
subsetIndex <- sample(1:nrow(dataFinal1), size = 15000)  
dataSubset <- dataFinal1[subsetIndex, ]  # Extract 6000 rows

# Split into 80% train (12000 rows) & 20% test (3000 rows)
trainIndex <- sample(1:nrow(dataSubset), size = 12000)  
trainData <- dataSubset[trainIndex, ]
testData <- dataSubset[-trainIndex, ]

trainData$Income <- as.numeric(trainData$Income)
trainData$Spending_Tier <- as.factor(trainData$Spending_Tier)  # If categorical
trainData$Ratings_Binary <- as.numeric(trainData$Ratings_Binary)  # Ensure binary format

trainData$Income <- scale(trainData$Income)
trainData$Total_Amount <- scale(trainData$Total_Amount)


# Convert to matrices for XGBoost
x_train <- as.matrix(trainData[, c("Income", "Spending_Tier", "Total_Amount", 
                                   "Total_Purchases", "PaymentMethod_Cash", 
                                   "PaymentMethod_PayPal", "PaymentMethod_DebitCard", 
                                   "PaymentMethod_CreditCard")])
y_train <- trainData$Ratings_Binary

x_test <- as.matrix(testData[, c("Income", "Spending_Tier", "Total_Amount", 
                                 "Total_Purchases", "PaymentMethod_Cash", 
                                 "PaymentMethod_PayPal", "PaymentMethod_DebitCard", 
                                 "PaymentMethod_CreditCard")])
y_test <- testData$Ratings_Binary



library(xgboost)

xgb_model <- xgboost(data = x_train, label = y_train, 
                     nrounds = 100, eta = 0.1, max_depth = 6, 
                     subsample = 0.8, colsample_bytree = 0.8, 
                     objective = "binary:logistic")

xgb_model <- xgboost(data = x_train, label = y_train, 
                     nrounds = 200,  # Increase training rounds for better convergence
                     eta = 0.05,  # Slower learning for deeper optimization
                     max_depth = 8,  # Allows more complex feature interactions
                     subsample = 0.8, colsample_bytree = 0.8, 
                     objective = "binary:logistic")

xgb.importance(model = xgb_model)

str(trainData$Total_Purchases)
trainData$Total_Purchases <- as.numeric(trainData$Total_Purchases)
testData$Total_Purchases <- as.numeric(testData$Total_Purchases)

x_train <- as.matrix(trainData[, c("Income", "Spending_Tier", "Total_Amount", 
                                   "Total_Purchases", "PaymentMethod_Cash", 
                                   "PaymentMethod_PayPal", "PaymentMethod_DebitCard", 
                                   "PaymentMethod_CreditCard")])
x_test <- as.matrix(testData[, c("Income", "Spending_Tier", "Total_Amount", 
                                 "Total_Purchases", "PaymentMethod_Cash", 
                                 "PaymentMethod_PayPal", "PaymentMethod_DebitCard", 
                                 "PaymentMethod_CreditCard")])

xgb_model <- xgboost(data = x_train, label = y_train, 
                     nrounds = 200, eta = 0.05, max_depth = 8, 
                     subsample = 0.8, colsample_bytree = 0.8, 
                     objective = "binary:logistic")

# Evaluate model performance
xgb_predictions <- predict(xgb_model, x_test)
xgb_pred_class <- ifelse(xgb_predictions > 0.5, 1, 0)

confusionMatrix(factor(xgb_pred_class), factor(y_test))

library(caret)
confusionMatrix(factor(xgb_pred_class), factor(y_test))
#the end bcs i cannot handle more of it


-------------------------------
  # Does the choice of payment method , total purchases and income level impact customer satisfaction as represented by product ratings? (analysis question)
  #Group customer income levels and calculate average product ratings
  aggregate(Ratings_Num ~ Income, data = dataFinal1, mean)

#Check how ratings vary based on income level and preferred payment method
aggregate(Ratings_Num ~ Income + Payment_Method, data = dataFinal1, mean)

#Use a boxplot to compare ratings distribution by income and payment method
ggplot(dataFinal1, aes(x = Income, y = Ratings_Num, fill = Payment_Method)) +
  geom_boxplot() +
  labs(title = "Product Ratings Across Customer Income Levels & Payment Methods",
       x = "Income Level", y = "Ratings") +
  theme_minimal()

cor.test(dataFinal1$Income, dataFinal1$Ratings_Num)

kruskal.test(Ratings_Num ~ Income, data = dataFinal1)

ggplot(dataFinal1, aes(x = factor(Income), y = Ratings_Num, fill = factor(Income))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFDDC1", "#FFABAB", "#D5AAFF")) + 
  labs(title = "Product Ratings Across Income Levels",
       x = "Income Level", y = "Ratings") +
  theme_minimal()

#deep insights
aggregate(Ratings_Num ~ Payment_Method, data = dataFinal1, summary)

dataFinal1 %>%
  mutate(Satisfied = ifelse(Ratings_Num >= 4, "Satisfied", "Not Satisfied")) %>%
  group_by(Satisfied) %>%
  summarize(Average_Purchases = mean(Total_Purchases))

interaction_model <- lm(Ratings_Num ~ Income * Payment_Method, data = dataFinal1)
summary(interaction_model)
# start
library(cluster)

data_cluster <- scale(dataFinal1[, c("Total_Amount", "Total_Purchases", "Ratings_Num")])
kmeans_result <- kmeans(data_cluster, centers = 3)
dataFinal1$Cluster <- kmeans_result$cluster

ggplot(dataFinal1, aes(x = factor(Cluster), y = Total_Amount, fill = factor(Cluster))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFDDC1", "#FFC3A0", "#D5AAFF", "#85E3FF")) +
  labs(title = "Spending Patterns Across Clusters",
       x = "Customer Cluster", y = "Total Amount Spent") +
  theme_minimal()

library(ggplot2)

ggplot(dataFinal1, aes(x = factor(Cluster), y = Ratings_Num, fill = Total_Purchases)) +
  geom_tile() +
  scale_fill_gradient(low = "#FFDDC1", high = "#FF3B3B") +  
  labs(title = "Satisfaction vs. Purchase Behavior Across Clusters",
       x = "Customer Cluster", y = "Ratings") +
  theme_minimal()

ggplot(dataFinal1, aes(x = factor(Cluster), fill = Payment_Method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#FFDDC1", "#FFC3A0", "#D5AAFF", "#85E3FF")) +
  labs(title = "Payment Method Preferences Across Clusters",
       x = "Customer Cluster", y = "Proportion of Payment Methods",
       fill = "Payment Method") +
  theme_minimal()

ggplot(dataFinal1, aes(x = Payment_Method, y = Total_Amount, fill = Payment_Method)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +  
  labs(title = "Spending Patterns Across Payment Methods",
       x = "Payment Method", y = "Total Amount Spent") +
  theme_minimal()

ggplot(dataFinal1, aes(x = factor(Cluster), y = Payment_Method, fill = Ratings_Num)) +
  geom_tile() +
  scale_fill_gradient(low = "#FFDDC1", high = "#FF3B3B") +  
  labs(title = "Satisfaction Across Payment Methods & Clusters",
       x = "Customer Cluster", y = "Payment Method") +
  theme_minimal()

#assumption 1:Does Payment Method Affect Ratings?

anova_payment <- aov(Ratings_Num ~ Payment_Method, data = dataFinal1)
summary(anova_payment)

aggregate(Ratings_Num ~ Payment_Method, data = dataFinal1, mean)

ggplot(dataFinal1, aes(x = Payment_Method, y = Total_Amount, fill = Payment_Method)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +  
  labs(title = "Spending Behavior Across Payment Methods",
       x = "Payment Method", y = "Total Amount Spent") +
  theme_minimal()

TukeyHSD(anova_payment)

dataFinal1 %>%
  group_by(Payment_Method) %>%
  summarize(Average_Rating = mean(Ratings_Num))

dataFinal1$Satisfied <- ifelse(dataFinal1$Ratings_Num >= 4, 1, 0)

logit_model <- glm(Satisfied ~ Payment_Method, data = dataFinal1, family = binomial)
summary(logit_model)

#more deeper
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Ensure Month Names Are Converted Properly (Handles Lowercase)
dataFinal1$Month <- match(tolower(dataFinal1$Month), tolower(month.name))

# Step 2: Check for NAs and handle missing values
dataFinal1 <- dataFinal1 %>%
  filter(!is.na(Month))  # Remove rows where Month conversion failed

# Step 3: Create Year-Month as a Date Format
dataFinal1$YearMonth <- as.Date(with(dataFinal1, paste(Year, Month, "01", sep = "-")), "%Y-%m-%d")

# Step 4: Extract Year-Month for Trend Analysis
dataFinal1$YearMonth <- format(dataFinal1$YearMonth, "%Y-%m")

# Step 5: Group by Year-Month and Payment Method to calculate average ratings
monthly_ratings <- dataFinal1 %>%
  group_by(YearMonth, Payment_Method) %>%
  summarize(Average_Rating = mean(Ratings_Num, na.rm = TRUE))

# Step 6: Plot Monthly Satisfaction Trends by Payment Method
ggplot(monthly_ratings, aes(x = YearMonth, y = Average_Rating, color = Payment_Method, group = Payment_Method)) +
  geom_line() +
  labs(title = "Monthly Satisfaction Trends by Payment Method",
       x = "Month-Year", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Step 7: Test whether month-year trends predict satisfaction while accounting for payment method
linear_model <- lm(Average_Rating ~ as.numeric(YearMonth) + Payment_Method, data = monthly_ratings)
summary(linear_model)

# Step 8: Visualize linear trend of satisfaction
ggplot(monthly_ratings, aes(x = YearMonth, y = Average_Rating, color = Payment_Method, group = Payment_Method)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Linear Trend of Satisfaction by Payment Method",
       x = "Month-Year", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#done
#end
--------------------------------------------------------------------------------------------------------------------------------------------
# Hew Pik Rou - TP071195
# To determine how subscription status impacts ratings and customer satisfaction
# subscription, ratings
# assumption(s):
#  1. Premium and Regular customers considered as Subscribed customers, New customers considered as non-subscribed customers  
  
# analysis 1 - How do ratings differ between subscribed and non-subscribed customers and does subscription status correlate with higher customer satisfaction?
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                   ifelse(dataFinal1$Ratings == "high", 5,NA ))
summary(dataFinal1$Ratings_Num)  # Should only contain 1 and 5

# Convert Subscription to factor
dataFinal1$Subscription <- factor(dataFinal1$Subscription)

print(levels(dataFinal1$Subscription))

# Compute mean ratings per Subscription category (using Ratings_Num)
avg_ratings <- dataFinal1 %>%
  group_by(Subscription) %>%
  summarise(Average_Rating = mean(Ratings_Num, na.rm = TRUE))

# Compute Min, Median, Mean per Subscription category
summary_stats <- dataFinal1 %>%
  group_by(Subscription) %>%
  summarise(
    Min = min(Ratings_Num),
    Median = median(Ratings_Num),
    Mean = round(mean(Ratings_Num, na.rm = TRUE), 2)  # Rounded to 2 decimal places
  )

plot_data_long <- summary_stats %>%
    pivot_longer(
    cols = c(Min, Median, Mean),
    names_to = "Statistic",
    values_to = "Rating_Value"
)

plot_data_long$Statistic <- factor(plot_data_long$Statistic, levels = c("Min", "Median", "Mean"))

# Merge summary stats with avg_ratings for plotting
plot_data <- avg_ratings %>%
  left_join(summary_stats, by = "Subscription")

# Plot average ratings per Subscription category
ggplot(plot_data_long, aes(y = Subscription, x = Rating_Value, color = Statistic)) +
  geom_point(aes(shape = Statistic), size = 5, alpha = 0.8) +
  geom_line(aes(group = Subscription), color = "gray", linewidth = 0.5, linetype = "dotted") +
  geom_text(data = filter(plot_data_long, Statistic == "Mean"),
            aes(label = round(Rating_Value, 2)),
            hjust = -0.5, size = 3.5, fontface = "bold", color = "#34495E") +
  scale_color_manual(values = c("Min" = "#EE6C4D", "Median" = "#2C3E50", "Mean" = "#3D7A9F"),
                     name = "Rating Statistic") +
  scale_shape_manual(values = c("Min" = 17, "Median" = 15, "Mean" = 16),
                     name = "Rating Statistic") +
  scale_x_continuous(breaks = seq(1, 5, by = 0.5), limits = c(0.8, 5.2), expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Rating Distribution by Subscription Status",
       subtitle = "Min, Median, and Mean Customer Ratings per Group",
       x = "Rating Value", y = "Subscription Status") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 11, color = "#7F8C8D"),
    axis.text.y = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ECF0F1", linetype = "dotted"), panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA), panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Perform ANOVA
anova_results <- aov(Ratings_Num ~ Subscription, data = dataFinal1)
summary(anova_results)

# Run Tukey's HSD test
tukey_results <- TukeyHSD(anova_results)

# Convert Tukey results to a dataframe
tukey_df <- as.data.frame(tukey_results$Subscription)
tukey_df$Comparison <- rownames(tukey_df)

# Convert Tukey HSD results into a named vector
tukey_named <- tukey_df$diff
names(tukey_named) <- tukey_df$Comparison

library(multcompView)
grouping <- multcompLetters(tukey_named)

print(grouping$Letters)

# Convert grouping results into a dataframe
group_df <- data.frame(
  Subscription = names(grouping$Letters),
  Group = unlist(grouping$Letters)  # Converts list to character vector
)

# Ensure Subscription column matches formatting before merging
group_df$Subscription <- tolower(group_df$Subscription)
avg_ratings$Subscription <- tolower(avg_ratings$Subscription)

# Ensure Subscription column matches formatting before merging
group_df$Subscription <- tolower(group_df$Subscription)
avg_ratings$Subscription <- tolower(avg_ratings$Subscription)

plot_data <- avg_ratings %>%
  left_join(group_df, by = "Subscription")

# Plot Tukey HSD results
ggplot(plot_data, aes(x = Subscription, y = Average_Rating, fill = Subscription)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(Average_Rating, 2), "\n", Group)),
            vjust = -0.6, size = 4, color = "#333333", fontface = "bold") +
  scale_fill_manual(values = c("new" = "#007BFF", "regular" = "#2ECC71", "premium" = "#EE6C4D")) +
  scale_y_continuous(limits = c(0, max(plot_data$Average_Rating) * 1.2)) +
  labs(title = "Average Ratings by Subscription Group with Tukey HSD Significance",
       subtitle = "Groups sharing a letter are not significantly different (p < 0.05)",
       x = "Subscription Status", y = "Average Rating") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 12, color = "#7F8C8D", face = "bold"),
    axis.text.y = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "none",
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ECF0F1", linetype = "dotted"), panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA), panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Convert Ratings_Num to binary format (1 = High rating, 0 = Low rating)
dataFinal1$High_Rating <- ifelse(dataFinal1$Ratings_Num == 5, 1, 0)

# Ensure Subscription is a factor
dataFinal1$Subscription <- factor(dataFinal1$Subscription, levels = c("new", "regular", "premium"))

# Perform logistic regression (Subscription predicting high ratings)
logit_model <- glm(High_Rating ~ Subscription, data = dataFinal1, family = binomial)
summary(logit_model)

# Calculate odds ratio & confidence intervals
exp(cbind(OR = coef(logit_model), confint(logit_model)))
#                             OR     2.5 %    97.5 %
#   (Intercept)         1.9338949 1.9076218 1.9605689
# Subscriptionregular 0.8039256 0.7901721 0.8179107
# Subscriptionpremium 1.3681255 1.3382828 1.3986617

# Create odds ratio dataframe
odds_df$Subscription <- factor(odds_df$Subscription,
                               levels = c("Intercept (New)", "Regular vs New", "Premium vs New"))

# Determine color based on OR value relative to 1 (no effect)
odds_df <- odds_df %>%
  mutate(Effect = case_when(
    OR > 1 & Lower_CI > 1 ~ "Increased Odds",
    OR < 1 & Upper_CI < 1 ~ "Decreased Odds",
    TRUE ~ "No Significant Effect" # If CI crosses 1
  ))

# Plot the odds ratios
ggplot(odds_df, aes(x = OR, y = Subscription)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#E4572E", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI, color = Effect), height = 0.2, linewidth = 1.2) +
  geom_point(aes(color = Effect), size = 6, shape = 19) +
  geom_text(aes(label = round(OR, 2)), hjust = -0.5, size = 4, fontface = "bold", color = "#2C3E50") +
  scale_color_manual(values = c("Increased Odds" = "#2CA4B0", "Decreased Odds" = "#EE6C4D", "No Significant Effect" = "#8D99AE")) +
  scale_x_continuous(breaks = seq(0.5, max(odds_df$Upper_CI) * 1.1, by = 0.25), limits = c(min(odds_df$Lower_CI) * 0.9, max(odds_df$Upper_CI) * 1.1)) +
  labs(title = "Likelihood of High Ratings Across Subscription Groups",
       subtitle = "Odds Ratios and 95% Confidence Intervals relative to 'New' subscribers",
       x = "Odds Ratio (OR)", y = "Subscription Comparison") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 11, color = "#7F8C8D"),
    axis.text.y = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ECF0F1", linetype = "dotted"), panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA), panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# analysis 2 - Are subscribed customers more likely to leave higher ratings through feedback compared to non-subscribed customers?
# Create count of high ratings per subscription group
prop_data <- table(dataFinal1$Subscription, dataFinal1$High_Rating)

# Run two-proportion Z-test
# Measures if subscribed customers are significantly more likely to leave high ratings than non-subscribers
print(prop_data)
prop_test <- prop.test(x = prop_data[,2], n = rowSums(prop_data))
print(prop_test)

# Convert table data into a dataframe
prop_df <- as.data.frame(prop_data)
colnames(prop_df) <- c("Subscription", "Rating", "Count")

prop_df$Rating <- factor(prop_df$Rating, levels = c("0", "1"), labels = c("Low Rating (0)", "High Rating (1)"))

# Plot proportion of high ratings by subscription type
ggplot(prop_df, aes(x = Subscription, y = Count, fill = Rating)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  geom_text(aes(label = percent(Count / tapply(Count, Subscription, sum)[Subscription], accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Low Rating (0)" = "#EE6C4D", "High Rating (1)" = "#3D7A9F"), name = "Rating Category") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Proportion of High vs. Low Ratings by Subscription Type",
       subtitle = "Analysis of feedback categories across customer groups",
       x = "Subscription Type", y = "Proportion of Ratings") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 13, color = "#34495E"),
    axis.text = element_text(size = 11, color = "#7F8C8D"),
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ECF0F1", linetype = "dotted"), panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA), panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Create contingency table for Subscription vs. Feedback categories
feedback_table <- table(dataFinal1$Subscription, dataFinal1$Feedback)

# Run Chi-Square Test
# Determines if subscription status significantly influences feedback type (Bad, Average, Good, Excellent)
chisq_test <- chisq.test(feedback_table)
print(chisq_test)

feedback_df <- feedback_df %>%
  mutate(
    # Create a new column 'Feedback_Category' by remapping the original 'Feedback' values
    Feedback_Category = case_when(
      Feedback == "bad" ~ "Negative",
      Feedback == "average" ~ "Neutral",
      Feedback %in% c("good", "excellent") ~ "Positive", # Group 'good' and 'excellent' into 'Positive'
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    Feedback_Category = factor(Feedback_Category, levels = c("Negative", "Neutral", "Positive"))
  )

# Plot feedback distributions
ggplot(feedback_df, aes(x = Subscription, y = Count, fill = Feedback_Category)) +
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  geom_text(aes(label = percent(Count / tapply(Count, Subscription, sum)[Subscription], accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Negative" = "#EE6C4D", "Neutral" = "#FFD23F", "Positive" = "#3D7A9F")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Feedback Distribution by Subscription Type",
    x = "Subscription Type",
    y = "Proportion of Feedback",
    fill = "Feedback Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.position = "bottom"
  )

# Identify first year of customer engagement
dataFinal1 <- dataFinal1 %>%
  group_by(Customer_ID) %>%
  mutate(First_Year = min(Year)) 

# Compare feedback across years for different cohorts
# Tracks how feedback changes over time for different subscription cohorts
cohort_trends <- dataFinal1 %>%
  group_by(First_Year, Year, Subscription) %>%
  summarize(Avg_Rating = mean(Ratings_Num))
print(cohort_trends)

# Prediction - are subscribed customers more likely to give high ratings through feedback compared to non-subscribed customers in the future
library(smotefamily)
library(xgboost)
library(caret)
# Convert feedback labels to numerical values
dataFinal1$Feedback_Num <- as.numeric(factor(dataFinal1$Feedback, levels = c("bad", "average", "good", "excellent"), labels = c(1, 2, 3, 4)))

# Select relevant features
feature_columns <- c("Subscription", "Income", "Total_Purchases", "Product_Category", "Order_Status", "Ratings_Num", "Year", "Month", "Shipping_Method", "Product_Brand")
dataFinal1_selected <- dataFinal1[, feature_columns]

# Convert categorical variables to numeric (one-hot encoding)
dummy_vars <- dummyVars(~ ., data = dataFinal1_selected, fullRank = TRUE)
dataFinal1_processed <- predict(dummy_vars, newdata = dataFinal1_selected)

colnames(dataFinal1_processed) <- make.names(colnames(dataFinal1_processed), unique = TRUE)

# Divide the dateset into training (70%) and testing (30%)
set.seed(123)
trainIndex <- createDataPartition(dataFinal1$Feedback_Num, p = 0.7, list = FALSE)
trainData <- dataFinal1_processed[trainIndex, ]
testData <- dataFinal1_processed[-trainIndex, ]

trainLabels <- dataFinal1$Feedback_Num[trainIndex]
testLabels <- dataFinal1$Feedback_Num[-trainIndex]

table(trainLabels)
# results:
# 1 - 30389
# 2 - 43846
# 3 - 66594
# 4 - 70572

# Apply oversampling to balance feedback categories
# Convert trainLabels to a factor
trainData <- as.data.frame(trainData)
trainLabels <- as.factor(trainLabels)
trainData$class <- trainLabels

# Apply oversampling
trainData_balanced <- upSample(x = trainData, y = trainData$class)

# Check the dataset after SMOTE - 1, 2, 3, 4 - 70572
table(trainData_balanced$class)

# Ensure all columns are numeric
testData <- as.data.frame(testData)
testData[] <- lapply(testData, as.numeric)

# Ensure trainData is numeric
trainData <- as.data.frame(trainData)
trainData[] <- lapply(trainData, as.numeric)

# Convert into XGBoost format
trainMatrix <- xgb.DMatrix(data = as.matrix(trainData), label = trainLabels)
testMatrix <- xgb.DMatrix(data = as.matrix(testData), label = testLabels)

trainLabels <- trainLabels[1:nrow(trainData)]
testLabels <- testLabels[1:nrow(testData)]

# Define XGBoost parameters
params <- list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eta = 0.005,
  max_depth = 10,
  subsample = 0.85,
  colsample_bytree = 0.8,
  gamma = 2,
  min_child_weight = 5
)

# Train XGBoost model
xgb_model_optimized <- xgb.train(params = params, data = trainMatrix, nrounds = 1000)

# Predict feedback ratings on test data
predictions_optimized <- predict(xgb_model_optimized, testMatrix)

# Evaluate model performance
library(Metrics)
mae_score <- mae(testLabels, predictions_optimized)
rmse_score <- rmse(testLabels, predictions_optimized)

print(paste("Mean Absolute Error (MAE):", round(mae_score, 2)))
print(paste("Root Mean Squared Error (RMSE):", round(rmse_score, 2)))

# Print first few predicted values vs actual ratings
results_df <- data.frame(Actual = testLabels, Predicted = round(predictions_optimized, 2))
head(results_df)

# Count correctly predicted ratings - 62.11%
correct_predictions <- sum(round(predictions_optimized) == testLabels)
accuracy_rate <- correct_predictions / length(testLabels)
print(paste("Prediction Accuracy:", round(accuracy_rate * 100, 2), "%"))

# Define month labels
month_labels <- c("January", "February", "March", "April", "May", "June", 
                  "July", "August", "September", "October", "November", "December"
                  )

library(caret)
# Convert predicted values to nearest integer ratings
rounded_predictions <- round(predictions_optimized)

# Generate confusion matrix
conf_matrix <- confusionMatrix(factor(rounded_predictions), factor(testLabels))
print(conf_matrix)

sensitivity(conf_matrix$table, positive = "4")

conf_matrix_subscribed <- confusionMatrix(factor(rounded_predictions[results_df$Subscription_Status == "Subscribed"]), factor(testLabels[results_df$Subscription_Status == "Subscribed"]))
conf_matrix_nonsubscribed <- confusionMatrix(factor(rounded_predictions[results_df$Subscription_Status == "Non-Subscribed"]), factor(testLabels[results_df$Subscription_Status == "Non-Subscribed"]))

sensitivity_subscribed <- sensitivity(conf_matrix_subscribed$table, positive = "4")
sensitivity_nonsubscribed <- sensitivity(conf_matrix_nonsubscribed$table, positive = "4")

print(paste("Sensitivity (Subscribed):", round(sensitivity_subscribed, 2)))
print(paste("Sensitivity (Non-Subscribed):", round(sensitivity_nonsubscribed, 2)))

# Ensure testIndex is defined
testIndex <- setdiff(1:nrow(dataFinal1), trainIndex)

results_df <- data.frame(
  Actual = testLabels,
  Predicted = round(predictions_optimized, 2),
  Month = dataFinal1$Month[testIndex],
  Subscription_Status = dataFinal1$Subscription[testIndex]
)

results_df <- results_df %>%
  mutate(
    Month = factor(tolower(Month), levels = tolower(month_labels), labels = month_labels),
    Subscription_Status = factor(
      tolower(Subscription_Status),
      levels = c("new", "regular", "premium"),
      labels = c("Non-Subscribed", "Subscribed", "Subscribed")
    )
  )

results_df <- results_df %>%
  mutate(Customer_Type = Subscription_Status)

# Plot for visualizing comparison of actual vs. predicted feedback ratings in the future
ggplot(results_df, aes(x = Month, group = Customer_Type)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2, alpha = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted", linetype = "Predicted"), size = 1.2) +
  geom_ribbon(aes(ymin = pmin(Actual, Predicted), ymax = pmax(Actual, Predicted)),
              fill = "red", alpha = 0.2) +
  scale_color_manual(values = c("Actual" = "steelblue", "Predicted" = "darkorange")) +
  scale_linetype_manual(values = c("Predicted" = "dashed")) +
  facet_wrap(~ Customer_Type, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Comparison: Actual vs Predicted Ratings",
    subtitle = "Differences between Actual and Predicted Ratings for Customer Types",
    x = "Month",
    y = "Feedback Ratings",
    color = "Rating Type",
    linetype = "Line Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

# analysis 3 - How does subscribed status influence ratings when combined with factors like shipping methods, payment methods and order status?
# Check basic statistics for each factor
summary(dataFinal1[, c("Subscription", "Shipping_Method", "Payment_Method", "Order_Status", "Ratings_Num")])

# Plot for visualizing trends between Subscription and Ratings
ggplot(dataFinal1, aes(x = Ratings_Num, fill = factor(Subscription))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Subscription) +
  theme_minimal() +
  labs(title = "Ratings Distribution by Subscription Type",
       x = "Ratings",
       y = "Density",
       fill = "Subscription Type")

# Plot for explore payment method influence on Ratings
ggplot(dataFinal1, aes(x = Ratings_Num, y = Payment_Method, fill = Payment_Method)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01, alpha = 0.8, color = "white", linewidth = 0.5) +
  stat_density_ridges(
    quantile_lines = TRUE,
    quantiles = 2,
    color = "black",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  scale_fill_manual(values = c("#3D7A9F", "#EE6C4D", "#2ECC71", "#FFD23F", "#8D99AE", "#C0B283")) +
  scale_x_continuous(breaks = 1:5, limits = c(0.8, 5.2), expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "Customer Rating Distribution by Payment Method",
       subtitle = "Higher density indicates more common ratings; black lines show medians",
       x = "Customer Rating (1-5)", y = "Payment Method") +
  theme_ridges(grid = FALSE) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 11, color = "#7F8C8D"),
    axis.text.y = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "#ECF0F1", linetype = "dotted"), panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA), panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.y = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Plot for Shipping Method and Order Status analysis
dataFinal1$Shipping_Method <- factor(dataFinal1$Shipping_Method, 
                                     labels = c("Express", "Same-Day", "Standard"))

library(RColorBrewer)
ggplot(dataFinal1, aes(x = Shipping_Method, fill = Subscription)) +
  geom_bar(position = "dodge", width = 0.65) +
  facet_wrap(~ Order_Status) +
  scale_fill_brewer(palette = "Paired") +
  geom_text(stat = "count", aes(y = after_stat(count), label = after_stat(count)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 14, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "top",
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  labs(title = "Shipping Preferences & Order Status by Subscription",
       x = "Shipping Method",
       y = "Customer Count",
       fill = "Subscription Type")

# To determine which factors most influence feedback ratings
library(caret)
dummy_vars <- dummyVars(~ Subscription + Shipping_Method + Payment_Method + Order_Status, data = dataFinal1, fullRank = TRUE)
data_encoded <- predict(dummy_vars, newdata = dataFinal1)

# Select only numeric features
num_features <- data.frame(data_encoded, Ratings_Num = dataFinal1$Ratings_Num)

# Compute correlation
cor_matrix <- cor(num_features)
print(cor_matrix)

library(randomForest)
# Prepare dataset
set.seed(123)
sample_index <- sample(1:nrow(num_features), 50000)
num_features_sampled <- num_features[sample_index, ]

# Convert Ratings_Num to a factor for classification
num_features_sampled$Ratings_Num <- factor(num_features_sampled$Ratings_Num)

rf_model <- randomForest(Ratings_Num ~ ., data = num_features_sampled, importance = TRUE, ntree = 200)

importance_values <- importance(rf_model)
print(importance_values)

# Remove low-impact features - Shipping_Methodsame.day, Shipping_Methodstandard
selected_features <- num_features_sampled %>%
  select(-Shipping_Methodsame.day, -Shipping_Methodstandard)

# Re-train model on selected features
# Balance the data
library(ROSE)
# Apply ROSE to balance ratings
balanced_features <- ROSE(Ratings_Num ~ ., data = selected_features)$data

table(balanced_features$Ratings_Num)  # Compare before vs. after balancing

# Train model
rf_model_balanced <- randomForest(Ratings_Num ~ ., data = balanced_features, importance = TRUE, ntree = 500, mtry = 3)

# Evaluate model confusion
print(rf_model_balanced$confusion)

# Extract confusion matrix from Random Forest model
rf_conf_matrix <- rf_model_balanced$confusion

# Calculate accuracy - 0.5657
accuracy_rf <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix)
print(paste("Accuracy:", round(accuracy_rf, 4)))

# Calculate sensitivity (True Positive Rate for Rating '1') - 0.564
sensitivity_rf <- rf_conf_matrix[2, 2] / (rf_conf_matrix[2, 2] + rf_conf_matrix[1, 2])
print(paste("Sensitivity:", round(sensitivity_rf, 4)))

# Calculate specificity (True Negative Rate for Rating '0') - 0.5675
specificity_rf <- rf_conf_matrix[1, 1] / (rf_conf_matrix[1, 1] + rf_conf_matrix[2, 1])
print(paste("Specificity:", round(specificity_rf, 4)))

library(xgboost)
xgb_data <- as.matrix(balanced_features[, -which(names(balanced_features) == "Ratings_Num")])

# Shift labels to start at 0 (convert 1 -> 0 and 5 -> 1)
balanced_features <- balanced_features %>% filter(Ratings_Num %in% c(1, 5))

# Recreate xgb_data **after filtering**
xgb_data <- as.matrix(balanced_features[, -which(names(balanced_features) == "Ratings_Num")])

# Convert labels properly
xgb_label <- as.numeric(balanced_features$Ratings_Num)

# Convert ratings 1 → 0 and 5 → 1 (and fix any errors)
xgb_label[xgb_label == 1] <- 0
xgb_label[xgb_label == 5] <- 1

# Fix unexpected `2` values
xgb_label[xgb_label == 2] <- 1  # Ensure no incorrect labels

print(num_classes)  # Should be 2 (values 0 and 1)
table(xgb_label)  # Should display only {0, 1}

# Train the model
xgb_model_weighted <- xgboost(data = xgb_data, label = xgb_label, nrounds = 500, scale_pos_weight = 1.2, objective = "multi:softmax", num_class = num_classes)

# Used xgb_model_weighted
preds <- predict(xgb_model_weighted, xgb_data)
table(preds, xgb_label)  # Confusion matrix to check classification performance

importance_matrix <- xgb.importance(model = xgb_model_weighted)
print(importance_matrix)

# Confusion Matrix - 0.89928
conf_matrix <- table(preds, xgb_label)
accuracy_xgb <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy_xgb)

# Measures how well the model correctly predicts high ratings (1) - 0.8780273
sensitivity_xgb <- conf_matrix[2,2] / (conf_matrix[2,2] + conf_matrix[1,2])
print(sensitivity_xgb)  # Sensitivity score for rating '1'

# Measures how well the model correctly predicts low ratings (0) - 0.910588
specificity_xgb <- conf_matrix[1,1] / (conf_matrix[1,1] + conf_matrix[2,1])
print(specificity_xgb)  # Specificity score for rating '0'

# Plots
# Ensure Metric is a factor for proper ordering.
model_comparison_long$Metric <- factor(model_comparison_long$Metric, levels = c("Accuracy",                                            "Sensitivity", "Specificity"))

# Plot for visualizing the comparison between Random Forest vs. XGBoost
library(scales)
ggplot(model_comparison_long, aes(x = Metric, y = Score, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = percent(Score, accuracy = 0.1)),
            position = position_dodge(width = 0.7),
            vjust = -0.25,
            size = 3,
            fontface = "bold",
            color = "#333333") +
  scale_fill_manual(values = c("Random_Forest" = "#EE6C4D", "XGBoost" = "#3D7A9F"), name = "Model") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Model Performance Comparison between Random Foreset and XGB",
    subtitle = "Accuracy, Sensitivity, and Specificity",
    x = "Metric",
    y = "Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Extract feature importance from XGBoost
importance_xgb <- xgb.importance(feature_names = colnames(selected_features[-which(names(selected_features) == "Ratings_Num")]), model = xgb_model)

# Convert to a data frame for plotting
importance_df <- importance_xgb[, c("Feature", "Gain")]
importance_df <- importance_df[order(-importance_df$Gain), ]

# Plot for visualizing the Feature Importance in Predicting Customer Ratings
ggplot(importance_df, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", aes(fill = Gain), width = 0.7) +
  geom_text(aes(label = round(Gain, 3)),
            hjust = -0.1,
            size = 3.5,
            color = "#555555",
            fontface = "bold") +
  coord_flip() +
  scale_fill_gradient(low = "#A2D2FF", high = "#007BFF", name = "Importance Score") +
  labs(
    title = "Key Factors Influencing Customer Ratings (XGBoost)",
    subtitle = "Features ranked by their contribution to the model's predictive power (Gain)",
    x = "Feature",
    y = "Importance (Gain Score)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 11, color = "#7F8C8D"),
    axis.text.y = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ECF0F1", linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA),
    panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# analysis 4 - Do subscribed customers give a higher percentage of extreme ratings (1 vs. 5) compared to non-subscribed customers?
# Compute proportion of 1-star and 5-star ratings separately
rating_breakdown <- selected_features %>%
  filter(Ratings_Num %in% c(1, 5)) %>%
  mutate(Rating_Type = ifelse(Ratings_Num == 1, "1-Star", "5-Star")) %>%
  group_by(Subscription_Group, Rating_Type) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(rating_breakdown)

rating_breakdown$Subscription_Group <- factor(rating_breakdown$Subscription_Group,
                                              levels = c("Non-Subscribed", "Subscribed"))

# Plot for visualizing the breakdown of 1-star and 5-star by subscription status
ggplot(rating_breakdown, aes(x = Subscription_Group, y = Percentage, fill = Rating_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 3.5,
            color = "#333333",
            fontface = "bold") +
  scale_fill_manual(
    values = c("1-Star" = "#EE6C4D", "5-Star" = "#3D7A9F"),
    name = "Rating Category"
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Extreme Ratings by Customer Subscription Status",
    subtitle = "Percentage of 1-Star and 5-Star Ratings Among Extreme Reviews",
    x = "Customer Group",
    y = "Percentage of Ratings"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 13, color = "#34495E"),
    axis.text = element_text(size = 11, color = "#7F8C8D"),
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ECF0F1", linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA),
    panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Analyze Product Brands that receive the most extreme ratings
# Compute percentage of extreme ratings (1 & 5) per brand
brand_extreme_summary <- dataFinal1 %>%
  filter(Ratings_Num %in% c(1, 5)) %>%
  group_by(Product_Brand, Ratings_Num) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(brand_extreme_summary)

# Chi-test
brand_table <- table(dataFinal1$Product_Brand, dataFinal1$Ratings_Num)
chi_test_brand <- chisq.test(brand_table)
print(chi_test_brand)

# Show residuals to verify accuracy
print(chi_test_brand$residuals)

subscription_brand_comparison <- dataFinal1 %>%
  filter(Ratings_Num %in% c(1, 5)) %>%
  group_by(Subscription, Product_Brand, Ratings_Num) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(subscription_brand_comparison)

brand_sentiment_data <- brand_extreme_summary %>%
  ungroup() %>%
  pivot_wider(names_from = Ratings_Num, values_from = Count, values_fill = 0) %>%
  rename(`_1_star_count` = `1`, `_5_star_count` = `5`) %>%
  mutate(
    Net_Sentiment = `_5_star_count` - `_1_star_count`,
    `_1_star_plot` = -`_1_star_count`
  ) %>%
  arrange(Net_Sentiment) %>%
  mutate(
    Product_Brand = factor(Product_Brand, levels = unique(Product_Brand))
  ) %>%
  pivot_longer(cols = c(`_1_star_plot`, `_5_star_count`), names_to = "Rating_Type_Plot", values_to = "Plot_Value")

# Plot for visualizing the which product brands received the most extreme ratings
ggplot(brand_sentiment_data, aes(x = Product_Brand, y = Plot_Value, fill = Rating_Type_Plot)) +
  geom_bar(stat = "identity", position = "identity", width = 0.8) +
  geom_text(data = filter(brand_sentiment_data, Rating_Type_Plot == "_1_star_plot"),
            aes(label = comma(abs(Plot_Value))),
            hjust = 1.2,
            size = 4,
            color = "black",
            fontface = "bold") +
  geom_text(data = filter(brand_sentiment_data, Rating_Type_Plot == "_5_star_count"),
            aes(label = comma(abs(Plot_Value))),
            hjust = -0.2,
            size = 4,
            color = "black",
            fontface = "bold") +
  scale_fill_manual(
    values = c("_1_star_plot" = "#C62828", "_5_star_count" = "#2E7D32"),
    labels = c("1-Star Ratings", "5-Star Ratings")
  ) +
  scale_y_continuous(
    labels = abs,
    expand = expansion(mult = c(0.2, 0.2))
  ) +
  coord_flip() +
  labs(
    title = "Product Brands that Received the Most Extreme Ratings",
    subtitle = "Number of 1-Star (Negative) vs. 5-Star (Positive) Reviews",
    x = NULL,
    y = "Number of Ratings",
    fill = "Rating Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "#2C3E50", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title.x = element_text(size = 14, color = "#34495E", margin = margin(t = 15)),
    axis.text.x = element_text(size = 12, color = "#7F8C8D"),
    axis.text.y = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.title = element_text(size = 13, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 12, color = "#7F8C8D"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.key.size = unit(0.8, "cm"),
    panel.grid.major.x = element_line(color = "#ECF0F1", linetype = "solid", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA),
    panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )
--------------------------------------------------------------------------------------------------------------------------------------------
# Irfan bin Ismail - TP070616
# age, gender, etc
# To study demographic factors like age and gender to identify trends in satisfaction

# 1. How do ratings vary across different age groups, and are certain age groups more likely to give higher ratings?
# Create age groups
dataFinal1 <- dataFinal1 %>%
  mutate(
    Age_Group = cut(Age,
                    breaks = c(18, 25, 35, 45, 55, 65, Inf), # Define age boundaries
                    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                    right = FALSE, # [18, 25) means 18 to 24.999...
                    include.lowest = TRUE) # Includes the lowest boundary (18)
  )

dataFinal1$Age_Group <- factor(dataFinal1$Age_Group,
                               levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))

# Plot for the summary of age and rating
library(Hmisc)
ggplot(dataFinal1, aes(x = Age_Group, y = Ratings_Num)) +
  geom_jitter(aes(color = Age_Group), width = 0.2, height = 0, alpha = 0.5, size = 2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1, color = "black", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "black", color = "black") +
  scale_color_manual(values = c("#3D7A9F", "#2ECC71", "#FFD23F", "#A2D2FF", "#EE6C4D", "#C0B283")) +
  labs(title = "Customer Ratings Across Age Groups",
       subtitle = "Individual Points & Means with 95% Confidence Intervals",
       x = "Age Group", y = "Customer Rating (1-5)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Kruskal-Wallis Test to compare age groups
kruskal.test(Ratings_Num ~ Age, data = dataFinal1)

# To determine which specific age groups differ significantly in their rating behavior
library(FSA)

# Post-hoc Dunn's test to compare age groups
dunnTest(Ratings_Num ~ Age, data = dataFinal1, method = "bh")

# Linear Regression to predict rating by age
model <- lm(Ratings_Num ~ Age, data = dataFinal1)
summary(model)  # Check coefficients & significance

# Chi-Square Test
table_data <- table(dataFinal1$Age, dataFinal1$High_Rating)
chisq.test(table_data)

# 2. Do ratings differ significantly between male and female customers, and which gender provides higher satisfaction ratings on average?
# Ensure 'Gender' is a factor.
dataFinal1$Gender <- as.factor(dataFinal1$Gender)

# Calculate the proportion of each rating for each gender
rating_proportions <- dataFinal1 %>%
  group_by(Gender, Ratings_Num) %>%
  summarize(Count = n(), .groups = 'drop_last') %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

# Plot for visualizing ratings by gender
library(scales)
ggplot(rating_proportions, aes(x = as.factor(Ratings_Num), y = Proportion, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = percent(Proportion, accuracy = 0.1)),
            position = position_dodge(width = 0.7),
            vjust = -0.25, size = 3, fontface = "bold", color = "#333333") +
  scale_fill_manual(values = c("female" = "pink", "male" = "blue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Distribution of Customer Ratings by Gender",
    subtitle = "Proportion of each rating (1-5) for Female and Male",
    x = "Rating (1-5)",
    y = "Proportion of Customers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )

# To predict which gender might give higher ratings in the future
dataFinal1 <- dataFinal1 %>%
  mutate(Rating_Class = ifelse(Ratings_Num == 5, "High", "Low")) %>%
  mutate(Rating_Class = as.factor(Rating_Class))

# Select relevant features
data_ml <- dataFinal1 %>%
  select(Gender, Age, Subscription, Income, Ratings_Num)

# Convert categorical variables to factors for Random Forest
data_ml$Gender <- as.factor(data_ml$Gender)
data_ml$Subscription <- as.factor(data_ml$Subscription)
data_ml$Income <- as.factor(data_ml$Income)

set.seed(42)  # Set seed for reproducibility
train_index <- sample(1:nrow(data_ml), 0.8 * nrow(data_ml))  # 80% train, 20% test
train_data <- data_ml[train_index, ]
test_data <- data_ml[-train_index, ]

library(randomForest)
# Train the model
rf_model <- randomForest(Ratings_Num ~ Gender + Age + Subscription + Income,
                         data = train_data, ntree = 300, importance = TRUE)
print(rf_model)

tuned_rf <- tuneRF(train_data[, -which(names(train_data) == "Rating_Class")], 
                   train_data$Rating_Class, 
                   ntreeTry = 500, 
                   stepFactor = 1.5, 
                   improve = 0.01)

# Make predictions
predicted_ratings <- predict(rf_model, test_data)

# Evaluate prediction accuracy
cor(test_data$Ratings_Num, predicted_ratings)  # Check correlation between actual vs. predicted ratings

library(caret)
# Convert predicted labels to match test data factor levels
predicted_class <- factor(predicted_class, levels = levels(test_data$Rating_Class))

# Compute confusion matrix without errors
conf_matrix <- confusionMatrix(predicted_class, test_data$Rating_Class)

# Print Accuracy, Sensitivity, Specificity
print(conf_matrix$overall["Accuracy"])
print(conf_matrix$byClass["Sensitivity"])
print(conf_matrix$byClass["Specificity"])

# 3. How do demographic factors like age and gender correlate with purchasing behavior (e.g., Total_Purchases, Total_Amount) and subsequent ratings?
--------------------------------------------------------------------------------------------------------------------------------------------
#Which product types receive the highest ratings, and how do specific brands within these product types influence satisfaction?
# Step 1: Data Preparation
data_clean <- dataFinal1 %>%
  filter(!is.na(Product_Type), !is.na(Ratings), !is.na(Product_Brand))

# Step 2: Explore Ratings by Product_Type and Product_Brand
ggplot(data_clean, aes(x = Product_Type, fill = Ratings)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Product_Brand) +
  labs(title = "Ratings by Product Type and Product Brand",
       x = "Product Type",
       y = "Proportion of Ratings") +
  theme_minimal()

# Step 3: Statistical Analysis
# Perform chi-square test for Ratings vs Product_Type
chi_product_ratings <- chisq.test(table(data_clean$Product_Type, data_clean$Ratings))
chi_product_ratings

# Perform chi-square test for Ratings vs Product_Brand
chi_brand_ratings <- chisq.test(table(data_clean$Product_Brand, data_clean$Ratings))
chi_brand_ratings

# extra analysis
# Do customers buy certain product types and brands more frequently, and does purchase frequency correlate with satisfaction?
# Step 1: Data Preparation
data_clean <- dataFinal1 %>%
  filter(!is.na(Product_Type), !is.na(Product_Brand), !is.na(Total_Purchases), !is.na(Ratings))

# Step 2: Explore Purchase Frequency by Product_Type and Product_Brand
ggplot(data_clean, aes(x = Product_Type, y = Total_Purchases, fill = Product_Brand)) +
  geom_boxplot() +
  labs(title = "Purchase Frequency by Product Type and Brand",
       x = "Product Type",
       y = "Total Purchases") +
  theme_minimal()

# Step 3: Statistical Analysis
# ANOVA test for Total_Purchases vs Product_Type
anova_product_purchases <- aov(Total_Purchases ~ Product_Type, data = data_clean)
summary(anova_product_purchases)

# ANOVA test for Total_Purchases vs Product_Brand
anova_brand_purchases <- aov(Total_Purchases ~ Product_Brand, data = data_clean)
summary(anova_brand_purchases)
 
# Can we predict whether a customer will rate a product highly based on its type, brand, and price?
library(caret)
library(randomForest)

# Step 1: Data Preparation
data_clean <- dataFinal1 %>%
  filter(!is.na(Product_Type), !is.na(Product_Brand), !is.na(Total_Amount), !is.na(Ratings))

# Convert Ratings to binary classification: High vs. Low
data_clean$Ratings <- factor(data_clean$Ratings, levels = c("low", "high"))

# Step 2: Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data_clean$Ratings, p = 0.8, list = FALSE)
train_data <- data_clean[trainIndex, ]
test_data <- data_clean[-trainIndex, ]

# Step 3: Train Random Forest model
rf_model <- randomForest(Ratings ~ Product_Type + Product_Brand + Total_Amount, 
                         data = train_data, 
                         importance = TRUE)

# Step 4: Predict Satisfaction Ratings
predictions <- predict(rf_model, test_data)

# Evaluate the model with a confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$Ratings)
print(confusion_matrix)

# Step 5: Feature Importance Plot
varImpPlot(rf_model)

# Do subscribed customers spend more and rate products higher compared to non-subscribers?
# Step 1: Data Preparation
data_clean <- dataFinal1 %>%
  filter(!is.na(Subscription), !is.na(Ratings), !is.na(Total_Amount))

# Convert Subscription into Yes/No for easier analysis
data_clean <- data_clean %>%
  mutate(Subscription_Group = case_when(
    Subscription %in% c("premium", "regular") ~ "Subscribed",
    Subscription == "new" ~ "Not Subscribed",
    
  )) %>%
  filter(!is.na(Subscription_Group))  # Remove rows with NA values

# Step 2: Compare Spending Across Subscription Groups
ggplot(data_clean, aes(x = Subscription_Group, y = Total_Amount, fill = Subscription_Group)) +
  geom_boxplot() +
  labs(title = "Spending Patterns by Subscription Status",
       x = "Subscription Status",
       y = "Total Amount") +
  theme_minimal()

# Step 3: Compare Satisfaction Across Subscription Groups
ggplot(data_clean, aes(x = Subscription_Group, fill = Ratings)) +
  geom_bar(position = "fill") +
  labs(title = "Ratings Distribution by Subscription Status",
       x = "Subscription Status",
       y = "Proportion of Ratings") +
  theme_minimal() 

# Step 4: Statistical Analysis
# T-test for Total Amount vs Subscription
t_test_subscription_spending <- t.test(Total_Amount ~ Subscription_Group, data = data_clean)
t_test_subscription_spending

# Chi-square test for Ratings vs Subscription
chi_subscription_ratings <- chisq.test(table(data_clean$Subscription_Group, data_clean$Ratings))
chi_subscription_ratings

------------------------------------------------------------------------------------------------
# Does having a subscription lead to higher ratings, and do subscribers tend to spend more compared to non-subscribers?
# Step 1: Data Preparation
data_clean <- dataFinal1 %>%
  filter(!is.na(Subscription), !is.na(Ratings), !is.na(Total_Amount))

# Step 2: Explore Ratings by Subscription
ggplot(data_clean, aes(x = Subscription, fill = Ratings)) +
  geom_bar(position = "fill") +
  labs(title = "Ratings by Subscription Status",
       x = "Subscription Status",
       y = "Proportion of Ratings") +
  theme_minimal()

# Step 3: Statistical Analysis
# Perform chi-square test for Ratings vs Subscription
chi_subscription_ratings <- chisq.test(table(data_clean$Subscription, data_clean$Ratings))
chi_subscription_ratings

# Compare Total_Amount by Subscription
ggplot(data_clean, aes(x = Subscription, y = Total_Amount, fill = Subscription)) +
  geom_boxplot() +
  labs(title = "Spending by Subscription Status",
       x = "Subscription Status",
       y = "Total Amount") +
  theme_minimal()

# Perform t-test for Total_Amount vs Subscription
table(data_clean$Subscription_Simplified)

# Simplify Subscription to two levels
data_clean <- dataFinal1 %>%
  mutate(Subscription_Simplified = case_when(
    tolower(trimws(Subscription)) %in% c("premium", "regular") ~ "Yes",  # Handle case and whitespace issues
    tolower(trimws(Subscription)) == "new" ~ "No",
    TRUE ~ NA_character_  # Mark all other cases as NA
  )) %>%
  filter(!is.na(Subscription_Simplified))  # Remove rows with NA values

t_test_amount_subscription <- t.test(Total_Amount ~ Subscription_Simplified, data = data_clean)
summary(t_test_amount_subscription)

#extra analysis
# Step 1: Data Preparation
data_clean <- dataFinal1 %>%
  filter(!is.na(Product_Type), !is.na(Ratings), !is.na(Product_Brand))

# Convert Ratings to binary sentiment: Positive (High) vs. Negative (Low)
data_clean <- data_clean %>%
  mutate(Sentiment = ifelse(Ratings == "high", "positive", "negative"))

# Step 2: Explore Sentiment Distribution by Product_Type and Product_Brand (change to radar plot)
ggplot(data_clean, aes(x = Product_Type, fill = Sentiment)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Product_Brand) +
  labs(title = "Customer Sentiment by Product Type and Brand",
       x = "Product Type",
       y = "Proportion of Sentiment") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90))

# Step 3: Statistical Analysis
# Perform chi-square test for Sentiment vs Product_Type
chi_product_sentiment <- chisq.test(table(data_clean$Product_Type, data_clean$Sentiment))
chi_product_sentiment

# Perform chi-square test for Sentiment vs Product_Brand
chi_brand_sentiment <- chisq.test(table(data_clean$Product_Brand, data_clean$Sentiment))
chi_brand_sentiment


----------------------------------------------------------------------------------------------------------------
# shopping frequency, future total purchase amount
# To evaluate the relationship between shopping frequency and future total purchase amount
# How does shopping frequency correlate with the total purchase amount in the future?
# Load Necessary Libraries
library(dplyr)
library(caret)

# Step 1: Data Preparation
# Ensure your dataset is loaded correctly
dataFinal1 <- read.csv("final_data_with_imputed_Customer_ID.csv")  # Replace with your actual dataset file
head(dataFinal1)

# Filter rows with missing values
data_clean <- dataFinal1 %>%
  filter(!is.na(Total_Purchases), !is.na(Total_Amount))  # Add any other relevant variables here

set.seed(123)  # For reproducibility

# Split data (e.g., 80% training, 20% testing)
trainIndex <- createDataPartition(data_clean$Total_Amount, p = 0.8, list = FALSE)
train_data <- data_clean[trainIndex, ]
test_data <- data_clean[-trainIndex, ]

# Fit a linear regression model
linear_model <- lm(Total_Amount ~ Total_Purchases, data = train_data)

# View model summary
summary(linear_model)

# Predict future total purchase amount
test_data$predicted_amount <- predict(linear_model, newdata = test_data)

# Compare actual vs predicted values
head(test_data[, c("Total_Purchases", "Total_Amount", "predicted_amount")])

# Calculate evaluation metrics
mae <- mean(abs(test_data$Total_Amount - test_data$predicted_amount))
mse <- mean((test_data$Total_Amount - test_data$predicted_amount)^2)
rmse <- sqrt(mse)

# Print metrics
print(paste("MAE:", mae))
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))

library(ggplot2)

# Scatter plot: Actual vs Predicted
ggplot(test_data, aes(x = Total_Amount, y = predicted_amount)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Perfect prediction line
  labs(title = "Actual vs Predicted Total Purchase Amount",
       x = "Actual Total Amount",
       y = "Predicted Total Amount") +
  theme_minimal()


# Can future total purchase amounts be forecasted based on shopping frequency trends?
install.packages("forecast")
# Example aggregation: Monthly data
data_time_series <- dataFinal1 %>%
  group_by(Year, Month) %>%
  summarize(
    shopping_frequency = sum(Total_Purchases),
    future_total_amount = sum(Total_Amount)
  )

library(forecast)

# Convert to time series
shopping_ts <- ts(data_time_series$shopping_frequency, start = c(2023, 1), frequency = 12)  # Adjust year and frequency
amount_ts <- ts(data_time_series$future_total_amount, start = c(2023, 1), frequency = 12)

# Fit an ARIMA model (for time-series forecasting)
model <- auto.arima(amount_ts, xreg = shopping_ts)

# Forecast future amounts based on shopping frequency
future_shopping <- forecast(model, xreg = c(30, 35, 40))  # Replace with projected shopping frequencies
plot(future_shopping)

# Linear regression to predict future total purchase amounts
linear_model <- lm(Total_Amount ~ Total_Purchases, data = dataFinal1)

# Summary of the model
summary(linear_model)

# Predict future purchase amounts based on shopping frequency
dataFinal1$predicted_purchase <- predict(linear_model, newdata = dataFinal1)

# Visualize actual vs predicted values
library(ggplot2)
ggplot(dataFinal1, aes(x = Total_Purchases, y = Total_Amount)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_purchase), color = "red") +
  labs(title = "Future Total Purchase Amount Forecast",
       x = "Shopping Frequency (Total Purchases)",
       y = "Total Purchase Amount") +
  theme_minimal()

# Calculate MAE, MSE, and RMSE
mae <- mean(abs(dataFinal1$Total_Amount - dataFinal1$predicted_purchase))
mse <- mean((dataFinal1$Total_Amount - dataFinal1$predicted_purchase)^2)
rmse <- sqrt(mse)

print(paste("MAE:", mae))
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))


# Does shopping frequency vary among customer groups with different purchase behaviors over time?
library(dplyr)

# Categorize customers into spending tiers
dataFinal1 <- dataFinal1 %>%
  mutate(
    spending_tier = case_when(
      Total_Amount <= 300 ~ "Low Spenders",
      Total_Amount > 300 & Total_Amount <= 1000 ~ "Medium Spenders",
      Total_Amount > 1000 ~ "High Spenders"
    )
  )

# Group by Year, Month, and Spending Tier
data_aggregated <- dataFinal1 %>%
  group_by(Year, Month, spending_tier) %>%
  summarize(
    avg_shopping_frequency = mean(Total_Purchases, na.rm = TRUE),
    .groups = 'drop'  # Ungroup after summarizing
  )

library(ggplot2)

ggplot(data_aggregated, aes(x = interaction(Year, Month, sep = "-"), 
                            y = avg_shopping_frequency, 
                            color = spending_tier, 
                            group = spending_tier)) +
  geom_line(size = 1) +
  labs(
    title = "Shopping Frequency Trends Across Spending Tiers",
    x = "Time (Year-Month)",
    y = "Average Shopping Frequency",
    color = "Spending Tier"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ANOVA test
anova_result <- aov(Total_Purchases ~ spending_tier, data = dataFinal1)
summary(anova_result)

# Create time variable
dataFinal1 <- dataFinal1 %>%
  mutate(time = interaction(Year, Month, sep = "-"))

# Two-way ANOVA (spending_tier * time)
anova_time <- aov(Total_Purchases ~ spending_tier * time, data = dataFinal1)
summary(anova_time)

# How does shopping frequency predict the likelihood of a decline in future purchases (churn)?
# Define churn based on future purchase thresholds

# Step 1: Define the threshold variable
threshold <- median(dataFinal1$Total_Amount, na.rm = TRUE)  # Example: Use the median of Total_Amount

# Step 2: Create a binary spending category based on the threshold
dataFinal1 <- dataFinal1 %>%
  mutate(Spending_Category = ifelse(Total_Amount > threshold, "Above Threshold", "Below Threshold"))

dataFinal1 <- dataFinal1 %>%
  mutate(
    churn = ifelse(Total_Amount < threshold, 1, 0)  # Replace threshold_amount with a realistic value
  )

library(dplyr)

# Filter necessary columns and remove missing values
data_clean <- dataFinal1 %>%
  filter(!is.na(Total_Purchases), !is.na(churn))

# Logistic regression to predict churn
log_model <- glm(churn ~ Total_Purchases, data = data_clean, family = binomial)

# View model summary
summary(log_model)

library(caret)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data_clean$churn, p = 0.8, list = FALSE)
train_data <- data_clean[trainIndex, ]
test_data <- data_clean[-trainIndex, ]

# Fit logistic regression model on the training set
log_model_train <- glm(churn ~ Total_Purchases, data = train_data, family = binomial)

# Predict churn probabilities on the test set
log_predictions <- predict(log_model_train, newdata = test_data, type = "response")

# Convert probabilities to binary predictions (threshold = 0.5)
log_predictions_binary <- ifelse(log_predictions > 0.5, 1, 0)

# Confusion Matrix
confusion_matrix <- table(test_data$churn, log_predictions_binary)
print(confusion_matrix)

# Calculate accuracy, precision, recall, etc.
confusion_metrics <- confusionMatrix(as.factor(log_predictions_binary), as.factor(test_data$churn))
print(confusion_metrics)

library(ggplot2)

ggplot(data_clean, aes(x = Total_Purchases, y = churn)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
  labs(title = "Shopping Frequency vs Churn Likelihood",
       x = "Shopping Frequency (Total Purchases)",
       y = "Churn Probability") +
  theme_minimal()

ggplot(test_data, aes(x = churn, y = log_predictions_binary)) +
  geom_jitter(alpha = 0.6, color = "green") +
  labs(title = "Actual vs Predicted Churn Outcomes",
       x = "Actual Churn",
       y = "Predicted Churn") +
  theme_minimal()


# extra - Churn analysis – investigate how shopping frequency predicts a decline in future purchases.
# Define churn threshold (e.g., customers with future purchases below a threshold are churned)
threshold_amount <- 300  # Example: Churn threshold

dataFinal1 <- dataFinal1 %>%
  mutate(
    churn = ifelse(Total_Amount < threshold_amount, 1, 0)  # 1 = Churned, 0 = Not Churned
  )

# Average shopping frequency for churned and non-churned customers
dataFinal1 %>%
  group_by(churn) %>%
  summarize(
    avg_shopping_frequency = mean(Total_Purchases, na.rm = TRUE),
    count = n()
  )

library(ggplot2)

ggplot(dataFinal1, aes(x = factor(churn), y = Total_Purchases, fill = factor(churn))) +
  geom_boxplot() +
  labs(title = "Shopping Frequency by Churn Status",
       x = "Churn Status (1 = Churned, 0 = Not Churned)",
       y = "Shopping Frequency") +
  theme_minimal()

# Logistic regression model
log_model <- glm(churn ~ Total_Purchases, data = dataFinal1, family = binomial)

# View the model summary
summary(log_model)

library(caret)

# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(dataFinal1$churn, p = 0.8, list = FALSE)
train_data <- dataFinal1[trainIndex, ]
test_data <- dataFinal1[-trainIndex, ]

# Train logistic regression on training data
log_model_train <- glm(churn ~ Total_Purchases, data = train_data, family = binomial)

# Predict probabilities on test data
log_predictions <- predict(log_model_train, newdata = test_data, type = "response")

# Convert probabilities to binary predictions
log_predictions_binary <- ifelse(log_predictions > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- table(test_data$churn, log_predictions_binary)
print(conf_matrix)

# Model Accuracy
confusion_metrics <- confusionMatrix(as.factor(log_predictions_binary), as.factor(test_data$churn))
print(confusion_metrics)

log_model_advanced <- glm(churn ~ Total_Purchases + Total_Amount + Feedback, data = train_data, family = binomial)
summary(log_model_advanced)

ggplot(dataFinal1, aes(x = Total_Purchases, y = churn)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
  labs(title = "Shopping Frequency vs Churn Probability",
       x = "Shopping Frequency",
       y = "Churn Probability") +
  theme_minimal()
