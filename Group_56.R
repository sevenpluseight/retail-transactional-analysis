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
# To Investigate the relationship between customer income levels and product ratings to determine if income impacts customer satisfaction 
# 1. Do income levels (low, medium, high) and country influence customer satisfaction, as reflected in product ratings?
# Data wrangling & modeling
library(dplyr)
library(purrr)
library(nnet)
library(pscl)
library(tidyr)

# Visualization
library(vcd)
library(ggplot2)
library(plotly)

# Data preparation
# Ensure consistent factor levels and ordering
dataFinal1$Income <- factor(tolower(dataFinal1$Income),
                            levels = c("low", "medium", "high"),
                            ordered = TRUE)

dataFinal1$Ratings <- factor(tolower(dataFinal1$Ratings),
                             levels = c("low", "high"),
                             ordered = TRUE)

dataFinal1$Country <- factor(dataFinal1$Country)

# Mosaic Plots (from vcd)
# Overall Three-Way Mosaic Plot
mosaic(~ Income + Country + Ratings, data = dataFinal1,
       shade = TRUE,
       legend = TRUE,
       gp = shading_hcl,
       labeling = labeling_values,
       main = "Overall Distribution and Associations")

# Conditioned Mosaic Plot by Country
mosaic(~ Income + Country + Ratings, data = dataFinal1,
       shade = TRUE,
       legend = TRUE,
       gp = shading_hcl,
       labeling = labeling_values,
       main = "Overall Distribution and Associations")

# Grouped Bar Chart (from ggplot2)
# Summarize Data
summary_income_country_counts <- dataFinal1 %>%
  group_by(Income, Country) %>%
  summarize(Count = n(), .groups = 'drop')

# Plot Grouped Bar Chart
ggplot(summary_income_country_counts, aes(x = Country, y = Count, fill = Income)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Customer Counts by Income Level and Country",
       x = "Country",
       y = "Number of Customers",
       fill = "Income Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap of Chi-Square Residuals (Faceted by Country)
# Calculate Residuals per Country
country_residual_dfs <- dataFinal1 %>%
  group_by(Country) %>%
  nest() %>%
  mutate(chi_test = map(data, ~ {
    tbl <- table(.x$Income, .x$Ratings)
    if(min(dim(tbl)) > 1 && all(colSums(tbl) > 0) && all(rowSums(tbl) > 0)) {
      chisq.test(tbl)
    } else {
      NULL
    }
  })) %>%
  mutate(residuals_df = map(chi_test, ~ {
    if(!is.null(.x)) as.data.frame(.x$residuals) else NULL
  })) %>%
  select(Country, residuals_df) %>%
  unnest(residuals_df)

colnames(country_residual_dfs) <- c("Country", "Income", "Ratings", "Pearson_Residual")

# Plot Residual Heatmap
ggplot(country_residual_dfs, aes(x = Ratings, y = Income, fill = Pearson_Residual)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       name = "Pearson Residual",
                       limits = c(min(country_residual_dfs$Pearson_Residual, na.rm = TRUE),
                                  max(country_residual_dfs$Pearson_Residual, na.rm = TRUE))) +
  facet_wrap(~ Country) +
  theme_minimal() +
  labs(title = "Heatmap of Chi-Squared Residuals (Income vs. Ratings by Country)",
       x = "Product Ratings",
       y = "Income Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Association Plot (from vcd)
income_ratings_table <- table(dataFinal1$Income, dataFinal1$Ratings)

assocplot(income_ratings_table,
          main = "Association Plot of Income and Product Ratings")

# Statistical Test: Pairwise Proportions (Bonferroni Corrected)
pairwise.prop.test(table(dataFinal1$Income, dataFinal1$Ratings), p.adjust.method = "bonferroni")

# 3D Scatter Plot of Income & Country Counts (from plotly)
# Summarize & Transform Data
summary_3d <- dataFinal1 %>%
  group_by(Country, Income) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Country_Num = as.numeric(factor(Country)),
         Income_Num = as.numeric(factor(Income, levels = c("low", "medium", "high"))))

# Plot 3D Scatter
plot_ly(summary_3d, x = ~Country_Num, y = ~Income_Num, z = ~Count,
        type = "scatter3d", mode = "markers",
        color = ~Income, text = ~Country) %>%
  layout(title = "3D Scatter Plot of Income Groups by Country",
         scene = list(
           xaxis = list(title = "Country", tickmode = "array",
                        tickvals = summary_3d$Country_Num,
                        ticktext = summary_3d$Country),
           yaxis = list(title = "Income Level", tickvals = 1:3,
                        ticktext = c("Low", "Medium", "High")),
           zaxis = list(title = "Customer Count")
         ))

# Multinomial Logistic Regression Model 
# Build Model
glm_model_multi <- multinom(Ratings ~ Income * Country, data = dataFinal1)

# Summary and Coefficients
summary(glm_model_multi)
z_values <- summary(glm_model_multi)$coefficients / summary(glm_model_multi)$standard.errors
print(z_values)

# Model Fit (McFadden's Pseudo R²)
pseudo_r2 <- pR2(glm_model_multi)
print(pseudo_r2)

# Predict on New Data
new_data <- data.frame(Income = factor(c("low", "high"), levels = c("low", "medium", "high")),
                       Country = factor(c("uk", "germany"), levels = unique(dataFinal1$Country)))

predicted_ratings <- predict(glm_model_multi, new_data)
probs <- predict(glm_model_multi, new_data, type = "probs")

print(predicted_ratings)
print(probs)


#------    unorganize      ---------------------------------------------------------- 
# Contingency table for Income, Country, and Ratings(add 2 more variables)
# Load necessary libraries
library(vcd)
library(dplyr)
library(purrr) # For map function if calculating residuals by country

# --- Ensure factors are ordered (if applicable) for better visualization ---
# It's good practice to explicitly set levels for 'Income' and 'Ratings'
# to ensure consistent ordering in plots (e.g., Low, Medium, High for Income).
dataFinal1$Income <- factor(tolower(dataFinal1$Income), # Ensure income is also lowercase if it might not be
                            levels = c("low", "medium", "high"),
                            ordered = TRUE)
dataFinal1$Ratings <- factor(tolower(dataFinal1$Ratings), # Ensure ratings are lowercase
                             levels = c("low", "high"),
                             ordered = TRUE) # 'low' before 'high' makes sense for ordinality


# --- 1. Mosaic Plots (from vcd) ---
# A great way to visualize relationships between multiple categorical variables,
# showing cell proportions and deviations from independence.

# Overall three-way mosaic plot
# Shading indicates Pearson residuals (blue = fewer than expected, red = more than expected)
mosaic(~ Income + Country + Ratings, data = dataFinal1,
       shade = TRUE,
       legend = TRUE, # Shows the legend for Pearson residuals
       gp = shading_hcl, # Handles multi-dimensional shading better
       labeling = labeling_values,  # <-- this adds cell values (counts or %)
       main = "Overall Distribution and Associations")

# Mosaic plot conditioned by Country (Income-Ratings relationship for each Country)
# This helps see if the Income-Ratings pattern differs across countries.
mosaic(~ Income + Ratings | Country, data = dataFinal1,
       shade = TRUE,
       legend = TRUE,
       gp = shading_hcl,
       labeling = labeling_values,  # <-- this adds cell values (counts or %)
       main = "Product Ratings by Income Level, Conditioned by Country")

# --- 2. Grouped and Stacked Bar Charts (from ggplot2) ---
# Excellent for showing counts and proportions across categories.

# Summarize data for plotting counts (if not already done exactly this way)
summary_income_country_counts <- dataFinal1 %>%
  group_by(Income, Country) %>%
  summarize(Count = n(), .groups = 'drop')

# Grouped Bar Chart: Customer Counts by Income and Country
ggplot(summary_income_country_counts, aes(x = Country, y = Count, fill = Income)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Customer Counts by Income Level and Country",
       x = "Country",
       y = "Number of Customers",
       fill = "Income Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 3. Heatmap of Chi-Squared Residuals (from ggplot2) ---
# Your existing heatmap is great. Here's how to refine it and add facets for Country.

# To get residuals for Income vs Ratings for *each* country:
# This generates a list of chi-square tests, one for each country, then extracts residuals.
country_residual_dfs <- dataFinal1 %>%
  group_by(Country) %>%
  nest() %>% # Nests data into a column for each country
  # Only perform chi-test if there's enough data in the group to avoid errors
  mutate(chi_test = map(data, ~ {
    tbl <- table(.x$Income, .x$Ratings)
    if(min(dim(tbl)) > 1 && all(colSums(tbl) > 0) && all(rowSums(tbl) > 0) && sum(tbl) > 0) { # Basic check for valid table for chi-test
      chisq.test(tbl)
    } else {
      NULL # Return NULL if table is not suitable for chi-test
    }
  })) %>%
  mutate(residuals_df = map(chi_test, ~ {
    if(!is.null(.x)) as.data.frame(.x$residuals) else NULL
  })) %>%
  select(Country, residuals_df) %>%
  unnest(residuals_df) # Unnests back into a single dataframe

# Rename columns for clarity
colnames(country_residual_dfs) <- c("Country", "Income", "Ratings", "Pearson_Residual")

# Heatmap visualization with Country facets
ggplot(country_residual_dfs, aes(x = Ratings, y = Income, fill = Pearson_Residual)) +
  geom_tile(color = "white") + # Adds white borders between tiles
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       name = "Pearson Residual", # Customizes legend title
                       # Ensure consistent color scale if residuals exist
                       limits = c(min(country_residual_dfs$Pearson_Residual, na.rm = TRUE),
                                  max(country_residual_dfs$Pearson_Residual, na.rm = TRUE))) +
  facet_wrap(~ Country) + # Creates a separate plot for each country
  theme_minimal() +
  labs(title = "Heatmap of Chi-Squared Residuals (Income vs. Ratings by Country)",
       x = "Product Ratings",
       y = "Income Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotates x-axis labels for readability


# --- 4. Association Plot (from vcd) ---
# Another way to visualize deviations from independence in a 2-way table.
# You'd typically use this on aggregated 2-way tables.

# Create a 2-way table for Income and Ratings
income_ratings_table <- table(dataFinal1$Income, dataFinal1$Ratings)

# Association plot for Income and Ratings
assocplot(income_ratings_table,
          main = "Association Plot of Income and Product Ratings")

# Proof for assumptions
# Conducts multiple comparisons while adjusting for error.
pairwise.prop.test(table(dataFinal1$Income, dataFinal1$Ratings), p.adjust.method = "bonferroni")

library(dplyr)
library(plotly)

# Step 1: Summarize data to get counts
summary_3d <- dataFinal1 %>%
  group_by(Country, Income) %>%
  summarise(Count = n(), .groups = "drop")

# Step 2: Convert categorical to numeric for axes
summary_3d <- summary_3d %>%
  mutate(Country_Num = as.numeric(factor(Country)),
         Income_Num = as.numeric(factor(Income, levels = c("low", "medium", "high"))))  # Ensure order

# Step 3: Create the 3D scatter plot
plot_ly(summary_3d, x = ~Country_Num, y = ~Income_Num, z = ~Count,
        type = "scatter3d", mode = "markers",
        color = ~Income, text = ~Country) %>%
  layout(title = "3D Scatter Plot of Income Groups by Country",
         scene = list(
           xaxis = list(title = "Country", tickmode = "array",
                        tickvals = summary_3d$Country_Num,
                        ticktext = summary_3d$Country),
           yaxis = list(title = "Income Level",
                        tickvals = 1:3,
                        ticktext = c("Low", "Medium", "High")),
           zaxis = list(title = "Customer Count")
         ))

# Load Libraries
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
# Library used
# Core Data Wrangling & Modeling
library(dplyr)
library(caret)
library(randomForest)
library(xgboost)
library(pdp)
library(prophet)
library(ggplot2)

# Prepare the Target Variable (Ratings_Num)
# Encode ratings as numeric
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 2, NA))

# Check distribution
table(dataFinal1$Ratings_Num)
prop.table(table(dataFinal1$Ratings_Num))

# Downsample for Class Balance
minority_class <- dataFinal1 %>% filter(Ratings_Num == 1)
majority_class <- dataFinal1 %>% filter(Ratings_Num == 2)

set.seed(42)
majority_downsampled <- majority_class %>% sample_n(nrow(minority_class))

data_downsampled <- rbind(majority_downsampled, minority_class)
data_downsampled$Ratings_Num <- factor(data_downsampled$Ratings_Num, levels = c("1", "2"))

# Check balance
table(data_downsampled$Ratings_Num)
summary(data_downsampled)

# Random Forest Classification
rf_model <- randomForest(Ratings_Num ~ Income + Country + Order_Status + Feedback,
                         data = data_downsampled,
                         ntree = 500, mtry = 3, importance = TRUE)

# Feature Importance
varImpPlot(rf_model)

# Evaluate Random Forest
predictions <- predict(rf_model, data_downsampled)
conf_matrix <- confusionMatrix(predictions, data_downsampled$Ratings_Num)
print(conf_matrix)

# Prophet Time-Series Forecasting
# Prepare month as numeric and binary
dataFinal1 <- dataFinal1 %>%
  mutate(Month = tolower(Month)) %>%
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
    TRUE ~ NA_real_
  )) %>%
  mutate(Month_Binary = ifelse(Month %in% 1:6, 1, 0)) %>%
  mutate(ds = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  filter(!is.na(ds))

prophet_data <- dataFinal1[, c("ds", "Ratings_Num")]
colnames(prophet_data) <- c("ds", "y")

model <- prophet(prophet_data)
future <- make_future_dataframe(model, periods = 12, freq = "month")
forecast <- predict(model, future)

# Visual: Prophet Forecast
plot(model, forecast)

# XGBoost Model
data_matrix <- model.matrix(Ratings_Num ~ Income + Country + Order_Status + Feedback, data = data_downsampled)[,-1]
labels <- as.numeric(data_downsampled$Ratings_Num) - 1

dtrain <- xgb.DMatrix(data = data_matrix, label = labels)
xgb_model <- xgboost(data = dtrain, nrounds = 500, objective = "binary:logistic")
xgb_preds <- predict(xgb_model, dtrain)

xgb_conf_matrix <- confusionMatrix(factor(round(xgb_preds)), factor(labels))
print(xgb_conf_matrix)

# Logistic Regression Model
log_model <- glm(Ratings_Num ~ Income + Country + Order_Status + Feedback, 
                 data = data_downsampled, family = binomial)
log_preds <- predict(log_model, data_downsampled, type = "response")
log_preds_rounded <- factor(round(log_preds), levels = c(1, 2))

log_conf_matrix <- confusionMatrix(log_preds_rounded, data_downsampled$Ratings_Num)
print(log_conf_matrix)

# Model Performance Comparison
model_performance <- data.frame(
  Model = c("Random Forest", "XGBoost", "Logistic Regression"),
  Accuracy = c(conf_matrix$overall["Accuracy"], 
               xgb_conf_matrix$overall["Accuracy"], 
               log_conf_matrix$overall["Accuracy"])
)
print(model_performance)

# Explore Interaction Terms
rf_model_interaction <- randomForest(Ratings_Num ~ Income * Feedback + Country + Order_Status,
                                     data = data_downsampled, ntree = 500)

print(rf_model_interaction)
importance(rf_model_interaction)
varImpPlot(rf_model_interaction)

rf_preds <- predict(rf_model_interaction, data_downsampled)
conf_matrix_interaction <- confusionMatrix(rf_preds, data_downsampled$Ratings_Num)
print(conf_matrix_interaction)

# Partial Dependence Plots
# Convert Income to numeric if not already
data_downsampled <- data_downsampled %>%
  mutate(Income = as.numeric(factor(Income, levels = c("low", "medium", "high"), ordered = TRUE))) %>%
  filter(!is.na(Income))

partialPlot(rf_model_interaction, data_downsampled, Income)
partialPlot(rf_model_interaction, data_downsampled, Feedback)

# Visualize Rating Trends Over Time
# Trend by Income
ggplot(dataFinal1, aes(x = ds, y = Ratings_Num, color = Income)) +
  geom_line() +
  theme_minimal()

# Trend by Month
ggplot(dataFinal1, aes(x = ds, y = Ratings_Num, color = Income)) +
  geom_line() +
  facet_wrap(~ Month) +
  theme_minimal()

# Trend by Country
ggplot(dataFinal1, aes(x = ds, y = Ratings_Num, color = Country)) +
  geom_line() +
  theme_minimal()

# --------------   unorganize     ---------------------------------  
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
# Load required libraries
library(dplyr)         # Data manipulation
library(ggplot2)       # Data visualization
library(caret)         # Model evaluation and utilities
library(randomForest)  # Random Forest modeling
library(prophet)       # Time series forecasting
library(xgboost)       # Gradient boosting modeling
library(lightgbm)      # LightGBM modeling
library(pROC)          # ROC curve analysis
library(PRROC)         # Precision-Recall curve analysis
library(pdp)           # Partial dependence plots

# Data Preparation
# Cross-tabulation of income, product category, and rating
table(dataFinal1$Income, dataFinal1$Product_Category, dataFinal1$Ratings)

# Convert ratings to numeric formats
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 5, NA))

dataFinal1$Ratings_Binary <- ifelse(dataFinal1$Ratings == "low", 0, 
                                    ifelse(dataFinal1$Ratings == "high", 1, NA))

# Explore Rating Patterns by Income and Product Type
# Mean rating per income group & product type
dataFinal1 %>%
  group_by(Income, Product_Type) %>%
  summarize(Average_Rating = mean(Ratings_Num, na.rm = TRUE))

# Chi-squared test: Income vs Product Type
income_product_ratings_table <- table(dataFinal1$Income, dataFinal1$Product_Type)
chi_income_product <- chisq.test(income_product_ratings_table)
chi_income_product

# Visualization – Rating Distribution Across Income Levels
ggplot(dataFinal1, aes(x = Product_Type, fill = Income)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Income-Level Effects on Product Ratings", x = "Product Type", y = "Proportion") +
  theme_minimal()

# Logistic Regression – Predict High Ratings by Income & Product Type
# Logistic regression model
model <- glm(Ratings_Binary ~ Income + Country + Product_Type, data = dataFinal1, family = binomial)
summary(model)

# Convert log-odds to odds ratios
exp(coef(model))

# Classification performance
predicted_ratings <- ifelse(predict(model, type = "response") > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_ratings), as.factor(dataFinal1$Ratings_Num))

# XGBoost Model – Predict Ratings with Feature Engineering
# Prepare numerical encodings for modeling
dataFinal1 <- dataFinal1 %>%
  mutate(
    Ratings_Num = ifelse(Ratings == "high", 1, 0),  
    Income_Num = as.numeric(factor(Income, levels = c("low", "medium", "high"))),
    Country_Num = as.numeric(factor(Country)),
    Product_Type_Num = as.numeric(factor(Product_Type))
  )

X <- as.matrix(select(dataFinal1, Income_Num, Country_Num, Product_Type_Num))
y <- dataFinal1$Ratings_Num

# Train XGBoost
xgb_data <- xgb.DMatrix(data = X, label = y)
params <- list(objective = "binary:logistic", eval_metric = "logloss", eta = 0.1, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)
xgb_model <- xgb.train(params, xgb_data, nrounds = 100, verbose = TRUE)

# Evaluate
predictions <- predict(xgb_model, X)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_labels), as.factor(y))

# Feature Engineering + LightGBM Alternative
# Add payment & shipping as features
dataFinal1 <- dataFinal1 %>%
  mutate(
    Payment_Num = as.numeric(factor(Payment_Method)),
    Shipping_Num = as.numeric(factor(Shipping_Method))
  )

library(lightgbm)
lgb_data <- lgb.Dataset(data = X, label = y)
lgb_params <- list(objective = "binary", metric = "logloss", learning_rate = 0.05, num_leaves = 31)
lgb_model <- lgb.train(params = lgb_params, data = lgb_data, nrounds = 200)

predictions_lgb <- predict(lgb_model, X)
confusionMatrix(as.factor(ifelse(predictions_lgb > 0.5, 1, 0)), as.factor(y))

# Explore Interactions + Threshold Tuning
# Add interaction terms
dataFinal1 <- dataFinal1 %>%
  mutate(
    Income_Product_Interaction = Income_Num * Product_Type_Num,
    Country_Payment_Interaction = Country_Num * Payment_Num
  )

# ROC Curve
library(pROC)
roc_curve <- roc(y, predictions_lgb)
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Threshold Analysis")

# Optimal threshold
optimal_threshold <- coords(roc_curve, "best", ret = "threshold")
abline(v = optimal_threshold[1], col = "red", lwd = 2, lty = 2)

# Precision-Recall Curve & Threshold Effects
library(PRROC)
pr_curve <- pr.curve(scores.class0 = predictions_lgb, weights.class0 = y, curve = TRUE)
plot(pr_curve, col = "green", lwd = 2, main = "Precision-Recall Curve for Threshold Tuning")

# Vary threshold and evaluate
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

# Final XGBoost Model with Updated Features
# Feature selection + update
X_updated <- as.matrix(select(dataFinal1, Income_Num, Product_Type_Num, Country_Num, Payment_Num, Income_Product_Interaction))
xgb_data_updated <- xgb.DMatrix(data = X_updated, label = y)

params <- list(eta = 0.05, max_depth = 8, subsample = 0.9, colsample_bytree = 0.7)
xgb_model_updated <- xgb.train(params, xgb_data_updated, nrounds = 100, verbose = TRUE)

# Evaluation
predictions_updated <- predict(xgb_model_updated, X_updated)
predicted_labels_updated <- ifelse(predictions_updated > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_labels_updated), as.factor(y))

# Feature importance
importance_matrix_updated <- xgb.importance(model = xgb_model_updated, feature_names = colnames(X_updated))
print(importance_matrix_updated)

# Compare AUC Scores (Original vs. Updated)
# AUC Scores
roc_curve_original <- roc(y, predictions)
roc_curve_updated <- roc(y, predictions_updated)

auc_original <- auc(roc_curve_original)
auc_updated <- auc(roc_curve_updated)

# Plot comparison
plot(roc_curve_original, col = "red", lwd = 2, main = "AUC-ROC Comparison")
lines(roc_curve_updated, col = "blue", lwd = 2)

legend("bottomright", legend = c(
  paste("Original AUC:", round(auc_original, 3)), 
  paste("Updated AUC:", round(auc_updated, 3))
), col = c("red", "blue"), lwd = 2)

# -------------------   unorganize     ------------------------------------------
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
# Required Libraries
library(dplyr)     # Data manipulation
library(ggplot2)   # Visualization
library(stats)     # Statistical tests (aov, lm, glm, chisq.test)

# Create Spending Tier & Ratings Columns
# Assign spending tiers based on Total_Amount
dataFinal1 <- dataFinal1 %>%
  mutate(Spending_Tier = case_when(
    Total_Amount < 500 ~ "Low",
    Total_Amount >= 500 & Total_Amount < 1000 ~ "Medium",
    Total_Amount >= 1000 ~ "High"
  ))

# Convert Spending_Tier to factor with correct order
dataFinal1$Spending_Tier <- factor(dataFinal1$Spending_Tier, levels = c("Low", "Medium", "High"))

# Create numerical ratings
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 5, NA))

# Create binary rating category
dataFinal1 <- dataFinal1 %>%
  mutate(Rating_Category = ifelse(Ratings_Num == 1, "High", "Low"))

# Count Ratings by Spending Tier and Income
rating_counts <- dataFinal1 %>%
  group_by(Income, Spending_Tier, Rating_Category) %>%
  summarize(Count = n()) %>%
  ungroup()

print(rating_counts)

# Visualization: Ratings by Spending Tier & Income
ggplot(rating_counts, aes(x = Spending_Tier, y = Count, fill = Rating_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Income) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  labs(title = "High vs. Low Ratings Across Spending Tiers",
       x = "Spending Tier", y = "Number of Ratings", fill = "Rating Level") +
  theme_minimal()

# Chi-Square Test: Spending Tier vs Rating Category
chi_spending_ratings <- chisq.test(table(dataFinal1$Spending_Tier, dataFinal1$Rating_Category))
print(chi_spending_ratings)

# ANOVA: Income & Spending Tier Effects on Ratings
anova_model <- aov(Ratings_Num ~ Income * Spending_Tier, data = dataFinal1)
summary(anova_model)

# Linear Regression: Ratings ~ Income + Spending Tier
reg_model <- lm(Ratings_Num ~ Income + Spending_Tier, data = dataFinal1)
summary(reg_model)

# Logistic Regression: Predicting Binary Satisfaction
dataFinal1$Ratings_Binary <- ifelse(dataFinal1$Ratings_Num == 1, 0, 
                                    ifelse(dataFinal1$Ratings_Num == 5, 1, NA))

log_model <- glm(Ratings_Binary ~ Income + Spending_Tier, family = binomial, data = dataFinal1)
summary(log_model)

# Ratings per Month
rating_month_counts <- dataFinal1 %>%
  group_by(Month, Rating_Category) %>%
  summarize(Count = n()) %>%
  ungroup()

print(rating_month_counts)

# Visualization: Monthly Ratings Trend
ggplot(rating_month_counts, aes(x = Month, y = Count, fill = Rating_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("brown", "lightblue")) +
  labs(title = "Monthly Distribution of High vs. Low Ratings",
       x = "Month", y = "Number of Ratings", fill = "Rating Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Chi-Square Test: Month vs Rating Category
chi_monthly_ratings <- chisq.test(table(dataFinal1$Month, dataFinal1$Rating_Category))
print(chi_monthly_ratings)

# Three-Way ANOVA: Income, Spending Tier, Month
anova_model_month <- aov(Ratings_Num ~ Income * Spending_Tier * Month, data = dataFinal1)
summary(anova_model_month)

# Additional Rating Analysis by Income
# Mean ratings by income
aggregate(Ratings_Num ~ Income, data = dataFinal1, mean)

# ANOVA: Ratings by Income only
aov_income <- aov(Ratings_Num ~ Income, data = dataFinal1)
summary(aov_income)

# Post-hoc test
TukeyHSD(aov_income)

# Visualization: Rating Proportion by Income
ggplot(dataFinal1, aes(x = Income, fill = Ratings)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of High VS Low Ratings by Income Level",
       x = "Income Level", y = "Proportion") +
  theme_minimal()

# Chi-Square: Ratings by Income
chisq.test(table(dataFinal1$Income, dataFinal1$Ratings_Num))

# ---------------------------------------       unorganize         ----------------------------------------  
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

# done
-------------------------------
# Which spending tier , payment method and total amount receive the highest ratings, and how does this reflect customer preferences? (analysis question) 
# For clustering and cluster visualization
library(cluster)
library(factoextra)

# For data manipulation and visualization
library(dplyr)
library(ggplot2)
library(tidyr)

# For statistical modeling
library(caret)

# For machine learning models
library(randomForest)

# -----------------------------
# STEP 1: Clustering Analysis
# -----------------------------
library(cluster)
library(factoextra)

dataFinal1$Income <- as.numeric(as.factor(dataFinal1$Income))
dataFinal1$Spending_Tier <- as.numeric(as.factor(dataFinal1$Spending_Tier))
dataFinal1$Payment_Method <- as.numeric(as.factor(dataFinal1$Payment_Method))

data_cluster <- dataFinal1 %>%
  select(Income, Spending_Tier, Ratings_Num, Total_Amount, Payment_Method) %>%
  scale()

set.seed(123)
kmeans_result <- kmeans(data_cluster, centers = 5)
dataFinal1$Cluster <- factor(kmeans_result$cluster)

fviz_cluster(kmeans_result, data = data_cluster, geom = "point")

cluster_centroids <- aggregate(data_cluster, by = list(Cluster = kmeans_result$cluster), FUN = mean)
print(cluster_centroids)

table(dataFinal1$Cluster, dataFinal1$Payment_Method)

anova_income <- aov(Income ~ Cluster, data = dataFinal1)
summary(anova_income)

anova_spending <- aov(Spending_Tier ~ Cluster, data = dataFinal1)
summary(anova_spending)

anova_ratings <- aov(Ratings_Num ~ Cluster, data = dataFinal1)
summary(anova_ratings)

anova_ratings <- aov(Total_Amount ~ Cluster, data = dataFinal1)
summary(anova_ratings)

# -----------------------------
# STEP 2: Spending & Rating by Cluster
# -----------------------------
cluster_spending <- dataFinal1 %>%
  group_by(Cluster, Ratings) %>%
  summarize(Total_Spent = sum(Total_Amount), Count = n()) %>%
  ungroup()
print(cluster_spending)

ggplot(dataFinal1, aes(x = Cluster, fill = factor(Payment_Method))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#FFABAB", "#85E3FF", "#A3A3F3", "#FFD166")) +
  labs(title = "Payment Method Preference Across Clusters",
       x = "Cluster", y = "Proportion", fill = "Payment Method") +
  theme_minimal()

# -----------------------------
# STEP 3: Spending Tier vs Payment Method
# -----------------------------
dataFinal1$Payment_Method <- factor(dataFinal1$Payment_Method)

payment_spending <- dataFinal1 %>%
  group_by(Spending_Tier, Payment_Method) %>%
  summarize(Average_Spend = mean(Total_Amount), Count = n()) %>%
  ungroup()

payment_usage <- dataFinal1 %>%
  group_by(Spending_Tier, Payment_Method) %>%
  summarize(Count = n()) %>%
  ungroup()
print(payment_usage)

ggplot(payment_usage, aes(x = Spending_Tier, y = Count, fill = factor(Payment_Method))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#FF6F61", "#6A5ACD", "#FFD166", "#85E3FF")) +
  labs(title = "Number of Users by Payment Method Across Spending Tiers",
       x = "Spending Tier", y = "Number of Users", fill = "Payment Method") +
  theme_minimal()

chi_payment_spending <- chisq.test(table(dataFinal1$Payment_Method, dataFinal1$Spending_Tier))
print(chi_payment_spending)

anova_payment_ratings <- aov(Ratings_Num ~ Payment_Method * Spending_Tier, data = dataFinal1)
summary(anova_payment_ratings)

# -----------------------------
# STEP 4: Logistic Regression and Sampling
# -----------------------------
library(caret)

log_model_refined <- glm(Ratings_Binary ~ Payment_Method + Spending_Tier, family = binomial, data = dataFinal1)
summary(log_model_refined)

predictions_refined <- ifelse(predict(log_model_refined, type = "response") > 0.5, 1, 0)
confusionMatrix(as.factor(predictions_refined), as.factor(dataFinal1$Ratings_Binary))

dataFinal1$Ratings_Binary <- factor(dataFinal1$Ratings_Binary, levels = c(0, 1))

# Oversample
set.seed(123)
data_over <- upSample(
  x = dataFinal1[, !(names(dataFinal1) %in% c("Ratings_Binary", "Ratings_Num", "Ratings", "Rating_Category"))],
  y = dataFinal1$Ratings_Binary,
  yname = "Ratings_Binary"
)

log_over <- glm(Ratings_Binary ~ Payment_Method + Spending_Tier, family = binomial, data = data_over)
pred_class_over <- ifelse(predict(log_over, type = "response") > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class_over), as.factor(data_over$Ratings_Binary))

# Downsample
set.seed(123)
data_down <- downSample(
  x = dataFinal1[, !(names(dataFinal1) %in% c("Ratings_Binary", "Ratings_Num", "Ratings", "Rating_Category"))],
  y = dataFinal1$Ratings_Binary,
  yname = "Ratings_Binary"
)

log_down <- glm(Ratings_Binary ~ Payment_Method + Spending_Tier, family = binomial, data = data_down)
pred_class_down <- ifelse(predict(log_down, type = "response") > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class_down), as.factor(data_down$Ratings_Binary))

# -----------------------------
# STEP 5: Random Forest Classification
# -----------------------------
library(randomForest)

set.seed(123)
rf_original <- randomForest(Ratings_Binary ~ Payment_Method + Spending_Tier, data = dataFinal1, importance = TRUE)
conf_rf_orig <- confusionMatrix(predict(rf_original, type = "response"), dataFinal1$Ratings_Binary)

set.seed(123)
rf_over <- randomForest(Ratings_Binary ~ Payment_Method + Spending_Tier, data = data_over, importance = TRUE)
conf_rf_over <- confusionMatrix(predict(rf_over, type = "response"), data_over$Ratings_Binary)

set.seed(123)
rf_down <- randomForest(Ratings_Binary ~ Payment_Method + Spending_Tier, data = data_down, importance = TRUE)
conf_rf_down <- confusionMatrix(predict(rf_down, type = "response"), data_down$Ratings_Binary)

# -----------------------------
# STEP 6: Model Comparison Visualization
# -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

results <- data.frame(
  Sample = rep(c("Original", "Oversample", "Downsample"), each = 2),
  Model = rep(c("Logistic Regression", "Random Forest"), times = 3),
  Accuracy = c(
    confusionMatrix(as.factor(pred_class_orig), dataFinal1$Ratings_Binary)$overall["Accuracy"],
    conf_rf_orig$overall["Accuracy"],
    confusionMatrix(as.factor(pred_class_over), data_over$Ratings_Binary)$overall["Accuracy"],
    conf_rf_over$overall["Accuracy"],
    confusionMatrix(as.factor(pred_class_down), data_down$Ratings_Binary)$overall["Accuracy"],
    conf_rf_down$overall["Accuracy"]
  ),
  Sensitivity = c(
    confusionMatrix(as.factor(pred_class_orig), dataFinal1$Ratings_Binary)$byClass["Sensitivity"],
    conf_rf_orig$byClass["Sensitivity"],
    confusionMatrix(as.factor(pred_class_over), data_over$Ratings_Binary)$byClass["Sensitivity"],
    conf_rf_over$byClass["Sensitivity"],
    confusionMatrix(as.factor(pred_class_down), data_down$Ratings_Binary)$byClass["Sensitivity"],
    conf_rf_down$byClass["Sensitivity"]
  ),
  Specificity = c(
    confusionMatrix(as.factor(pred_class_orig), dataFinal1$Ratings_Binary)$byClass["Specificity"],
    conf_rf_orig$byClass["Specificity"],
    confusionMatrix(as.factor(pred_class_over), data_over$Ratings_Binary)$byClass["Specificity"],
    conf_rf_over$byClass["Specificity"],
    confusionMatrix(as.factor(pred_class_down), data_down$Ratings_Binary)$byClass["Specificity"],
    conf_rf_down$byClass["Specificity"]
  )
)

results_long <- results %>%
  pivot_longer(cols = c("Accuracy", "Sensitivity", "Specificity"),
               names_to = "Metric", values_to = "Value")

ggplot(results_long, aes(x = Sample, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Performance Comparison: Logistic Regression vs Random Forest",
       y = "Score", x = "Sampling Strategy") +
  scale_fill_manual(values = c("#FFB347", "#77DD77")) +
  theme_minimal()

# -----------------------------
# STEP 7: Answer to Main Question
# -----------------------------
get_high_rating_summary <- function(data, sample_name) {
  data %>%
    filter(Ratings_Num >= 4) %>%
    group_by(Spending_Tier, Payment_Method) %>%
    summarize(
      Avg_Rating = mean(Ratings_Num),
      Avg_Amount = mean(Total_Amount),
      Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(Sample = sample_name)
}

data_over$Ratings_Num <- dataFinal1$Ratings_Num[match(rownames(data_over), rownames(dataFinal1))]
data_over$Total_Amount <- dataFinal1$Total_Amount[match(rownames(data_over), rownames(dataFinal1))]

data_down$Ratings_Num <- dataFinal1$Ratings_Num[match(rownames(data_down), rownames(dataFinal1))]
data_down$Total_Amount <- dataFinal1$Total_Amount[match(rownames(data_down), rownames(dataFinal1))]

high_rating_orig <- get_high_rating_summary(dataFinal1, "Original")
high_rating_over <- get_high_rating_summary(data_over, "Oversample")
high_rating_down <- get_high_rating_summary(data_down, "Downsample")

high_rating_all <- bind_rows(high_rating_orig, high_rating_over, high_rating_down)

ggplot(high_rating_all, aes(x = Spending_Tier, y = Avg_Amount, fill = Payment_Method)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Sample) +
  labs(title = "Average Spend for High Ratings by Tier and Payment Method (All Samples)",
       y = "Average Spend", x = "Spending Tier", fill = "Payment Method") +
  theme_minimal()

# -----------------------------
# STEP 8: Cluster Profiling
# -----------------------------
cluster_profile <- dataFinal1 %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Rating = mean(Ratings_Num),
    Avg_Amount = mean(Total_Amount),
    Most_Common_Payment = names(which.max(table(Payment_Method))),
    Most_Common_Tier = names(which.max(table(Spending_Tier))),
    Count = n()
  )

print(cluster_profile)

# ---------------------------  unorganize         ------------------------------------  
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

#data tuning oversample and downsample
#prepare dataset
dataFinal1$Ratings_Binary <- factor(dataFinal1$Ratings_Binary, levels = c(0, 1))

# Comparison oversample & downsample
#oversample
set.seed(123)
data_over <- upSample(
  x = dataFinal1[, !(names(dataFinal1) %in% c("Ratings_Binary", "Ratings_Num", "Ratings", "Rating_Category"))],
  y = dataFinal1$Ratings_Binary,
  yname = "Ratings_Binary"
)

log_over <- glm(Ratings_Binary ~ Payment_Method + Spending_Tier, family = binomial, data = data_over)
pred_over <- predict(log_over, type = "response")
pred_class_over <- ifelse(pred_over > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class_over), as.factor(data_over$Ratings_Binary))

#downsample
set.seed(123)
data_down <- downSample(
  x = dataFinal1[, !(names(dataFinal1) %in% c("Ratings_Binary", "Ratings_Num", "Ratings", "Rating_Category"))],
  y = dataFinal1$Ratings_Binary,
  yname = "Ratings_Binary"
)
log_down <- glm(Ratings_Binary ~ Payment_Method + Spending_Tier, family = binomial, data = data_down)
pred_down <- predict(log_down, type = "response")
pred_class_down <- ifelse(pred_down > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class_down), as.factor(data_down$Ratings_Binary))

# Original sample
log_original <- glm(Ratings_Binary ~ Payment_Method + Spending_Tier, family = binomial, data = dataFinal1)
pred_orig <- predict(log_original, type = "response")
pred_class_orig <- ifelse(pred_orig > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class_orig), as.factor(dataFinal1$Ratings_Binary))

# Use Random Forest
library(randomForest)
library(caret)

# Original sample
set.seed(123)
rf_original <- randomForest(Ratings_Binary ~ Payment_Method + Spending_Tier,
                            data = dataFinal1, importance = TRUE)

pred_rf_orig <- predict(rf_original, type = "response")
conf_rf_orig <- confusionMatrix(pred_rf_orig, dataFinal1$Ratings_Binary)
print(conf_rf_orig)

# Oversample
set.seed(123)
rf_over <- randomForest(Ratings_Binary ~ Payment_Method + Spending_Tier,
                        data = data_over, importance = TRUE)

pred_rf_over <- predict(rf_over, type = "response")
conf_rf_over <- confusionMatrix(pred_rf_over, data_over$Ratings_Binary)
print(conf_rf_over)

#Downsample
set.seed(123)
rf_down <- randomForest(Ratings_Binary ~ Payment_Method + Spending_Tier,
                        data = data_down, importance = TRUE)

pred_rf_down <- predict(rf_down, type = "response")
conf_rf_down <- confusionMatrix(pred_rf_down, data_down$Ratings_Binary)
print(conf_rf_down)

conf_rf_over$overall["Accuracy"]
conf_rf_over$byClass["Sensitivity"]
conf_rf_over$byClass["Specificity"]

# visualization
library(ggplot2)
library(dplyr)
library(caret)
library(tidyr)

# Extract performance metrics
results <- data.frame(
  Sample = rep(c("Original", "Oversample", "Downsample"), each = 2),
  Model = rep(c("Logistic Regression", "Random Forest"), times = 3),
  Accuracy = c(
    confusionMatrix(as.factor(pred_class_orig), dataFinal1$Ratings_Binary)$overall["Accuracy"],
    confusionMatrix(pred_rf_orig, dataFinal1$Ratings_Binary)$overall["Accuracy"],
    
    confusionMatrix(as.factor(pred_class_over), data_over$Ratings_Binary)$overall["Accuracy"],
    confusionMatrix(pred_rf_over, data_over$Ratings_Binary)$overall["Accuracy"],
    
    confusionMatrix(as.factor(pred_class_down), data_down$Ratings_Binary)$overall["Accuracy"],
    confusionMatrix(pred_rf_down, data_down$Ratings_Binary)$overall["Accuracy"]
  ),
  Sensitivity = c(
    confusionMatrix(as.factor(pred_class_orig), dataFinal1$Ratings_Binary)$byClass["Sensitivity"],
    confusionMatrix(pred_rf_orig, dataFinal1$Ratings_Binary)$byClass["Sensitivity"],
    
    confusionMatrix(as.factor(pred_class_over), data_over$Ratings_Binary)$byClass["Sensitivity"],
    confusionMatrix(pred_rf_over, data_over$Ratings_Binary)$byClass["Sensitivity"],
    
    confusionMatrix(as.factor(pred_class_down), data_down$Ratings_Binary)$byClass["Sensitivity"],
    confusionMatrix(pred_rf_down, data_down$Ratings_Binary)$byClass["Sensitivity"]
  ),
  Specificity = c(
    confusionMatrix(as.factor(pred_class_orig), dataFinal1$Ratings_Binary)$byClass["Specificity"],
    confusionMatrix(pred_rf_orig, dataFinal1$Ratings_Binary)$byClass["Specificity"],
    
    confusionMatrix(as.factor(pred_class_over), data_over$Ratings_Binary)$byClass["Specificity"],
    confusionMatrix(pred_rf_over, data_over$Ratings_Binary)$byClass["Specificity"],
    
    confusionMatrix(as.factor(pred_class_down), data_down$Ratings_Binary)$byClass["Specificity"],
    confusionMatrix(pred_rf_down, data_down$Ratings_Binary)$byClass["Specificity"]
  )
)

# Reshape to long format for plotting
results_long <- results %>%
  pivot_longer(cols = c("Accuracy", "Sensitivity", "Specificity"),
               names_to = "Metric", values_to = "Value")

# Plot
ggplot(results_long, aes(x = Sample, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Performance Comparison: Logistic Regression vs Random Forest",
       y = "Score", x = "Sampling Strategy") +
  scale_fill_manual(values = c("#FFB347", "#77DD77")) +
  theme_minimal()

# Answer to the Main Question
library(dplyr)
library(ggplot2)

# Define a function to compute high-rating summary
get_high_rating_summary <- function(data, sample_name) {
  data %>%
    filter(Ratings_Num >= 4) %>%
    group_by(Spending_Tier, Payment_Method) %>%
    summarize(
      Avg_Rating = mean(Ratings_Num),
      Avg_Amount = mean(Total_Amount),
      Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(Sample = sample_name)
}

# Add Ratings_Num and Total_Amount to oversample/downsample sets
data_over$Ratings_Num <- dataFinal1$Ratings_Num[match(rownames(data_over), rownames(dataFinal1))]
data_over$Total_Amount <- dataFinal1$Total_Amount[match(rownames(data_over), rownames(dataFinal1))]

data_down$Ratings_Num <- dataFinal1$Ratings_Num[match(rownames(data_down), rownames(dataFinal1))]
data_down$Total_Amount <- dataFinal1$Total_Amount[match(rownames(data_down), rownames(dataFinal1))]

# Compute summaries
high_rating_orig <- get_high_rating_summary(dataFinal1, "Original")
high_rating_over <- get_high_rating_summary(data_over, "Oversample")
high_rating_down <- get_high_rating_summary(data_down, "Downsample")

# Combine all
high_rating_all <- bind_rows(high_rating_orig, high_rating_over, high_rating_down)

# Plot
ggplot(high_rating_all, aes(x = Spending_Tier, y = Avg_Amount, fill = Payment_Method)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Sample) +
  labs(title = "Average Spend for High Ratings by Tier and Payment Method (All Samples)",
       y = "Average Spend", x = "Spending Tier", fill = "Payment Method") +
  theme_minimal()

# Cluster Profiling
cluster_profile <- dataFinal1 %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Rating = mean(Ratings_Num),
    Avg_Amount = mean(Total_Amount),
    Most_Common_Payment = names(which.max(table(Payment_Method))),
    Most_Common_Tier = names(which.max(table(Spending_Tier))),
    Count = n()
  )

print(cluster_profile)

-------------------------------
# Does the choice of payment method , total purchases and income level impact customer satisfaction as represented by product ratings? (analysis question)
# ============================
# Libraries
# ============================
library(dplyr)
library(ggplot2)
library(cluster)
library(patchwork)
library(caret)
library(zoo)

# ============================
# Preprocessing
# ============================
# Add rating_Num and rating_Binary
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 5,NA ))

dataFinal1$Ratings_Binary <- ifelse(dataFinal1$Ratings == "low", 0, 
                                    ifelse(dataFinal1$Ratings == "high", 1,NA ))

# Convert ordered income factor to numeric scale (e.g., low = 1, medium = 2, high = 3)
dataFinal1$Income_Num <- as.numeric(factor(dataFinal1$Income, levels = c("low", "medium", "high")))

# Convert Month to numeric and create YearMonth
dataFinal1$Month <- match(tolower(dataFinal1$Month), tolower(month.name))
dataFinal1 <- dataFinal1 %>% filter(!is.na(Month))
dataFinal1$YearMonth <- as.Date(with(dataFinal1, paste(Year, Month, "01", sep = "-")), "%Y-%m-%d")
dataFinal1$YearMonth <- format(dataFinal1$YearMonth, "%Y-%m")

# ============================
# Basic Aggregation & Testing
# ============================
aggregate(Ratings_Num ~ Income, data = dataFinal1, mean)
aggregate(Ratings_Num ~ Income + Payment_Method, data = dataFinal1, mean)
aggregate(Ratings_Num ~ Payment_Method, data = dataFinal1, summary)
aggregate(Ratings_Num ~ Payment_Method, data = dataFinal1, mean)

cor.test(dataFinal1$Income_Num, dataFinal1$Ratings_Num)
kruskal.test(Ratings_Num ~ Income_Num, data = dataFinal1)

# ============================
# Visualizations - Income & Payment
# ============================
# violin plots
set.seed(42)

# Define categories
incomes <- c('high', 'low', 'medium')
payment_methods <- c('cash', 'credit card', 'debit card', 'paypal')

# Corrected: Use list(...) instead of c(...)
combo_means <- list(
  list(income='high', pm='cash', mean=3.51),
  list(income='low', pm='cash', mean=3.90),
  list(income='medium', pm='cash', mean=3.68),
  list(income='high', pm='credit card', mean=3.58),
  list(income='low', pm='credit card', mean=3.90),
  list(income='medium', pm='credit card', mean=3.71),
  list(income='high', pm='debit card', mean=3.28),
  list(income='low', pm='debit card', mean=3.52),
  list(income='medium', pm='debit card', mean=3.38),
  list(income='high', pm='paypal', mean=3.33),
  list(income='low', pm='paypal', mean=3.67),
  list(income='medium', pm='paypal', mean=3.46)
)

data_list <- list()
num_samples_per_combo <- 2500

for (combo_info in combo_means) {
  income <- combo_info$income
  pm <- combo_info$pm
  mean_rating <- combo_info$mean
  
  ratings <- rnorm(n = num_samples_per_combo, mean = mean_rating, sd = 0.8)
  ratings <- round(pmax(1, pmin(5, ratings)))
  
  temp_df <- data.frame(
    Income = rep(income, num_samples_per_combo),
    Payment_Method = rep(pm, num_samples_per_combo),
    Ratings_Num = ratings
  )
  
  data_list[[length(data_list) + 1]] <- temp_df
}

# Combine all into one data frame
simulated_data <- do.call(rbind, data_list)

# Ensure 'Income' and 'Payment_Method' are factors for proper ordering
dataFinal1$Income <- factor(dataFinal1$Income, levels = c('low', 'medium', 'high'), ordered = TRUE)
dataFinal1$Payment_Method <- factor(dataFinal1$Payment_Method, levels = c('cash', 'credit card', 'debit card', 'paypal'), ordered = TRUE)


# --- 2. Create the Violin Plot: Ratings by Income & Payment Method ---
library(ggplot2)

ggplot(data = dataFinal1, aes(x = Income, y = Ratings_Num, fill = Payment_Method)) +
  geom_violin(trim = FALSE, scale = "width") + # trim = FALSE extends violins to min/max data, scale = "width" makes them same width
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", color = "black") + # Add boxplot inside for quartiles
  scale_fill_viridis_d() + # Use a viridis color palette (discrete)
  labs(
    title = "Distribution of Ratings by Income Level and Payment Method",
    x = "Income Level",
    y = "Ratings (1-5)"
  ) +
  theme_minimal() + # Use a minimal theme
  theme(
    plot.title = element_text(size = 16, hjust = 0.5), # Center and size title
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right" # Place legend to the right
  ) +
  coord_cartesian(ylim = c(0.5, 5.5)) # Set y-axis limits without clipping data points


# --- Optional: Violin Plot just for Ratings by Income ---
ggplot(data = dataFinal1, aes(x = Income, y = Ratings_Num, fill = Income)) +
  geom_violin(trim = FALSE, scale = "width") +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", color = "black") +
  scale_fill_viridis_d(option = "magma") + # Use a magma color palette
  labs(
    title = "Distribution of Ratings by Income Level",
    x = "Income Level",
    y = "Ratings (1-5)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none" # No legend needed for income only
  ) +
  coord_cartesian(ylim = c(0.5, 5.5))

# ============================
# Income * Payment Method Interaction
# ============================
interaction_model <- lm(Ratings_Num ~ Income * Payment_Method, data = dataFinal1)
summary(interaction_model)

# ============================
# Satisfaction Logic
# ============================
dataFinal1 %>%
  mutate(Satisfied = ifelse(Ratings_Num >= 4, "Satisfied", "Not Satisfied")) %>%
  group_by(Satisfied) %>%
  summarize(Average_Purchases = mean(Total_Purchases))

dataFinal1$Satisfied <- ifelse(dataFinal1$Ratings_Num >= 4, 1, 0)
logit_model <- glm(Satisfied ~ Payment_Method, data = dataFinal1, family = binomial)
summary(logit_model)

# ============================
# ANOVA - Payment Method & Ratings
# ============================
anova_payment <- aov(Ratings_Num ~ Payment_Method, data = dataFinal1)
summary(anova_payment)
TukeyHSD(anova_payment)

dataFinal1 %>%
  group_by(Payment_Method) %>%
  summarize(Average_Rating = mean(Ratings_Num))

# ============================
# Visualizations - Ratings & Payment Method using ANOVA
# ============================
ggplot(dataFinal1, aes(x = Payment_Method, y = Ratings_Num, fill = Payment_Method)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_fill_viridis_d() +
  labs(
    title = "Average Ratings by Payment Method",
    x = "Payment Method", y = "Average Rating"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

library(multcompView)

# Get compact letter display
tukey_result <- TukeyHSD(anova_payment)
letters <- multcompView::multcompLetters4(anova_payment, tukey_result)

# Attach letters to summary
means <- dataFinal1 %>%
  group_by(Payment_Method) %>%
  summarize(Average_Rating = mean(Ratings_Num)) %>%
  mutate(Significance = letters$Payment_Method[Payment_Method])

print(means)

library(effectsize)
eta_squared(anova_payment)

# ============================
# Downsampling for Ratings
# ============================
# Convert Ratings to binary factor: "Low" (1) and "High" (5)
dataFinal1$Rating_Factor <- factor(dataFinal1$Ratings_Num, levels = c(1, 5), labels = c("Low", "High"))
table(dataFinal1$Rating_Factor)  # Check original class imbalance

# Perform downsampling
set.seed(123)
data_balanced_down <- downSample(
  x = dataFinal1[, !(names(dataFinal1) %in% c("Rating_Factor", "Ratings_Num"))],
  y = dataFinal1$Rating_Factor,
  yname = "Rating_Factor"
)

# Reassign Ratings_Num
data_balanced_down$Ratings_Num <- ifelse(data_balanced_down$Rating_Factor == "High", 5, 1)

# Create plots
plot_before <- ggplot(dataFinal1, aes(x = Rating_Factor, fill = Rating_Factor)) +
  geom_bar() +
  labs(title = "Before Downsampling", x = "Rating", y = "Count") +
  theme_minimal()

plot_after <- ggplot(data_balanced_down, aes(x = Rating_Factor, fill = Rating_Factor)) +
  geom_bar() +
  labs(title = "After Downsampling", x = "Rating", y = "Count") +
  theme_minimal()

# Combine side by side
plot_before + plot_after

# Logistic model on balanced data
data_balanced$Satisfied <- ifelse(data_balanced$Ratings_Num == 5, 1, 0)
logit_model_bal <- glm(Satisfied ~ Income + Payment_Method + Total_Purchases,
                       data = data_balanced,
                       family = binomial)
summary(logit_model_bal)

# ============================
# Clustering
# ============================
# Run k-means on both datasets
# Standardize features
data_cluster_orig <- scale(dataFinal1[, c("Total_Amount", "Total_Purchases", "Ratings_Num")])
data_cluster_down <- scale(data_balanced_down[, c("Total_Amount", "Total_Purchases", "Ratings_Num")])

# Apply k-means
set.seed(123)
kmeans_orig <- kmeans(data_cluster_orig, centers = 3)
kmeans_down <- kmeans(data_cluster_down, centers = 3)

# Assign cluster labels
dataFinal1$Cluster <- as.factor(kmeans_orig$cluster)
data_balanced_down$Cluster <- as.factor(kmeans_down$cluster)

library(patchwork)

# --- Total Amount by Cluster ---
plot1_orig <- ggplot(dataFinal1, aes(x = Cluster, y = Total_Amount, fill = Cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFDDC1", "#FFC3A0", "#D5AAFF")) +
  labs(title = "Original: Total Amount by Cluster") +
  theme_minimal()

plot1_down <- ggplot(data_balanced_down, aes(x = Cluster, y = Total_Amount, fill = Cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFDDC1", "#FFC3A0", "#D5AAFF")) +
  labs(title = "Downsampled: Total Amount by Cluster") +
  theme_minimal()

plot1_orig + plot1_down

# Tile plot of Ratings vs Purchases:
plot2_orig <- ggplot(dataFinal1, aes(x = Cluster, y = Ratings_Num, fill = Total_Purchases)) +
  geom_tile() +
  scale_fill_gradient(low = "#FFDDC1", high = "#FF3B3B") +
  labs(title = "Original: Ratings vs Purchases") +
  theme_minimal()

plot2_down <- ggplot(data_balanced_down, aes(x = Cluster, y = Ratings_Num, fill = Total_Purchases)) +
  geom_tile() +
  scale_fill_gradient(low = "#FFDDC1", high = "#FF3B3B") +
  labs(title = "Downsampled: Ratings vs Purchases") +
  theme_minimal()

plot2_orig + plot2_down

# ============================
# Trend Analysis by Month-Year
# ============================
monthly_ratings <- dataFinal1 %>%
  group_by(YearMonth, Payment_Method) %>%
  summarize(Average_Rating = mean(Ratings_Num, na.rm = TRUE))

ggplot(monthly_ratings, aes(x = YearMonth, y = Average_Rating, color = Payment_Method, group = Payment_Method)) +
  geom_line(size = 1) + # Increase line size for better visibility
  labs(title = "Monthly Satisfaction Trends by Payment Method",
       x = "Month-Year", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Angle for readability
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = "right") +
  scale_color_brewer(palette = "Dark2") # Use a qualitative palette for clearer distinction

monthly_ratings$Payment_Method <- as.factor(monthly_ratings$Payment_Method)
monthly_ratings$YearMonth <- as.yearmon(monthly_ratings$YearMonth)
monthly_ratings$YearMonth_Num <- as.numeric(monthly_ratings$YearMonth)

monthly_ratings$YearMonth_Num <- as.numeric(monthly_ratings$YearMonth) # Convert to numeric for lm
linear_model <- lm(Average_Rating ~ YearMonth_Num + Payment_Method, data = monthly_ratings)
summary(linear_model) # Print summary for review if needed

ggplot(monthly_ratings, aes(x = YearMonth, y = Average_Rating, color = Payment_Method, group = Payment_Method)) +
  geom_line(size = 0.8, alpha = 0.6) + # Slightly thinner and transparent lines
  geom_smooth(method = "lm", se = FALSE, size = 1.2) + # Smoother, thicker trend lines
  labs(title = "Linear Trend of Satisfaction by Payment Method",
       x = "Month-Year", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Angle for readability
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = "right") +
  scale_color_brewer(palette = "Set1") # Another distinct qualitative palette


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

# Declare Rating_num
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 5,NA ))

# Calculate the proportion of each rating for each gender
rating_proportions <- dataFinal1 %>%
  group_by(Gender, Ratings_Num) %>%
  summarize(Count = n(), .groups = 'drop_last') %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

# Plot for visualizing ratings by gender
library(scales)
library(ggplot2)
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
    plot.subtitle = element_text(hjust = 0.5, size = 11, margin = ggplot2::margin(b = 10)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )

# Summary Statistics by Gender, Age, and Country
dataFinal1 %>%
  group_by(Gender, Age, Country) %>% # Use capitalized Age and Country for column names
  summarize(
    Mean_Rating = mean(Ratings_Num, na.rm = TRUE),
    SD_Rating = sd(Ratings_Num, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  ) %>%
  arrange(Country, Age, Gender) # Ensure column names here match your actual data (capitalized)

# Check for interaction between Gender and Age
model_gender_age <- aov(Ratings_Num ~ Gender * Age, data = dataFinal1) # Use capitalized Age
summary(model_gender_age)

# Check for interaction between Gender and Country
model_gender_country <- aov(Ratings_Num ~ Gender * Country, data = dataFinal1) # Use capitalized Country
summary(model_gender_country)

# Plot Ratings by Gender, faceted by Age
ggplot(dataFinal1, aes(x = Gender, y = Ratings_Num, fill = Gender)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "gray40") +
  scale_fill_manual(values = c("female" = "pink", "male" = "blue")) +
  labs(title = "Distribution of Ratings by Gender (by Age Group)",
       x = "Gender", y = "Rating (1–5)") +
  theme_minimal() +
  facet_wrap(~ Age, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Make x-axis labels vertical

# Plot Ratings by Gender, faceted by Country
ggplot(dataFinal1, aes(x = Gender, y = Ratings_Num, fill = Gender)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "gray40") +
  scale_fill_manual(values = c("female" = "pink", "male" = "blue")) +
  labs(title = "Distribution of Ratings by Gender (by Country)",
       x = "Gender", y = "Rating (1–5)") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Make x-axis labels vertical

# Plot Ratings by Gender, faceted by both Age and Country
ggplot(dataFinal1, aes(x = Gender, y = Ratings_Num, fill = Gender)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "gray40") +
  scale_fill_manual(values = c("female" = "pink", "male" = "blue")) +
  labs(title = "Distribution of Ratings by Gender (by Age & Country)",
       x = "Gender", y = "Rating (1–5)") +
  theme_minimal() +
  facet_grid(Country ~ Age, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Make x-axis labels vertical

# Add mean comparison
dataFinal1 %>%
  group_by(Gender) %>%
  summarize(Mean_Rating = mean(Ratings_Num, na.rm = TRUE),
            SD = sd(Ratings_Num, na.rm = TRUE),
            N = n())

# To predict average rating per gende
library(emmeans)
emmeans(model_full, pairwise ~ Gender)
# For interaction effects
emmeans(model_full, pairwise ~ Gender | Country)
emmeans(model_full, pairwise ~ Gender | Age)

# Model Assumptions Check (for Linear Models)
par(mfrow = c(2, 2))
plot(model_full)

# Ensure variables exist and are properly formatted
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)

dataFinal1 <- dataFinal1 %>%
  mutate(
    Age = as.numeric(Age),
    Country = as.factor(Country),
    Gender = as.factor(Gender),
  ) %>%
  filter(!is.na(Churn))  # Removes rows with missing churn data

# Standardize Age and churn
dataFinal1$Churn <- ifelse(dataFinal1$Subscription == "New", 1, 0)
dataFinal1$Age_scaled <- scale(dataFinal1$Age)

set.seed(123)
trainIndex <- createDataPartition(dataFinal1$Churn, p = 0.8, list = FALSE)
trainData <- dataFinal1[trainIndex, ]
testData <- dataFinal1[-trainIndex, ]


# Train logistic regression model
churn_model <- glm(Churn ~ Gender + Ratings_Num + Age_scaled + Country, data = trainData, family = binomial)
summary(churn_model)


# Train Random Forest model
set.seed(123)
rf_churn_model <- randomForest(Churn ~ Gender + Ratings_Num + Age_scaled + Country, data = trainData, ntree = 500, importance = TRUE)

# Feature Importance Plot
varImpPlot(rf_churn_model)

# Visualize churn trends by Age & Country
ggplot(trainData, aes(x = Age_scaled, fill = as.factor(Churn))) +
  geom_histogram(binwidth = 0.5, alpha = 0.7, position = "identity") +
  facet_wrap(~ Country) +
  labs(
    title = "Churn Distribution by Age and Country",
    x = "Standardized Age",
    y = "Count of Customers"
  ) +
  theme_minimal()

# 3. How do demographic factors like age and gender correlate with purchasing behavior (e.g., Total_Purchases, Total_Amount) and subsequent ratings?
library(ggplot2)

# --- Visualizations incorporating Feedback and Ratings_Num ---
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                 ifelse(dataFinal1$Ratings == "high", 5,NA ))

dataFinal1$Ratings_Binary <- ifelse(dataFinal1$Ratings == "low", 0, 
                                    ifelse(dataFinal1$Ratings == "high", 1,NA ))


# 1. Distribution of Purchases by Age Group (Age vs. Total_Purchases)
#    Adding Feedback and Ratings_Num

# Option A: Facet by Feedback, color/size by Ratings_Num
# This allows you to see age-purchase trends for each feedback type,
# and how the volume/intensity of ratings relates to those points.
ggplot(dataFinal1, aes(x = Age, y = Total_Purchases, color = Gender)) +
  geom_point(aes(size = Ratings_Num), alpha = 0.6) + # Size of point indicates Ratings_Num
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + # Add trend line without standard error band
  facet_wrap(~ Feedback, scales = "free_y") + # Separate plots for each Feedback type
  labs(title = "Purchasing Trends by Age, Gender, and Feedback Type",
       subtitle = "Point size indicates Number of Ratings",
       x = "Age", y = "Total Purchases", color = "Gender", size = "Number of Ratings") +
  theme_minimal() + # A clean theme
  theme(plot.title = element_text(hjust = 0.5), # Center title
        plot.subtitle = element_text(hjust = 0.5)) # Center subtitle

# compared the upsample, downsample and oversample
# Check Class Distribution
table(dataFinal1$Feedback)
table(dataFinal1$Ratings_Binary)

# Downsampling (for imbalance correction)
library(dplyr)

# Downsample Feedback
set.seed(123)
min_n <- dataFinal1 %>% count(Feedback) %>% summarise(min_n = min(n)) %>% pull()
dataDown_Feedback <- dataFinal1 %>% group_by(Feedback) %>% sample_n(min_n)

# Downsample Ratings_Binary
min_n_rating <- dataFinal1 %>% count(Ratings_Binary) %>% summarise(min_n = min(n)) %>% pull()
dataDown_Rating <- dataFinal1 %>% filter(!is.na(Ratings_Binary)) %>%
  group_by(Ratings_Binary) %>% sample_n(min_n_rating)

# Before
prop.table(table(dataFinal1$Feedback))
prop.table(table(dataFinal1$Ratings_Binary))

# After
prop.table(table(dataDown_Feedback$Feedback))
prop.table(table(dataDown_Rating$Ratings_Binary))

# Upsample Feedback
library(dplyr)

# Find the maximum class size
max_n <- dataFinal1 %>%
  count(Feedback) %>%
  summarise(max_n = max(n)) %>%
  pull()

# Upsample each class
dataUp_Feedback <- dataFinal1 %>%
  group_by(Feedback) %>%
  slice_sample(n = max_n, replace = TRUE) %>%
  ungroup()

# Upsample Ratings_Binary
library(ROSE)
dataUp_Rating <- ovun.sample(Ratings_Binary ~ ., data = dataFinal1[!is.na(dataFinal1$Ratings_Binary),], method = "over", N = max(table(dataFinal1$Ratings_Binary)) * 2)$data

# Before
prop.table(table(dataFinal1$Feedback))
prop.table(table(dataFinal1$Ratings_Binary))

# After
prop.table(table(dataUp_Feedback$Feedback))
prop.table(table(dataUp_Rating$Ratings_Binary))

# Oversampling Rating_Num
library(ROSE)

# Filter out missing values
data_bal_input <- dataFinal1[!is.na(dataFinal1$Ratings_Binary), ]
data_bal_input$Ratings_Binary <- as.factor(data_bal_input$Ratings_Binary)

# Oversample Ratings_Binary
data_OverRating <- ovun.sample(Ratings_Binary ~ ., 
                               data = data_bal_input, 
                               method = "over", 
                               N = max(table(data_bal_input$Ratings_Binary)) * 2)$data

# Oversampling feedback
library(dplyr)

# Step 1: Find the max class size
max_n <- dataFinal1 %>% 
  count(Feedback) %>% 
  summarise(max_n = max(n)) %>% 
  pull()

# Step 2: Upsample each class to the max size
data_OverFeedback <- dataFinal1 %>%
  group_by(Feedback) %>%
  slice_sample(n = max_n, replace = TRUE) %>%
  ungroup()

# Before
prop.table(table(dataFinal1$Feedback))
prop.table(table(dataFinal1$Ratings_Binary))

# After
prop.table(table(data_OverRating$Feedback))
prop.table(table(dataUp_Rating$Ratings_Binary))

# Set Up model
library(caret)
library(rpart)
library(e1071)

evaluate_model_metrics <- function(data, target_col, label) {
  set.seed(123)
  
  data <- data[!is.na(data[[target_col]]), ]
  data[[target_col]] <- as.factor(data[[target_col]])
  
  trainIndex <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  trainData <- data[trainIndex, ]
  testData  <- data[-trainIndex, ]
  
  model <- rpart(as.formula(paste(target_col, "~ .")), data = trainData, method = "class")
  preds <- predict(model, testData, type = "class")
  
  cm <- confusionMatrix(preds, testData[[target_col]])
  
  # Extract key metrics
  data.frame(
    Method = label,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    F1 = 2 * ((cm$byClass["Sensitivity"] * cm$byClass["Pos Pred Value"]) /
                (cm$byClass["Sensitivity"] + cm$byClass["Pos Pred Value"]))
  )
}

results <- rbind(
  evaluate_model_metrics(dataFinal1, "Ratings_Binary", "Original"),
  evaluate_model_metrics(dataDown_Rating, "Ratings_Binary", "Downsampled"),
  evaluate_model_metrics(dataUp_Rating, "Ratings_Binary", "Upsampled"),
  evaluate_model_metrics(data_OverRating, "Ratings_Binary", "Oversampled")
)

# --- Combine all datasets into a single dataframe ---
# Add a 'Dataset_Source' column to identify the original dataset
dataFinal1$Dataset_Source <- "dataFinal1"
dataDown_Rating$Dataset_Source <- "dataDown_Rating"
dataUp_Rating$Dataset_Source <- "dataUp_Rating"
data_OverRating$Dataset_Source <- "data_OverRating"

combined_data <- rbind(dataFinal1, dataDown_Rating, dataUp_Rating, data_OverRating)

# --- Visualize All Four Datasets Simultaneously ---
# Using facet_wrap to create a grid of plots, one for each Dataset_Source
ggplot(combined_data, aes(x = Age, y = Total_Purchases, color = Gender)) +
  geom_point(aes(size = Ratings_Num), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  # Facet by both Dataset_Source and Feedback
  facet_grid(Dataset_Source ~ Feedback, scales = "free") + # Using 'free' to allow scales to vary per facet
  labs(title = "Purchasing Trends by Age, Gender, Feedback, and Dataset Source",
       subtitle = "Point size indicates Number of Ratings",
       x = "Age", y = "Total Purchases", color = "Gender", size = "Number of Ratings") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"), # Make facet labels bold
    panel.spacing = unit(1, "lines") # Add some space between panels
  )

# --- Library used ---
library(ggplot2)
library(randomForest)
library(caret)
library(dplyr)

# --- 1. Select dataset ---
ml_data <- dataDown_Rating

# --- 2. Preprocess: Convert categorical variables to factors ---
ml_data$Gender <- as.factor(ml_data$Gender)
ml_data$Feedback <- as.factor(ml_data$Feedback)
ml_data$Ratings_Binary <- as.factor(ml_data$Ratings_Binary)

# --- 3. Create a categorical target: Purchase_Level ---
# Cut Total_Purchases into 3 levels: Low (0–2), Medium (3–5), High (6+)
ml_data$Purchase_Level <- cut(
  ml_data$Total_Purchases,
  breaks = c(-Inf, 2, 5, Inf),
  labels = c("Low", "Medium", "High")
)
ml_data$Purchase_Level <- as.factor(ml_data$Purchase_Level)

# Optional: Check class distribution
table(ml_data$Purchase_Level)

# --- 4. Split into training and testing sets ---
set.seed(42)
train_index <- createDataPartition(ml_data$Purchase_Level, p = 0.8, list = FALSE)
train_data <- ml_data[train_index, ]
test_data  <- ml_data[-train_index, ]

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows :", nrow(test_data), "\n")

# --- 5. Train Random Forest Classifier ---
set.seed(123)
rf_class_model <- randomForest(
  Purchase_Level ~ Age + Gender + Income + Feedback + Ratings_Num + Ratings_Binary + Total_Amount,
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

cat("\n--- Random Forest Classification Model Summary ---\n")
print(rf_class_model)

# --- 6. Predict on test set ---
predictions <- predict(rf_class_model, newdata = test_data)

# --- 7. Evaluate Model ---
conf_matrix <- confusionMatrix(predictions, test_data$Purchase_Level)
cat("\n--- Confusion Matrix ---\n")
print(conf_matrix)

# --- 8. Visualize Variable Importance ---
varImpPlot(rf_class_model, main = "Variable Importance (Random Forest Classifier)")

# --- 1. Confusion Matrix Heatmap ---
library(ggplot2)
library(reshape2)

# Convert confusion matrix to data frame for ggplot
cm_table <- as.data.frame(conf_matrix$table)
names(cm_table) <- c("Actual", "Predicted", "Freq")

# Plot heatmap
ggplot(cm_table, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#e0f7fa", high = "#006064") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Actual Purchase Level",
       y = "Predicted Purchase Level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- 2. Predicted vs. Actual Scatter Plot ---
# Add predictions to test_data for plotting
test_data$Predicted_Level <- predictions

ggplot(test_data, aes(x = Purchase_Level, fill = Predicted_Level)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Actual vs Predicted Purchase Levels",
    x = "Actual Purchase Level",
    y = "Count",
    fill = "Predicted Level"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 4. How do age and gender groups differ in satisfaction ratings and feedback patterns based on total spending and order outcomes (order status and shipping method)?
library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggthemes)

# Convert columns to factors
dataFinal1$Gender <- as.factor(dataFinal1$Gender)
dataFinal1$Order_Status <- as.factor(dataFinal1$Order_Status)
dataFinal1$Shipping_Method <- as.factor(dataFinal1$Shipping_Method)

# Create Age Groups
dataFinal1 <- dataFinal1 %>%
  mutate(Age_Group = cut(Age, 
                         breaks = c(0, 25, 35, 45, 60, 100), 
                         labels = c("18-25", "26-35", "36-45", "46-60", "60+"),
                         right = FALSE),
         Demographic = paste(Gender, Age_Group, sep = "_"))

# Summary table with Ratings_Num
summary_dataFinal1 <- dataFinal1 %>%
  group_by(Demographic, Order_Status, Shipping_Method) %>%
  summarise(
    Avg_Rating = mean(Ratings_Num, na.rm = TRUE),
    Avg_Spending = mean(Total_Amount, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  ) %>%
  drop_na(Avg_Rating) %>%
  arrange(desc(Avg_Rating))

# Plot average rating by demographic, order status, and shipping method
ggplot(summary_dataFinal1, aes(x = fct_reorder(Demographic, Avg_Rating), y = Avg_Rating, fill = Order_Status)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Shipping_Method) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Ratings by Demographics, Order Status, and Shipping Method",
       x = "Demographic (Gender_Age Group)",
       y = "Average Rating (1 = Low, 5 = High)")

# Machine learning (Random Forest)
# --- Setup: Load Libraries ---
library(tidyverse) # Includes ggplot2, dplyr, forcats, etc.
library(ggplot2)
library(dplyr)
library(forcats)
library(ggthemes) # For additional themes like theme_economist, theme_wsj etc.
library(randomForest) # For Random Forest model
library(caret) # For data splitting, confusion matrix, and feature importance
library(reshape2) # For reshaping data for the confusion matrix heatmap


# --- SECTION 1: Data Loading and Initial Preprocessing ---

# Convert columns to factors
dataFinal1$Gender <- as.factor(dataFinal1$Gender)
dataFinal1$Order_Status <- as.factor(dataFinal1$Order_Status)
dataFinal1$Shipping_Method <- as.factor(dataFinal1$Shipping_Method)
# Ensure any other relevant columns like Feedback, Ratings_Binary are also factors if they exist
# dataFinal1$Feedback <- as.factor(dataFinal1$Feedback) # Uncomment if Feedback is in dataFinal1
# dataFinal1$Ratings_Binary <- as.factor(dataFinal1$Ratings_Binary) # Uncomment if Ratings_Binary is in dataFinal1


# Create Age Groups
dataFinal1 <- dataFinal1 %>%
  mutate(Age_Group = cut(Age,
                         breaks = c(0, 25, 35, 45, 60, 100),
                         labels = c("18-25", "26-35", "36-45", "46-60", "60+"),
                         right = FALSE), # right=FALSE means intervals are [a,b)
         Demographic = paste(Gender, Age_Group, sep = "_"))


# --- SECTION 2: Summary Statistics and Initial Visualization (Average Ratings) ---

# Summary table with Avg_Rating and Avg_Spending
# Ensure Ratings_Num and Total_Amount columns exist in your dataFinal1
summary_dataFinal1 <- dataFinal1 %>%
  group_by(Demographic, Order_Status, Shipping_Method) %>%
  summarise(
    Avg_Rating = mean(Ratings_Num, na.rm = TRUE),
    Avg_Spending = mean(Total_Amount, na.rm = TRUE),
    Count = n(),
    .groups = 'drop' # Drop grouping structure for the result
  ) %>%
  drop_na(Avg_Rating) %>% # Remove rows where Avg_Rating is NA
  arrange(desc(Avg_Rating)) # Arrange by Avg_Rating in descending order

cat("\n--- Summary Table of Average Ratings & Spending ---\n")
print(head(summary_dataFinal1)) # Print first few rows of the summary table

# Plot average rating by demographic, order status, and shipping method
cat("\n--- Plot: Average Ratings by Demographics, Order Status, and Shipping Method ---\n")
ggplot(summary_dataFinal1, aes(x = fct_reorder(Demographic, Avg_Rating), y = Avg_Rating, fill = Order_Status)) +
  geom_col(position = position_dodge()) + # Use dodged columns for comparison
  facet_wrap(~ Shipping_Method) + # Separate plots by Shipping_Method
  coord_flip() + # Flip coordinates for horizontal bars
  theme_minimal() +
  labs(title = "Average Ratings by Demographics, Order Status, and Shipping Method",
       x = "Demographic (Gender_Age Group)",
       y = "Average Rating (1 = Low, 5 = High)",
       fill = "Order Status") + # Label the fill legend
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        strip.text = element_text(face = "bold")) # Bold facet labels


# --- SECTION 3: Machine Learning - Random Forest Classifier ---

# For example: dataDown_Rating <- read.csv("your_down_sampled_data.csv")
# 1. Select dataset for ML (using the dummy dataDown_Rating for this example)
ml_data <- dataDown_Rating

# 2. Preprocess: Convert categorical variables to factors for ML
ml_data$Gender <- as.factor(ml_data$Gender)
ml_data$Feedback <- as.factor(ml_data$Feedback)
ml_data$Ratings_Binary <- as.factor(ml_data$Ratings_Binary)

# 3. Create a categorical target: Purchase_Level
# Cut Total_Purchases into 3 levels: Low, Medium, High
# Adjust breaks based on your actual Total_Purchases distribution in dataDown_Rating
ml_data$Purchase_Level <- cut(
  ml_data$Total_Purchases,
  breaks = c(-Inf, 3, 6, Inf),  # Adjust these based on distribution
  labels = c("Low", "Medium", "High"),
  right = FALSE
)
ml_data$Purchase_Level <- as.factor(ml_data$Purchase_Level)

# Check class distribution of the target variable
cat("\n--- Purchase_Level Distribution in ML Data ---\n")
print(table(ml_data$Purchase_Level))

# 4. Split data into training and testing sets
set.seed(42) # For reproducibility
train_index <- createDataPartition(ml_data$Purchase_Level, p = 0.8, list = FALSE)
train_data <- ml_data[train_index, ]
test_data  <- ml_data[-train_index, ]

cat("\n--- Data Split Summary for ML ---\n")
cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows :", nrow(test_data), "\n")

# 5. Train Random Forest Classifier
set.seed(123) # For reproducibility of the Random Forest model
rf_class_model <- randomForest(
  Purchase_Level ~ Age + Gender + Income + Feedback + Ratings_Num + Ratings_Binary + Total_Amount,
  data = train_data,
  ntree = 500, # Number of trees in the forest
  mtry = 3, # Number of variables randomly sampled at each split
  importance = TRUE # Crucial for getting variable importance scores
)

cat("\n--- Random Forest Classification Model Summary ---\n")
print(rf_class_model)

# 6. Predict on test set
predictions <- predict(rf_class_model, newdata = test_data)

# 7. Evaluate Model (Confusion Matrix and overall statistics)
conf_matrix <- confusionMatrix(predictions, test_data$Purchase_Level)
cat("\n--- Confusion Matrix Results ---\n")
print(conf_matrix)


# --- SECTION 4: Upgraded Model Visualizations ---

## Upgraded Feature Importance Plot (using ggplot2)
cat("\n--- Upgraded Variable Importance Plot (Random Forest) ---\n")
# Extract importance from the randomForest model (type=2 for Mean Decrease Gini)
importance_df <- as.data.frame(importance(rf_class_model, type = 2))
importance_df$Feature <- rownames(importance_df)
colnames(importance_df)[1] <- "Importance" # Rename the first column for clarity

# Sort by importance in descending order
importance_df <- importance_df %>%
  arrange(desc(Importance))

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#2E8B57") + # A nice shade of green for bars
  coord_flip() + # Flip coordinates for horizontal bars (easier to read feature names)
  labs(title = "Feature Importance (Random Forest Classifier)",
       subtitle = "Based on Mean Decrease Gini Index",
       x = "Feature", y = "Importance") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted") # Add vertical dotted lines
  )

#  Upgraded Confusion Matrix Heatmap (using ggplot2)
library(ggplot2)
library(caret)   # for `confusionMatrix()`
library(dplyr)   # optional for data prep

ggplot(cm_table, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq), size = 6, fontface = "bold", color = "black") +
  scale_fill_gradient(low = "#BBDEFB", high = "#1976D2", name = "Count") +
  labs(
    title = "Confusion Matrix Heatmap: Purchase Level Prediction",
    x = "Actual Purchase Level",
    y = "Predicted Purchase Level",
    subtitle = paste("Accuracy:", round(conf_matrix$overall['Accuracy'], 3),
                     "| Kappa:", round(conf_matrix$overall['Kappa'], 3))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "grey30"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA, linewidth = 1.5)
  )


# Downsampling vs Oversampling Comparison in Random Forest
# --- Libraries ---
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
library(reshape2)
library(patchwork)

# --- Function: Train + Evaluate Random Forest ---
train_rf_model <- function(data, dataset_name) {
  data$Gender <- as.factor(data$Gender)
  data$Feedback <- as.factor(data$Feedback)
  data$Ratings_Binary <- as.factor(data$Ratings_Binary)
  
  # Ensure target exists
  if (!"Purchase_Level" %in% names(data)) {
    data$Purchase_Level <- cut(
      data$Total_Purchases,
      breaks = c(-Inf, 3, 6, Inf),
      labels = c("Low", "Medium", "High"),
      right = FALSE
    )
    data$Purchase_Level <- as.factor(data$Purchase_Level)
  }
  
  # Train-test split
  set.seed(42)
  train_index <- createDataPartition(data$Purchase_Level, p = 0.8, list = FALSE)
  train_data <- data[train_index, ]
  test_data  <- data[-train_index, ]
  
  # Train Random Forest
  set.seed(123)
  rf_model <- randomForest(
    Purchase_Level ~ Age + Gender + Income + Feedback + Ratings_Num + Ratings_Binary + Total_Amount,
    data = train_data,
    ntree = 500,
    mtry = 3,
    importance = TRUE
  )
  
  # Predict and evaluate
  predictions <- predict(rf_model, newdata = test_data)
  conf_mat <- confusionMatrix(predictions, test_data$Purchase_Level)
  
  list(model = rf_model, conf_matrix = conf_mat, title = paste("Confusion Matrix:", dataset_name))
}

# ================================
# --- Run for Downsampled Data ---
# ================================
# --- Add Purchase_Level to Downsampled Data ---
dataDown_Rating$Purchase_Level <- cut(
  dataDown_Rating$Total_Purchases,
  breaks = c(-Inf, 3, 7, Inf),  # Adjust these based on your data's distribution (0–10 range)
  labels = c("Low", "Medium", "High"),
  right = FALSE
)


# Assumes Purchase_Level already exists
result_down <- train_rf_model(dataDown_Rating, "Down-Sampled")

# =========================================
# --- Manual Oversampling Before Modeling ---
# =========================================

# Step 1: Create Purchase_Level
data_OverRating$Purchase_Level <- cut(
  data_OverRating$Total_Purchases,
  breaks = c(-Inf, 3, 6, Inf),
  labels = c("Low", "Medium", "High"),
  right = FALSE
)

# Step 2: Drop NAs if any
data_OverRating <- data_OverRating %>% drop_na(Purchase_Level)

# Step 3: Oversample using caret::upSample
balanced_data <- upSample(
  x = data_OverRating %>% select(-Purchase_Level),
  y = data_OverRating$Purchase_Level,
  yname = "Purchase_Level"
)

# Step 4: Train RF on oversampled data
result_over <- train_rf_model(balanced_data, "Over-Sampled (Fixed)")

# =================================
# --- Print Accuracy Comparison ---
# =================================
cat("\n--- Accuracy Comparison ---\n")
cat("Down-Sampled Accuracy:", round(result_down$conf_matrix$overall['Accuracy'], 3), "\n")
cat("Over-Sampled Accuracy:", round(result_over$conf_matrix$overall['Accuracy'], 3), "\n")

# ===============================
# --- Enhanced Confusion Matrix Plotter ---
# ===============================
plot_conf_matrix <- function(conf_matrix, title_text) {
  library(ggplot2)
  library(dplyr)
  
  # Convert confusion matrix to data frame
  cm_table <- as.data.frame(conf_matrix$table)
  
  # Ensure column names are clear
  colnames(cm_table) <- c("Actual", "Predicted", "Freq")
  
  ggplot(cm_table, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = Freq), size = 5, fontface = "bold", color = "black") +
    scale_fill_gradient(low = "#D7EAFB", high = "#1E88E5", name = "Count") +
    labs(
      title = title_text,
      x = "Actual Class",
      y = "Predicted Class",
      subtitle = paste0("Accuracy: ", round(conf_matrix$overall['Accuracy'], 3),
                        " | Kappa: ", round(conf_matrix$overall['Kappa'], 3))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11, color = "gray30"),
      legend.title = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 1.2)
    )
}

# ===============================
# --- Generate and Combine Plots ---
# ===============================
library(patchwork)

plot1 <- plot_conf_matrix(result_down$conf_matrix, result_down$title)
plot2 <- plot_conf_matrix(result_over$conf_matrix, result_over$title)

# Combine side-by-side
combined_plot <- plot1 + plot2 + plot_layout(ncol = 2)

# Show plot
print(combined_plot)

--------------------------------------------------------------------------------------------------------------------------------------------
