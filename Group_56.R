# NOTE: 
# 1. include good programming practices such as comments, variable naming conventions and indentation
# 2. validation should be done for each entry from the users to avoid logical errors
# 3. perform additional research to further understand the information on the given dataset during evaluation of data
# 4. the analysis should be meaningful and effective in providing the information for decision-making

# data import
dataFinal1 <- read.csv("cleaned_data.csv")
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
# customer income levels, ratings
# To Investigate the relationship between customer income levels and product ratings to determine if income impacts satisfaction 
# Do income levels (low, medium, high) and country influence customer satisfaction, as reflected in product ratings?
# Contingency table for Income, Country, and Ratings
income_country_ratings_table <- table(dataFinal1$Income, dataFinal1$Country, dataFinal1$Ratings)

# Summarize distribution across income levels and countries
dataFinal1 %>%
  group_by(Income, Country) %>%
  summarize(Count = n())

# Perform chi-square test on Income and Ratings with Country as context
chi_income_country_ratings <- chisq.test(table(dataFinal1$Income, dataFinal1$Ratings))
chi_income_country_ratings

#deep insights
chi_income_country_ratings$expected
chi_income_country_ratings$observed

#visualize
install.packages("vcd")
library(vcd)
mosaic(~ Income + Ratings, data = dataFinal1, shade = TRUE)

#proof for assumptions
pairwise.prop.test(table(dataFinal1$Income, dataFinal1$Ratings), p.adjust.method = "bonferroni")

library(vcd)

mosaic(~ Income + Ratings, data = dataFinal1, shade = TRUE, legend = TRUE)

ggplot(dataFinal1, aes(x = Income, fill = Ratings)) +
  geom_bar(position = "fill") +  # Stacked proportions
  labs(title = "Proportion of Ratings by Income Level", x = "Income Level", y = "Proportion") +
  theme_minimal()

-----------------------------------
# Is there a relationship between customers income and spending tier, their satisfaction levels and month, as shown by product ratings?
  dataFinal1 <- dataFinal1 %>%
  mutate(Spending_Tier = case_when(
    Total_Amount < 500 ~ "Low",
    Total_Amount >= 500 & Total_Amount < 1000 ~ "Medium",
    Total_Amount >= 1000 ~ "High"
  ))

dataFinal1$Spending_Tier <- factor(dataFinal1$Spending_Tier, levels = c("Low", "Medium", "High"))

  ggplot(dataFinal1, aes(x = Spending_Tier, y = Ratings, color = Income, group = Income)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +  # Use `linewidth` instead of `size`
  labs(title = "Trend Analysis: Income vs Ratings Across Spending Tiers",
       x = "Spending Tier", y = "Average Ratings") +
  theme_minimal()
  
dataFinal1$Ratings_Num <- ifelse(dataFinal1$Ratings == "low", 1, 
                                   ifelse(dataFinal1$Ratings == "high", 5,NA ))

anova_model <- aov(Ratings_Num ~ Income * Spending_Tier, data = dataFinal1)
summary(anova_model)

reg_model <- lm(Ratings_Num ~ Income + Spending_Tier, data = dataFinal1)
summary(reg_model)

dataFinal1$Ratings_Binary <- ifelse(dataFinal1$Ratings_Num == 1, 0, 
                                    ifelse(dataFinal1$Ratings_Num == 5, 1, NA))

log_model <- glm(Ratings_Binary ~ Income + Spending_Tier, family = binomial, data = dataFinal1)
summary(log_model)

#assumptions 1
aggregate(Ratings_Num ~ Income, data = dataFinal1, mean)

aov_income <- aov(Ratings_Num ~ Income, data = dataFinal1)
summary(aov_income)

TukeyHSD(aov_income)

dataFinal1$Ratings_Factor <- factor(dataFinal1$Ratings_Num, levels = c(1, 5), labels = c("Low", "High"))

ggplot(dataFinal1, aes(x = Income, fill = Ratings_Factor)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of High and Low Ratings by Income Level",
       x = "Income Level", y = "Proportion") +
  theme_minimal()

chisq.test(table(dataFinal1$Income, dataFinal1$Ratings_Num))

-------------------------------
# Which spending tier , payment method and total amount receive the highest ratings, and how does this reflect customer preferences? 
install.packages("factoextra")
library(cluster)
library(factoextra)

dataFinal1$Income <- as.numeric(as.factor(dataFinal1$Income))
dataFinal1$Spending_Tier <- as.numeric(as.factor(dataFinal1$Spending_Tier))

# Select relevant variables for clustering
data_cluster <- dataFinal1 %>%
  select(Income, Spending_Tier, Ratings_Num, Total_Amount) %>%
  scale()  # Standardize for better clustering

# Run K-Means with optimal clusters
set.seed(123)
kmeans_result <- kmeans(data_cluster, centers = 4)  # Adjust centers based on patterns

# Add cluster labels to dataset
dataFinal1$Cluster <- factor(kmeans_result$cluster)

# Visualize clusters
fviz_cluster(kmeans_result, data = data_cluster, geom = "point")

#deep insight
install.packages("ClusterR")
# Extract correct cluster labels
cluster_labels <- kmeans_result$cluster  # Use "cluster" instead of "Clusters"

# Create a dataframe with cluster assignments
data_clustered <- data_cluster %>%
  as.data.frame() %>%
  mutate(Cluster = factor(cluster_labels))  # Ensure Cluster is a factor

# Now visualize clusters using ggplot2
library(ggplot2)
ggplot(data_clustered, aes(x = Income, y = Ratings_Num, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "K-Means Clustering of Customer Ratings",
       x = "Income Level", y = "Ratings") +
  theme_minimal()

aggregate(cbind(Income, Ratings_Num, Total_Amount) ~ Cluster, data = data_clustered, mean)

ggplot(data_clustered, aes(x = Cluster, fill = factor(Income))) +
  geom_bar(position = "fill") +
  labs(title = "Income Distribution Across Clusters",
       x = "Cluster", y = "Proportion") +
  theme_minimal()

table(data_clustered$Cluster)

data_clustered$Ratings_Factor <- cut(data_clustered$Ratings_Num, 
                                     breaks = 2, 
                                     labels = c("low", "high"))

ggplot(data_clustered, aes(x = Cluster, y = Income, fill = Ratings_Factor)) +
  geom_boxplot(outlier.shape = NA) +  # Hide extreme outliers that might skew results
  labs(title = "Income Distribution Across Customer Clusters",
       x = "Cluster", y = "Income Level") +
  theme_minimal()

#machine learning
#prepare dataset
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

# Split into 40% train (6000 rows) & 10% test (1500 rows)
trainIndex <- sample(1:nrow(dataSubset), size = 6000)  
trainData <- dataSubset[trainIndex, ]
testData <- dataSubset[-trainIndex, ]

trainData$Income <- as.numeric(trainData$Income)
trainData$Spending_Tier <- as.factor(trainData$Spending_Tier)  # If categorical
trainData$Ratings_Binary <- as.numeric(trainData$Ratings_Binary)  # Ensure binary format

trainData$Income <- scale(trainData$Income)
trainData$Total_Amount <- scale(trainData$Total_Amount)


# Convert to matrices for XGBoost
x_train <- as.matrix(trainData[, c("Income", "Spending_Tier", "Total_Amount", 
                                   "PaymentMethod_Cash", "PaymentMethod_PayPal", 
                                   "PaymentMethod_DebitCard", "PaymentMethod_CreditCard")])
y_train <- trainData$Ratings_Binary

x_test <- as.matrix(testData[, c("Income", "Spending_Tier", "Total_Amount", 
                                 "PaymentMethod_Cash", "PaymentMethod_PayPal", 
                                 "PaymentMethod_DebitCard", "PaymentMethod_CreditCard")])
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

xgb.importance(model = xgb_model)

xgb_predictions <- predict(xgb_model, x_test)
xgb_pred_class <- ifelse(xgb_predictions > 0.5, 1, 0)

library(caret)
confusionMatrix(factor(xgb_pred_class), factor(y_test))
#the end bcs i cannot handle more of it

------------------------------------
# Does the choice of payment method , total purchases and income level impact customer satisfaction as represented by product ratings?
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

library(cluster)

data_cluster <- scale(dataFinal1[, c("Total_Amount", "Total_Purchases", "Ratings_Num")])
kmeans_result <- kmeans(data_cluster, centers = 3)
dataFinal1$Cluster <- kmeans_result$cluster

ggplot(dataFinal1, aes(x = Total_Amount, y = Ratings_Num, color = factor(Cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = "Customer Clusters Based on Spending & Satisfaction",
       x = "Total Amount Spent", y = "Product Ratings",
       color = "Cluster") +
  theme_minimal()

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
# Step 1: Convert Month Names to Numeric Values (Handles Lowercase)
dataFinal1$Month <- match(tolower(dataFinal1$Month), tolower(month.name))

# Step 2: Create Year-Month as a Date Format
dataFinal1$YearMonth <- as.Date(paste(dataFinal1$Year, dataFinal1$Month, "01", sep = "-"), format = "%Y-%m-%d")

# Step 3: Extract Year-Month for Trend Analysis
dataFinal1$YearMonth <- format(dataFinal1$YearMonth, "%Y-%m")

monthly_ratings <- dataFinal1 %>%
  group_by(YearMonth, Payment_Method) %>%
  summarize(Average_Rating = mean(Ratings_Num))

ggplot(monthly_ratings, aes(x = YearMonth, y = Average_Rating, color = Payment_Method, group = Payment_Method)) +
  geom_line() +
  labs(title = "Monthly Satisfaction Trends by Payment Method",
       x = "Month-Year", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Test whether month-year trends predict satisfaction, while accounting for payment method
linear_model <- lm(Average_Rating ~ YearMonth + Payment_Method, data = monthly_ratings)
summary(linear_model)

#Gives a clear visual of whether satisfaction ratings increase or decrease over time
ggplot(monthly_ratings, aes(x = YearMonth, y = Average_Rating, color = Payment_Method, group = Payment_Method)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Linear Trend of Satisfaction by Payment Method",
       x = "Month-Year", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#end
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

-------------------------------------------------------------------------------------------------
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
