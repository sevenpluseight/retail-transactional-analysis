# NOTE: 
# 1. include good programming practices such as comments, variable naming conventions and indentation
# 2. validation should be done for each entry from the users to avoid logical errors
# 3. perform additional research to further understand the information on the given dataset during evaluation of data
# 4. the analysis should be meaningful and effective in providing the information for decision-making

# data import (cleaned)
dataFinal1 <- read.csv("datasets/cleaned_data.csv")
View(dataFinal1)

# required libraries: dplyr, ggplot2, tidyr
library(dplyr)
library(ggplot2)
library(tidyr)
-----------------------------------------------------------------------
# Elysha Sophia binti Ridzuan - TP071162
# customer income levels, ratings, etc
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
  geom_text(data = dplyr::filter(plot_data_long, Statistic == "Mean"),
            aes(label = round(Rating_Value, 2)),
            hjust = -0.5, size = 3.5, fontface = "bold", color = "#34495E") +
  scale_color_manual(values = c("Min" = "#EE6C4D", "Median" = "#2C3E50", "Mean" = "#3D7A9F")) +
  scale_shape_manual(values = c("Min" = 17, "Median" = 15, "Mean" = 16)) +
  scale_x_continuous(breaks = seq(1, 5, by = 0.5), limits = c(0.8, 5.2), expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Rating Distribution by Subscription Status",
       subtitle = "Min, Median, and Mean Customer Ratings per Group",
       x = "Rating Value", y = "Subscription Status",
       color = "Rating Statistic", shape = "Rating Statistic") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

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

# Extract odds ratios and confidence intervals
odds_df <- data.frame(
  Subscription = c("Intercept (New)", "Regular vs New", "Premium vs New"),
  OR = exp(coef(logit_model)),  # Odds ratio
  Lower_CI = exp(confint(logit_model)[,1]),  # Lower confidence interval
  Upper_CI = exp(confint(logit_model)[,2])   # Upper confidence interval
)

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
library(forcats)
ggplot(odds_df %>% arrange(OR), aes(x = OR, y = fct_inorder(Subscription))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#E4572E", linewidth = 0.9) +
  geom_pointrange(aes(xmin = Lower_CI, xmax = Upper_CI, color = Effect),
                  size = 1.5, linewidth = 1.5,
                  fatten = 2) +
  geom_text(aes(label = round(OR, 2)),
            hjust = -0.3,
            size = 4.5, fontface = "bold", color = "#2C3E50") +
  scale_color_manual(values = c("Increased Odds" = "#2CA4B0", "Decreased Odds" = "#EE6C4D", "No Significant Effect" = "#8D99AE")) +
  scale_x_continuous(breaks = seq(0, max(odds_df$Upper_CI) * 1.1, by = 0.5),
                     limits = c(min(0.1, odds_df$Lower_CI) * 0.9, max(odds_df$Upper_CI) * 1.2),
                     trans = "log10") +
  labs(title = "Likelihood of High Ratings Across Subscription Groups",
       subtitle = "Odds Ratios (relative to 'New' subscribers) and 95% Confidence Intervals",
       x = "Odds Ratio (OR) [Log Scale]", y = "Subscription Comparison") +
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

library(pROC)
# Compute predicted probabilities from logistic regression model
dataFinal1$Predicted_Prob <- predict(logit_model, type = "response")

# Generate ROC curve
roc_curve <- roc(dataFinal1$High_Rating, dataFinal1$Predicted_Prob)

# Plot ROC curve
plot(roc_curve, col = "#2CA4B0", lwd = 3, main = "ROC Curve for Subscription-Based Ratings Prediction")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Print AUC (Area Under Curve) for model evaluation - 0.5522
auc(roc_curve)

--------------------------------------------------------------------------------
# analysis 2 - Are subscribed customers more likely to leave higher ratings through feedback compared to non-subscribed customers?
# Create count of high ratings per subscription group
dataFinal1$High_Rating <- ifelse(dataFinal1$Ratings_Num == 5, 1, 
                                  ifelse(dataFinal1$Ratings_Num == 1, 0, NA))

prop_data <- table(dataFinal1$Subscription, dataFinal1$High_Rating)

# Run two-proportion Z-test
# Measures if subscribed customers are significantly more likely to leave high ratings than non-subscribers
print(prop_data)
prop_test <- prop.test(x = prop_data[,2], n = rowSums(prop_data))
print(prop_test)

# Calculate proportions within each Subscription group
prop_df_proportions <- prop_df %>%
  group_by(Subscription) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

# Plot for visualizing the proportion of high vs. low ratings by subscription
ggplot(prop_df_proportions, aes(x = Subscription, y = Percentage, fill = factor(High_Rating))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("0" = "grey", "1" = "blue"),
                    labels = c("Low Rating", "High Rating"),
                    name = "Rating Type") +
  labs(title = "Proportion of High vs. Low Ratings by Subscription Group",
       subtitle = "Percentage of ratings in each category per subscription type",
       x = "Subscription Status",
       y = "Percentage of Ratings") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 12, color = "#34495E", face = "bold"),
    axis.text.y = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ECF0F1", linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA),
    panel.background = element_rect(fill = "#FDFDFD", color = NA),
    axis.line.x = element_line(color = "#BDC3C7", linewidth = 0.7),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Create contingency table for Subscription vs. Feedback categories
feedback_table <- table(dataFinal1$Subscription, dataFinal1$Feedback)

# Run Chi-Square Test
# Determines if subscription status significantly influences feedback type (Bad, Average, Good, Excellent)
chisq_test <- chisq.test(feedback_table)
print(chisq_test)

feedback_df <- dataFinal1 %>%
  select(Subscription, Feedback) %>%
  mutate(
    # Create a new column 'Feedback_Category' by remapping the original 'Feedback' values
    Feedback_Category = case_when(
      Feedback == "bad" ~ "Negative",
      Feedback == "average" ~ "Neutral",
      Feedback %in% c("good", "excellent") ~ "Positive",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    Feedback_Category = factor(Feedback_Category, levels = c("Negative", "Neutral", "Positive"))
  ) %>%
  group_by(Subscription, Feedback_Category) %>%
  summarise(Count = n(), .groups = 'drop') %>% # Count occurrences and drop grouping
  ungroup() # Remove grouping structure for subsequent operations

# Plot feedback distributions
library(scales)
ggplot(feedback_df, aes(x = Subscription, y = Count, fill = Feedback_Category)) +
  geom_bar(stat = "identity", position = "fill", width = 0.8, color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(Count / tapply(Count, Subscription, sum)[Subscription], accuracy = 1)),
            position = position_fill(vjust = 0.5), size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Negative" = "#EE6C4D", "Neutral" = "#FFD23F", "Positive" = "#3D7A9F")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Feedback Distribution by Subscription Type",
    subtitle = "Proportion of Negative, Neutral, and Positive Feedback per Group",
    x = "Subscription Type",
    y = "Proportion of Feedback",
    fill = "Feedback Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 12, color = "#34495E", face = "bold"),
    axis.text.y = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ECF0F1", linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA),
    panel.background = element_rect(fill = "#FDFDFD", color = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
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
library(xgboost)
library(caret)
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert feedback labels to numerical values
dataFinal1$Feedback_Num <- as.numeric(factor(dataFinal1$Feedback, 
                                             levels = c("bad", "average", "good", "excellent"), 
                                             labels = c(1, 2, 3, 4)))

# Select relevant features
feature_columns <- c("Subscription", "Income", "Total_Purchases", "Product_Category", 
                     "Order_Status", "Ratings_Num", "Year", "Month", "Shipping_Method", "Product_Brand")
dataFinal1_selected <- dataFinal1[, feature_columns]

# Convert categorical variables to numeric (one-hot encoding)
dummy_vars <- dummyVars(~ ., data = dataFinal1_selected, fullRank = TRUE)
dataFinal1_processed <- predict(dummy_vars, newdata = dataFinal1_selected)
colnames(dataFinal1_processed) <- make.names(colnames(dataFinal1_processed), unique = TRUE)

# Divide the dataset into training (70%) and testing (30%)
set.seed(123)
trainIndex <- createDataPartition(dataFinal1$Feedback_Num, p = 0.7, list = FALSE)
trainData <- dataFinal1_processed[trainIndex, ]
testData <- dataFinal1_processed[-trainIndex, ]

trainLabels <- dataFinal1$Feedback_Num[trainIndex]
testLabels <- dataFinal1$Feedback_Num[-trainIndex]

# Apply oversampling to balance feedback categories
trainData <- as.data.frame(trainData)
trainLabels <- as.factor(trainLabels)
trainData$class <- trainLabels

# Apply oversampling
trainData_balanced <- upSample(x = trainData, y = trainData$class)

# Check the dataset after oversampling - 1, 2, 3, 4: 70572
table(trainData_balanced$class)

# Ensure all columns are numeric
testData <- as.data.frame(testData)
testData[] <- lapply(testData, as.numeric)

# Convert all column names to lowercase to ensure consistency
colnames(trainData_balanced) <- tolower(colnames(trainData_balanced))
colnames(testData) <- tolower(colnames(testData))

# Remove 'class' column before creating trainData
trainData <- trainData_balanced[, !colnames(trainData_balanced) %in% "class"]
trainData[] <- lapply(trainData, as.numeric)

# Ensure testData structure matches trainData
missing_cols <- setdiff(colnames(trainData), colnames(testData))
testData[missing_cols] <- 0  # Add missing columns with default value
testData <- testData[, colnames(trainData)]  # Match column order

# Convert into XGBoost format
trainMatrix <- xgb.DMatrix(data = as.matrix(trainData), label = as.numeric(as.character(trainData_balanced$class)))
testMatrix <- xgb.DMatrix(data = as.matrix(testData), label = testLabels)

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

# Feature Importance Analysis
importance <- xgb.importance(model = xgb_model_optimized)
print(importance)
# Top 7 feature importance analysis
# 1:                    ratings_num 9.535082e-01 0.106572304 0.007844297
# 2:         product_brandwhirlpool 6.165025e-03 0.056769790 0.009884208
# 3:             product_brandpepsi 4.252880e-03 0.056122862 0.023601872
# 4:        product_brandmitsubishi 4.008055e-03 0.053581309 0.008573540
# 5:                       monthmay 3.770792e-03 0.053443765 0.016309436
# 6:                    monthaugust 3.711088e-03 0.085083748 0.019512195
# 7:            subscriptionpremium 3.210237e-03 0.041577385 0.030766199

# Predict feedback ratings on test data
predictions_optimized <- predict(xgb_model_optimized, testMatrix)

# Save the trained model into an .rds file
saveRDS(xgb_model_optimized, file = "saved_models/Hew_Pik_Rou/analysis2_xgb_model.rds")
xgb_model <- readRDS("saved_models/Hew_Pik_Rou/analysis2_xgb_model.rds") # Read when needed

# Evaluate model performance
library(Metrics)
mae_score <- mae(testLabels, predictions_optimized)
rmse_score <- rmse(testLabels, predictions_optimized)

print(paste("Mean Absolute Error (MAE):", round(mae_score, 2)))
print(paste("Root Mean Squared Error (RMSE):", round(rmse_score, 2)))

# Round predicted values
results_df <- data.frame(Actual = testLabels, Predicted = round(predictions_optimized, 2))
results_df$Predicted <- round(results_df$Predicted)

# Compute confusion matrix - 0.6074
confusionMatrix(as.factor(results_df$Predicted), as.factor(results_df$Actual))

# Print first few predicted values vs actual ratings
head(results_df)

# Count correctly predicted ratings - 60.59%
correct_predictions <- sum(round(predictions_optimized) == testLabels)
accuracy_rate <- correct_predictions / length(testLabels)
print(paste("Prediction Accuracy:", round(accuracy_rate * 100, 2), "%"))

# Define month labels
month_labels <- c("January", "February", "March", "April", "May", "June", 
                  "July", "August", "September", "October", "November", "December"
                  )

# Convert predicted values to nearest integer ratings
rounded_predictions <- round(predictions_optimized)

# Convert test labels and predictions to factors with consistent levels
rounded_predictions <- factor(rounded_predictions, levels = c(1, 2, 3, 4))
testLabels <- factor(testLabels, levels = c(1, 2, 3, 4))

# Create a mapping rule for subscription categories
dataFinal1$Subscription_Status <- ifelse(dataFinal1$Subscription %in% c("premium", "regular"), "Subscribed", "Non-Subscribed")

# Create results_df with Subscription_Status included
results_df <- data.frame(
  Actual = testLabels,
  Predicted = rounded_predictions,
  Month = dataFinal1$Month[testIndex],
  Subscription_Status = dataFinal1$Subscription_Status[testIndex]
)

# Generate overall confusion matrix - 0.6059
conf_matrix <- confusionMatrix(rounded_predictions, testLabels)
print(conf_matrix)

# Compute specificity for subscribed and non-subscribed customers
subscribed_indices <- which(results_df$Subscription_Status == "Subscribed")
nonsubscribed_indices <- which(results_df$Subscription_Status == "Non-Subscribed")

conf_matrix_subscribed <- confusionMatrix(
  factor(rounded_predictions[subscribed_indices], levels = levels(testLabels)), 
  factor(testLabels[subscribed_indices], levels = levels(testLabels))
)

conf_matrix_nonsubscribed <- confusionMatrix(
  factor(rounded_predictions[nonsubscribed_indices], levels = levels(testLabels)), 
  factor(testLabels[nonsubscribed_indices], levels = levels(testLabels))
)

# Compute specificity for each class (1 to 4)
specificity_scores_subscribed <- sapply(levels(testLabels), function(class) {
  specificity(conf_matrix_subscribed$table, negative = class)
})

specificity_scores_nonsubscribed <- sapply(levels(testLabels), function(class) {
  specificity(conf_matrix_nonsubscribed$table, negative = class)
})

print(specificity_scores_subscribed)
# 1 - 0.6811992
# 2 - 0.4765286
# 3 - 0.7233101
# 4 - 0.5526149

print(specificity_scores_nonsubscribed)
# 1 - 0.6954143
# 2 - 0.4303659
# 3 - 0.6257275
# 4 - 0.6296258

test_original_subscriptions <- dataFinal1$Subscription[-trainIndex] # Get original Subscription for test data

results_df_cleaned <- results_df %>%
  mutate(
    # Make sure Predicted_Rating_Num is numeric for calculations
    Predicted_Rating_Num = as.numeric(as.character(Predicted)),
    # Re-factor Month with the global month_labels for correct order
    Month = factor(Month, levels = month_labels),
    # Assign Subscription_Status correctly for the test set
    Subscription_Status = ifelse(test_original_subscriptions %in% c("premium", "regular"), "Subscribed", "Non-Subscribed"),
    # Convert to factor with explicit levels for consistent plotting and legend order
    Subscription_Status = factor(Subscription_Status, levels = c("Non-Subscribed", "Subscribed"))
  )

# Calculate the average predicted rating per month per customer type
average_predicted_ratings_monthly <- results_df_cleaned %>%
  group_by(Month, Subscription_Status) %>%
  summarise(
    Avg_Predicted_Rating = mean(Predicted_Rating_Num),
    .groups = 'drop'
  ) %>%
  ungroup()

# Identify the month(s) with the highest average predicted rating for each customer type
highest_rating_months <- average_predicted_ratings_monthly %>%
  group_by(Subscription_Status) %>%
  filter(Avg_Predicted_Rating == max(Avg_Predicted_Rating)) %>%
  ungroup()

# Construct the dynamic subtitle string
subtitle_text <- highest_rating_months %>%
  rowwise() %>%
  mutate(info = paste0(Subscription_Status, ": ", Month, " (Avg. Rating: ", round(Avg_Predicted_Rating, 2), ")")) %>%
  ungroup() %>%
  pull(info) %>%
  paste(collapse = " | ")

full_subtitle <- paste0("Months with highest predicted ratings are marked with a star (*). \n",
                        "Highest predicted ratings: ", subtitle_text)

# Plot for visualizing comparison of actual vs. predicted feedback ratings in the future
library(scales)
ggplot(average_predicted_ratings_monthly, aes(x = Month, y = Avg_Predicted_Rating, color = Subscription_Status, group = Subscription_Status)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_point(data = highest_rating_months, aes(x = Month, y = Avg_Predicted_Rating, color = Subscription_Status),
             shape = 8, size = 6, stroke = 1.5) +
  scale_color_manual(values = c("Subscribed" = "#4CAF50", "Non-Subscribed" = "#FF9800")) +
  scale_y_continuous(breaks = seq(1, 4, by = 0.25), limits = c(min(average_predicted_ratings_monthly$Avg_Predicted_Rating)*0.95, 4.1)) +
  labs(
    title = "Predicted Average Customer Rating Trend Over Months",
    subtitle = full_subtitle,
    x = "Month",
    y = "Average Predicted Rating",
    color = "Customer Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "#34495E"),
    axis.text.x = element_text(size = 11, color = "#7F8C8D", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11, color = "#7F8C8D"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, color = "#34495E", face = "bold"),
    legend.text = element_text(size = 11, color = "#7F8C8D"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ECF0F1", linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#FDFDFD", color = NA),
    panel.background = element_rect(fill = "#FDFDFD", color = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

--------------------------------------------------------------------------------
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
model_comparison_long$Metric <- factor(model_comparison_long$Metric, levels = c("Accuracy", "Sensitivity", "Specificity"))

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
# Train the model with 100 trees
rf_model <- randomForest(Ratings_Num ~ Gender + Age + Subscription + Income,
                         data = train_data, ntree = 500, importance = TRUE)
print(rf_model)

# Make predictions
predicted_ratings <- predict(rf_model, test_data)

# Evaluate prediction accuracy
cor(test_data$Ratings_Num, predicted_ratings)  # Check correlation between actual vs. predicted ratings

library(caret)
# Predict rating categories
predicted_class <- predict(rf_model, test_data)

# Compute confusion matrix
conf_matrix <- confusionMatrix(predicted_class, test_data$Rating_Class)

# Extract metrics
print(conf_matrix$overall["Accuracy"])  # Overall model accuracy
print(conf_matrix$byClass["Sensitivity"])  # Ability to detect High ratings
print(conf_matrix$byClass["Specificity"])  # Ability to detect Low ratings




















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
