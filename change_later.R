# NOTE: 
# 1. include good programming practices such as comments, variable naming conventions and indentation
# 2. validation should be done for each entry from the users to avoid logical errors
# 3. perform additional research to further understand the information on the given dataset during evaluation of data
# 4. the analysis should be meaningful and effective in providing the information for decision-making

# data import
data <- read.csv("retail_data.csv")

# required libraries: dplyr, ggplot2
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  libraru(ggplot2)
}
# -------------------------------
# Elysha Sophia binti Ridzuan - TP071162
# total amount spend, positive feedback likelihood
# To investigate how the total amount spent influences positive feedback
# Is there a significant relationship between the total amount spent and the likelihood of receiving positive feedback?

# How accurately can positive feedback likelihood be predicted based on the total amount spent?

# Does spending above a specific threshold significantly increase the chances of receiving positive feedback?

# Do customers in different spending tiers (low, medium, high) show varying levels of positive feedback likelihood?

# extra - Segmentation analysis – group customers into spending tiers (low, medium, high) and compare their feedback likelihood


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
