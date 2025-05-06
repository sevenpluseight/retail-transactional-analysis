# data import
data <- read.csv("retail_data.csv")
View(data)

#Drop 9 columns all together
library(dplyr)
data <- data %>%
  select(-c(Name, Email, Phone, Address, Date, Time, Zipcode, State, City))

# View the updated data frame
print(data)
--------------------------------------
#Change values in the table to lowercase all together
# Using lapply to apply tolower to all character columns in the data frame
# Apply tolower to all character columns
data[] <- lapply(data, function(x) {
  if (is.character(x)) tolower(x) else x
})

# View the updated data frame
print(data)
--------------------------------------
# Remove extra space all together
# Apply trimws to all character columns in the data frame
  data[] <- lapply(data, function(x) {
    if (is.character(x)) trimws(x) else x
  })

# View the updated data frame
print(data)
-------------------------------------
#Remove duplicate data by rows
data <- distinct(data)

# View the updated data frame
print(data)
-------------------------------------
# change empty string/ "" to NA
  data[] <- lapply(data, function(x) {
    if (is.character(x)) x[x == ""] <- NA
    return(x)
  })
-------------------------------------
# transaction ID
  unique(data$Transaction_ID)
# customer ID
unique(data$Customer_ID)
# country
unique(data$Country)
# age
unique(data$Age)
# gender
unique(data$Gender)
# income
unique(data$Income)
# customer segment
  # Rename the 'Customer_Segment' column to 'Subscription'
  colnames(data)[colnames(data) == "Customer_Segment"] <- "Subscription"
# year
  unique(data$Year)
# month
  unique(data$Month)
# total purchases
  unique(data$Total_Purchases)
# amount
  unique(data$Amount)
# total amount
  unique(data$Total_Amount)
# product category
unique(data$Product_Category)
------------------------------------------
# product brand
unique(data$Product_Brand)
# identified issues:
# 1. empty strings ("") - missing brand names
# 2. misspelled brand names - Whirepool, Mitsubhisi

sum(is.na(data$Product_Brand))
# missing values - 0

# remove rows with empty strings
data <- data[duplicated(data) != "", ]

# correct known misspellings
data$Product_Brand[data$Product_Brand == "whirepool"] <- "whirlpool"
data$Product_Brand[data$Product_Brand == "mitsubhisi"] <- "mitsubishi"

# standardize brand names to lowercase
data$Product_Brand <- tolower(data$Product_Brand)

valid_brands <- c("nike", "samsung", "penguin books", "home depot", "nestle", "apple", "zara", 
                  "random house", "coca-cola", "adidas", "pepsi", "ikea", "harpercollins", 
                  "bed bath & beyond", "sony", "whirlpool", "mitsubishi", "bluestar")

# identify invalid entries
invalid_brands <- setdiff(unique(data$Product_Brand), valid_brands)

if (length(invalid_brands) > 0) {
  warning("Invalid brand names found: ", paste(invalid_brands, collapse = ", "))
}

# sort in descending order of frequency
brand_table <- sort(table(data$Product_Brand), decreasing = TRUE)
print(brand_table)
----------------------------------------------
# product type
unique(data$Product_Type)

# filter rows where Product_Type is "children's" for further inspection 
childrens_rows <- data[data$Product_Type == "Children's", ]

# filter rows where Product_Type is "tools" for further inspection 
tools_rows <- data[data$Product_Type == "Tools", ]

# view filtered rows
View(childrens_rows) # belongs to books
View(tools_rows) # belongs to home and living
---------------------------------------------------
# feedback
# change the variable to lowercase
data$Feedback <- tolower(data$Feedback)

unique(data$Feedback)
# there is 4 type of feedback from customer
# identified issue: empty string

sum(is.na(data$Feedback))
# missing value - 0

# Delete empty String
data <- data[data$Feedback != "", ]
table(data$Feedback)

#Check misspelling
#define the variable correct spelling
expected_feedback <- c("excellent", "good", "average", "bad")

#run the code to check
invalid_feedback <- data$Feedback[!data$Feedback %in% expected_feedback]
unique(invalid_feedback)
#character(0) - meaning there is no misspelling
-------------------------------------------------------------
# shipping method
unique(data$Shipping_Method)

# payment method
# change the variable to lowercase
data$Payment_Method <- tolower(data$Payment_Method)

unique(data$Payment_Method)
# there is 4 type of payment method made from customer
# identified issue: empty string

sum(is.na(data$Payment_Method))
# missing value - 0

# Delete empty String
data <- data[data$Payment_Method != "", ]
table(data$Payment_Method)

#Check misspelling
#define the variable correct spelling
expected_payment_method <- c("credit card", "debit card", "paypal", "cash")

#run the code to check
invalid_payment_method <- data$Payment_Method[!data$Payment_Method %in% expected_payment_method]
unique(invalid_payment_method)

#character(0) - meaning there is no misspelling
-----------------------------------------------------------
# order status
# change the variable to lowercase
data$Order_Status <- tolower(data$Order_Status)

unique(data$Order_Status)
# there is 4 type of order status from customer
# identified issue: empty string

sum(is.na(data$Order_Status))
# missing value - 0

# Delete empty String
data <- data[data$Order_Status != "", ]
table(data$Order_Status)

#Check misspelling
#define the variable correct spelling
expected_order_status <- c("delivered", "pending", "processing", "shipped")

#run the code to check
invalid_order_status <- data$Order_Status[!data$Order_Status %in% expected_order_status]
unique(invalid_order_status)

#character(0) - meaning there is no misspelling
------------------------------------------------------
# ratings
unique(data$Ratings)
# products
unique(data$products)

------------------------------------------------------
# install mice
  install.packages("mice")

#check the data format
#transactionID & customerID consider not meaningful for imputation = drop the columns
data <- data %>%
  select(-Transaction_ID, -Customer_ID)
#Use the cor() function to check if any columns are perfectly correlated:
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
print(correlation_matrix)
#Identify and remove columns with too many missing values or no variability:
# Remove constant columns
data <- data[, sapply(data, function(x) length(unique(x)) > 1)]
# Remove columns with excessive missing values
data <- data[, colMeans(is.na(data)) < 0.9]

#filter numeric columns
# Select only numeric columns
numeric_data <- data[, sapply(data, is.numeric)] #then return to the cor() function

# ready for imputation
summary(data)


#Imputation process
# Assuming your dataset is named 'data'
half_size <- nrow(data) / 2
first_half <- data[1:half_size, ]  # First half
second_half <- data[(half_size + 1):nrow(data), ]  # Second half

library(mice)
# Impute only the first half
imputed_first_half <- mice(first_half, m = 5, method = "pmm", maxit = 50, seed = 123)
completed_first_half <- complete(imputed_first_half, action = 1)

# Combine the imputed first half with the second half
final_data <- rbind(completed_first_half, second_half)

summary(final_data)

#Recheck the data after imputation
# Check for duplicates
# attempt 1 = no duplicate data
duplicates <- data[duplicated(data), ]
print(duplicates)

# Confirm no remaining missing values
#attempt 1 = got missing values
missing_summary <- colSums(is.na(data))
print(missing_summary)

# Inspect outlier detection (example for Total_Amount)
boxplot(data$Total_Amount, main = "Outlier Check for Total_Amount")
------------------------------------------------------------------------
# Load the mice package
library(mice)

# Check and convert character columns to factors, as mice works well with factors for categorical variables
data[] <- lapply(data, function(x) {
  if (is.character(x)) as.factor(x) else x
})

# Perform imputation on the entire dataset
imputed_data <- mice(data, m = 5, method = "pmm", maxit = 50, seed = 123)

# Review the imputation summary
summary(imputed_data)

# Extract the first completed dataset (or another, e.g., action = 2, if preferred)
final_data02 <- complete(imputed_data, action = 1)

# Save the final imputed dataset (optional: CSV format)
write.csv(final_data02, "final_data02.csv", row.names = FALSE)

# Check the final imputed dataset
print(final_data02)
------------------------------------------------------------------------------------
#used chunk
  library(mice)

# Function to process a single chunk
process_chunk <- function(chunk, file_name) {
  # Convert character columns to factors
  chunk[] <- lapply(chunk, function(x) {
    if (is.character(x)) as.factor(x) else x
  })
  
  # Perform imputation on the chunk
  imputed_chunk <- mice(chunk, m = 1, method = "pmm", maxit = 20, seed = 123)
  
  # Extract completed data
  completed_chunk <- complete(imputed_chunk, action = 1)
  
  # Save the completed chunk
  write.csv(completed_chunk, file_name, row.names = FALSE)
  
  return(completed_chunk)
}

# Process the dataset in smaller chunks (e.g., 1/8 of the data)
chunk_size <- round(nrow(data) / 8)  # Adjust the number as needed
for (i in 1:8) {
  start_row <- (i - 1) * chunk_size + 1
  end_row <- min(i * chunk_size, nrow(data))  # Avoid going beyond dataset size
  
  # Extract the chunk
  chunk <- data[start_row:end_row, ]
  
  # Process and save each chunk with a unique file name
  file_name <- paste0("final_dataSet", i, ".csv")
  process_chunk(chunk, file_name)
}

# Combine all chunks into one dataset
final_data <- do.call(rbind, lapply(1:8, function(i) {
  read.csv(paste0("final_dataSet", i, ".csv"))
}))

# Save the final combined dataset
write.csv(final_data, "final_data_full.csv", row.names = FALSE)
 
# Check the save file
getwd()

# check the 8 file of the chunk data in the deskstop or not
lapply(1:8, function(x) x)

# Combine all chunks into one dataset
final_data <- do.call(rbind, lapply(1:8, function(i) {
  +   read.csv(paste0("final_dataSet", i, ".csv"))
  + }))
final_data

# view final_data
str(final_data)

# Save the final combined dataset
write.csv(final_data, "final_data_full.csv", row.names = FALSE)
# check if there missing empty file or na values
sapply(final_data, function(x) sum(is.na(x)))

#add customerID in the table using comparing method based on old n new dataset
#import new dataset
newdata <- read.csv("final_data_full.csv")
View(newdata)
-----------------------------------------------------------------------------------
# Assuming 'data' is the old dataset and 'newdata' is the new dataset
# Extract the Customer_ID column from the old dataset
customer_ids <- data$Customer_ID

# Add Customer_ID to the new dataset
newdata_with_customer_ID <- cbind(customer_ids[1:nrow(newdata)], newdata)
colnames(newdata_with_customer_ID)[1] <- "Customer_ID"  # Rename the column appropriately

# Save this as a new file to ensure the original newdata remains untouched
write.csv(newdata_with_customer_ID, "newdata_with_customer_ID.csv", row.names = FALSE)

# View a confirmation message
print("New dataset with Customer_ID saved as 'newdata_with_customer_ID.csv'.")

#view the new file that has customerID
dataFinal <- read.csv("newdata_with_customer_ID.csv")
View(dataFinal)
-------------------------------------------------------------------------------
# the customerID has NA values
#impute only customerID, it won't afffect other variable
library(mice)

# Create a predictor matrix manually
predictorMatrix <- matrix(0, ncol = ncol(dataFinal), nrow = ncol(dataFinal))  # Initialize with 0s
colnames(predictorMatrix) <- colnames(dataFinal)
rownames(predictorMatrix) <- colnames(dataFinal)

# Allow other columns to predict Customer_ID (except itself)
predictorMatrix["Customer_ID", ] <- 1  # Set predictors for Customer_ID
predictorMatrix["Customer_ID", "Customer_ID"] <- 0  # Ensure Customer_ID is not used to predict itself

# Run the imputation process using the adjusted predictorMatrix
imputed_data <- mice(dataFinal, method = "pmm", m = 5, maxit = 50, seed = 123, predictorMatrix = predictorMatrix)

# Extract the completed dataset
final_data <- complete(imputed_data, action = 1)

# Save the updated dataset with imputed Customer_ID
write.csv(final_data, "final_data_with_imputed_Customer_ID.csv", row.names = FALSE)

# View the dataset
print("Customer_ID has been imputed successfully!")
#import dataFinal1
dataFinal1 <- read.csv("final_data_with_imputed_Customer_ID.csv")
View(dataFinal1)

#save the recent dataset into project file
getwd()
"C:/Users/ELYSHA SOPHIA/OneDrive - Asia Pacific University/Documents/retail-transactional-analysis"
 write.csv(final_data, "C:/Users/ELYSHA SOPHIA/OneDrive - Asia Pacific University/Documents/retail-transactional-analysis/final_data_with_imputed_Customer_ID.csv", row.names = FALSE)