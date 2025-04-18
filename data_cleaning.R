# data import
data <- read.csv("retail_data.csv")
View(data)

# transaction ID

# customer ID

# name

# email

# phone

# address

# city

# state

# zip code

# country

# age

# gender

# income

# customer segment

# date

# year

# month

# time

# total purchases

# amount

# total amount

# product category

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
data$Product_Brand[data$Product_Brand == "Whirepool"] <- "Whirlpool"
data$Product_Brand[data$Product_Brand == "Mitsubhisi"] <- "Mitsubishi"

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

# product type

# feedback

# shipping method

# payment method

# order status

# ratings

# products