# total amount spend









# product brand
data <- read.csv("retail_data.csv")

# check data type
str(data)

# see unique values
unique(data$Product_Brand)
# identified issues:
# 1. empty strings ("") - missing brand names
# 2. misspelled brand names - Whirepool, Mitsubhisi

sum(is.na(data$Product_Brand))
# missing values - 0

# replace empty strings to Unknown
data$Product_Brand[data$Product_Brand == ""] <- "Unknown"

# correct known misspellings
data$Product_Brand[data$Product_Brand == "Whirepool"] <- "Whirlpool"
data$Product_Brand[data$Product_Brand == "Mitsubhisi"] <- "Mitsubishi"

# standardize brand names to lowercase
data$Product_Brand <- tolower(data$Product_Brand)

valid_brands <- c("nike", "samsung", "penguin books", "home depot", "nestle", "apple", "zara", "random house", "coca-cola", "adidas", "pepsi", "ikea", "harpercollins", "bed bath & beyond", "sony", "whirlpool", "mitsubishi", "bluestar", "unknown")

# identify invalid entries
invalid_brands <- setdiff(unique(data$Product_Brand), valid_brands)
print(invalid_brands)
# character(0) - no invalid entries in the Product_Brand column

# re-check unique values after cleaning
unique(data$Product_Brand)

# frequency of each brand
table(data$Product_Brand)