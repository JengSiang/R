library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(mice)

# Data import
fileUrl = "retail_data.csv"
dataset = read.csv(fileUrl)
dataset

#Lowercase all column name
names(dataset) <- tolower(names(dataset))
str(dataset)
View(dataset)

#Check NA value in each column
na_counts <- colSums(is.na(dataset))
print("NA counts by column:")
print(na_counts)

#Check empty string in each column
empty_string_counts <- colSums(dataset == "", na.rm = TRUE)
print("Empty string counts by column:")
print(empty_string_counts)

# Loop through each column to trim
for (col in names(dataset)) {
  # If the column is character or factor
  if (is.character(dataset[[col]]) || is.factor(dataset[[col]])) {
    # Convert to character (in case it's factor), then trim
    dataset[[col]] <- trimws(as.character(dataset[[col]]))
  }
}

#Replace empty strings into NA
dataset[] <- lapply(dataset, function(x) {
  if (is.character(x) || is.factor(x)) {
    x[x == ""] <- NA
  }
  return(x)
})

#Check for duplicated rows in the entire dataset
duplicate_rows <- sum(duplicated(dataset))
print(paste("Number of completely duplicated rows:", duplicate_rows))
#Remove duplicated rows
dataset <- dataset[!duplicated(dataset),]
print(paste("Rows after removing duplicates:", nrow(dataset)))
duplicate_rows <- sum(duplicated(dataset))
print(paste("Number of completely duplicated rows:", duplicate_rows))

# Combine date and time into a single datetime column
dataset$date_time <- as.POSIXct(paste(dataset$date, dataset$time), format="%m/%d/%Y %H:%M:%S")
head(dataset$date_time)
# Remove date, time, year, month
dataset <- dataset %>% select(-date, -time, -year, -month)
# Remove NA in data_time
dataset <- dataset[!is.na(dataset$date_time), ]
sum(is.na(dataset$date_time))

# Drop Name, Email, Phone, Address, City, State, ZipCode, Country
dataset <- dataset %>% select(-name, -email, -phone, -address, -city, -state, -zipcode, -country, -transaction_id, -customer_id)

summary(dataset)
View(dataset)

# Check for any remaining NA values
final_na_counts <- colSums(is.na(dataset))
print("Final NA counts:")
print(final_na_counts)

# Check data types of all columns
#print("Final column types:")
#print(sapply(dataset, class))

# 1. Identify rows with missing values
rows_with_na <- which(rowSums(is.na(dataset)) > 0)
dataset_complete <- dataset[-rows_with_na, ]
dataset_na <- dataset[rows_with_na, ]

cat("Number of rows with missing values:", nrow(dataset_na), "\n")
cat("Number of rows without missing values:", nrow(dataset_complete), "\n")

if (nrow(dataset_na) > 0) {
  # 2. Identify columns with missing values in the subset with NAs
  missing_cols_na <- names(dataset_na)[colSums(is.na(dataset_na)) > 0]
  cat("Columns with missing values in the NA subset:", missing_cols_na, "\n")
  
  # 3. Perform MICE imputation ONLY on the subset with missing values
  # Be mindful of the predictors available in 'dataset_na'.
  # MICE needs non-NA values in the predictor columns to perform imputation.
  imputed_data_na <- mice(dataset_na, m = 5, maxit = 50, seed = 123)
  
  # 4. Get the completed subset
  dataset_na_mice <- complete(imputed_data_na, 1)
  
  # 5. Verify that there are no more missing values in the imputed subset
  cat("\n📊 Summary of missing values after MICE imputation on NA subset:\n")
  print(colSums(is.na(dataset_na_mice)))
  
  # 6. Add the imputed rows back to the complete dataset
  dataset_mice <- rbind(dataset_complete, dataset_na_mice)
  
  # Optional: Sort the combined dataset back to the original order if needed
  # dataset_mice <- dataset_mice[order(as.numeric(rownames(dataset))), ]
} else {
  # If there were no rows with missing values
  dataset_mice <- dataset_complete
  cat("\nNo rows with missing values found. Skipping MICE.\n")
}

# 7. Final check for missing values in the entire dataset
cat("\n📊 Summary of missing values in the final dataset after potential MICE:\n")
print(colSums(is.na(dataset_mice)))
