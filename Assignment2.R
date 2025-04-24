library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(mice)

# Data import
fileUrl = "C:\\Users\\User\\OneDrive\\Desktop\\Degree\\PDA\\Assignment\\retail_data.csv"
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
dataset <- dataset %>% select(-name, -email, -phone, -address, -city, -state, -zipcode, -country, -transaction_id, -customer_id, -products)

#summary(dataset)
#View(dataset)

# --- REPLACING NA values ---

# Ensure the specified columns are factors
cols_to_factor <- c("gender", "income", "customer_segment", "product_category",
                    "product_brand", "feedback", "shipping_method", "order_status", "ratings")

# Check if columns exist in the dataset before converting
cols_exist <- cols_to_factor %in% names(dataset)
if (any(!cols_exist)) {
  warning(paste("Columns not found in dataset:", paste(cols_to_factor[!cols_exist], collapse = ", ")))
  cols_to_factor <- cols_to_factor[cols_exist] # Keep only existing columns
}

for (col in cols_to_factor) {
  if (!is.null(dataset[[col]]) && !is.factor(dataset[[col]])) {
    dataset[[col]] <- as.factor(as.character(dataset[[col]]))
  }
}

# Check the structure to confirm factor conversions
str(dataset)

# --- MANUAL IMPUTATION BASED ON FORMULA ---

print("Performing manual imputation for amount, total_purchases, total_amount")

# Ensure columns exist before attempting imputation
cols_for_formula <- c("amount", "total_purchases", "total_amount")
if (all(cols_for_formula %in% names(dataset))) {
  
  # Case 1: total_amount is missing, amount and total_purchases are observed and non-zero
  missing_total_amount_idx <- which(is.na(dataset$total_amount) & !is.na(dataset$amount) & dataset$amount != 0 & !is.na(dataset$total_purchases) & dataset$total_purchases != 0)
  if (length(missing_total_amount_idx) > 0) {
    dataset$total_amount[missing_total_amount_idx] <- dataset$amount[missing_total_amount_idx] * dataset$total_purchases[missing_total_amount_idx]
    print(paste("Manually imputed", length(missing_total_amount_idx), "missing total_amount values."))
  }
  
  # Case 2: total_purchases is missing, amount and total_amount are observed, amount is non-zero
  missing_total_purchases_idx <- which(is.na(dataset$total_purchases) & !is.na(dataset$amount) & dataset$amount != 0 & !is.na(dataset$total_amount))
  if (length(missing_total_purchases_idx) > 0) {
    dataset$total_purchases[missing_total_purchases_idx] <- dataset$total_amount[missing_total_purchases_idx] / dataset$amount[missing_total_purchases_idx]
    print(paste("Manually imputed", length(missing_total_purchases_idx), "missing total_purchases values."))
  }
  
  
  # Case 3: amount is missing, total_purchases and total_amount are observed, total_purchases is non-zero
  missing_amount_idx <- which(is.na(dataset$amount) & !is.na(dataset$total_purchases) & dataset$total_purchases != 0 & !is.na(dataset$total_amount))
  if (length(missing_amount_idx) > 0) {
    dataset$amount[missing_amount_idx] <- dataset$total_amount[missing_amount_idx] / dataset$total_purchases[missing_amount_idx]
    print(paste("Manually imputed", length(missing_amount_idx), "missing amount values."))
  }
  
} else {
  warning("Amount, total_purchases, or total_amount column(s) not found for formula imputation.")
}

# --- END MANUAL IMPUTATION ---

# Identify columns with missing values
na_cols <- names(dataset)[colSums(is.na(dataset)) > 0]
cols_without_na <- names(dataset)[colSums(is.na(dataset)) == 0]

# If there are no columns with NA, no need run mice
if (length(na_cols) == 0) {
  print("No missing values found in the dataset. Imputation is not needed.")
} else {
  # Define the imputation methods - use "pmm" for all columns with NA
  methods <- rep("", ncol(dataset))
  names(methods) <- names(dataset)
  
  for(col in na_cols) {
    if(col %in% names(dataset)) {
      methods[col] <- "pmm" # Use PMM for all columns with missing values
    }
  }
  
  # --- APPLY A UNIVERSAL PREDICTOR STRATEGY ---
  
  # 1. Initialize a matrix with ALL predictors initially excluded (all 0s)
  predictor_matrix <- matrix(0, nrow = ncol(dataset), ncol = ncol(dataset),
                             dimnames = list(names(dataset), names(dataset)))
  
  # 2. Define the set of variables we want to allow as predictors universally
  universal_predictors <- c(
    "age", "gender", "income", "customer_segment", "amount", "product_category", "product_brand","total_purchases", "total_amount",
    "shipping_method", "order_status", "feedback"
  )
  
  # 3. For each variable that needs imputation (has NAs),
  # set the columns corresponding to the universal_predictors to 1.
  # Ensure the variable doesn't predict itself.
  
  # Re-identify na_cols after manual imputation BEFORE creating predictor matrix
  na_cols_after_manual <- names(dataset)[colSums(is.na(dataset)) > 0]
  
  for(row_var in na_cols_after_manual) {
    if(row_var %in% rownames(predictor_matrix)) {
      for(col_var in universal_predictors) {
        # Only set to 1 if the column exists as a predictor
        # AND the row variable is not the same as the column variable (no self-prediction)
        if(col_var %in% colnames(predictor_matrix) && row_var != col_var) {
          predictor_matrix[row_var, col_var] <- 1
        }
      }
    }
  }
  
  # Inspect the *modified* predictor matrix for the first few variables with NAs
  print("Predictor matrix rows for first few variables with NAs:")
  print(head(predictor_matrix[na_cols, universal_predictors, drop = FALSE])) # Show subset using universal_predictors
  
  # --- Run the mice imputation with the manually created predictor matrix ---
  
  print(paste("Imputing missing values in columns:", paste(na_cols, collapse = ", ")))
  
  # Before running the mice() function, after loading and cleaning your data
  # Check the unique values and counts for payment_method in the original dataset
  
  # --- Handle payment_method separately (e.g., impute with mode) ---
  if("payment_method" %in% names(dataset) && colSums(is.na(dataset))["payment_method"] > 0) {
    # Find the mode (most frequent value) among observed cases
    observed_payment_methods <- dataset$payment_method[!is.na(dataset$payment_method)]
    if (length(observed_payment_methods) > 0) {
      # Calculate mode - handle potential multiple modes by taking the first
      payment_method_mode <- names(sort(table(observed_payment_methods), decreasing = TRUE))[1]
      
      # Impute NAs with the mode
      dataset$payment_method[is.na(dataset$payment_method)] <- payment_method_mode
    } else {
      print("No observed values in payment_method to calculate mode.")
      # Decide how to handle if no observed values at all - maybe impute with a placeholder or remove.
    }
    
    # Remove payment_method from na_cols so mice doesn't try to impute it anymore
    na_cols <- names(dataset)[colSums(is.na(dataset)) > 0]
  }
  
  imputed_data <- mice(dataset,
                       method = methods,
                       predictorMatrix = predictor_matrix, # <-- Use the manually created matrix
                       m = 5,
                       seed = 456) 
  
  # Summary of the imputation
  print("Mice imputation summary:")
  print(summary(imputed_data))
  
  # --- Extract a complete dataset and check NA counts ---
  
  # Extract the first complete dataset (you can choose any from 1 to m)
  complete_dataset_1 <- complete(imputed_data, 1)
  
  
  # Make sure to use the dataset *before* any separate imputation if you did that
  original_dataset_before_mice <- dataset
  
  rows_with_na_in_imputed_cols <- which(rowSums(is.na(original_dataset_before_mice[, na_cols])) > 0)
  
  # View the original rows with NAs
  if(length(rows_with_na_in_imputed_cols) > 0) {
    View(original_dataset_before_mice[rows_with_na_in_imputed_cols, ])
  } else {
    print("No rows had NAs in the columns that were imputed by mice.")
  }
  
  
  # View the corresponding rows in the first complete dataset (with imputed values)
  if(length(rows_with_na_in_imputed_cols) > 0) {
    View(complete_dataset_1[rows_with_na_in_imputed_cols, ])
  } else {
    print("No corresponding rows to view in the complete datasets.")
  }
}