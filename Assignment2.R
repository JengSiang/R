library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(VIM)
library(mice)
library(randomForest)

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
dataset <- dataset %>% select(-name, -email, -phone, -address, -city, -state, -zipcode, -country, -transaction_id, -customer_id)

summary(dataset)
View(dataset)

# Check for any remaining NA values
final_na_counts <- colSums(is.na(dataset))
print("Final NA counts:")
print(final_na_counts)

# Check data types of all columns
print("Final column types:")
print(sapply(dataset, class))


# Store subset of data
data_income_subset <- subset(dataset, is.na(dataset$income))
data_gender_subset <- subset(dataset, is.na(dataset$gender))
data_customer_segment_subset <- subset(dataset, is.na(dataset$customer_segment))
data_shipping_method_subset <- subset(dataset, is.na(dataset$shipping_method))
data_payment_method_subset <- subset(dataset, is.na(dataset$payment_method))



md.pattern(dataset)

aggr_plot <-aggr(dataset, col=c('navyblue','red'),
                 numbers = TRUE,
                 sortVars = TRUE,
                 labels = names(data),
                 cex.axis = .7,
                 gap = 3,
                 ylab = c("Histogram of Missing Data","Pattern"))

dataset <- as.data.frame(dataset)
dataset[] <- lapply(dataset, function(x) {
  if (is.character(x)) as.factor(x) else x
})

method_vec <- make.method(dataset)

method_vec[] <- ""
method_vec[c("gender", "income", "customer_segment", "payment_method", "shipping_method")] <- "rf"

pred_matrix <- make.predictorMatrix(dataset)

vars_to_impute <- c("gender", "income", "customer_segment", "payment_method", "shipping_method")
pred_matrix[,] <- 0

for (var in vars_to_impute) {
  pred_matrix[var, setdiff(vars_to_impute, var)] <- 1
}

set.seed(1)
imputed_data <- mice(dataset, m = 5, method = method_vec, predictorMatrix = pred_matrix)
summary(imputed_data)
completed_data <- complete(imputed_data)
sapply(completed_data[vars_to_impute], function(x) sum(is.na(x)))


# Gender Distribution
ggplot(completed_data, aes(x = gender)) +
  geom_bar() +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count")

# Income Distribution
ggplot(completed_data, aes(x = income)) +
  geom_bar() +
  labs(title = "Distribution of Income", x = "Income", y = "Count")

# Customer Segment Distribution
ggplot(completed_data, aes(x = customer_segment)) +
  geom_bar() +
  labs(title = "Distribution of Customer Segment", x = "Customer Segment", y = "Count")

# Contingency Table between Gender and Income
table_gender_income <- table(completed_data$gender, completed_data$income)

# Logistic regression between Gender and Income
model <- glm(gender ~ income + shipping_method + payment_method, data = completed_data, family = "binomial")


method_vec <- make.method(dataset)

method_vec[] <- ""
method_vec[c("ratings", "feedback")] <- "rf"

pred_matrix <- make.predictorMatrix(dataset)

vars_to_impute <- c("ratings", "feedback")
pred_matrix[,] <- 0

for (var in vars_to_impute) {
  pred_matrix[var, setdiff(vars_to_impute, var)] <- 1
}

set.seed(2)
imputed_data <- mice(completed_data, m = 5, method = method_vec, predictorMatrix = pred_matrix)
summary(imputed_data)
completed_data1 <- complete(imputed_data)
sapply(completed_data1[vars_to_impute], function(x) sum(is.na(x)))

vars_to_impute_all <- c("gender", "income", "customer_segment", "payment_method", "shipping_method", "ratings", "feedback")
sapply(completed_data1[vars_to_impute_all], function(x) sum(is.na(x)))
