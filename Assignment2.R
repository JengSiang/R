library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
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
dataset <- dataset %>% select(-name, -email, -phone, -address, -city, -state, -zipcode, -country)

summary(dataset)
View(dataset)

# Check for any remaining NA values
final_na_counts <- colSums(is.na(dataset))
print("Final NA counts:")
print(final_na_counts)

# Check data types of all columns
print("Final column types:")
print(sapply(dataset, class))



# 1. Age plot before prediction
dataset %>%
  mutate(age_group = ifelse(is.na(age), "Missing", 
                            as.character(cut(age, breaks = seq(0, 100, by = 5))))) %>%
  ggplot(aes(x = age_group)) +
  geom_bar(fill = "tomato", color = "white") +
  theme_minimal() +
  labs(title = "Age Distribution (Before Prediction, Including Missing Values)",
       x = "Age Group", y = "Count of Rows") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Prepare dataset
dataset_cleaned <- dataset %>%
  mutate(
    gender = ifelse(is.na(gender), "Unknown", gender),
    income = ifelse(is.na(income), "Unknown", income),
    customer_segment = ifelse(is.na(customer_segment), 
                              "Unknown", customer_segment),
    total_purchases = ifelse(is.na(total_purchases), 
                             median(total_purchases, na.rm = TRUE), 
                             total_purchases)
  ) %>%
  mutate(across(c(gender, income, customer_segment), as.factor))

# 3. Select only relevant columns
age_vars <- dataset_cleaned %>%
  select(age, gender, income, customer_segment, total_purchases)

# 4. Split data
train_age <- age_vars %>% filter(!is.na(age))
test_age <- age_vars %>% filter(is.na(age))

# 5. Train random forest
rf_model <- randomForest(age ~ ., data = train_age)

# 6. Predict
predicted_ages <- predict(rf_model, newdata = test_age)

# 7. Replace missing values in the original dataset
dataset_cleaned$age[is.na(dataset_cleaned$age)] <- predicted_ages

# 8. Age plot after prediction
dataset_cleaned %>%
  mutate(age_group = cut(age, breaks = seq(0, 100, by = 5), right = FALSE)) %>%
  ggplot(aes(x = age_group)) +
  geom_bar(fill = "skyblue", color = "white") +
  theme_minimal() +
  labs(title = "Age Group Distribution (After Prediction)", 
       x = "Age Group", 
       y = "Count of Rows") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate the missing data distribution
before_dist <- dataset %>%
  mutate(age_group = ifelse(is.na(age), "Missing", as.character(cut(age, breaks = seq(0, 100, by = 5))))) %>%
  count(age_group)

# Assign the predicted values to the missing data (NA rows)
dataset_cleaned <- dataset
dataset_cleaned$age[is.na(dataset$age)] <- predicted_ages

# Calculate the new age group distribution after imputation
after_dist <- dataset_cleaned %>%
  mutate(age_group = cut(age, breaks = seq(0, 100, by = 5), right = FALSE)) %>%
  count(age_group)

# View before and after distributions to ensure the count hasn't exceeded 172
before_dist
after_dist

# Make sure the sum of the newly imputed values equals 172
sum(after_dist$n) - sum(before_dist$n)

