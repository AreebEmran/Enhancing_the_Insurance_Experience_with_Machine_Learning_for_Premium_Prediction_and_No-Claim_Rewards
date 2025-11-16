
insurance <- read.csv("C:\\Users\\recovery\\OneDrive - Asia Pacific University of Technology And Innovation (APU)\\Desktop\\Insurance Premium Prediction.csv")

install.packages("ggplot2")

install.packages("dplyr")
View(insurance)
#1
summary(insurance)

# 4 Dimensions of Insurance dataset
dim(insurance)

str(insurance)



#View first few rows
head(insurance)

# Check column names
colnames(insurance)


#Identify categorical variables
cat_vars <- names(insurance)[sapply(insurance, is.character)]
print(cat_vars)

#Identify numerical variables
num_vars <- names(insurance)[sapply(insurance, is.numeric)]
print(num_vars)



#Check for duplicate rows
num_dupes <- sum(duplicated(insurance))
cat("Number of Duplicates:", num_dupes, "\n")


# Check missing values per column
missing_values <- colSums(is.na(insurance))
print(missing_values)



# Load necessary library
library(ggplot2)

# Create a data frame for variable types
variable_types <- data.frame(
  Variable = colnames(insurance),
  Type = ifelse(sapply(insurance, is.numeric), "Numerical", "Categorical"),
  Subtype = c(
    "Continuous", "Nominal", "Continuous", "Nominal", "Discrete",
    "Ordinal", "Nominal", "Continuous", "Nominal", "Nominal",
    "Discrete", "Discrete", "Continuous", "Discrete", "Continuous",
    "Nominal", "Ordinal", "Nominal", "Ordinal", "Nominal"
  )
)

# Plot the distribution of variable types
ggplot(variable_types, aes(x = Type, fill = Subtype)) +
  geom_bar() +
  labs(title = "Distribution of Variable Types",
       x = "Variable Type",
       y = "Count") +
  theme_minimal()











#Check unique values for all columns
for (col in colnames(insurance)) {
  unique_values <- unique(insurance[[col]])
  num_unique_values <- length(unique_values)
  print(paste("Column:", col, "| Number of Unique Values:", num_unique_values))
}



#Data frame with missing and blank values
missing_blank_counts <- data.frame(
  Column = colnames(insurance),
  Missing_Values = colSums(is.na(insurance)),
  Blank_Values = colSums(insurance == "", na.rm = TRUE)
)

# Print the table
print(missing_blank_counts)



# Load necessary library
library(ggplot2)

# List of numerical variables
num_vars <- c("Age", "Annual.Income", "Health.Score", "Credit.Score", "Premium.Amount")

# Create histograms for numerical variables
for (var in num_vars) {
  print(
    ggplot(insurance, aes(x = .data[[var]])) +
      geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
      labs(
        title = paste("Distribution of", var),
        x = var,
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  )
}






#Visualizations


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Create ranges for Annual Income
insurance$Income.Range <- cut(insurance$Annual.Income,
                              breaks = c(0, 30000, 60000, 90000, 120000, 150000, Inf),
                              labels = c("0-30k", "30k-60k", "60k-90k", "90k-120k", "120k-150k", "150k+"))

# Create ranges for Premium Amount
insurance$Premium.Range <- cut(insurance$Premium.Amount,
                               breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
                               labels = c("0-500", "500-1000", "1000-1500", "1500-2000", "2000-2500", "2500+"))

# Calculate count of Premium.Range by Income.Range
premium_by_income <- insurance %>%
  group_by(Income.Range, Premium.Range) %>%
  summarise(Count = n(), .groups = 'drop')

# View the result
print(premium_by_income)

# Grouped Bar Plot: Premium Amount Ranges by Income Ranges
ggplot(premium_by_income, aes(x = Income.Range, y = Count, fill = Premium.Range)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use position = "dodge" for grouped bars
  labs(
    title = "Grouped Bar Plot: Premium Amount Ranges by Annual Income Range",
    x = "Annual Income Range",
    y = "Count",
    fill = "Premium Range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# Stacked Bar Chart: Marital Status vs Smoking Status
ggplot(insurance, aes(x = Marital.Status, fill = Smoking.Status)) +
  geom_bar(position = "stack") +
  labs(
    title = "Marital Status vs Smoking Status",
    x = "Marital Status",
    y = "Count"
  ) +
  theme_minimal()




# Create ranges for Credit Score
insurance$Credit.Range <- cut(insurance$Credit.Score,
                              breaks = c(300, 400, 500, 600, 700, 800, 850),
                              labels = c("300-400", "400-500", "500-600", "600-700", "700-800", "800-850"))

# Heatmap: Annual Income vs Credit Score Ranges
ggplot(insurance, aes(x = Income.Range, y = Credit.Range)) +
  geom_bin2d() +  # Create a 2D histogram
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Customize color gradient
  labs(
    title = "Heatmap: Annual Income vs Credit Score (Ranges)",
    x = "Annual Income Range",
    y = "Credit Score Range",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





ggplot(insurance, aes(x = Education.Level, fill = Occupation)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Education Level vs Occupation",
    x = "Education Level",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Create ranges for Health Score
insurance$Health.Range <- cut(insurance$Health.Score,
                              breaks = c(0, 20, 40, 60, 80, 200),
                              labels = c("0-20", "20-40", "40-60", "60-80", "80-200"))

# Create ranges for Vehicle Age
insurance$Vehicle.Age.Range <- cut(insurance$Vehicle.Age,
                                   breaks = c(0, 5, 10, 15, 20),
                                   labels = c("0-5", "5-10", "10-15", "15-20"))

# Aggregate data
agg_data <- insurance %>%
  group_by(Health.Range, Vehicle.Age.Range, Property.Type) %>%
  summarise(Avg_Previous.Claims = mean(Previous.Claims, na.rm = TRUE), .groups = 'drop')


# Stacked Bar Plot
ggplot(agg_data, aes(x = Health.Range, y = Avg_Previous.Claims, fill = Vehicle.Age.Range)) +
  geom_bar(stat = "identity", position = "stack") +  # Use position = "stack" for stacked bars
  facet_wrap(~ Property.Type) +  # Facet by Property Type
  labs(
    title = "Stacked Bar Plot: Health Score, Vehicle Age, Property Type vs Previous Claims",
    x = "Health Score Range",
    y = "Average Previous Claims",
    fill = "Vehicle Age Range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  











# Grouped Bar Chart: Policy Type vs Customer Feedback
ggplot(insurance, aes(x = Policy.Type, fill = Customer.Feedback)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Policy Type vs Customer Feedback",
    x = "Policy Type",
    y = "Count"
  ) +
  theme_minimal()





# Create Income Ranges
insurance$Income.Range <- cut(insurance$Annual.Income, 
                              breaks = c(0, 30000, 60000, 90000, 120000, 150000, Inf),
                              labels = c("0-30k", "30k-60k", "60k-90k", "90k-120k", "120k-150k", "150k+"))

# Grouped Bar Chart: Income Range and Number of Dependents vs Property Type
ggplot(insurance, aes(x = Income.Range, fill = Property.Type)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Number.of.Dependents) +
  labs(
    title = "Income Range and Number of Dependents vs Property Type",
    x = "Annual Income Range",
    y = "Count"
  ) 


# Create ranges for Insurance Duration
insurance$Duration.Range <- cut(insurance$Insurance.Duration,
                                breaks = c(0, 3, 6, 9, 12),
                                labels = c("0-3", "3-6", "6-9", "9-12"))

# Create ranges for Premium Amount
insurance$Premium.Range <- cut(insurance$Premium.Amount,
                               breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
                               labels = c("0-500", "500-1000", "1000-1500", "1500-2000", "2000-2500", "2500+"))

# Scatterplot with ranges
ggplot(insurance, aes(x = Duration.Range, y = Premium.Range)) +
  geom_point(alpha = 0.5, color = "orange", width = 0.2, height = 0.2) +
  labs(
    title = "Insurance Duration vs Premium Amount (Ranges)",
    x = "Insurance Duration Range",
    y = "Premium Amount Range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 






# Scatterplot: Age vs Health Score
ggplot(insurance, aes(x = Age, y = Health.Score)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(
    title = "Age vs Health Score",
    x = "Age",
    y = "Health Score"
  ) +
  theme_minimal()














ggplot(insurance, aes(x = Smoking.Status, y = Health.Score, fill = Smoking.Status)) +
  geom_boxplot() +
  labs(
    title = "Smoking Status vs Health Score",
    x = "Smoking Status",
    y = "Health Score"
  ) +
  theme_minimal()





ggplot(insurance, aes(y = Annual.Income)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Boxplot of Annual Income",
    y = "Annual Income"
  ) +
  theme_minimal()




ggplot(insurance, aes(x = Location, y = Premium.Amount, fill = Property.Type)) +
  geom_boxplot() +
  labs(
    title = "Location and Property Type vs Premium Amount",
    x = "Location",
    y = "Premium Amount"
  ) +
  theme_minimal()





# Identify numerical columns
num_cols <- names(insurance)[sapply(insurance, is.numeric)]
print(num_cols)

# Load necessary library
library(dplyr)

# Calculate min and max for numerical columns
min_max_values <- insurance %>%
  summarise(across(all_of(num_cols), list(min = min, max = max), na.rm = TRUE))

# View the result
print(min_max_values)

# Transpose the result for better readability
min_max_values <- as.data.frame(t(min_max_values))
colnames(min_max_values) <- c("Min", "Max")

# View the formatted result
print(min_max_values)  

# Transpose the data frame to display min and max in a table format
min_max_table <- as.data.frame(t(min_max_values))

# Rename columns for clarity
colnames(min_max_table) <- c("Min", "Max")

# Print the formatted table
print(min_max_table)
















# Data Pre-Processing

# Load necessary library
library(dplyr)

# Drop the Policy.Start.Date column
insurance <- insurance %>%
  select(-Policy.Start.Date)

# Verify the column has been dropped
colnames(insurance)

View(insurance)




# Load necessary libraries
library(dplyr)

# Convert blank values ("") to NA for consistency
insurance[insurance == ""] <- NA



# NUMERICAL COLUMNS 

# Age - Impute with Median 
insurance$Age[is.na(insurance$Age)] <- median(insurance$Age, na.rm = TRUE)

# Annual Income - Impute with Mean 
insurance$Annual.Income[is.na(insurance$Annual.Income)] <- mean(insurance$Annual.Income, na.rm = TRUE)

# Health Score - Impute with Median 
insurance$Health.Score[is.na(insurance$Health.Score)] <- median(insurance$Health.Score, na.rm = TRUE)


# Credit Score - Impute with Mean 
insurance$Credit.Score[is.na(insurance$Credit.Score)] <- mean(insurance$Credit.Score, na.rm = TRUE)

# Premium Amount - Impute with Median 
insurance$Premium.Amount[is.na(insurance$Premium.Amount)] <- median(insurance$Premium.Amount, na.rm = TRUE)

# Number of Dependents - Impute with Median (better for count data)
insurance$Number.of.Dependents[is.na(insurance$Number.of.Dependents)] <- median(insurance$Number.of.Dependents, na.rm = TRUE)

insurance$Previous.Claims[is.na(insurance$Previous.Claims)] <- median(insurance$Previous.Claims, na.rm = TRUE)

# CATEGORICAL COLUMNS

# Function to get Mode (most frequent value)
mode_impute <- function(x) {
  ux <- unique(x[!is.na(x)])  # Get unique values ignoring NA
  ux[which.max(tabulate(match(x, ux)))]  # Return most frequent value
}

# Marital Status - Impute with Mode
insurance$Marital.Status[is.na(insurance$Marital.Status)] <- mode_impute(insurance$Marital.Status)

# Customer Feedback - Impute with Mode
insurance$Customer.Feedback[is.na(insurance$Customer.Feedback)] <- mode_impute(insurance$Customer.Feedback)

# Smoking Status - Impute with Mode
insurance$Smoking.Status[is.na(insurance$Smoking.Status)] <- mode_impute(insurance$Smoking.Status)


# Occupation Replace with "Unknown" due to high missing rate
insurance$Occupation[is.na(insurance$Occupation)] <- "Unknown"

summary(insurance)


#Data frame with missing and blank values
missing_blank_counts <- data.frame(
  Column = colnames(insurance),
  Missing_Values = colSums(is.na(insurance)),
  Blank_Values = colSums(insurance == "", na.rm = TRUE)
)

# Print the table
print(missing_blank_counts)












#Outlier

# Load necessary library
library(dplyr)

# Function to remove extreme outliers using IQR
remove_extreme_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)  # 25th percentile
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)  # 75th percentile
  IQR <- Q3 - Q1  # Interquartile Range
  
  # Define lower and upper limits (extreme outliers are 3 times the IQR)
  lower_bound <- Q1 - 3 * IQR
  upper_bound <- Q3 + 3 * IQR
  
  # Remove only extreme outliers (keeping reasonable ones)
  df <- df %>%
    filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
  
  return(df)
}

# Apply outlier removal to necessary numerical columns
insurance <- remove_extreme_outliers(insurance, "Annual.Income")
insurance <- remove_extreme_outliers(insurance, "Credit.Score")
insurance <- remove_extreme_outliers(insurance, "Premium.Amount")
insurance <- remove_extreme_outliers(insurance, "Health.Score")
insurance <- remove_extreme_outliers(insurance, "Number.of.Dependents")


# Define the columns you want to check
columns_to_check <- c("Annual.Income", "Credit.Score", "Premium.Amount", "Health.Score", "Number.of.Dependents")

# Function to count outliers
count_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 3 * IQR
  upper_bound <- Q3 + 3 * IQR
  
  sum(df[[column]] < lower_bound | df[[column]] > upper_bound, na.rm = TRUE)
}

# Check for all columns
for (col in columns_to_check) {
  outliers_count <- count_outliers(insurance, col)
  print(paste("Outliers remaining in", col, ":", outliers_count))
}

#Outliers of Premium.Amount
Q1 <- quantile(insurance$Premium.Amount, 0.25, na.rm = TRUE)
Q3 <- quantile(insurance$Premium.Amount, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 3 * IQR
upper_bound <- Q3 + 3 * IQR

extreme_values <- insurance$Premium.Amount[insurance$Premium.Amount < lower_bound | insurance$Premium.Amount > upper_bound]
print(extreme_values)
summary(insurance$Premium.Amount)
















# Data Transformation

#Binning
insurance <- insurance %>%
  mutate(
    Age_Group = case_when(
      Age < 25 ~ "Young",
      Age >= 25 & Age < 45 ~ "Adult",
      Age >= 45 ~ "Senior"
    )
  )

insurance <- insurance %>%
  mutate(
    Credit_Category = case_when(
      Credit.Score < 400 ~ "Poor",
      Credit.Score >= 400 & Credit.Score < 600 ~ "Fair",
      Credit.Score >= 600 & Credit.Score < 800 ~ "Good",
      Credit.Score >= 800 ~ "Excellent"
    )
  )

insurance <- insurance %>%
  mutate(
    Premium_Tier = case_when(
      Premium.Amount < quantile(Premium.Amount, 0.25, na.rm = TRUE) ~ "Low",
      Premium.Amount >= quantile(Premium.Amount, 0.25, na.rm = TRUE) & 
        Premium.Amount <= quantile(Premium.Amount, 0.75, na.rm = TRUE) ~ "Medium",
      Premium.Amount > quantile(Premium.Amount, 0.75, na.rm = TRUE) ~ "High"
    )
  )

View(insurance)


#Label Encoding

# Convert categorical variables to numeric levels
insurance$Property_Type_Code <- as.numeric(factor(insurance$Property.Type, levels = c("Apartment", "Condo", "House")))
insurance$Marital_Status_Code <- as.numeric(factor(insurance$Marital.Status, levels = c("Single", "Married", "Divorced")))
insurance$Occupation_Code <- as.numeric(factor(insurance$Occupation, levels = c("Unemployed", "Self-Employed", "Employed", "Unknown")))
insurance$Education_Code <- as.numeric(factor(insurance$Education, levels = c("High School", "Bachelor's", "Master's", "PhD")))

View(insurance)

#One hot encoding

# Load necessary library
install.packages("fastDummies")
library(dplyr)
library(fastDummies)

# Perform one-hot encoding
insurance <- dummy_cols(insurance, select_columns = c("Smoking.Status", "Gender"), remove_first_dummy = TRUE)

View(insurance)















#Data Standardization

# Load required library
library(dplyr)

# List of numerical columns to standardize
standardize_cols <- c("Annual.Income", "Premium.Amount")

# Apply Standardization (Z-score normalization)
insurance_standardized <- insurance %>%
  mutate(across(all_of(standardize_cols), ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

# Check summary statistics after standardization
summary(insurance_standardized[standardize_cols])










#Data Normalization

# Load required library
library(dplyr)

# List of numerical columns to normalize
normalize_cols <- c("Credit.Score", "Health.Score")

# Min-Max Normalization function
min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply Min-Max Normalization
insurance_normalized <- insurance %>%
  mutate(across(all_of(normalize_cols), min_max_norm))

# Check summary statistics after normalization
summary(insurance_normalized[normalize_cols])

View(insurance)


# Load necessary library
library(dplyr)

# Drop the Policy.Start.Date column
insurance <- insurance %>%
  select(-Health.Range)

# Verify the column has been dropped
colnames(insurance)


missing_values <- colSums(is.na(insurance))
print(missing_values)

View(insurance)

#Sem 2

insurance$No_Claim_Vault_Eligibility <- ifelse(
  insurance$Previous.Claims <= 1 &
    insurance$Credit.Score > 400 &
    insurance$Insurance.Duration > 2 &
    insurance$Health.Score > 40,
  1, 0
)

View(insurance)
table(insurance$No_Claim_Vault_Eligibility)



library(dplyr)

print(filter)


insurance <- insurance %>%
filter(insurance$Premium.Amount!= 0)
sum(insurance$premium.amount == 0)


library(dplyr)

insurance <- insurance %>%
  mutate(premium_range = case_when(
    Premium.Amount < 600 ~ "Low",
    Premium.Amount >= 600 & Premium.Amount < 1800 ~ "Medium",
    Premium.Amount >= 1800 ~ "High",
    
  ))

insurance <- insurance %>%
  mutate(premium_range_encoded = recode(premium_range,
                                        "Low" = 0,
                                        "Medium" = 1,
                                        "High" = 2
                                        ))






table(insurance$premium_range)
table(insurance$premium_range_encoded)



library(dplyr)

insurance <- insurance %>%
  mutate(Policy_Type_Range = case_when(
    Annual.Income >= 30000 & Premium.Amount >= 700 ~ "Premium",
    Annual.Income < 30000 & Premium.Amount < 700 ~ "Comprehensive",
    TRUE ~ "Comprehensive"  # default fallback
  ))

insurance <- insurance %>%
  mutate(Policy_Type_Encoded = recode(Policy_Type_Range,
                                      "Comprehensive" = 0,
                                      "Premium" = 1))

table(insurance$Policy_Type_Encoded)

View(insurance)
write.csv(insurance, "C:\\Users\\recovery\\OneDrive - Asia Pacific University of Technology And Innovation (APU)\\Desktop\\Insurance Premium Prediction.csv", row.names = FALSE)




























































































