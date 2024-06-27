# Load necessary libraries
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

# Read data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("index", "feature"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

# Read training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# Read test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

# Merge the datasets
subject <- rbind(subject_train, subject_test)
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)

merged_data <- cbind(subject, y_data, x_data)

# Clean up feature names to match the column names in merged_data
clean_feature_names <- function(name) {
  name <- gsub("\\(\\)", "", name)
  name <- gsub("-", ".", name)
  name <- gsub(",", ".", name)
  name <- gsub("BodyBody", "Body", name)
  name <- gsub("([a-z])([A-Z])", "\\1.\\2", name)
  name <- gsub("\\.\\.", ".", name)  # Replace double dots with single dot
  name <- gsub("\\.\\.", ".", name)  # Replace any remaining double dots with single dot
  name
}

# Apply the cleaning function to features
features$clean_feature <- sapply(features$feature, clean_feature_names)

# Apply the cleaning function to the column names of merged_data
colnames(merged_data) <- sapply(colnames(merged_data), clean_feature_names)

# Extract only the measurements on the mean and standard deviation
selected_features <- features$clean_feature[grep("mean\\.|std\\.", features$clean_feature)]
mismatched_features <- setdiff(selected_features, colnames(merged_data))
selected_features <- setdiff(selected_features, mismatched_features)
selected_data <- merged_data %>%
  select(subject, code, all_of(selected_features))

# Use descriptive activity names to name the activities in the data set
selected_data <- merge(selected_data, activities, by = "code", all.x = TRUE)
selected_data <- selected_data %>%
  select(subject, activity, everything(), -code)

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
tidy_data <- selected_data %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean))

# Write the tidy data set to a file
write.table(tidy_data, "tidy_data.txt", row.name = FALSE)

# Display the first few rows of the tidy data set
head(tidy_data)
