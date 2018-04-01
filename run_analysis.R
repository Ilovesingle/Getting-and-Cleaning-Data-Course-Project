library(dplyr)
library(stringr)

# Read in data sets
test_measurement <- read.table("./test/x_test.txt")
test_actcode <- read.table("./test/y_test.txt", col.names = "actcode")
test_subjid <- read.table("./test/subject_test.txt", col.names = "subjid")

train_measurement <- read.table("./train/x_train.txt")
train_actcode <- read.table("./train/y_train.txt", col.names = "actcode")
train_subjid <- read.table("./train/subject_train.txt", col.names = "subjid")

# Read in features and activity labels
features <- read.table("./features.txt")
activity_labels <- read.table("./activity_labels.txt")

# Extract variable names from features and activity labels
var_names <- features[, 2]

# Assign variable names to the measurement data frame
names(test_measurement) <- var_names
names(train_measurement) <- var_names

# Create complete data frame for test and train data set respectively
test_complete <- test_actcode %>% 
        left_join(activity_labels, by = c("actcode" = "V1")) %>% 
        bind_cols(test_subjid, test_measurement) %>% 
        rename(actname = V2)

train_complete <- train_actcode %>% 
        left_join(activity_labels, by = c("actcode" = "V1")) %>% 
        bind_cols(train_subjid, train_measurement) %>% 
        rename(actname = V2)


# Merge the test and the training sets to create one data set
test_train_joined <- test_complete %>%
        full_join(train_complete)


# Create the indicies to extract only the mean and standard deviation for each measurement
mean_std_indicies <- grep("mean|std", names(test_train_joined))

# Create final data frame only with the mean and standard deviation columns
test_train_final <- bind_cols(test_train_joined[, 1:3], test_train_joined[, mean_std_indicies])

# Variable name error fix : "BodyBody" to "Body"
names(test_train_final) <- str_replace(names(test_train_final), pattern = "BodyBody", replacement = "Body")

# Create independent tidy data set with the average of each variable for each activity and each subject
test_train_final %>% 
        group_by(subjid, actcode, actname) %>% 
        summarise_all(mean) %>% 
        write.table("tidy_dataset.csv")

