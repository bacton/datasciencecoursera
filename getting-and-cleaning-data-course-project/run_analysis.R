
clean_data_set <- function() {
    library(data.table)
    library(dplyr)
    library(dtplyr)
    # Read the "features.txt" file -- this contains the 
    # names of the variables in the ./test/X_test.txt and
    # ./train/X_train.txt files
    features_table <- read.table("features.txt",
                                 header=FALSE,sep="",
                                 stringsAsFactors = FALSE)
    # Get the variables names from the features_table as a 
    # character vector
    variables <- features_table$V2
    
    # Read the training observations from 
    # "./train/X_train.txt", naming the columns using
    # the variables vector
    training_table <- read.table("./train/X_train.txt",
                                 header=FALSE,sep="",
                                 col.names=variables)
    # Now delete all columns from "training_table" 
    # whose variable name does not contain either "std" 
    # or "mean" because these are the only variables that
    # we're interested in
    training_table <- training_table %>% 
        # (In addition, remove the "^angle.*" variables as they
        # are derived from other variables not on the mean or
        # std. dev.)
        select(-matches('^angle')) %>%
        select(matches('mean|std')) %>% 
        rename_(.dots=setNames(names(.),
                               gsub("\\.+",".",names(.)))) %>% 
        rename_(.dots=setNames(names(.),gsub("\\.$","",names(.))))
    #
    # Now add the acceleration and gyro computed mean and sd value for each
    # observation ("train" case) to the training_table
    #
    
    # First, create a few new data frames which will act 
    # as "source" dataframes for the mutate() function
    # setwd("./train/Inertial Signals")
    total_accel_x_train <- fread("./train/Inertial Signals/total_acc_x_train.txt")
    #
    # These are the new variables that will be added to 
    # the training_table; they're all related to the 
    # accelerometer and gyroscope data from the 
    # experiments:
    # Total_Accel_x_Mean Total_Accel_x_SD 
    # Total_Accel_y_Mean Total_Accel_y_SD 
    # Total_Accel_z_Mean Total_Accel_z_SD 
    # Est_Body_Accel_x_Mean 
    # Est_Body_Accel_x_SD Est_Body_Accel_y_Mean 
    # Est_Body_Accel_y_SD Est_Body_Accel_z_Mean 
    # Est_Body_Accel_z_SD Gyro_Velocity_x_Mean 
    # Gyro_Velocity_x_SD Gyro_Velocity_y_Mean 
    # Gyro_Velocity_y_SD Gyro_Velocity_z_Mean 
    # Gyro_Velocity_z_SD
    #
    
    # We can extract data from this table by row, and 
    # use dplyr's mutate function to calculate the mean 
    # and sd
    # Compute the first new variable in the list above, 
    # i.e.: Total_Accel_x_Mean, for _training_ data
    total_accel_x_train_mutated <- total_accel_x_train %>% 
        mutate(Total_Accel_x_Mean=apply(.,1,mean),
               Total_Accel_x_SD=apply(.,1,sd)) %>% 
        select(Total_Accel_x_Mean,Total_Accel_x_SD)
    # Now mutate these two new variables onto the 
    # training_table:
    training_table <- training_table %>% 
        mutate(Total_Accel_x_Mean=total_accel_x_train_mutated$Total_Accel_x_Mean, 
               Total_Accel_x_SD=total_accel_x_train_mutated$Total_Accel_x_SD)
    
    # Similarly, compute the rest of the variables in the
    # list above for training data, then repeat for 
    # testing data after joining all of the training
    # computations with the training_table
    # [Total_Accel_y_Mean & Total_Accel_y_SD]:
    total_accel_y_train <- fread("./train/Inertial Signals/total_acc_y_train.txt")
    total_accel_y_train_mutated <- total_accel_y_train %>% 
        mutate(Total_Accel_y_Mean=apply(.,1,mean),
               Total_Accel_y_SD=apply(.,1,sd)) %>% 
        select(Total_Accel_y_Mean,Total_Accel_y_SD)
    # Mutate these two new variables onto the 
    # training_table
    training_table <- training_table %>% 
        mutate(Total_Accel_y_Mean=total_accel_y_train_mutated$Total_Accel_y_Mean, 
               Total_Accel_y_SD=total_accel_y_train_mutated$Total_Accel_y_SD)
    # [Total_Accel_z_Mean & Total_Accel_z_SD]
    total_accel_z_train <- fread("./train/Inertial Signals/total_acc_z_train.txt")
    total_accel_z_train_mutated <- total_accel_z_train %>% 
        mutate(Total_Accel_z_Mean=apply(.,1,mean),
               Total_Accel_z_SD=apply(.,1,sd)) %>% 
        select(Total_Accel_z_Mean,Total_Accel_z_SD)
    # Mutate these two new variables onto the 
    # training_table
    training_table <- training_table %>% 
        mutate(Total_Accel_z_Mean=total_accel_z_train_mutated$Total_Accel_z_Mean, 
               Total_Accel_z_SD=total_accel_z_train_mutated$Total_Accel_z_SD)
    # [Est_Body_Accel_x_Mean & Est_Body_Accel_x_SD]
    Est_Body_Accel_x_train <- fread("./train/Inertial Signals/body_acc_x_train.txt")
    Est_Body_Accel_x_train_mutated <- Est_Body_Accel_x_train %>% 
        mutate(Est_Body_Accel_x_Mean=apply(.,1,mean),
               Est_Body_Accel_x_SD=apply(.,1,sd)) %>% 
        select(Est_Body_Accel_x_Mean,Est_Body_Accel_x_SD)
    # Mutate these two new variables onto the 
    # training_table
    training_table <- training_table %>% 
        mutate(Est_Body_Accel_x_Mean=Est_Body_Accel_x_train_mutated$Est_Body_Accel_x_Mean, 
               Est_Body_Accel_x_SD=Est_Body_Accel_x_train_mutated$Est_Body_Accel_x_SD)
    # [Est_Body_Accel_y_Mean & Est_Body_Accel_y_SD]
    Est_Body_Accel_y_train <- fread("./train/Inertial Signals/body_acc_y_train.txt")
    Est_Body_Accel_y_train_mutated <- Est_Body_Accel_y_train %>% 
        mutate(Est_Body_Accel_y_Mean=apply(.,1,mean),
               Est_Body_Accel_y_SD=apply(.,1,sd)) %>% 
        select(Est_Body_Accel_y_Mean,Est_Body_Accel_y_SD)
    # Mutate these two new variables onto the training_table:
    training_table <- training_table %>% 
        mutate(Est_Body_Accel_y_Mean=Est_Body_Accel_y_train_mutated$Est_Body_Accel_y_Mean, 
               Est_Body_Accel_y_SD=Est_Body_Accel_y_train_mutated$Est_Body_Accel_y_SD)
    # [Est_Body_Accel_z_Mean Est_Body_Accel_z_SD]
    Est_Body_Accel_z_train <- fread("./train/Inertial Signals/body_acc_z_train.txt")
    Est_Body_Accel_z_train_mutated <- Est_Body_Accel_z_train %>% 
        mutate(Est_Body_Accel_z_Mean=apply(.,1,mean),
               Est_Body_Accel_z_SD=apply(.,1,sd)) %>% 
        select(Est_Body_Accel_z_Mean,Est_Body_Accel_z_SD)
    # Mutate these two new variables onto the training_table
    training_table <- training_table %>% 
        mutate(Est_Body_Accel_z_Mean=Est_Body_Accel_z_train_mutated$Est_Body_Accel_z_Mean, 
               Est_Body_Accel_z_SD=Est_Body_Accel_z_train_mutated$Est_Body_Accel_z_SD)
    # [Gyro_Velocity_x_Mean Gyro_Velocity_x_SD]
    Gyro_Velocity_x_train <- fread("./train/Inertial Signals/body_gyro_x_train.txt")
    Gyro_Velocity_x_train_mutated <- Gyro_Velocity_x_train %>% 
        mutate(Gyro_Velocity_x_Mean=apply(.,1,mean),
               Gyro_Velocity_x_SD=apply(.,1,sd)) %>% 
        select(Gyro_Velocity_x_Mean,Gyro_Velocity_x_SD)
    # Mutate these two new variables onto the training_table
    training_table <- training_table %>% 
        mutate(Gyro_Velocity_x_Mean=Gyro_Velocity_x_train_mutated$Gyro_Velocity_x_Mean, 
               Gyro_Velocity_x_SD=Gyro_Velocity_x_train_mutated$Gyro_Velocity_x_SD)
    # [Gyro_Velocity_y_Mean & Gyro_Velocity_y_SD]
    Gyro_Velocity_y_train <- fread("./train/Inertial Signals/body_gyro_y_train.txt")
    Gyro_Velocity_y_train_mutated <- Gyro_Velocity_y_train %>% 
        mutate(Gyro_Velocity_y_Mean=apply(.,1,mean),
               Gyro_Velocity_y_SD=apply(.,1,sd)) %>% 
        select(Gyro_Velocity_y_Mean,Gyro_Velocity_y_SD)
    # Mutate these two new variables onto the training_table
    training_table <- training_table %>% 
        mutate(Gyro_Velocity_y_Mean=Gyro_Velocity_y_train_mutated$Gyro_Velocity_y_Mean, 
               Gyro_Velocity_y_SD=Gyro_Velocity_y_train_mutated$Gyro_Velocity_y_SD)
    # [Gyro_Velocity_z_Mean & Gyro_Velocity_z_SD]
    Gyro_Velocity_z_train <- fread("./train/Inertial Signals/body_gyro_z_train.txt")
    Gyro_Velocity_z_train_mutated <- Gyro_Velocity_z_train %>% 
        mutate(Gyro_Velocity_z_Mean=apply(.,1,mean),
               Gyro_Velocity_z_SD=apply(.,1,sd)) %>% 
        select(Gyro_Velocity_z_Mean,Gyro_Velocity_z_SD)
    # Mutate these two new variables onto the training_table
    training_table <- training_table %>% 
        mutate(Gyro_Velocity_z_Mean=Gyro_Velocity_z_train_mutated$Gyro_Velocity_z_Mean, 
               Gyro_Velocity_z_SD=Gyro_Velocity_z_train_mutated$Gyro_Velocity_z_SD)
    
    # Now,do all of the above again, but for the "test" data this time
    # First, read the testing observations from 
    # "./test/X_test.txt"
    testing_table <- read.table("./test/X_test.txt",
                                header=FALSE,
                                sep="",
                                col.names=variables)
    # Now delete all columns from "testing_table" whose 
    # variable name does not contain either "std" or "mean"
    testing_table <- testing_table %>% 
        # (In addition, remove the "^angle.*" variables as they
        # are derived from other variables not on the mean or
        # std. dev.)
        select(-matches('^angle')) %>%
        select(matches('mean|std')) %>% 
        rename_(.dots=setNames(names(.),
                               gsub("\\.+",".",names(.)))) %>% 
        rename_(.dots=setNames(names(.),
                               gsub("\\.$","",names(.))))
    
    # Create a few new data frames which will act as 
    # "source" dataframes for the mutate() function
    #setwd("./test/Inertial Signals")
    total_accel_x_test <- fread("./test/Inertial Signals/total_acc_x_test.txt")
    total_accel_x_test_mutated <- total_accel_x_test %>% 
        mutate(Total_Accel_x_Mean=apply(.,1,mean),
               Total_Accel_x_SD=apply(.,1,sd)) %>% 
        select(Total_Accel_x_Mean,Total_Accel_x_SD)
    # Now mutate these two new variables onto the 
    # testing_table
    testing_table <- testing_table %>% 
        mutate(Total_Accel_x_Mean=total_accel_x_test_mutated$Total_Accel_x_Mean, 
               Total_Accel_x_SD=total_accel_x_test_mutated$Total_Accel_x_SD)
    # [Total_Accel_y_Mean & Total_Accel_y_SD]
    total_accel_y_test <- fread("./test/Inertial Signals/total_acc_y_test.txt")
    total_accel_y_test_mutated <- total_accel_y_test %>% 
        mutate(Total_Accel_y_Mean=apply(.,1,mean),
               Total_Accel_y_SD=apply(.,1,sd)) %>% 
        select(Total_Accel_y_Mean,Total_Accel_y_SD)
    # Mutate these two new variables onto the 
    # testing_table
    testing_table <- testing_table %>% 
        mutate(Total_Accel_y_Mean=total_accel_y_test_mutated$Total_Accel_y_Mean, 
               Total_Accel_y_SD=total_accel_y_test_mutated$Total_Accel_y_SD)
    # [Total_Accel_z_Mean & Total_Accel_z_SD]
    total_accel_z_test <- fread("./test/Inertial Signals/total_acc_z_test.txt")
    total_accel_z_test_mutated <- total_accel_z_test %>% 
        mutate(Total_Accel_z_Mean=apply(.,1,mean),
               Total_Accel_z_SD=apply(.,1,sd)) %>% 
        select(Total_Accel_z_Mean,Total_Accel_z_SD)
    # Mutate these two new variables onto the 
    # testing_table
    testing_table <- testing_table %>% 
        mutate(Total_Accel_z_Mean=total_accel_z_test_mutated$Total_Accel_z_Mean, 
               Total_Accel_z_SD=total_accel_z_test_mutated$Total_Accel_z_SD)
    # [Est_Body_Accel_x_Mean & Est_Body_Accel_x_SD]
    Est_Body_Accel_x_test <- fread("./test/Inertial Signals/body_acc_x_test.txt")
    Est_Body_Accel_x_test_mutated <- Est_Body_Accel_x_test %>% 
        mutate(Est_Body_Accel_x_Mean=apply(.,1,mean),
               Est_Body_Accel_x_SD=apply(.,1,sd)) %>% 
        select(Est_Body_Accel_x_Mean,Est_Body_Accel_x_SD)
    # Mutate these two new variables onto the 
    # testing_table
    testing_table <- testing_table %>% 
        mutate(Est_Body_Accel_x_Mean=Est_Body_Accel_x_test_mutated$Est_Body_Accel_x_Mean, 
               Est_Body_Accel_x_SD=Est_Body_Accel_x_test_mutated$Est_Body_Accel_x_SD)
    # [Est_Body_Accel_y_Mean & Est_Body_Accel_y_SD]
    Est_Body_Accel_y_test <- fread("./test/Inertial Signals/body_acc_y_test.txt")
    Est_Body_Accel_y_test_mutated <- Est_Body_Accel_y_test %>% 
        mutate(Est_Body_Accel_y_Mean=apply(.,1,mean),
               Est_Body_Accel_y_SD=apply(.,1,sd)) %>% 
        select(Est_Body_Accel_y_Mean,Est_Body_Accel_y_SD)
    # Mutate these two new variables onto the 
    # testing_table
    testing_table <- testing_table %>% 
        mutate(Est_Body_Accel_y_Mean=Est_Body_Accel_y_test_mutated$Est_Body_Accel_y_Mean, 
               Est_Body_Accel_y_SD=Est_Body_Accel_y_test_mutated$Est_Body_Accel_y_SD)
    # [Est_Body_Accel_z_Mean Est_Body_Accel_z_SD]
    Est_Body_Accel_z_test <- fread("./test/Inertial Signals/body_acc_z_test.txt")
    Est_Body_Accel_z_test_mutated <- Est_Body_Accel_z_test %>% 
        mutate(Est_Body_Accel_z_Mean=apply(.,1,mean),
               Est_Body_Accel_z_SD=apply(.,1,sd)) %>% 
        select(Est_Body_Accel_z_Mean,Est_Body_Accel_z_SD)
    # Mutate these two new variables onto the 
    # testing_table
    testing_table <- testing_table %>% 
        mutate(Est_Body_Accel_z_Mean=Est_Body_Accel_z_test_mutated$Est_Body_Accel_z_Mean, 
               Est_Body_Accel_z_SD=Est_Body_Accel_z_test_mutated$Est_Body_Accel_z_SD)
    # [Gyro_Velocity_x_Mean Gyro_Velocity_x_SD]
    Gyro_Velocity_x_test <- fread("./test/Inertial Signals/body_gyro_x_test.txt")
    Gyro_Velocity_x_test_mutated <- Gyro_Velocity_x_test %>% 
        mutate(Gyro_Velocity_x_Mean=apply(.,1,mean),
               Gyro_Velocity_x_SD=apply(.,1,sd)) %>% 
        select(Gyro_Velocity_x_Mean,Gyro_Velocity_x_SD)
    # Mutate these two new variables onto the testing_table
    testing_table <- testing_table %>% 
        mutate(Gyro_Velocity_x_Mean=Gyro_Velocity_x_test_mutated$Gyro_Velocity_x_Mean, 
               Gyro_Velocity_x_SD=Gyro_Velocity_x_test_mutated$Gyro_Velocity_x_SD)
    # [Gyro_Velocity_y_Mean & Gyro_Velocity_y_SD]
    Gyro_Velocity_y_test <- fread("./test/Inertial Signals/body_gyro_y_test.txt")
    Gyro_Velocity_y_test_mutated <- Gyro_Velocity_y_test %>% 
        mutate(Gyro_Velocity_y_Mean=apply(.,1,mean),
               Gyro_Velocity_y_SD=apply(.,1,sd)) %>% 
        select(Gyro_Velocity_y_Mean,Gyro_Velocity_y_SD)
    # Mutate these two new variables onto the 
    # testing_table
    testing_table <- testing_table %>% 
        mutate(Gyro_Velocity_y_Mean=Gyro_Velocity_y_test_mutated$Gyro_Velocity_y_Mean, 
               Gyro_Velocity_y_SD=Gyro_Velocity_y_test_mutated$Gyro_Velocity_y_SD)
    # [Gyro_Velocity_z_Mean & Gyro_Velocity_z_SD]
    Gyro_Velocity_z_test <- fread("./test/Inertial Signals/body_gyro_z_test.txt")
    Gyro_Velocity_z_test_mutated <- Gyro_Velocity_z_test %>% 
        mutate(Gyro_Velocity_z_Mean=apply(.,1,mean),
               Gyro_Velocity_z_SD=apply(.,1,sd)) %>% 
        select(Gyro_Velocity_z_Mean,Gyro_Velocity_z_SD)
    # Mutate these two new variables onto the testing_table
    testing_table <- testing_table %>% 
        mutate(Gyro_Velocity_z_Mean=Gyro_Velocity_z_test_mutated$Gyro_Velocity_z_Mean, 
               Gyro_Velocity_z_SD=Gyro_Velocity_z_test_mutated$Gyro_Velocity_z_SD)
    # Read the "activity_labels.txt" file into a 
    # separate dataframe. This data will be used to add 
    # a new variable, "Activity", to the testing 
    # observations dataframe.
    activity_label_mapping <- read.table("./activity_labels.txt",header=FALSE,sep="")
    # Read the ./test/y_test.txt file (i.e. containing 
    # the activity code for each observation during the 
    # testing phase)
    activity_codes_during_testing <- read.table("./test/y_test.txt",header=FALSE,sep="")
    # Use dplyr's left_join() function to replace the 
    # activity code in the df 
    # "activity_codes_during_testing" with the label 
    # mapped to the activity code in the df 
    # "activity_label_mapping":
    mapped_activity_codes_test <- activity_codes_during_testing %>% 
        left_join(activity_label_mapping)
    # For now, add both the V1 (activity code) and V2 
    # (activity label) vectors to the "testing_table" df 
    # -- to check that the mapping is correct. Later, 
    # will remove the "activity code" column, leaving 
    # only the "activity label" column.
    testing_table <- testing_table %>% 
        mutate(Activity_Code = mapped_activity_codes_test$V1,
               Activity_Label=mapped_activity_codes_test$V2)
    # Add the Subject ID data from ./test/subject_test.txt 
    # as a new column in the "testing_table" df.
    # First, read the subject ID data as a separate df
    subject_ID_test <- read.table("./test/subject_test.txt",
                                  header=FALSE,
                                  sep="")
    
    # Next, use dplyr's mutate() function again, to add 
    # this vector as a new column in the "testing_table" 
    # df
    testing_table <- testing_table %>% 
        mutate(Subject_ID = subject_ID_test$V1)
    #
    # Repeat the steps above, but for the training 
    # table data instead 
    #
    
    # Read the ./train/y_train.txt file (i.e. containing 
    # the activity code for each observation during the
    # training phase)
    activity_codes_during_training <- read.table("./train/y_train.txt",
                                                 header=FALSE,
                                                 sep="")
    # Use dplyr's left_join() function to replace the 
    # activity code in the df 
    # "activity_codes_during_training" with the label 
    # mapped to the activity code in the df 
    # "activity_label_mapping"
    mapped_activity_codes_train <- activity_codes_during_training %>% 
        left_join(activity_label_mapping)
    # For now, add both the V1 (activity code) and V2 
    # (activity label) vectors to the "training_table" 
    # df -- to check that the mapping is correct. 
    # Later, will remove the "activity code" column, 
    # leaving only the "activity label" column.
    training_table <- training_table %>% 
        mutate(Activity_Code = mapped_activity_codes_train$V1,Activity_Label=mapped_activity_codes_train$V2)
    
    # Add the Subject ID data from ./train/subject_train.txt 
    # as a new column in the "training_table" df
    # First, read the subject ID data as a separate df:
    subject_ID_train <- read.table("./train/subject_train.txt",
                               header=FALSE,
                               sep="")
    # Next, use dplyr's mutate() function again, to add 
    # this vector as a new column in the 
    # "training_table" df
    training_table <- training_table %>% 
        mutate(Subject_ID = subject_ID_train$V1)
    # Now append the testing table to the training 
    # table, but first, add a variable to both the test 
    # and train df, indicating which mode, i.e. either 
    # Test or Train
    training_table <- training_table %>% 
        mutate(Mode="Train")
    testing_table <- testing_table %>% 
        mutate(Mode="Test")
    # Now bind the two tables together
    combined_table <- bind_rows(training_table,
                                testing_table)
    # [bind_rows() is a dplyr function]
    # ...and convert the "Mode" variable from character 
    # to factor class (because it makes more sense that
    # way)
    combined_table <- combined_table %>% 
        mutate_each(funs(as.factor),Mode)
    # After debugging, remove the "Activity_Code" 
    # variable, as it's already been mapped to another 
    # variable (Activity_Label) that will remain in the 
    # table. So "Activity_Code" is now redundant.
    combined_table <- combined_table %>% 
        select(-Activity_Code)
    # One last step: convert all variable names to 
    # lowercase, after converting all remaining "." to 
    # "_"
    combined_table <- combined_table %>% 
        rename_(.dots=setNames(names(.),
                               gsub("\\.","_",names(.)))) %>% 
        rename_(.dots=setNames(names(.),tolower(names(.))))
    # At this point we have a "base" data set on which 
    # we can perform grouping, summarising, etc. Now we 
    # have to do: "From the data set in step 4, creates 
    # a second, independent tidy data set with the 
    # average of each variable for each activity and 
    # each subject."
    # This code does it:
    combined_table <- combined_table %>% 
        group_by(activity_label,subject_id) %>% 
        select(-one_of(c("activity_label","subject_id","mode"))) %>% 
        summarise_each(funs(mean))
    # (For debugging only)
    # Wrote out the result to a .CSV file:
    # write.csv(combined_table_cleaned_grouped, "combined_tbl_cleaned_grouped_wip.csv")
    
    # Return the dataframe after all processing steps
    # are complete
    combined_table
}
