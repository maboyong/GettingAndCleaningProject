clean_data <- function () {
    
    ## read the datasets and feature list in
    x_train = read.table("train/X_train.txt")
    x_test = read.table("test/X_test.txt")
    features = read.table("features.txt")
    features = features[["V2"]]
    colnames(x_test) <- features
    colnames(x_train) <- features
    
    ## use the feature list as the column names, and merge the training/test date sets together
    trainsbj = read.table("train/subject_train.txt")
    colnames(trainsbj) <- c("subject")
    testsbj = read.table("test/subject_test.txt")
    colnames(testsbj) <- c("subject")
    trainset = cbind(trainsbj, x_train)
    testset = cbind(testsbj, x_test)
    fullset = rbind(testset, trainset)
    
    ## get the activity data, label it to the merged dataset
    act_train <- read.table("train/y_train.txt")
    act_test <- read.table("test/y_test.txt")
    act <- rbind(act_test, act_train)
    fullset = cbind(act, fullset)
    
    ## get the logicals for column name (mean() and std() measurements)
    t <- lapply(features, FUN = contains)
    ##add a true for the subject ids, the activity
    t <- c(TRUE, TRUE, t)
    ## change to vector 
    t <-unlist(t)
    ## this is the dataset that's merged, appropriately labeled, and filtered
    filtered <- fullset[,t]
    
    ## merge to get the activity label
    labelled = merge(filtered, labels, by.x = "V1", by.y = "V1")
    
    ## arrange columns and add name
    labelled = arrange(labelled, subject, V2)
    finalds = labelled[,2:82]
    finalds = finalds[,c(1, 81, 2:80)]
    colnames(finalds)[2] <- "activity"

    attach(finalds)
    tidy <- aggregate(finalds, by=list(subject, activity), FUN=mean, na.rm = TRUE)
    detach(finalds)
    tidy
}

## function to check if the column name contains "mean()" or "std()"
contains <- function(string) {
    result <- FALSE
    if(length(grep("mean()",string))>0 || length(grep("std()", string)) > 0)
        result <- TRUE
    
    result
}
