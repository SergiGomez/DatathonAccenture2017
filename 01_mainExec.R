source('00_initialization.R')
source('02_preProcessing.R')

# ----- Data Import ------------------------------------------------------------
cat("Reading the train and test data \n")
train  <- fread('data/train.csv')
test   <- fread('data/test.csv')
segimon <- fread('data/taula_segimon.csv')

# Exploratory Data Analysis and basic Data Pre-processing-----------------------
# Get those features that are integers or numeric
numIntVars <- colnames(train) [lapply(train,class) %in% c("integer","numeric")]

# Check for the correlations between numeric variables
correlations <- cor(x = train[, numIntVars, with = FALSE],
                    use = "complete.obs")
# Check for Skewness
skewValues <- apply(train[, numIntVars, with = FALSE], 2, e1071::skewness)

# Basic Pre-processing
transTrain <- preProcess(train[, numIntVars, with = FALSE], 
                    method = c("BoxCox", "center", "scale", "pca"))
transTest <- preProcess(test[, numIntVars, with = FALSE], 
                    method = c("BoxCox", "center", "scale", "pca"))
pcaObject_train <- prcomp(train[, numIntVars, with = FALSE],
                          center = TRUE, scale. = TRUE)
pcaObject_test <- prcomp(test[, numIntVars, with = FALSE],
                         center = TRUE, scale. = TRUE)

# Percentage of Missing values for each of the predictors
frequencyMissingValues <- data.table()
for (pred in names(train)) {
  dt <- data.table(predName = paste0("frequency_",pred), 
                   freqNA = nrow(train[ is.na(get(pred)) ]) / nrow(train))
  frequencyMissingValues <- rbind(frequencyMissingValues, dt)
}

# Glimpse of the predictors importance
impValues <- filterVarImp(x = train, 
                          y = train$wheelchair_accessible)

dtVarsInfo <- data.table(predName = names(train), 
                       imp = impValues$Overall)

dtVarsInfo <- merge(dtVarsInfo,
                    frequencyMissingValues, 
                    by = "predName", 
                    all = TRUE)

dtVarsInfo <- dtVarsInfo[order(imp, decreasing = T)]

# Data Pre-processing and Feature Engineering ----------------------------------
trainPre <- copy(train)
testPre <- copy(test)

# Transform label into numeric
train$wheelchair_accessible <- ifelse(train$wheelchair_accessible == TRUE, 1, 0)
train[, wheelchair_accessible := as.factor(wheelchair_accessible)]

# We perform the first pre-processing, concerning the Date and Zip code
trainPre <- preProcessDateZip(trainPre, segimon)
testPre <- preProcessDateZip(testPre, segimon)

# We want to add some other variables
trainPre <- addLanguageVar(trainPre)
testPre <- addLanguageVar(trainPre)

# First, we will only consider the "main" variables"
trainPre <- addLocationVars(trainPre)
testPre <- addLocationVars(testPre)

# Create all the dummy variables
trainPre <- convertTypeToDummy(trainPre)
testPre <- convertTypeToDummy(testPre)
# Convert all dummy variables to factors
trainPre <- dummyToFactor(trainPre,17)
testPre <- dummyToFactor(testPre,17)

# New Feature: Whether the place it is public
trainPre <- checkPublicPlace(trainPre)
testPre <- checkPublicPlace(testPre)

# New Feature: How many reviews mention the word family
trainPre <- checkFamilyReview(trainPre)
testPre <- checkFamilyReview(testPre)

# Pre-modelling phase: Make sure the training is done to the same vars that are contained in the test
interVars <- intersect(names(trainPre), 
                        names(testPre))

deleteTrainPre <- names(trainPre) [! names(trainPre) %in%  interVars ]
deleteTestPre <- names(testPre) [! names(testPre) %in%  interVars ]

trainPre <- as.data.table(trainPre[,  c(interVars,
                             "wheelchair_accessible"), with = F])
testPre <- as.data.table(testPre[,  c(interVars), with = F])

colsOrderTrain <- c(colnames(testPre), "wheelchair_accessible")
setcolorder(trainPre, colsOrderTrain)

# Saving Training and test set
saveRDS(trainPre, '/data/trainPreprocessed.RData')
saveRDS(testPre, '/data/testPreprocessed.RData')

# The training is done in the script 03_modelTraining
