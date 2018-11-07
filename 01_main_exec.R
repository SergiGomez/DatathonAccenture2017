source('00_initialization.R')
source('02_preProcessing.R')
source('03_modelTraining.R')

# ----- Execution parameters ---------------------------------------------------
# Features that can be added to the Dataset
addZipCode <- TRUE
useSegimon <- FALSE
addLangVars <- TRUE
addGeoVars <- TRUE
useDummyVars <- TRUE
addPublicPlaceVar <- TRUE
addFamilyReviewVar <- TRUE

# ----- Data Import ------------------------------------------------------------
cat("Reading the train and test data \n")
train  <- fread('data/train.csv')
test   <- fread('data/test.csv')
segimon <- fread('data/taula_segimon.csv')

# Exploratory Data Analysis ----------------------------------------------------
exploratoryDataAnalysis(train)

# Data Processing and Feature Engineering --------------------------------------
trainPro <- performDataProcessing(dt = copy(train), 
                                  set = "train",
                                  segimon = segimon,
                                  addZipCode = addZipCode,
                                  useSegimon = useSegimon,
                                  addLangVars = addLangVars,
                                  addGeoVars = addGeoVars,
                                  useDummyVars = useDummyVars,
                                  addPublicPlaceVar = addPublicPlaceVar, 
                                  addFamilyReviewVar = addFamilyReviewVar)

testPro <- performDataProcessing(dt = copy(test),
                                 set = "test",
                                 segimon = segimon,
                                 addZipCode = addZipCode,
                                 useSegimon = useSegimon,
                                 addLangVars = addLangVars,
                                 addGeoVars = addGeoVars,
                                 useDummyVars = useDummyVars,
                                 addPublicPlaceVar = addPublicPlaceVar,
                                 addFamilyReviewVar = addFamilyReviewVar)

# Pre-modelling phase ---------------------------------------------------------- 
dtModeling <- preModelingChecks(train = trainPro, 
                                test = testPro)
trainModeling <- dtModeling$train
testModeling <- dtModeling$test

# Saving Training and test set so they can be picked by someone else in the team
saveRDS(trainModeling, '/data/trainModeling.RData')
saveRDS(testModeling, '/data/testModeling.RData')

# The training is done in the script 03_modelTraining
