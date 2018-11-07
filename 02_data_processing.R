# ------------------------ Exploratory Data Analysis -------
exploratoryDataAnalysis <- function(dt) {
  
  # Get those features that are integers or numeric
  numIntVars <- colnames(dt) [lapply(dt,class) %in% c("integer","numeric")]
  
  # Check for the correlations between numeric variables
  correlations <- cor(x = dt[, numIntVars, with = FALSE],
                      use = "complete.obs")
  # Check for Skewness
  skewValues <- apply(dt[, numIntVars, with = FALSE], 2, e1071::skewness)
  
  # depending if numerical variables need to be pre-processed
  transTrain <- preProcess(dt[, numIntVars, with = FALSE], 
                           method = c("BoxCox", "center", "scale", "pca"))
  pcaObject_train <- prcomp(dt[, numIntVars, with = FALSE],
                            center = TRUE, scale. = TRUE)
  
  # Percentage of Missing values for each of the predictors
  frequencyMissingValues <- data.table()
  for (pred in names(dt)) {
    dtMissing <- data.table(predName = paste0("frequency_",pred), 
                     freqNA = nrow(train[ is.na(get(pred)) ]) / nrow(train))
    frequencyMissingValues <- rbind(frequencyMissingValues, dtMissing)
  }
  
  # Glimpse of the predictors importance
  impValues <- filterVarImp(x = dt, 
                            y = dt$wheelchair_accessible)
  
  dtVarsInfo <- data.table(predName = names(dt), 
                           imp = impValues$Overall)
  
  dtVarsInfo <- merge(dtVarsInfo,
                      frequencyMissingValues, 
                      by = "predName", 
                      all = TRUE)
  
  dtVarsInfo <- dtVarsInfo[order(imp, decreasing = T)]
  
}

# ------------------------ PreProcessing Functions --------
performDataProcessing <- function(dt,
                                  set = "train",
                                  addZipCode= FALSE,
                                  useSegimon = FALSE,
                                  addLangVars = FALSE,
                                  addGeoVars = FALSE,
                                  useDummyVars = FALSE,
                                  addPublicPlaceVar = FALSE,
                                  addFamilyReviewVar = FALSE) {
  
  if (set == "train") {
    # Transform label into numeric
    dt$wheelchair_accessible <- ifelse(dt$wheelchair_accessible == TRUE, 1, 0)
    dt[, wheelchair_accessible := as.factor(wheelchair_accessible)]
  }
  
  # We perform the first pre-processing, concerning the Date and Zip code
  dt <- preProcessDateZip(dt, segimon, addZipCode, useSegimon)
 
  # If we want to add some variables regarding the language of the reviews
  if (addLangVars) dt <- addLanguageVar(dt)
  
  #If we want to add some variables regarding the location of the place
  if (addGeoVars) dt <- addLocationVars(dt)
 
  if (useDummyVars) {
    # Create all the dummy variables
    dt <- convertTypeToDummy(dt)
    # Convert all dummy variables to factors
    dt <- dummyToFactor(dt,17)
  }
  
  # New Feature: Whether the place it is public
  if (addPublicPlaceVar) dt <- checkPublicPlace(dt)
  
  # New Feature: How many reviews mention the word family
  if (addFamilyReviewVar) dt <- checkFamilyReview(dt)
  
  return(dt)
}
preProcessDateZip <- function(dt, segimon, 
                              addZipCode = FALSE,
                              useSegimon = FALSE) {
  
  # Convert the datecreation to an understandble date for users
  dt[, datecreation_fs := as.Date(as.POSIXct(datecreation_fs, 
                                origin = "1970-01-01",tz = "GMT"))]
  
  # Extract the ZIP code from the venue_address, but first I need to change the Encoding
  dt[, venue_address := iconv(enc2utf8(venue_address),sub="byte")]
  dt[, zip_code := regmatches(venue_address, gregexpr("\\d{5}([-]?\\d{4})?", 
                                                      venue_address, perl = TRUE))]
  dt[, zip_code := substr(zip_code,1,5)]
  dt[, zip_code := as.factor(zip_code)]
  # Variable with the ZIP Code ending
  dt[, zip_code_Ending := substr(zip_code,4,5)]
  
  # We add a variable that will tell us if the place belongs to Barcelona
  dt[, isBarcelona := as.factor("0")]
  dt[ substr(zip_code,1,3) == "080", isBarcelona := as.factor("1")]
  
  if (!addZipCode) dt[, ':='(zip_code = NULL, 
                             zip_code_Ending = NULL, 
                             isBarcelona = NULL)]
  
  # After the first iteration, we have seen that the data coming from segimon 
  # doesn't seem to add predicting value
  if (useSegimon) {
    dt[ isBarcelona == "1", endingZipCodeBcn := zip_code_Ending]
    # We merge the train datatable with the segimon dt to obtain the Zip code
    # of the suburbs
    segimon[, endingZipCodeBcn := DIST_POST]
    segimon <- segimon[!is.na(endingZipCodeBcn)]
    segimon[, endingZipCodeBcn := as.character(endingZipCodeBcn)]
    
    dt <- merge(dt,
                unique(segimon[, .(endingZipCodeBcn, BARRI)]),
                by = "endingZipCodeBcn",
                all.x = TRUE,
                allow.cartesian = TRUE)

  }
  
  return(dt)
}

aggregateZipCode <- function(train) {
  
  setkey(train)
  trainZip <- train[,.( meanLatitude = mean(latitude, na.rm = T),
                        meanLongitude = mean(longitude, na.rm = T),
                        meanAccessibility = mean(wheelchair_accessible, na.rm = T)),
                    by = c("zip_code")]
  
  return(trainZip)
}

addLocationVars <- function(dt) {
  
  latitudeMean <- mean(dt[,latitude], na.rm = T)
  longitudeMean <- mean(dt[,longitude], na.rm = T)
  
  dt[, distanceCenter := sqrt((latitude - latitudeMean)^2 + (longitude - longitudeMean)^2)]
  # Input missing value for the distance to the center
  dt[is.na(distanceCenter), distanceCenter := 99]
  
  dt[, verifiedbyowner_fs := ifelse(verifiedbyowner_fs == TRUE,
                                    as.factor("1"),
                                    as.factor("0"))]
  # Input missing value for the  owner's verification
  dt[is.na(verifiedbyowner_fs), verifiedbyowner_fs := 0]
  
  dt[ , userRepeat := checkinscount_fs / userscount_fs]
  
  return(train)
  
}

convertTypeToDummy <- function(dt) {
  
  # create vector with all types 
  types_list <- list()
  types <- c()
  for (i in 1:nrow(dt)) {
    # split based on comma
    row_types <- strsplit(dt$type[i], ",")
    # remove leading and trailing whitespace
    row_types[[1]] <- sapply(row_types[[1]], 
                             function(x) gsub("^\\s+|\\s+$", "", x))
    # add to list
    types_list <- c(types_list, row_types)
    # add to unique vector
    types <- unique(c(types, unlist(row_types)))
  }
  
  # remove NA
  types <- types[!(is.na(types))]
  
  # create new column for each type
  for (i in 1:length(types)) {
    dt[[types[i]]] <- 0
  } 
  
  # fill up dummy columns
  for (i in 1:length(types_list)) {
    for (j in types) {
      if ( j %in% types_list[[i]]) {
        dt[[j]][i] <- 1
      }
    }
  }
  
  return(dt)
}

dummyToFactor <- function(dt, nColStart) {
  
  dt[,nColStart:ncol(train)] <- lapply(dt[,nColStart:ncol(train)], 
                                          function(x) as.factor(x))
  
  return(dt)
}

addLanguageVar <- function(dt) {
  
  # Array with all the variables referring to the Language reviews
  language_reviews <- c("Language_Review_1", "Language_Review_2", "Language_Review_3",
                        "Language_Review_4", "Language_Review_5")

  dt[, (language_reviews) := lapply(.SD, function(x){
                             ifelse(!is.na(x), x, "Other")}),
                             .SDcols = language_reviews]

 dt[, numberLocal := (Language_Review_1 %in% c("es","ca") +
                      Language_Review_2 %in% c("es","ca") +
                      Language_Review_3 %in% c("es","ca") +
                      Language_Review_4 %in% c("es","ca") +
                      Language_Review_5 %in% c("es","ca"))]
 
 dt[, numberForeig := ((!Language_Review_1 %in% c("es","ca","Other")) +
                       (!Language_Review_2 %in% c("es","ca","Other")) +
                       (!Language_Review_3 %in% c("es","ca","Other")) +
                       (!Language_Review_4 %in% c("es","ca","Other")) +
                       (!Language_Review_5 %in% c("es","ca","Other")))]
   
 # We normalize to 1
 dt[ , numberLocal := numberLocal / 5]
 dt[ , numberForeig := numberForeig / 5]
  
}

checkFamilyReview <- function(dt) {
  
  dt[ , isFamiliyReview1 := length(grep("family|familia|hijos|children", c(Review_1,
                                                                      Review_2,
                                                                      Review_3,
                                                                      Review_4,
                                                                      Review_5)), value = T)]
  
  familyNames <- c("family","familiar","hijos","children")
  dt[ Language_Review_1 == "es" | Language_Review_1 == "ca", isFamiliyR1 := grepl("familiar", Review_1, ignore.case=TRUE)]
  
}

checkPublicPlace <- function(dt) {
  
  dt[ , isPublic := "0"]
  dt[    library == 1 |
        museum == 1 |
        school == 1 | university == 1 |
        hospital == 1 | political == 1 |
        police == 1, isPublic := "1"]

dt[, isPublic := as.factor(isPublic)]

return(dt)  
}

# ------------------------ Premodelling checking --------
preModelingChecks <- function(train, test) {
  # Make sure the training is done to the same vars that are contained in the test
  
  interVars <- intersect(names(train), 
                         names(test))
  
  deleteTrainPre <- names(train) [! names(train) %in%  interVars ]
  deleteTestPre <- names(test) [! names(test) %in%  interVars ]
  
  train <- as.data.table(train[, c(interVars,
                                        "wheelchair_accessible"), with = F])
  test <- as.data.table(test[, c(interVars), with = F])
  
  colsOrderTrain <- c(colnames(test), "wheelchair_accessible")
  setcolorder(train, colsOrderTrain)
  
  return(list(train = train, test = test))
}