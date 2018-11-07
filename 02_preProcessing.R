# ------------------------ PreProcessing Functions --------

preProcessDateZip <- function(dt, segimon) {
  
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
  
  # After the first iteration, we have seen that the data coming from segimon 
  # doesn't seem to add predicting value
  
  #dt[ isBarcelona == "1", endingZipCodeBcn := zip_code_Ending]
  # We merge the train datatable with the segimon dt to obtain the Zip code
  # of the suburbs
  #segimon[, endingZipCodeBcn := DIST_POST]
  #segimon <- segimon[!is.na(endingZipCodeBcn)]
  #segimon[, endingZipCodeBcn := as.character(endingZipCodeBcn)]

  # dt <- merge(dt, 
  #             unique(segimon[, .(endingZipCodeBcn, BARRI)]), 
  #             by = "endingZipCodeBcn", 
  #             all.x = TRUE,
  #             allow.cartesian = TRUE)
  
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