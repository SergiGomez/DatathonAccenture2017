
#library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(lattice)
library(mlbench)
library(reshape2)
library(car)
library(plyr)
library(MASS)
library(ggplot2)
library(gridExtra)
library(pls)
library(elasticnet)
library(lars)
library(stats)
library(stringr)
library(iterators)
library(foreach)
library(lubridate)
library(randomForest)
library(RCurl)
library(data.table)
library(doSNOW)
library(doMC)
library(RMySQL)
library(matrixStats)
library(profr)
library(ssh.utils)
library(LaF)
library(GGally)
library(xgboost)

usePackage <- function(p) {
  if ( !is.element(p, installed.packages()[,1]) ) {
    install.packages(p, dep = TRUE)}
  require(p, character.only = TRUE)}

packages <- c("dplyr","reshape2","lubridate", "ggplot2", "ggmap", "caret", "ROCR", 
              "doParallel", "xgboost","rvest","stringr","foreach","doParallel", 
              "RCurl", "leaflet","rgdal", "caret", "data.table")

for (p in packages) { usePackage(p) }

