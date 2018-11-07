# Here some helpful tools for Predictive Modelling 
segData <- subset(segmentationOriginal, Case == "Train")
dt <- copy(segData)

# Chapter 3: Pre-processing

# TRANSFORMATIONS of Predictors
   # Skewness
  skewValues <- apply(x, 2, skewness)
 
  # To determine which type of transformation should be used,
  # BoxCoxTrans, can find the appropriate transformation and apply them to the new data:
  areaCh1Transf <- BoxCoxTrans(dt[,AreaCh1]) 
  # After transformation
  predict(areaCh1Transf, head(dt[,AreaCh1]))
  # preProcess(), applies this transformation to a set of predictors
  
  # prcomp can be used for PCA. The data are centered and scaled prior to PCA.
  pcaObject <- prcomp(dt, center = TRUE, scale. = TRUE)
  # Calculate the cumulative percentage of variance which each component accounts for.
  percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
  # The transformed values are stored in pcaObject as a sub-object called x
  pcaObject[,x]
  # sub-object called rotation stores the variable loadings,
  pcaObject[,rotation]
  # spatialSign contains functionality for the spatial sign transformation
  spatialSign(dt)
  # IMPUT MISSING VALUES: impute.knn uses K- nearest neighbors to estimate the missing data. 
  
  # PRE-PROCESS To administer a series of transformations to multiple data sets, 
  # the caret class preProcess has the ability to transform, center, scale, or impute values, 
  # as well as apply the spatial sign transformation and feature extraction. 
  #The function calculates the required quantities for the transformation. 
  trans <- preProcess(dt, method = c("BoxCox", "center", "scale", "pca"))
  #After call- ing the preProcess function, the predict method applies the results to a set of data.
  transformed <- predict(trans, dt)
  # The order in which the possible transformation are applied is transformation, centering, scaling, imputation, feature extraction, and then spatial sign.
  
# FILTERING 
  nearZeroVar(dt)
  # When predictors should be removed, a vector of integers is  
  # returned that indicates which columns should be removed.
  # to Filter on BETWEEN-PREDICTOR correlations,
  # the cor function can calculate the correlations between predictor variables:
  correlations <- cor(dt)
  #To visualize the correlation structure of the data
  #corrplot(correlations, is.corr = T, method = "square", order = "hclust")
  ggcorr(correlations)

# CREATING DUMMY VARIABLES
  
  
# Chapter4: OVERFITTING AND MODEL TUNNING
  
  # DATA SPLITTING
  trainingRows <- createDataPartition(outcomeDT, p = 0.80, list = F)
  trainPredictors <- predictorsDT[ trainingRows ]
  trainOutcome <- outcomeDT[ trainingRows ]
  testPredictors <- predictorsDT[ - trainingRows ]
  testOutcome <- outcomeDT[ - trainingRows ]
  # RESAMPLING
  set.seed(1)
  #Page 82 APM
  createResamples() #(for bootstrapping)
  createFolds() # (for k-old crossvalidation)
  createMultiFolds() #  (for repeated cross-validation)
  
  # Basic Model Building in R (page 83)
       # The formula interface (∼)
       # The non-formula interface:
  modelFunction(x = housePredictors, y = price)
  
  # Determination of TUNING PARAMETERS
      # Choose Tuning Parameters using Resampling: Set of candidate values are evaluated using different resamples
      # A profile can be created to understand the relationship between performance and the parameter values
      # tune() -> can evaluate four types of models across a range of parameters.
      # errorest() -> can resample single models 
      # train() -> has built-in modules for 144 models for different resampling methods, and algorithms for choosing the best model
  fitControl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE, verboseIter = TRUE)
  svmFit <- train(Class~.,
                  data=GermanCreditTrain,
                  method="svmRadial",
                  preProcess = c("center","scale"),
                  tuneLength = 10,
                  trControl= fitControl)
  
  # Performance Profile -> A line plot of the average performance
  plot(svmFit, scales = list(x=list(log=2)))
  
  # New Samples with this model: 
  predictedClasses_probs <- predict(svmFit, newdata=GermanCreditTest, type="prob")
  
  #COMPARISON BETWEEN MODELS
     # We compare these two models with RESAMPLES
     # VISUALIZING the comparison: the resamples class has --> xyplot.resamples
     # ASSESS NUMERICALLY the difference between the models
  
  
  #EVALUATING THE MODEL 
  
  #IMPORTANT STEP: For evaluating the quality of the model --> VISUALIZE THE RESULTS. 
       # Observed Values vs Predicted Values
       # Predicted Values vs Residuals 
  
  # --------------------------- OTHER TOOLS ----------------------------
  # Remove columns by their j-index
  dt <- dt[, -(1:3)]
  # Remove some variables that have a pattern in their name
  ColNameWithPattern <- grep("Status", names(dt))
  dt <- dt[, c(ColNameWithPattern) := NULL ]
  # Convert all variables of a dt
  dtCols <- names(dt)
  for (col in dtCols) {
    dt[ , (col) := as.numeric(dt[,(col)]) ]
  }
  