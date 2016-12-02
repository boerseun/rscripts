########################################
## PREDICT 422
## Common Functions
########################################



# Example code - Credit Dr. Jennifer Wightman
separateRFA = function(xData,varName)
{
  bytes = c("R","F","A")
  newVarNames = paste(varName,bytes, sep="_")
  
  for (ii in 1:length(bytes)) # Loop over 1 to 3 (corresponding to R, F, and A)
  {
    # Find the unique values for current byte
    byteVals = unique(substr(levels(xData[,varName]),ii,ii))
    
    for (jj in 1:length(byteVals)) # Loop over unique byte values
    {
      rowIdx = substr(xData[,varName],ii,ii) == byteVals[jj]
      xData[rowIdx,newVarNames[ii]] = byteVals[jj]
    }
    
    xData[,newVarNames[ii]] = factor(xData[,newVarNames[ii]])
  }
  
  return(xData)
}

#### Removing specific columns from a Dataframe based on the ####
# Column Name
# Example use:
# DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA), z=c(NA, 33, 22))
# completeFun(DF, "y")
##   x  y  z
## 1 1  0 NA
## 2 2 10 33

#completeFun(DF, c("y", "z"))
##   x  y  z
## 2 2 10 33
####
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}



#### Removing outliers in dataset ####
# Example use:
# set.seed(1)
# x <- rnorm(100)
# x <- c(-10, x, 10)
# y <- remove_outliers(x)
# par(mfrow = c(1, 2))
# boxplot(x)
# boxplot(y)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


# Prediction function from Section 6.5.3 of ISLR
predict.regsubsets = function(object,newdata,id,...)
{
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object,id=id)
  xvars = names(coefi)
  result = mat[,xvars] %*% coefi
  return(result)
}

# Model validation step comparing various MSEs
# Function Credit: Dr. Wightman 
calcMSE = function(model,modelLabel,dataSet,trainIdx,newX=NULL)
{
  # The predict method for glmnet will need to be called differently from the
  # other predict methods.
  if ("glmnet" %in% class(model)) {
    predVals = predict(model,newX,type="response")
  } else {
    predVals = predict(model,dataSet)
  }
  MSE = list(
    name = modelLabel,
    train = mean( (predVals[trainIdx] - dataSet$DAMT[trainIdx])^2 ),
    test = mean( (predVals[-trainIdx] - dataSet$DAMT[-trainIdx])^2 )
  )
  
  return(MSE)
}


#--------------------------------------
# Classification Performance
#--------------------------------------
class.perf <- function(target, probs, thresh){

    # Classify based on predicted probabilities and optimal threshold.
    predClass <- rep("0", length(probs))
    predClass[probs > thresh] <- "1"
    predClass <- factor(predClass)

    # Generate confusion matrix for training data
    confMat <- table(target, predClass, dnn = c("Actual", "Predicted"))

    # Calculate TP and FP rates.
    # Note:these calculations will need to be modified if you transpose
    #   the confusion matrix from the version above.
    TPrate <- confMat["1", "1"] / sum(confMat["1", ])
    FPrate <- confMat["0", "1"] / sum(confMat["0", ])

    # Store results in list to pass out of function.
    results <- list(predClass = predClass, confMat = confMat, 
                    TPrate = TPrate, FPrate = FPrate)
}
