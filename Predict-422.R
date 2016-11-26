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
#completeFun(DF, "y")
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
