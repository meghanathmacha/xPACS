createSplitsouter <- function(data, outerfolds = 2, name)
{
  flds <- createFolds(data$label, k = (outerfolds), list = TRUE, returnTrain = FALSE)
  for(i in 1:outerfolds)
  {
    write.csv(file = paste0(name,"trainouter",i,".csv"), row.names = F,x = data[flds[[i]],])
  }
}

setwd("/nfshome/SHARED/BreastCancerData/BaseLines/KDE/")
name = "breast"
data = read.csv(paste0(name,'data.csv'),header = T) # Synthetic data
head(data)
# Data ready. Let us split the data into test and train and save it.
library(caret)
library(rpart)
library("e1071")
library(ROCR)
# createSplits(data = data, cv = 5,name,balance = F)
# svmrun(cv = 5,name = "breast", kernel = "radial")
# svmrun(cv = 5,name = "breast", kernel = "linear")

cl = rainbow(7)
outerfolds = 3
innerfolds = 3
createSplitsouter(data = data,outerfolds = 3,name = "breast") # Created data for the outerfold splits.
