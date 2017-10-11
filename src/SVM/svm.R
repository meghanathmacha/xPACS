'%!in%' <- function(x,y)!('%in%'(x,y))

createSplits <- function(data, cv = 5, name, balance = FALSE)
{
  if(balance == TRUE)
  {
    library(DMwR)
    data$label = as.factor(data$label)
    data <- SMOTE(label ~ ., data  = data) 
  }
  flds <- createFolds(data$label, k = (cv+1), list = TRUE, returnTrain = FALSE)
  testdata = data[flds[[cv+1]],] 
  write.csv(file = paste0(name,"test.csv"), row.names = F,x = testdata)
  traindata = data[unlist(flds[1:cv]),]
  write.csv(file = paste0(name,"train.csv"), row.names = F, x = traindata)
  
  # Split train data into (cv) samples.
  require(caret)
  library(caret)
  flds <- createFolds(traindata$label, k = cv, list = TRUE, returnTrain = FALSE)
  for( iter in (1:cv))
  {
    sets = (1:cv)[(1:cv) %!in% iter] # Picked the training sets.
    train = traindata[unlist(flds[sets]),] # Sampled the train data
    write.csv(file = paste0(name,"train",iter,".csv"), row.names = F,x = train)    
    valid = traindata[unlist(flds[iter]),] # Sampled the validation data.
    write.csv(file = paste0(name,"valid",iter,".csv"), row.names = F,x = valid)    
    
  }
}

createSplitsouter <- function(data, outerfolds = 2, name)
{
  flds <- createFolds(data$label, k = (outerfolds), list = TRUE, returnTrain = FALSE)
  for(i in 1:outerfolds)
  {
    write.csv(file = paste0(name,"trainouter",i,".csv"), row.names = F,x = data[flds[[i]],])
  }
}

createSplitsinner <- function(oftdata, innerfolds = 3, outerfoldid, name)
{
  # Split train data into (innerfold) samples.
  library(caret)
  flds <- createFolds(oftdata$label, k = innerfolds, list = TRUE, returnTrain = FALSE)
  for( iter in (1:innerfolds))
  {
    sets = (1:innerfolds)[(1:innerfolds) %!in% iter] # Picked the training sets.
    train = oftdata[unlist(flds[sets]),] # Sampled the train data
    write.csv(file = paste0(name,"trainouter",outerfoldid,iter,".csv"), row.names = F,x = train)    
    valid = oftdata[unlist(flds[iter]),] # Sampled the validation data.
    write.csv(file = paste0(name,"valid",outerfoldid,iter,".csv"), row.names = F,x = valid)    
    
  }
  
}

createFoldsData <- function(outerfolds, innerfolds, name = "breast")
{
  for(outerid in 1:outerfolds)
  {
    traindata = read.csv(paste0(name,"trainouter",outerid,".csv"))
    createSplitsinner(oftdata = traindata,innerfolds = 3,outerfoldid = outerid,name = "breast")
    
  }
}


readData <- function(name)
{
  data = read.csv(paste0(name,".csv"))
  return(data)
}


chosen_svm = function(tdata,kernel_name, innerfolds, gamma = 10^(-3:3), cost = 10^(-3:3)){
  model <- tune.svm(label~., data = tdata, gamma =gamma, cost = cost, kernel = kernel_name,tunecontrol = tune.control(cross = innerfolds,
                                                                          sampling = "cross",best.model = TRUE))
  return(model$best.model)
}

svmrun <- function(outerfolds,innerfolds, kernel = "linear", add = TRUE, color , gamma, cost, model = "svm")
{
  for(outerid in 1:outerfolds)
  {
    sets = (1:outerfolds)[(1:outerfolds) %!in% outerid] # Picked the training sets. The outerid is the testing set.
    tdata = read.csv(paste0(name,"trainouter",sets[1],".csv"))
    for(id in sets[-1])
    {
      tdata = rbind(tdata,read.csv(paste0(name,"trainouter",id,".csv")))
    }
    svm_model = chosen_svm(tdata,kernel,innerfolds = 3, gamma = gamma, cost = cost)
    test = read.csv(paste0(name,"trainouter",outerid,".csv"), header = T)
    predprobs = predict(svm_model,subset(test,select = -c(label)))
    test = cbind(test,data.frame(prob1 = predprobs , prob0 = 1-predprobs))
    write.csv(file = paste0(name,model,kernel,outerid,"probs.csv"), row.names = F,x = test)
    
  }
  probs = read.csv(paste0(name,model,kernel,1,"probs.csv"))
  for(outerid in 2:outerfolds)
  {
    probs = rbind(probs,read.csv(paste0(name,model,kernel,outerid,"probs.csv")))
  }
  roc_pred <- prediction(probs$prob0,!probs$label)
  perf <- performance(roc_pred, "tpr", "fpr")
  plot(perf,col = color,main = "ROC", add = add)
  abline(0,1,col="grey")
  #get area under the curve
  write.csv(file = paste0(name,model,kernel,"probs.csv"), row.names = F,x = probs)
  
}


setwd("/nfshome/SHARED/BreastCancerData/BaseLines/SVM")
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
svmrun(outerfolds, innerfolds, model = "svm", add = F,color = cl[1], kernel = "linear", gamma = 10^(-3:3), cost = 10^(-3:3))
svmrun(outerfolds, innerfolds, model = "svm", add = T,color = cl[2], kernel = "polynomial", gamma = 10^(-3:3), cost = 10^(-3:3))
svmrun(outerfolds, innerfolds, model = "svm", add = T,color = cl[3], kernel = "radial", gamma = 10^(-3:3), cost = 10^(-3:3))


