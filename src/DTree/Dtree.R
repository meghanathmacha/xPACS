'%!in%' <- function(x,y)!('%in%'(x,y))

ctree <- function(cv = 5,name, model = "ctree", balance = TRUE)
{
  auc = list()
  for(iter in (1:cv))
  {
    train = read.csv(paste0(name,"train",iter,".csv"))
    valid = read.csv(paste0(name,"valid",iter,".csv"))
    set.seed(9560)
    fit <- rpart(label ~.,
                 data=train,
                 method="class",
                 control=rpart.control(minbucket=10, cp=0))
    # Look at rpart.control.
    # minbucket will ensure there are atleast that many number of points in the leaves
    # Direct interpretation to the mass threshold that we have.
    vpredict = predict(fit, valid,type="prob")
    valid = cbind(valid,data.frame(prob0 = vpredict[,1], prob1 = vpredict[,2]))
    library(ROCR)
    
    # plot ROC
    roc_pred <- prediction(valid$prob1,valid$label)
    # perf <- performance(roc_pred, "tpr", "fpr")
    # plot(perf, col="red")
    # abline(0,1,col="grey")
    # get area under the curve
    auc[iter] = performance(roc_pred,"auc")@y.values
  }
  # Choosing the model with the best auc.
  bestauc = which.max(auc)
  train = read.csv(paste0(name,"train",bestauc,".csv"))
  test = read.csv(paste0(name,"test.csv"))
  fit <- rpart(label ~.,
               data=train,
               method="class",
               control=rpart.control(minbucket=10, cp=0))
  tpredict = predict(fit, test,type="prob")
  test = cbind(test,data.frame(prob0 = tpredict[,1], prob1 = tpredict[,2]))
  roc_pred <- prediction(test$prob1,test$label)
  perf <- performance(roc_pred, "tpr", "fpr")
  cl = rainbow(7)
  plot(perf, col=cl[6], main = "ROC for Test Data", add = T)
  abline(0,1,col="grey")
  #get area under the curve
  write.csv(file = paste0(name,model,"testprobs.csv"), row.names = F,x = test)
}


createSplits <- function(data, cv = 5, name, balance = FALSE)
{
  if(balance == TRUE)
  {
    # Not using SMOTE since we plan to undersample.
    set.seed(9560)
    library(dplyr)
    anoms = data[data$label ==0,]
    nanoms = data[data$label ==1,]
    data = rbind(anoms,dplyr::sample_n(nanoms,nrow(anoms)))
    # library(DMwR)
    # data$label = as.factor(data$label)
    # data <- SMOTE(label ~ ., data  = data) 
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

createSplitsouter <- function(data, outerfolds = 2, name, balance = TRUE)
{
  if(balance == TRUE)
  {
    # Not using SMOTE since we plan to undersample.
    set.seed(9560)
    library(dplyr)
    anoms = data[data$label ==0,]
    nanoms = data[data$label ==1,]
    data = rbind(anoms,dplyr::sample_n(nanoms,nrow(anoms)))
    # library(DMwR)
    # data$label = as.factor(data$label)
    # data <- SMOTE(label ~ ., data  = data) 
  }
  flds <- createFolds(data$label, k = (outerfolds), list = TRUE, returnTrain = FALSE)
  for(i in 1:outerfolds)
  {
    write.csv(file = paste0(name,"trainouter",i,".csv"), row.names = F,x = data[flds[[i]],])
  }
}

chosen_dtree = function(tdata, innerfolds, maxdepth)
  {
  model <- tune.rpart(label~., data = tdata, maxdepth = maxdepth, 
                    tunecontrol = tune.control(cross = innerfolds, sampling = "cross",best.model = TRUE))
  return(model$best.model)
}

dtreerun <- function(outerfolds,innerfolds, add = TRUE, color , maxdepth = maxdepth, model = "dtree")
{
  for(outerid in 1:outerfolds)
  {
    sets = (1:outerfolds)[(1:outerfolds) %!in% outerid] # Picked the training sets. The outerid is the testing set.
    tdata = read.csv(paste0(name,"trainouter",sets[1],".csv"))
    for(id in sets[-1])
    {
      tdata = rbind(tdata,read.csv(paste0(name,"trainouter",id,".csv")))
    }
    anoms = tdata[tdata$label ==0,]
    nanoms = tdata[tdata$label ==1,]
    tdata = rbind(anoms,dplyr::sample_n(nanoms,nrow(anoms)))
    dtreemodel = chosen_dtree(tdata,innerfolds = 3,  maxdepth = maxdepth)
    test = read.csv(paste0(name,"trainouter",outerid,".csv"), header = T)
    predprobs = predict(dtreemodel,subset(test,select = -c(label)))
    test = cbind(test,data.frame(prob1 = predprobs , prob0 = 1-predprobs))
    write.csv(file = paste0(name,model,outerid,"probs.csv"), row.names = F,x = test)
    
  }
  probs = read.csv(paste0(name,model,1,"probs.csv"))
  for(outerid in 2:outerfolds)
  {
    probs = rbind(probs,read.csv(paste0(name,model,outerid,"probs.csv")))
  }
  roc_pred <- prediction(probs$prob0,!probs$label)
  perf <- performance(roc_pred, "tpr", "fpr")
  plot(perf,col = color,main = "ROC", add = add)
  abline(0,1,col="grey")
  #get area under the curve
  write.csv(file = paste0(name,model,"probs.csv"), row.names = F,x = probs)
  
}


readData <- function(name)
{
  data = read.csv(paste0(name,".csv"))
  return(data)
}
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/DTree")
name = "breast"
data = read.csv(paste0(name,'data.csv'),header = T) # 1 is normal 0 is anoms
head(data)
# Data ready. Let us split the data into test and train and save it.
library(caret)
library(rpart)
library("e1071")
library(ROCR)
# createSplits(data = data, cv = 5,name,balance = TRUE)
# ctree(cv = 5,name = name)

cl = rainbow(7)
outerfolds = 3
innerfolds = 3
createSplitsouter(data = data,outerfolds = 3,name = "breast", balance = FALSE) # Created data for the outerfold splits.
dtreerun(outerfolds = outerfolds, innerfolds = innerfolds,color = cl[4],maxdepth = c(1,5,10,15,20,30),model = "dtree", add = F)
