'%!in%' <- function(x,y)!('%in%'(x,y))

createSplits <- function(data, outerfolds = 5, name, balance = FALSE)
{
  library(caret)
  if(balance == TRUE)
  {
    library(DMwR)
    data$label = as.factor(data$label)
    data <- SMOTE(label ~ ., data  = data) 
  }
  flds <- createFolds(data$label, k = (outerfolds), list = TRUE, returnTrain = FALSE)
  for( iter in (1:outerfolds))
  {
    sets = (1:outerfolds)[(1:outerfolds) %!in% iter] # Picked the training sets.
    train = data[unlist(flds[sets]),] # Sampled the train data
    write.csv(file = paste0(name,"train",iter,".csv"), row.names = F,x = train)    
    valid = data[unlist(flds[iter]),] # Sampled the validation data.
    write.csv(file = paste0(name,"valid",iter,".csv"), row.names = F,x = valid)    
    
  }
}

readData <- function(name)
{
  data = read.csv(paste0(name,".csv"))
  return(data)
}
marginal <- function(x,pcluster,predprobs)
{
  return(sum(predprobs$z[x,]*pcluster))
}

gaussianrun <- function(cv = 5,name, model = "gaussian", balance = FALSE, nclusters, add = FALSE, color)
{
  for(iter in (1:cv))
  {
    train = read.csv(paste0(name,"train",iter,".csv"))
    valid = read.csv(paste0(name,"valid",iter,".csv"))
    #Subsetting only the anomalies
    train = train[train$label == 0,]
    
    cluster = Mclust(subset(train,select = -c(label))) 
    # Multiple Guassian hence, we have 4 clusters
    # Ideally we set the number of clusters to the patterns we found.
    
    predprobs = predict(cluster, subset(valid,select = -c(label)))
    pcluster = cluster$parameters$pro
    
    certainity = lapply(1:nrow(valid),function(x){return(marginal(x,pcluster,predprobs))})
    
    valid = cbind(valid,data.frame(prob1 = 1 - unlist(certainity) , prob0 = unlist(certainity)))
    library(ROCR)
    
    # plot ROC
    roc_pred <- prediction(valid$prob0,!valid$label)
    write.csv(file = paste0(name,nclusters,model,iter,"probs.csv"), row.names = F,x = valid)

  }
  probs = read.csv(paste0(name,nclusters,model,1,"probs.csv"))
  for(outerid in 2:cv)
  {
    probs = rbind(probs,read.csv(paste0(name,nclusters,model,outerid,"probs.csv")))
  }
  roc_pred <- prediction(probs$prob0,!probs$label)
  perf <- ROCR::performance(roc_pred, "tpr", "fpr")
  write.csv(file = paste0(name,nclusters,model,"probs.csv"), row.names = F,x = probs)
  plot(perf,col = color,main = "ROC", add = add)
  abline(0,1,col="grey")

}
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/Gaussian")
name = "breast"
data = read.csv(paste0(name,'data.csv'),header = T) # Synthetic data
head(data)
# Data ready. Let us split the data into test and train and save it.
library(caret)
library(rpart)
library("e1071")
library(ROCR)
library(mclust)
cl = rainbow(7)
outerfolds = 3
innerfolds = 3
createSplits(data = data, outerfolds = outerfolds,name)
#gaussian(cv = 5,name = "breast",nclusters = 1)
cl = rainbow(7)
gaussianrun(cv = 3, name = name, nclusters = 2, color = cl[1], add = F)

