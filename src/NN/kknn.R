'%!in%' <- function(x,y)!('%in%'(x,y))

createSplitsouter <- function(data, outerfolds = 2, name, balance = FALSE)
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


nn <- function(cv = 5,name, model = "nn", nclusters = 5)
{
  auc = list()
  for(iter in (1:cv))
  {
    train = read.csv(paste0(name,"train",iter,".csv"))
    valid = read.csv(paste0(name,"valid",iter,".csv"))
    set.seed(9560)
    #Subsetting only the normal points ! 
    # train = train[train$label == 0,] SVDD paper does not do this!
    
    cluster = knn(train = as.matrix(subset(train,select = -c(label))),
                  test = as.matrix(subset(valid,select = -c(label))),
                  k = 1, prob = TRUE, cl = as.factor(train$label)) 
    # Ideally we set the number of clusters to the patterns we found.
    
    predprobs = predict(cluster, as.matrix(subset(valid,select = -c(label))))
    valid$prob1 = 1
    for(id in (1:nrow(valid)))
    {
      point = as.matrix(subset(valid[id,],select = -c(label,prob1)))
      cid = predprobs$cluster[id]
      center = predprobs$centers[cid,]
      valid[id,]$prob1 = dist(rbind(center,point),method ="euclidean")
    }
    library(clusterSim)
    valid$prob0 <- data.Normalization(valid$prob1, type = "n4")
    library(ROCR)
    valid$prob0 = 1 - valid$prob1
    
    # plot ROC
    roc_pred <- prediction(valid$prob1,valid$label)
    # perf <- performance(roc_pred, "tpr", "fpr")
    # plot(perf, col="red")
    # abline(0,1,col="grey")
    # get area under the curve
    auc[iter] = performance(roc_pred,"auc")@y.values
  }
  bestauc = which.max(auc)
  train = read.csv(paste0(name,"train",bestauc,".csv"))
  test = read.csv(paste0(name,"test.csv"))
  set.seed(9560)
  #Subsetting only the anomalies
  #train = train[train$label == 1,]
  train = train[train$label == 0,]
  
  cluster = cclust(as.matrix(subset(train,select = -c(label))),method= "kmeans", centers = nclusters) 
  # Ideally we set the number of clusters to the patterns we found.
  predprobs = predict(cluster, as.matrix(subset(test,select = -c(label))))
  test$prob1 = 1
  for(id in (1:nrow(test)))
  {
    point = as.matrix(subset(test[id,],select = -c(label,prob1)))
    cid = predprobs$cluster[id]
    center = predprobs$centers[cid,]
    test[id,]$prob1 = dist(rbind(center,point),method ="euclidean")
  }
  library(clusterSim)
  test$prob1 <- data.Normalization(test$prob1, type = "n4")
  library(ROCR)
  test$prob0 = 1 - test$prob1
  
  roc_pred <- prediction(test$prob1,test$label)
  perf <- performance(roc_pred, "tpr", "fpr")
  plot(perf, col="red", main = "ROC for Test Data")
  abline(0,1,col="grey")
  #get area under the curve
  write.csv(file = paste0(name,model,"testprobs.csv"), row.names = F,x = test)
  
}
scale_data <- function(raw.data)
{
  data <- as.data.frame(apply(raw.data, MARGIN = 2, 
                              FUN = function(X) (X - min(X))/(max(X) - min(X))))
  
  return(data)
}

nnrun <- function(outerfolds = 5,name, model = "nn", color, add = FALSE)
{
  library(FNN)
  for(outerid in 1:outerfolds)
  {
    sets = (1:outerfolds)[(1:outerfolds) %!in% outerid] # Picked the training sets. The outerid is the testing set.
    tdata = read.csv(paste0(name,"trainouter",sets[1],".csv"))
    for(id in sets[-1])
    {
      tdata = rbind(tdata,read.csv(paste0(name,"trainouter",id,".csv")))
    }
    test = read.csv(paste0(name,"trainouter",outerid,".csv"), header = T)
    # Now our training data is restricted to only normal points.
    tndata = tdata[tdata$label ==1,] 
    labels = tndata$label
    train = tndata[,-length(tndata)]
    tlabels = test$label
    test= test[,-length(test)]
    testmodel <- knn(train, test, labels, k = 1, algorithm="cover_tree")
    indices = attr(testmodel, "nn.index")
    numerdists = attr(testmodel, "nn.dist")
    newtest = train[indices,]
    newtrain = train[-indices,]
    newlabels = labels[-indices]
    trainmodel <- knn(newtrain, newtest, newlabels, k = 1, algorithm="cover_tree")
    denomdists = attr(trainmodel, "nn.dist")
    dists = numerdists/denomdists
    dists[is.na(dists)] = 0
    test$prob0 = dists
    test$label = tlabels
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
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/NN")
name = "breast"
data = read.csv(paste0(name,'data.csv'),header = T) # Synthetic data

head(data)
# Data ready. Let us split the data into test and train and save it.
library(caret)
library(rpart)
library(FastKNN)
library(ROCR)
outerfolds = 5
createSplitsouter(data = data, outerfolds =  outerfolds,name,balance = F)
nnrun(outerfolds = outerfolds,name = "breast",model = "nn",color = "red")
