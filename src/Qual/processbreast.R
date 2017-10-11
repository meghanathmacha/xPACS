
'%!in%' <- function(x,y)!('%in%'(x,y))

sigmoid = function(x) {
  1 / (1 + exp(-x))
}

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

dfileread <- function(x, testdata, iter)
{
  id = unique(x$subid)
  print(id)
  decisions = data.frame("id0" = rep(-1,nrow(testdata)))
  if(file.exists(paste0(getwd(),"/Part2/Train",iter,'/DFile',id,'.csv'))){
    ell = read.csv(paste0(getwd(),"/Part2/Train",iter,'/DFile',id,'.csv'),header = F)
    names(ell) = c(paste0("id",id))
    decisions = cbind(decisions,ell)
  }
  return(decisions[,-1])
}

tfileread <- function(x, testdata, iter)
{
  id = unique(x$subid)
  print(id)
  decisions = data.frame("id0" = rep(-1,nrow(testdata)))
  if(file.exists(paste0(getwd(),"/Part2/Train",iter,'/TFile',id,'.csv'))){
    ell = read.csv(paste0(getwd(),"/Part2/Train",iter,'/TFile',id,'.csv'),header = F)
    names(ell) = c(paste0("id",id))
    decisions = cbind(decisions,ell)
  }
  return(decisions[,-1])
}

getDecisions <- function(subs,testdata,cv)
{
  decisions = bind_cols(ddply(subs, .(subid), dfileread,testdata,cv))
  return(decisions)
}

getDecisionsTest <- function(subs,testdata,cv)
{
  decisions = bind_cols(ddply(subs, .(subid), tfileread,testdata,cv))
  return(decisions)
}
model <- function(cv = 5,name, model = "model")
{
  auc = list()
  for(id in 1:cv)
  {
    valid = read.csv(paste0(getwd(),"/Part2/",name,"valid",id,".csv"))
    selpacks = read.csv(paste0(getwd(),"/Part2/","selectpacks",id,".csv"))
    decision = getDecisions(selpacks,valid,id)
    #Transposing the data frame.
    df.aree <- as.data.frame(t(decision))
    colnames(df.aree) <- df.aree[1, ]
    df.aree <- df.aree[-1, ]
    
    decision =df.aree
    # The minimum distance from all the selected packs is used for ranking
    decision = sigmoid(decision)
    decision$decision = apply(decision, 1, max) 
    decision = decision$decision
    # Decision will have the length from sphere for all the testing points !
    
    #valid$prob1 <- (decision-min(decision))/(max(decision)-min(decision))
    # Larger distances mean more normal.
    
    # Here for us 1 is the normal points and 0 is the anomalies.
    library(ROCR)
    valid$prob0 = decision
    valid$prob1 = 1-valid$prob0
    
    roc_pred <- prediction(valid$prob0,!valid$label)
    auc[id] = performance(roc_pred,"auc")@y.values
  }
  bestauc = which.max(auc)
  print(paste0("Found best",bestauc))
  test = read.csv(paste0(getwd(),"/Part2/",name,"test",".csv"))
  selpacks = read.csv(paste0(getwd(),"/Part2/","selectpacks",bestauc,".csv"))
  decision = getDecisionsTest(selpacks,test,bestauc)
  df.aree <- as.data.frame(t(decision))
  colnames(df.aree) <- df.aree[1, ]
  df.aree <- df.aree[-1, ]
  
  decision =df.aree
  # The minimum distance from all the selected packs is used for ranking
  
  decision = sigmoid(decision)
  decision$decision = apply(decision, 1, max) 
  decision = decision$decision
  # Decision will have the length from sphere for all the testing points !
  
#  test$prob1 <- (decision-min(decision))/(max(decision)-min(decision))
  # Larger distances mean more normal.
  
  # Here for us 1 is the normal points and 0 is the anomalies.
  library(ROCR)
  test$prob0 = decision
  test$prob1 = 1 - decision

  roc_pred <- prediction(test$prob0,!test$label)
  perf <- performance(roc_pred, "tpr", "fpr")
  cl = rainbow(7)
  plot(perf, col = cl[7],main = "ROC for Test Data", add = T)
  abline(0,1,col="grey")
  #get area under the curve
  write.csv(file = paste0(name,model,"testprobs.csv"), row.names = F,x = test)
}



# total of 9 features and 1 label.
# Scaling the data
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/Qual")
source("subspacev4.R")
name = "breast"
data = read.csv(paste0(name,'data.csv'))


#createSplits(data = sdata, outerfolds = outerfolds,name,balance = F)
source("subspacev4.R")
rect = computeFirstPart(data, name = name, pruneinit = FALSE, dom = FALSE, precision = 3, trainid = 1) 

# Created the subs required for Part-2 for all training datasets.
# Run the Part 2 by running ellipse.bash
# Run the Part 3 by running R --slave -f parseEllipsoids.R in Part2/
# Run Part 3 decision.bash


