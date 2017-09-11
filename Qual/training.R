
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
scale_data <- function(raw.data)
{
  data <- as.data.frame(apply(raw.data, MARGIN = 2, 
                              FUN = function(X) (X - min(X))/diff(range(X))))
  
  return(data)
}

model <- function(cv = 5,name, model = "model", add = FALSE, color,measure)
{
  for(id in 1:cv)
  {
    print("id")
    valid = read.csv(paste0(getwd(),"/Part2/",name,"train",id,".csv"))
    selpacks = read.csv(paste0(getwd(),"/Part2/","selectpacks",id,".csv"))
    decision = getDecisions(selpacks,valid,id)
    #Transposing the data frame.
    df.aree <- as.data.frame(t(decision))
    colnames(df.aree) <- df.aree[1, ]
    df.aree <- df.aree[-1, ]
    
    decision =as.data.frame(df.aree)
    #decision = df.aree[,is.nan(colSums(df.aree)) == 0]
    # The minimum distance from all the selected packs is used for ranking
    tvalid = valid
    tvalid$decision = apply(decision, 1, max) 
    
    decision = sigmoid(decision)
    decision$decision = apply(decision, 1, max) 
    decision = decision$decision
    

    tp = sum((tvalid$label ==0) & (tvalid$decision > 0))
    fp = sum((tvalid$label ==1) & (tvalid$decision > 0))
    tn = sum((tvalid$label ==1) & (tvalid$decision < 0))
    fn = sum((tvalid$label ==0) & (tvalid$decision < 0))
    print(paste0("Precision",tp/(tp+fp)))
    print(paste0("Recall",tp/(tp+fn)))
    # Here for us 1 is the normal points and 0 is the anomalies.
    library(ROCR)
    valid$prob0 = decision
    #valid$prob1 = 1-valid$prob0
    
    roc_pred <- prediction(valid$prob0,!valid$label)
    write.csv(file = paste0(name,model,id,"probs.csv"), row.names = F,x = valid)
  }
  probs = read.csv(paste0(name,model,1,"probs.csv"))
  roc_pred <- prediction(probs$prob0,!probs$label)
  perf <- ROCR::performance(roc_pred, "tpr", "fpr")
  write.csv(file = paste0(name,model,"probs.csv"), row.names = F,x = probs)
  plot(perf,col = color,main = "ROC", add = add)
  auc.perf = ROCR::performance(roc_pred, measure = measure)
  
  library(PRROC)
  s0 = probs[probs$label ==0,]$prob0
  s1 = probs[probs$label ==1,]$prob0
  prcurve = pr.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
  print(paste0("AUPRC",prcurve$auc.integral))
  prcurve = roc.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
  print(paste0("AUC",prcurve$auc))
  
}
library(dplyr)
library(plyr)
# We have the TFiles, DFiles in respective Train(i) folders.
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/Qual/")
# legend("bottomright", legend=c("Model", "SVM-Rad",
#                                "kGaussian", "DTree", "SVDD"), 
#        fill= c(cl[7],cl[2],cl[3], cl[6], cl[1]), ncol = 2, cex = 1.3)
cv = 1
cl= rainbow(7)
model(cv = 1,name = "breast",model = "model",add = F,color = cl[1],"f")
