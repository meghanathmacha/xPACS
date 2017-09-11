

fileread <- function(x)
{
  id = unique(x$subid)
  packs = data.frame("lb"=-1,"ub"=-1,"dim"=-1,"subid" = -1,"status" = -1)
  if(file.exists(paste0('SFile',id,'.csv'))){
    ell = read.csv(paste0('SFile',id,'.csv'),header = F)
    names(ell) = c("lb","ub","dim","status")
    # Give an id to each pack in the previous step.
    # Giving the id manually for now.
    ell$subid = id
    if(mean(ell$status) == 1)
    {
      packs = rbind(packs,ell)
    }
    # ell has the ParetoFront for the given pack from Part -1.
  }
  return(packs[-1,])
}

getSpheres <- function(subs)
{
  packs = bind_rows(ddply(subs, .(subid), fileread))
  return(packs)
}

dfileread <- function(x, testdata)
{
  id = unique(x$fid)
  print(id)
  decisions = data.frame("id0" = rep(-1,nrow(testdata)))
  if(file.exists(paste0('DFile',id,'.csv')) & (id %in% selectedids)){
    ell = read.csv(paste0('DFile',id,'.csv'),header = F)
    names(ell) = c(paste0("id",id))
    decisions = cbind(decisions,ell)
  }
  return(decisions[,-1])
}

getDecisions <- function(subs,testdata)
{
  decisions = bind_cols(ddply(subs, .(fid), dfileread,testdata))
  return(decisions)
}
getIDs <- function(data, sub) {
  library(data.table)
  library(plyr)
  library(dplyr)
  t = data[,-length(data)]
  for(j in (1:nrow(sub)))
  {
    x = sub[j,]
    dim = x$dim
    tmp.dat <- data.table(as.data.frame(t[,dim]))
    names(tmp.dat) <- "V1"
    tmp <- tmp.dat %>%
      filter(V1 >= x$lb, V1 <= x$ub)
    t <- t[t[,dim] %in% unlist(tmp),]
  }
  return(row.names(t))
}


count <- function(x,data)
{
  tpack = x
  adata = data[data$label ==0,]
  tpack$ng = length(getIDs(data = adata,sub = tpack))
  ndata = data[data$label ==1,]
  tpack$nb = length(getIDs(data = ndata,sub = tpack))
  return(tpack)
}
addCounts <- function(packs,data)
{
  packs = bind_rows(ddply(packs, .(subid), count,data))
}
# addCounts <- function(packs,data)
# {
#   
#   
#   npacks = length(unique(packs$subid))
#   for(i in 1:npacks)
#   {
#     tpack = packs[packs$subid ==i,]
#     adata = data[data$label ==0,]
#     packs[packs$subid ==i,]$ng = length(getIDs(data = adata,sub = tpack))
#     ndata = data[data$label ==1,]
#     packs[packs$subid ==i,]$nb = length(getIDs(data = ndata,sub = tpack))
#   }
#   return(packs)
#   
# }

findDistances <- function(packs,testdata)
{
  npacks = length(unique(packs$subid))
  for(i in 1:npacks)
  {
    tpack = packs[packs$subid ==i,]
    adata = data[data$label ==0,]
    packs[packs$subid ==i,]$ng = length(getIDs(data = adata,sub = tpack))
    ndata = data[data$label ==1,]
    packs[packs$subid ==i,]$nb = length(getIDs(data = ndata,sub = tpack))
  }
  
  
}

getIDs <- function(data, sub) {
  t = data[,-length(data)]
  for(j in (1:nrow(sub)))
  {
    x = sub[j,]
    dim = x$dim
    tmp.dat <- data.table(as.data.frame(t[,dim]))
    names(tmp.dat) <- "V1"
    tmp <- tmp.dat %>%
      filter(V1 >= x$lb, V1 <= x$ub)
    t <- t[t[,dim] %in% unlist(tmp),]
  }
  return(row.names(t))
}
totalAnoms <- function(packids,packs,data)
{
  ids = list()
  for(id in packids)
  {
    pack = packs[packs$subid == id,]
    adata = data[data[length(data)] == 0,]
    ids = append(ids,getIDs(data = adata,sub = pack))
  }
  unionanoms = length(unique(unlist(ids)))
  return(unionanoms)  
}
selectPacks <- function(packs,data)
{
  anoms = totalAnoms(unique(packs$subid),packs,data)
  packs$subid = as.numeric(as.factor(packs$subid))
  selected = list()
  tested = list()
  ppacked = list()
  npacked = list()
  ipacks = unique(packs$subid)
  while(length(unique(unlist(ppacked))) != anoms)
  {
    gain = list()
    for(i in ipacks)
    {
      if(!(i %in% unlist(tested)))
      {
        tpack = packs[packs$subid ==i,]
        tpids = getIDs(data[data$label ==0,],tpack)
        tnids = getIDs(data[data$label ==1,],tpack)
        ppgain = length(setdiff(tpids,unlist(ppacked)))*(mean(tpack$ng)/mean(tpack$nb))
        npgain = length(setdiff(tnids,unlist(npacked)))*(mean(tpack$nb)/mean(tpack$ng))
        gain[[i]] = ppgain - npgain
      }
      else
      {
        gain[[i]] = -10^10
      }
    }
    
    tested = append(tested,which.max(gain))
    chosen = which.max(gain)
    print(chosen)
    tpids = getIDs(data[data$label ==0,],packs[packs$subid == chosen,])
    ppgain = length(setdiff(tpids,unlist(ppacked)))     
    if(ppgain > 0)
    {
      ppacked = append(ppacked,getIDs(data[data$label ==0,],packs[packs$subid == chosen,]))
      npacked = append(ppacked,getIDs(data[data$label ==1,],packs[packs$subid == chosen,]))
      selected = append(selected,which.max(gain))
    }
    print(length(unique(unlist(ppacked))))
  }
  return(packs[packs$subid %in% selected,])
}

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

readData <- function(name)
{
  data = read.csv(paste0(name,".csv"))
  return(data)
}

svdd <- function(cv = 5,name, model = "svdd")
{
  auc = list()
  for(id in 1:cv)
  {
    print(id)
    valid = read.csv(paste0(name,"valid",id,".csv"))
    singledecision = read.csv(paste0('DFile',id,'.csv'),header = F)
    valid$prob1 <- (singledecision-min(singledecision))/(max(singledecision)-min(singledecision))
    library(ROCR)
    valid$prob0 = 1 - valid$prob1
    
    roc_pred <- prediction(valid$prob1,valid$label)
    auc[id] = performance(roc_pred,"auc")@y.values
  }
  bestauc = which.max(auc)
  decision = read.csv(paste0('TFile',bestauc,'.csv'),header = F)
  test = read.csv(paste0(name,"test.csv"))
  test$prob1 <- (decision-min(decision))/(max(decision)-min(decision))
  test$prob0 = 1 - test$prob1
  
  svddpred <- prediction(test$prob1,test$label)
  svddperf <- performance(svddpred, "tpr", "fpr")
  cl = rainbow(7)
  plot(svddperf, main = "ROC for Test Data", add = T, col = cl[1])
  abline(0,1,col="grey")
  write.csv(file = paste0(name,model,"testprobs.csv"), row.names = F,x = test)
  performance(svddpred,"auc")@y.values
}

createSplitsouter <- function(data, outerfolds = 2, name)
{
  flds <- createFolds(data$label, k = (outerfolds), list = TRUE, returnTrain = FALSE)
  for(i in 1:outerfolds)
  {
    write.csv(file = paste0(name,"trainouter",i,".csv"), row.names = F,x = data[flds[[i]],])
  }
}
##### Data preparation #########
##### Label 1 for non anomalies and 0 for anomalies. !!!!!!!
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/SVDD")

name = "breast"
data = read.csv(paste0(name,'data.csv'),header = T) # Synthetic data
head(data)
# Data ready. Let us split the data into test and train and save it.
library(caret)
outerfolds = 3
innerfolds = 3
createSplitsouter(data = data, outerfolds = 3,name)
# Run the matlab script on all the train and valid files.
svdd(cv = 5,name = name, model = "svdd")
