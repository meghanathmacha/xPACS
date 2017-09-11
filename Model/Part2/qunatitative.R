getDistances <- function(spacks,valid,id)
{
  distances = bind_cols(ddply(spacks, .(subid), dfileread,valid,id))
  return(distances)
}

dfileread <- function(x, valid,fid)
{
  id = unique(x$subid)
  print(id)
  decisions = data.frame("id0" = rep(-1,nrow(valid)))
  if(file.exists(paste0(getwd(),'/Train',fid,'/DFile',id,'.csv'))){
    ell = read.csv(paste0(getwd(),'/Train',fid,'/DFile',id,'.csv'),header = F)
    names(ell) = c(paste0("id",id))
    decisions = cbind(decisions,ell)
  }
  return(decisions[,-1])
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

setwd("/nfshome/SHARED/BreastCancerData/BaseLines/Model/Part2")
cv = 5
name = "breast"
for(i in 1:cv)
{
  spacks = read.csv(paste0('selectpacks',i,'.csv'))
  valid = read.csv(paste0(name,"valid",i,".csv"))
  distances = getDistances(spacks,valid,i)
  
  
  df.aree <- as.data.frame(t(decision))
  
  colnames(df.aree) <- df.aree[1, ]
  df.aree <- df.aree[-1, ]
  
  decision =df.aree
  # The minimum distance from all the selected packs is used for ranking
  
  decision$decision = apply(decision, 1, min) 
  decision = decision$decision
  # Decision will have the length from sphere for all the testing points !
  
  testdata$prob1 <- (decision-min(decision))/(max(decision)-min(decision))
}