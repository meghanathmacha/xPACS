# 2 is benign 4 is malignant. Column is V11.
# 7. Attribute Information: (class attribute has been moved to last column)
# 
# #  Attribute                     Domain
# -- -----------------------------------------
#   1. Sample code number            id number
# 2. Clump Thickness               1 - 10
# 3. Uniformity of Cell Size       1 - 10
# 4. Uniformity of Cell Shape      1 - 10
# 5. Marginal Adhesion             1 - 10
# 6. Single Epithelial Cell Size   1 - 10
# 7. Bare Nuclei                   1 - 10
# 8. Bland Chromatin               1 - 10
# 9. Normal Nucleoli               1 - 10
# 10. Mitoses                       1 - 10
# 11. Class:                        (2 for benign, 4 for malignant)

# Set the malign class to be anamolous. That is we need the label 0 for the Part-1 for these.
# total of 9 features and 1 label.

#!/usr/bin/Rscript
getPacks <- function(subs,fullsubs,trainid)
{
  
  names(subs) = names(fullsubs)
  unsubs = unique(subs$subid)
  packs = data.frame("lb"=-1,"ub"=-1,"dim"=-1,
                     "status"=-1,"lambda"=-1,
                     "C"=-1,"ng"=-1,"nb"=-1,
                     "ngb"=-1,"subid"=-1,"packid"=-1)
  for(id in unsubs)
  {
    print(id)
    if(file.exists(paste0('Part2/Train',trainid,'/MFile',id,'.csv'))){
      ell = read.csv(paste0('Part2/Train',trainid,'/MFile',id,'.csv'),header = F)
      ell = ell[complete.cases(ell),]
      names(ell) = c("lb","ub","dim","status","lambda","C","ng","nb","ngb")
      # Give an id to each pack in the previous step.
      # Giving the id manually for now.
      ell$subid = as.factor(paste0(ell$lambda,ell$C))
      ell$subid = as.numeric(as.factor(ell$subid))
      ell$packid = id
      cpid = length(unique(packs$subid))
      ell$subid = cpid + as.numeric(as.factor(ell$subid))
      packs = rbind(packs,ell)
      # ell has the ParetoFront for the given pack from Part -1.
    }
  }
  packs = packs[-1,]
  packs$subid = as.numeric(as.factor(packs$subid))
  return(packs)
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

totalNorm <- function(packids,packs,data)
{
  ids = list()
  for(id in packids)
  {
    pack = packs[packs$subid == id,]
    adata = data[data[length(data)] == 1,]
    ids = append(ids,getIDs(data = adata,sub = pack))
  }
  unionanoms = length(unique(unlist(ids)))
  return(unionanoms)  
}

addCounts <- function(packs,data)
{
  npacks = unique(packs$subid)
  for(i in npacks)
  {
    print(i)
    tpack = packs[packs$subid ==i,]
    adata = data[data$label ==0,]
    packs[packs$subid ==i,]$ng = length(getIDs(data = adata,sub = tpack))
    ndata = data[data$label ==1,]
    packs[packs$subid ==i,]$nb = length(getIDs(data = ndata,sub = tpack))
    packs[packs$subid ==i,]$ngb = 0
  }
  return(packs)
  
}


setwd("/nfshome/SHARED/BreastCancerData/BaseLines/Qual/")
name = "breast"
trainid = 1
precision = 3
sdata = read.csv(paste0(name,'data.csv'),header = T)
sdata = round(sdata,precision)
packs = read.csv("Part2/selectpacks1.csv",header = T)
totalAnoms(unique(unlist(packs$subid)),packs = packs,data = sdata)
packs = addCounts(packs,sdata)

hpacks = read.csv(paste0(name,"train",trainid,"subs.csv"),header = T)
data = read.csv(paste0(name,"train",trainid,'.csv'),header = T)
print("Read Data")
cpacks = getPacks(packs,packs,trainid)
cpacks = addCounts(cpacks,sdata)
write.csv(file = "confpacks.csv",x = cpacks,row.names = F)
totalAnoms(unique(unlist(cpacks$subid)),packs = cpacks,data = sdata)
totalNorm(unique(unlist(cpacks$subid)),packs = cpacks,data = sdata)
# #  Attribute                     Domain
# -- -----------------------------------------
#   1. Sample code number            id number
# 2. Clump Thickness               1 - 10
# 3. Uniformity of Cell Size       1 - 10
# 4. Uniformity of Cell Shape      1 - 10
# 5. Marginal Adhesion             1 - 10
# 6. Single Epithelial Cell Size   1 - 10
# 7. Bare Nuclei                   1 - 10
# 8. Bland Chromatin               1 - 10
# 9. Normal Nucleoli               1 - 10
# 10. Mitoses                       1 - 10
# 11. Class:                        (2 for benign, 4 for malignant)
dimnames = c("clumpthickness","cellsize","cellshape","marginaladh","epicellsize","barenuclei",
             "chromatin","nucleoili","mitoses")

addCI <- function(packs,data,ci = 3)
{
  npacks = unique(packs$subid)
  for(i in npacks)
  {
    print(i)
    tpack = packs[packs$subid ==i,]
    points = getIDs(data = data,sub = tpack)
    dim = tpack$dim
    points = data[points,dim]
    pmean = apply(as.data.frame(points),2,mean)
    psd = apply(as.data.frame(points),2,sd)
    linterval = pmean-ci*psd
    hinterval = pmean+ci*psd
    intervals = as.data.frame(cbind(as.matrix(linterval),as.matrix(hinterval)))
    names(intervals) = c("linterval","hinterval")
    packs[packs$subid ==i,]$lb = intervals$linterval
    packs[packs$subid ==i,]$ub = intervals$hinterval
    
  }
  return(packs)
  
}
cpacks= addCI(cpacks,sdata,ci = 2)
cpacks$status = NULL
cpacks$packid = NULL
cpacks$ngb = NULL
cpacks$C = NULL
cpacks$lambda = NULL
names(cpacks) = c("lower.bound","upper.bound","attribute","malign","normal","packid")
cpacks[cpacks$lower.bound <=0,]$lower.bound = 0
cpacks[cpacks$upper.bound >=1,]$upper.bound = 1
cpacks$attribute = dimnames[cpacks$attribute]
write.csv(file = "resultpacks.csv",x = cpacks,row.names = F)




packs = read.csv('Part2/breasttrain1dompacks.csv')
data = read.csv('breastdata.csv')
precision = 3
f = 10^precision
Econstant = BitLengthPack(packids = unique(packs$subid),packs = packs,sdata = data,f = f) + 
  logstar(length(unique(packs$subid))) 
result = readRDS("Part2/resultbreast.RDS")
cpacks = result$cpacks
fpacks = result$fpacks
baseline = result$baseline
reduction = result$reduction
(max(unlist(cpacks)) - Econstant)*100/max(unlist(baseline))

