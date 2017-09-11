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
    if(file.exists(paste0('Train',trainid,'/MFile',id,'.csv'))){
      ell = read.csv(paste0('Train',trainid,'/MFile',id,'.csv'),header = F)
      ell = ell[complete.cases(ell),]
      names(ell) = c("lb","ub","dim","status","lambda","C","ng","nb","ngb")
      # Give an id to each pack in the previous step.
      # Giving the id manually for now.
      ell$subid = as.factor(paste0(ell$lambda,ell$C))
      ell$subid = as.numeric(as.factor(ell$subid))
      ell$packid = id
      dom.list = list()
      remrow <- function(x, rows) x[-rows,, drop = FALSE]
      ids = unique(ell$subid)
      count = 0
      for(i in ids)
      {
        for(j in ids)
        {
          p = ell[ell$subid == i,][1,]
          q = ell[ell$subid == j,][1,]
          count = count + 1
          if((p$ng + p$ngb ) > (q$ng + q$ngb) 
             && (p$nb <= q$nb) && p$subid != q$subid || q$ng == 0)
            #If I put equality in the subspaces, we will have cases where 
            # i dominates j and j dominates i and hence we may end up with 
            # nothing.
          {
            dom.list = append(dom.list,j)
          }
        }
      }
      if(length(dom.list))
      {
        ell = remrow(ell, as.numeric(rownames(ell[ell$subid %in% dom.list,])))
      }
      t <- ell[order(ell$subid),]
      t <- t[!duplicated(t[c("ng", "nb","ngb","dim")]),]
      ell = t
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
comb <- function(n, x) {
  return(factorial(n) / (factorial(x) * factorial(n-x)))
}

logstar <- function(x)if(x>1)1+logstar(log(x))else 0

hugecombvalue <- function(m,n){
  t = m*log(m)- (m-n)*log(m-n) -n*log(1+n)
  #print(t)
  if(t > 700)
  {
    t = 700
  }
  return(exp(1)^t)
}

BitLengthPack <- function(packids,packs,sdata, f,type = "D")
{
  #costnpacks = logstar(length(packids))
  d = (length(sdata) -1) # 1 for the label.
  #print(head(sdata))
  tcost = 0.0
  for(id in packids)
  {
    pack = packs[packs$subid == id,]
    dk = length(pack$dim)
    costdims = logstar(dk) # Cost for number of dimensions.
    costiddims = log2(hugecombvalue(d,dk)) # Cost for identity of dimensions.
    costcenter = dk*log2(f) # Cost for center of ellipsoid.
    # Cost for transmitting Covariance Matrix below.
    if(type == "D")
    {
      costcovariance = (dk)*log2(f) # Diagonal 
    }
    else
    {
      costcovariance = (dk^2)*log2(f) # Full
    }
    nb = unique(pack$nb)
    ng = unique(pack$ng)
    ngb = unique(pack$ngb)
    costnormalpoints = logstar(nb) # Cost of number of normal points
    costidnormalpoints = logstar(hugecombvalue(ng + nb + ngb,nb)) 
    # Cost of identity of normal points.
    # Too costly this step.
    lpack = (costdims + costiddims + costcenter + costcovariance +
               costnormalpoints + costidnormalpoints)
    tcost = tcost + lpack
  }
  return(tcost)
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
    ##dim = dim - 1 ######## Doing this just for this data since we are removing one dimension
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
Reduction <- function(packids,packs,data,Econstant,f)
{
  d = length(data) -1
  if(length(packids) == 0)
  {
    return(Econstant)
  }
  
  redpackCost = BitLengthPack(packids,packs,data,f)
  totanomcost = totalAnoms(packids,packs,data)
  tcost = totanomcost*d*log2(f) - redpackCost + Econstant
  return(tcost)
}

greedyRandomatK <- function(packs, k, Econstant,f,data)
{
  S = list()
  U = unique(packs$subid)
  for(i in (1:k))
  {
    costs = data.frame('packid'=-1,'cost'=-1)
    for(j in U)
    {
      cost = Reduction(packids = unlist(append(S,j)),packs,data,Econstant =Econstant,f) - Reduction(packids = unlist(S),packs,data,Econstant = Econstant,f)
      packid = j
      tcosts = data.frame('packid'=j,'cost'=cost)
      costs = rbind(costs,tcosts)
    }
    costs = costs[-1,]
    library(plyr)
    selectedrows = head(arrange(costs,desc(cost)), n = k)   

    # combinations = combn(U,k)
    # sums = combn(U,k,function(x)
    # {sum(costs[costs$packid %in% x,]$cost)})
    # M = combinations[,which(sums == max(sums, na.rm = TRUE))] # Selecting all ids which lead to maximum reduction.
    chosen = sample_n(selectedrows,1)
    chosenid = chosen$packid
    #chosenid is pack we need to add in S and remove in U
    S = append(S,chosenid)
    #print(paste0('S',unlist(S)))
    U = U[U!=chosenid]
    #print(paste0('U',unlist(U)))
  }
  return(unlist(S))
}


greedyRandomatKParallel <- function(packs, k, Econstant,f)
{
  S = list()
  U = unique(packs$subid)
  for(i in (1:k))
  {
    costs = data.frame('packid'=-1,'cost'=-1)
    for(j in U)
    {
      cost = Reduction(packids = unlist(append(S,j)),packs,data,Econstant =Econstant,f) - Reduction(packids = unlist(S),packs,data,Econstant = Econstant,f)
      packid = j
      tcosts = data.frame('packid'=j,'cost'=cost)
      costs = rbind(costs,tcosts)
    }
    costs = costs[-1,]
    library(plyr)
    selectedrows = head(arrange(costs,desc(cost)), n = k)    # Selecting top k at each iteration!
    
    # combinations = combn(U,k)
    # sums = combn(U,k,function(x)
    # {sum(costs[costs$packid %in% x,]$cost)})
    # M = combinations[,which(sums == max(sums, na.rm = TRUE))] # Selecting all ids which lead to maximum reduction.
    chosen = sample_n(selectedrows,1) 
    chosenid = chosen$packid
    #chosenid is pack we need to add in S and remove in U
    S = append(S,chosenid)
    #print(paste0('S',unlist(S)))
    U = U[U!=chosenid]
    #print(paste0('U',unlist(U)))
  }
  return(unlist(S))
}


addCounts <- function(packs,data)
{
  npacks = length(unique(packs$subid))
  for(i in 1:npacks)
  {
    tpack = packs[packs$subid ==i,]
    adata = data[data$label ==0,]
    packs[packs$subid ==i,]$ng = length(getIDs(data = adata,sub = tpack))
    ndata = data[data$label ==1,]
    packs[packs$subid ==i,]$nb = length(getIDs(data = ndata,sub = tpack))
    packs[packs$subid ==i,]$ngb = 0
  }
  return(packs)
  
}

delete <- function(DT, del.idxs) {           # pls note 'del.idxs' vs. 'keep.idxs'
  keep.idxs <- setdiff(DT[, .I], del.idxs);  # select row indexes to keep
  cols = names(DT);
  DT.subset <- data.table(DT[[1]][keep.idxs]); # this is the subsetted table
  setnames(DT.subset, cols[1]);
  for (col in cols[2:length(cols)]) {
    DT.subset[, (col) := DT[[col]][keep.idxs]];
    DT[, (col) := NULL];  # delete
  }
  return(DT.subset);
}

dominance <- function(fsubs,data)
{
  dominated = list()
  k = 1
  fsubs$subid <- as.numeric(as.factor(fsubs$subid)) 
  sub.ids = unique(fsubs$subid)
  adata = data[data$label ==0,]
  for(i in rev(sub.ids))
  {
    if(!(i %in% unlist(dominated)))
    {
    hsub.temp = fsubs[fsubs$subid == i,]
    len.hsub = nrow(hsub.temp)
    dim.hsub = hsub.temp$dim
      for(j in sub.ids)
      {
        if(!(j %in% unlist(dominated)))
        {
        lsub.temp = fsubs[fsubs$subid == j,]
        len.lsub = nrow(lsub.temp)
        dim.lsub = lsub.temp$dim
        if(all(dim.lsub %in% dim.hsub) & 
           lsub.temp[1,]$ng + lsub.temp[1,]$ngb <= hsub.temp[1,]$ng + hsub.temp[1,]$ngb & 
           lsub.temp[1,]$nb >= hsub.temp[1,]$nb & 
           len.lsub < len.hsub &
           all(getIDs(lsub.temp,data = adata) %in% getIDs(hsub.temp,data = adata))) 
        {
          dominated[[k]] = j
          k = k + 1
        }
        
        }
      }
    }
  }
  tsubs = fsubs
  fsubs = delete(data.table(fsubs),which(fsubs$subid %in% unique(unlist(dominated))))
  return(fsubs)
}


# computePart3 <- function(name, precision = 3, dom = TRUE, iter = 10, trainid)
# {
#   hpacks = read.csv(paste0(name,"train",trainid,"subs.csv"),header = T)
#   print("Read hpacks from Part - 1")
#   data = read.csv(paste0(name,"train",trainid,'.csv'),header = T)
#   print("Read Data")
#   data = round(data,precision) # Need not do. Already done earlier and saved as name+data.csv
#   packs = getPacks(hpacks,hpacks,trainid)
#   print("Read packs from Part - 2")
#   packs$ub = round(packs$ub,precision) # Need not do. Already done earlier and saved as name+data.csv
#   packs$lb = round(packs$lb,precision)
#   if(dom)
#   {
#     packs = dominance(packs,data)
#   }
#   print("Generated Dominant Packs after Part 2")
#   write.csv(file = paste0(name,"train",trainid,'dompacks.csv'),x = packs,row.names = F) # Writes the file after deleting the dominated packs.
#   print("Wrote Dominated packs onto file")
#   print("Starting selection of packs")
#   f = 10^precision
#   Econstant = BitLengthPack(packids = unique(packs$subid),packs = packs,data = data,f = f) + logstar(length(unique(packs$subid))) 
#   # This is the last two terms of the Rl(s)
#   # The last term is included because we do not include the pack length cost 
#   # in BitLengthPack.
#   cpacks = list()
#   fpacks = list()
#   reduction = list()
#   baseline = list()
#   nanoms = nrow(data[data$label == 0,])
#   for(k in 1:(nanoms))
#   {
#     simfpacks = list()
#     simcpacks = list()
#     for(sim in (1:iter))
#     {
#       simfpacks[[sim]] = greedyRandomatK(packs,k,Econstant,f = f)
#       print(k)
#       
#       simcpacks[[sim]] = Reduction(packids = unlist(simfpacks[[sim]] ),
#                               packs,data,Econstant =Econstant,f = f) - logstar(k)
#     }
#     fpacks[[k]] = unlist(simfpacks[which.max(unlist(simcpacks))])
#     cpacks[[k]] = max(unlist(simcpacks))
#     reduction[[k]] = cpacks[[k]] - Econstant
#     totanoms = nrow(data[data$label ==0,])
#     d = length(data)-1
#     baseline[[k]] = totanoms*d*log2(f)
#   }
#   selpacks = packs[unlist(fpacks[which.max(cpacks)]),]
#   write.csv(paste0('selectpacks',trainid,'.csv'),x = selpacks, row.names = F)
#   print("Wrote the selected packs! -- Finally ! :(")
#   return (list(fpacks = fpacks,cpacks = cpacks,reduction = reduction,baseline = baseline,selpacks = selpacks))
# }

perKcomputation <- function(k,iter,packs,Econstant,f,data) 
{
  print(k)
  simfpacks = lapply(1:iter,function(x){return(greedyRandomatK(packs,k,Econstant,f = f,data))})
  simcpacks = lapply(1:iter,function(x){return(Reduction(packids = unlist(simfpacks),
                                                         packs,data,Econstant =Econstant,f = f) - logstar(k))})
  fpacks = unlist(simfpacks[which.max(unlist(simcpacks))])
  cpacks = max(unlist(simcpacks))
  return(list(fpacks = fpacks,cpacks=cpacks))
}
computePart3Parallel <- function(name, precision = 3, dom = TRUE, iter = 10, trainid)
{
  hpacks = read.csv(paste0(name,"train",trainid,"subs.csv"),header = T)
  print("Read hpacks from Part - 1")
  data = read.csv(paste0(name,"train",trainid,'.csv'),header = T)
  print("Read Data")
  data = round(data,precision) # Need not do. Already done earlier and saved as name+data.csv
  packs = getPacks(hpacks,hpacks,trainid)
  print("Read packs from Part - 2")
  packs$ub = round(packs$ub,precision) # Need not do. Already done earlier and saved as name+data.csv
  packs$lb = round(packs$lb,precision)
  if(file.exists(paste0(name,"train",trainid,'dompacks.csv')))
  {
    print("Dominant Packs were already generated")
    packs = read.csv(paste0(name,"train",trainid,'dompacks.csv'))
  }
  else if(dom)
  {
    packs = dominance(packs,data)
    print("Generated Dominant Packs after Part 2")
    write.csv(file = paste0(name,"train",trainid,'dompacks.csv'),x = packs, row.names = F) # Writes the file after deleting the dominated packs.
    #packs = read.csv(paste0(name,"train",trainid,'dompacks.csv'))
    print("Wrote Dominated packs onto file")
  }
  print("Starting selection of packs")
  f = 10^precision
  Econstant = BitLengthPack(packids = unique(packs$subid),packs = packs,sdata = data,f = f) + logstar(length(unique(packs$subid))) 
  # This is the last two terms of the Rl(s)
  # The last term is included because we do not include the pack length cost 
  # in BitLengthPack.
  cpacks = list()
  fpacks = list()
  reduction = list()
  baseline = list()
  nanoms = nrow(data[data$label == 0,])
  nanoms = 10 # Hard coding the number of packs.
  packedanoms = 0
  possiblepackanoms = totalAnoms(unique(unlist(packs$subid)),packs = packs,data = data)
  print(paste0("Possible anomalies packing",possiblepackanoms))
  for(k in 1:nanoms)
  {
      print(k)
      result = perKcomputation(k,iter,packs,Econstant,f,data)
      fpacks[[k]] = result$fpacks
      cpacks[[k]] = result$cpacks
      reduction[[k]] = cpacks[[k]] - Econstant
      totanoms = nrow(data[data$label ==0,])
      d = length(data)-1
      baseline[[k]] = totanoms*d*log2(f)
      packedanoms = totalAnoms(unique(unlist(fpacks[which.max(cpacks)])),packs = packs,data = data)
      print(packedanoms)
  }
  
  #fpacks = lapply(1:nanoms,function(x){return(total[[x]]$fpacks)})
  #cpacks = lapply(1:nanoms,function(x){return(total[[x]]$cpacks)})
  selpacks = packs[packs$subid %in% unlist(fpacks[which.max(cpacks)]),]
  write.csv(paste0('selectpacks',trainid,'.csv'),x = selpacks, row.names = F)
  print("Wrote the selected packs! -- Finally ! :(")
  return (list(fpacks = fpacks,cpacks = cpacks,reduction = reduction,baseline = baseline,selpacks = selpacks))
}

precision = 3
f = 10^precision
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/Model/Part2")
cv = 5
for(i in 1:cv)
{
  result = computePart3Parallel("breast",precision = 3,dom = TRUE,iter = 1,trainid = i)
  # cpacks = result$cpacks
  # fpacks = result$fpacks
  # baseline = result$baseline
  # reduction = result$reduction
  # selpacks = result$selpacks
}
for(trainid in 1:cv)
{
  name = "breast"
  hpacks = read.csv(paste0(name,"train",trainid,"subs.csv"),header = T)
  selpacks = read.csv(paste0("selectpacks",trainid,".csv"),header = T)
  for(sub in selpacks$packid)
  {
    reqsub = hpacks[hpacks$subid == sub,]
    selsub = selpacks[selpacks$packid == sub,]
    selpacks[selpacks$packid == sub,]$lb = reqsub$lb
    selpacks[selpacks$packid == sub,]$ub = reqsub$ub
  }
  write.csv(paste0('selectpacks',trainid,'.csv'),x = selpacks, row.names = F)
}


#results = lapply(1:cv, function(x) {computePart3Parallel("breast",precision=3,dom=TRUE,iter=1,trainid = x)})
