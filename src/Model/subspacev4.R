#Subspace clustering
library(data.table)
library(magrittr)
library(dplyr)
downward <- function(d.th = 0.2, subspaces, sub.dim)
{
 print(sub.dim) 
  subspaces = subspaces[subspaces$density >= d.th]
  return(subspaces)
}

upward <- function(p.th = 0.4, subspaces, sub.dim)
{
  sub.final <- subspaces[subspaces$impurity <= p.th]
  cand <- subspaces[subspaces$impurity > p.th]
  return(list(cand = cand,final=sub.final))
}

getValue <- function(data, sub) {
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
  return(nrow(t))
}

# count <- function(x,data,labels)
# {
#   temp.sub = x
#   temp.sub$ngcount = getValue(data[data[,length(data)] == 0,],temp.sub)
#   temp.sub$nbcount = getValue(data[data[,length(data)] == 1,],temp.sub)
#   temp.sub$density = temp.sub$ngcount/length(labels[labels == 0])
#   temp.sub$impurity = temp.sub$nbcount/length(labels[labels == 1])
#   temp.sub$sub.dim = nrow(temp.sub)
#   return(temp.sub)
# }
# counts_higher <- function(subspaces,data,labels)
# {
#   subspaces = bind_rows(ddply(subspaces, .(subid), count,data,labels))
#   return(subspaces)
# }

counts_higher <- function(subspaces,data,labels)
{
  labels = data[length(data)]
  subs = unique(subspaces$subid)
  for( i in (subs))
  {
    temp.sub = subspaces[subspaces$subid == i]
    subspaces[subspaces$subid == i]$ngcount = getValue(data[data[,length(data)] == 0,],temp.sub)
    subspaces[subspaces$subid == i]$nbcount = getValue(data[data[,length(data)] == 1,],temp.sub)
    subspaces[subspaces$subid == i]$density = subspaces[subspaces$subid == i]$ngcount/length(labels[labels == 0])
    subspaces[subspaces$subid == i]$impurity = subspaces[subspaces$subid == i]$nbcount/length(labels[labels == 1])
  }
  return(subspaces)
}

# Changed the counts_higher to make it faster.

# K = total number of dimensions
# K.range = Range of the dimension K -This will be in between 0 and 1.
# KDE threshold = kde.th
scale_data <- function(raw.data)
{
  #library(clusterSim)
  data <- as.data.frame(apply(raw.data, MARGIN = 2, 
                              FUN = function(X) (X - min(X))/diff(range(X))))

  return(data)
}

kde <- function(features,labels,data, kde.th = 0.5, factor = 5)
{
  # Input : Sclaed Raw data. Format = user * feature data table
  # Output : One dimensional intervals which satisfy the kde threshold.
  subspace = data.table(data.frame(lb = -1,ub = -1,dim = -1))
  K = length(features)
  features = features[which(labels == 0),]
  # We want the high dense regions only of the points with labels 0 !
  #data = cbind(features,labels)
  d.dim <- apply(features, 2, FUN = function(x){ 
    x <- sort(x)
    dens <- density(x, n = 512, from = 0, to = 1)
    yval = quantile(dens$y, probs = c(kde.th))
    k <- which(dens$y > yval)
    p <- split(k, cumsum(c(1, diff(k) != 1)))
    bounds = rapply(p,function(x)
      { return(list(lb = dens$x[x[1]], ub = dens$x[x[length(x)]]))},how = c("list"))
    return(bounds)
  })
  

  k = 1
  for( i in (1:length(d.dim)))
  {
    temp = unlist(d.dim[i][[1]])
    for( j in (1:(0.5*length(temp))))
    {
      x = sort(features[,i])
      ub = temp[2*j]
      lb = temp[2*j-1]
      if(ub < lb){
        ub = ub + lb
        lb = ub - lb
        ub = ub - lb
      }
      ngcount = nrow(data[data[i] >= lb & data[i]<= ub & labels == 0,][i])
      nbcount = nrow(data[data[i] >= lb & data[i]<= ub & labels == 1,][i])
      density = ngcount/length(labels[labels == 0])
      impurity = nbcount/length(labels[labels == 1]) 
      temp.dat <- data.table(data.frame(lb = lb,
                                        ub = ub,
                                        dim = i,
                                        ngcount = ngcount,
                                        nbcount = nbcount,
                                        sub.dim = 1,
                                        density = density,
                                        impurity = impurity
      ))
      subspace <- bind_rows(subspace,temp.dat)
    }
  }
  return(subspace[-1])
  
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

candidate_gen <- function(subspaces)
{
  
  # The connect step would not be required since we are using KDE !
  
  # Join step 
  # Check if the sub spaces are similar in exactly (k - 1) dimensions - 
  # obtained by just taking combinations of all the subspaces in k dimensions
  #First find the (k - 1) dimensions in the subspaces 
  #Since the cadidate generation subspaces will 
  subspaces$subid = as.numeric(as.factor(subspaces$subid))
  subs = unique(subspaces$subid)
  dims.list = list()
  subs.list = list()
  for(i in subs)
  {
    dims.list[[i]] = unique(subspaces[subspaces$subid == i,]$dim)
    subs.list[[i]] = i
  }
  
  #Elgiible Subspaces to combine, Check if they match on exactly k - 1 dimensions
  
  cand.subspaces = data.table(data.frame(lb = -1,ub = -1,dim = -1))
  
  sub.ids = list()
  idcounter = 1
  K = length(dims.list[[1]])
  t = combn((1:length(dims.list)),m = 2)
  IDs = list()
  k = 1
  for(p in (1:length(t[1,])))
  {
    i = t[1,p]
    j = t[2,p]
    if(length(intersect(dims.list[[i]],dims.list[[j]])) == K-1) 
    {
      new.dim = union(dims.list[[i]],dims.list[[j]])
      new.id = union(subs.list[[i]],subs.list[[j]])
      #print(new.id)
      temp = subspaces[subspaces$subid %in% new.id]
      temp = unique(temp, by = "dim")
      temp = temp[order(dim)]
      if(!any(unlist(lapply(IDs,function(x) identical(unlist(x),
                                               paste0(temp$lb,temp$ub,temp$dim))))))
      {  
        IDs[[k]] = paste0(temp$lb,temp$ub,temp$dim)
        k = k + 1
        temp$sub.dim = K + 1
        temp$subid = idcounter
        cand.subspaces = bind_rows(cand.subspaces,temp)
        idcounter = idcounter + 1
      }


    }
  }
  
  # Join step finished. Going for prune step
  cand.subspaces = cand.subspaces[-1]
  # Pruning.
  # I will need to use the dims.list and the dimensions 
  total.subs = length(unique(cand.subspaces$subid))
  if(total.subs > 1)
  {
    for(i in (1:total.subs))
    {
      temp.sub = cand.subspaces[cand.subspaces$subid == i]
      temp.sub = within(temp.sub,rm(subid,sub.dim))
      for( j in (1:nrow(temp.sub)))
      {
        temp = temp.sub[!j,]
        projection = suppressMessages(semi_join(subspaces,temp))
        if(sum(projection$subid) < 1)
        {
          cand.subspaces = cand.subspaces[!cand.subspaces$subid == i]
          # Deleting the subspace totally since the projection was not found in the k dimensional subspaces.
        }
        
      }
      
    }
  }
  
  # Pruning done !
  return(cand.subspaces)
}

return_subspaces <- function(initial.subspaces,data,d.th = 0.2, p.th = 0.5, 
                             interesting = TRUE)
{
  print(paste0("Density threshold--",d.th))
  print(paste0("Purity threshold--",p.th))
  
  total.dims = length(data) - 1
  
  final.subspaces = data.table(data.frame(lb = -1,ub = -1,dim = -1))
  
  isub = initial.subspaces
  
  for(i in (1:total.dims))
  {
    #Downward Closure -- Check for density
    
    dsub <- downward(isub, d.th = d.th, sub.dim = i)
    usub <- upward(dsub, p.th = p.th, sub.dim = i)
    idcounter = max(final.subspaces[-1]$subid,0) + 1
    
    #cand.subspaces = rbind(usub$cand,usub$final)
    cand.subspaces = rbind(usub$cand,usub$final) # If I want higher dimensional pure and dense subspaces.
    #print(nrow(cand.subspaces))
    #cand.subspaces = rbind(usub$cand)
    usub$final$subid <- usub$final$subid + idcounter
    final.subspaces <- bind_rows(final.subspaces,usub$final)
    
    #Moving to the candidates for the next subspace
    #cand.subspaces = rbind(usub$cand,usub$final)
    
    if(length(unique(cand.subspaces$subid)) > 1)
    {
      print(paste0("Generating subspace candidates for --",i))
      candidates = candidate_gen(cand.subspaces)
      # Candidates generated
      
      candidates = counts_higher(candidates, data = data, labels = data[K])
      #Calculating the densities and impurities of the candidates.
      isub = candidates
      
    }
    else
    {
      final.subspaces = final.subspaces
      return(final.subspaces[-1,])
      print("Exiting at")
      print(i)
    }
  }
  return(final.subspaces[-1,])
}

summary_subs <- function(subs)
{
  library(plyr)
  subs$subid <- as.numeric(as.factor(subs$subid)) 
  t = subs
  total.dims = length(data) - 1
  tot = 0
  for(i in (1:max(t$sub.dim)))
  {
    print(paste0("Total number of subspaces found of dimension ", i," are ",
                 sum(t$sub.dim == i)/i))
    tot = tot + sum(t$sub.dim == i)/i
  }
  print(paste0("Total number of subspaces found are ",
               tot))
  
  print(paste0("To look at the subspaces, type subs[subs$sub.dim == i,]"))
  return(t)
  
}
clean_data <- function(data,gsubs)
{
  subs = nrow(gsubs)
  for(i in (1:subs))
  {
    tsub = gsubs[i,]
    data = data[!(data[,tsub$dim] >= tsub$lb & 
           data[,tsub$dim] <= tsub$ub & 
           data$label ==1),] 
  }
  return(data)
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

dominance <- function(fsubs)
{
  dominated = list()
  k = 1
  fsubs$subid <- as.numeric(as.factor(fsubs$subid)) 
  sub.ids = unique(fsubs$subid)
  for(i in rev(sub.ids))
  {
    hsub.temp = fsubs[fsubs$subid == i,]
    len.hsub = nrow(hsub.temp)
    dim.hsub = hsub.temp$dim
    for(j in sub.ids)
    {
      lsub.temp = fsubs[fsubs$subid == j,]
      len.lsub = nrow(lsub.temp)
      dim.lsub = lsub.temp$dim
      dim.int = intersect(dim.lsub,dim.hsub)
      hub.int = hsub.temp[hsub.temp$dim %in% dim.int,]$ub
      hlb.int = hsub.temp[hsub.temp$dim %in% dim.int,]$lb
      lub.int = lsub.temp[lsub.temp$dim %in% dim.int,]$ub
      llb.int = lsub.temp[lsub.temp$dim %in% dim.int,]$lb
      if(all(dim.lsub %in% dim.hsub) & 
         lsub.temp[1,]$ngcount <= hsub.temp[1,]$ngcount & 
         lsub.temp[1,]$nbcount >= hsub.temp[1,]$nbcount & 
         len.lsub < len.hsub &
         !(length(setdiff(hub.int,lub.int))) &
         !(length(setdiff(hlb.int,llb.int))))
      {
        dominated[[k]] = j
        k = k + 1
      }
      
    }
  }
  tsubs = fsubs
  fsubs = delete(data.table(fsubs),which(fsubs$subid %in% unique(unlist(dominated))))
  return(fsubs)
}

performance <- function(cons.data, subspaces)
{
  subspaces$subid <- as.numeric(subspaces$subid)
  ids = unique(subspaces$subid)
  temp.data = cons.data
  temp.data$picked = rep(0,length(temp.data$label))
  temp.data$subspaceid = rep(0,length(temp.data$label))
  temp.data$count = rep(0,length(temp.data$label))
  
  for( i in (1:length(ids)))
  {
    
    temp.sub = subspaces[subspaces$subid == ids[[i]],]
    
    temp = getIDs(cons.data,temp.sub)
    temp.data[temp,]$picked = 1
    temp.data[temp,]$subspaceid = ids[[i]]
    temp.data[temp,]$count = temp.data[temp,]$count + 1
  }
  temp.data =temp.data[complete.cases(temp.data),]
  tp = sum(temp.data[temp.data$label == 0,]$picked)
  p = length(temp.data[temp.data$label == 0,]$label)
  recall = tp/p
  print(paste0("Recall-",recall))
  fp = sum(temp.data[temp.data$label == 1,]$picked)
  n = length(temp.data[temp.data$label == 1,]$label)
  precision = tp/(tp + fp)
  print(paste0("Precision-",precision))
  redundancy = sum(temp.data[temp.data$label == 0,]$count) - sum(temp.data[temp.data$label == 0,]$picked)
  hist(temp.data[temp.data$label == 0 & temp.data$picked == 1,]$count, main = "Redundancy", ylab = "Number of points",
       xlab = "Number of times collected")
  
  
  return(temp.data)
  
}

addIDs <- function(anoms, fsubs)
{
  fsubs$subid <- as.numeric(fsubs$subid)
  fsubs$anomlist <- rep("100",nrow(fsubs))
  ids = unique(fsubs$subid)
  temp.data = anoms
  for( i in (1:length(ids)))
  {
    
    temp.sub = fsubs[fsubs$subid == ids[[i]],]
    
    temp = getIDs(anoms,temp.sub)
    fsubs[fsubs$subid == ids[[i]],]$anomlist = paste0(sort(as.numeric(unlist(temp))),collapse = ",")
  }
  return(fsubs)
}

cperformance <- function(gsubs,rsubs,data,osubs)
{
  
  print(paste0("Data summary"))
  print(paste0("Number of positve points--",nrow(data[data$label ==0,])))
  print(paste0("Number of negative points--",nrow(data[data$label ==1,])))
  print(paste0("Ratio between positive and negative--",nrow(data[data$label ==0,])
               /nrow(data[data$label ==1,])))
  
  ground.min.d = min(gsubs$density)
  # The d.th can only be lesser than ground.min.d 
  ground.max.p = max(gsubs$impurity)

  print(paste0("Ground truth clusters summary"))
  print(paste0("Minimum number of positive points in clusters--", ground.min.d*
                 nrow(data[data$label ==0,])))
  print(paste0("Maximum number of negative points in clusters--", ground.max.p*
                 nrow(data[data$label ==1,])))
  print(paste0("Density threshold of positive points in clusters--", ground.min.d))
  print(paste0("Impurity threshold of negative points in clusters--", ground.max.p))
  
  print("Summary of the clusters in the ground truth")
  summary_subs(gsubs)  
  
  r.min.d = min(rsubs$density)
  # The d.th can only be lesser than ground.min.d 
  r.max.p = max(rsubs$impurity)
  
  print(paste0("Clusters retrieved summary"))
  print(paste0("Minimum number of positive points in clusters retireved--", r.min.d*
                 nrow(data[data$label ==0,])))
  print(paste0("Maximum number of negative points in clusters retrieved--", r.max.p*
                 nrow(data[data$label ==1,])))
  print(paste0("Density threshold of positive points in clusters retrieved--", r.min.d))
  print(paste0("Impurity threshold of negative points in clusters retreived--", r.max.p))
  
  print("Summary of the clusters retrieved (Has lot of redundancies as expected)")
  summary_subs(osubs)  
  
  print(paste0("Overall performance"))
  tp = sum(rsubs$density)*nrow(data[data$label ==0,])
  p = sum(gsubs$density)*nrow(data[data$label ==0,])
  print(paste0("Recall--",tp/p))
  
  n = sum(gsubs$impurity)*nrow(data[data$label ==1,])
  fp = sum(rsubs$impurity)*nrow(data[data$label ==1,])
  
  gcl = unique(gsubs$subid)
  rcl = unique(rsubs$subid)
  c.rec = length(intersect(gcl,rcl))
  print(paste0("Clusters recovered - ",c.rec*100/length(gcl),"%"))
  print(paste0("Performance in each cluster--"))
  for( i in (1:length(gcl)))
  {
    gtcld = gsubs[gsubs$subid == i,]
    rtcld = rsubs[rsubs$subid == i,]
    print(paste0("Cluster--",i))
    
    tp = sum(rtcld$density)*nrow(data[data$label ==0,])
    p = sum(gtcld$density)*nrow(data[data$label ==0,])
    print(paste0("Recall--",tp*100/p))
  }
  
    
}

ground_truth <- function(filename = "clusters.txt",data,labels)
{
  gclusters = read.csv(filename,header = FALSE)    
  ground = data.table(data.frame(lb = -1,
                                 ub = -1,
                                 dim = -1,
                                 subid = -1,
                                 ngcount = -1,
                                 nbcount = -1,
                                 impurity = -1,
                                 density = -1,
                                 sub.dim = -1))
  k = 0
  for(i in (1:dim(gclusters)[1]))
  {
    row = unlist(strsplit(as.character(gclusters[i,]),'[$]'))
    print(i)
    for(ele in row)
    {
      temp = unlist(strsplit(as.character(ele),'[:]'))
      dim = as.numeric(temp[1])
      lb = as.numeric(temp[2])
      ub = as.numeric(temp[3])
      sub.dim = length(row)
      df = data.frame(dim = dim,
                      lb = lb,
                      ub = ub,
                      subid = i,
                      sub.dim = sub.dim,
                      ngcount = -1,
                      nbcount = -1,
                      impurity = -1,
                      density = -1)
      ground = rbind(ground,df)
    }
  }
  ground = ground[-1]
  ground = counts_higher(subspaces = ground,data = data, labels = labels)
  return(ground)
}

bind <- function(fsubs,gsubs)
{
  gsubs$subid <- as.numeric(gsubs$subid)
  gids = unique(gsubs$subid)
  
  fsubs$subid <- as.numeric(fsubs$subid)
  fids = unique(fsubs$subid)
  
  redfsub = fsubs[1,] # Will remove later.
  
  for( i in (1:length(gids)))
  {
    
    temp.gsub = gsubs[gsubs$subid == gids[[i]],]
    
    for( j in (1:length(fids)))
    {
      temp.fsub = fsubs[fsubs$subid == fids[[j]],]
      
      if(nrow(temp.fsub) == nrow(temp.fsub) & 
         identical(sort(temp.fsub$dim),sort(temp.gsub$dim)))
      {
        temp.fsub$subid = temp.gsub$subid
        redfsub = rbind(redfsub,temp.fsub)
      }
    }
  }
  return(redfsub[-1])
  
}


computeFirstPart <- function(sdata, name, pruneinit, dom, precision, trainid) 
{
  # precision is used in Part 3 as well. Need to take care.
  
  data = round(sdata,precision) # This is the sampled data for Glasses/ORGlasses or could be any data with labels as last row.
  write.csv(data,paste0(name,"train",trainid,'.csv'),row.names = F) # Writing the data, required for Part 2.
  hpacks = data.table(data.frame(lb = -1,
                                 ub = -1,
                                 dim = -1,
                                 ngcount = -1,
                                 nbcount = -1,
                                 sub.dim = -1,
                                 density = -1,
                                 impurity = -1,
                                 subid = -1)) 
  # Structure of the packs we are expecting from Part - 1.
  kde.range = c(0.75,0.8,0.85,0.9,0.95) # KDE threshold range as in Algo 2.
  K = length(sdata)
  for(k in kde.range)
  {
    initial.subspaces <- kde(features = data[1:(K-1)],labels = data[K], data = data,kde.th = k)
    fa = initial.subspaces$ngcount
    fb = initial.subspaces$nbcount
    d.th.list = unique(quantile(fa,c(0.5,0.6,0.7,0.8,0.9,0.95,0.99))) # Quantiles from the initial subspaces.
    d.th.list = d.th.list[d.th.list!=0]
    p.th.list = unique(round(quantile(fb,c(0.5,0.4,0.3,0.2,0.1,0.05,0.01))))  # Quantiles from the initial subspaces.
    if(pruneinit)
    {
      pruneinit = ceiling(quantile(fb,c(0.31))) # Removing the 
      initial.subspaces = delete(initial.subspaces,which(initial.subspaces$nbcount >=pruneinit,))
    }
    initial.subspaces$subid <- seq(1,length(initial.subspaces$lb))
    
    for(d in d.th.list)
    {
      for(p in p.th.list)
      {
        fsubs = return_subspaces(initial.subspaces,data = data,
                                 d.th = d/nrow(data[data$label ==0,]),
                                 p.th = p/nrow(data[data$label ==1,]))
        if(dom)
        {
          fsubs = dominance(fsubs)
        }
        #fsubs = addIDs(data[data$label ==0,], fsubs) # Adds IDs of the points. Not necessary. Was doing for debugging.
        fsubs$subid <- as.numeric(as.factor(fsubs$subid)) 
        fsubs$subid = fsubs$subid + max(unique(hpacks$subid),0)
        hpacks = rbind(hpacks,fsubs)
      }
    }
    
  }
  hpacks = hpacks[-1,]
  write.csv(hpacks,paste0(name,"train",trainid,'subs.csv'),row.names = F)
  return(hpacks)
}




# syn.data = read.csv('output.csv',header = FALSE)
# clusters = syn.data$V21
# syn.data$V21 = NULL
# syn.data$V21 = syn.data$V22
# syn.data$V22 = NULL
# syn.data$label = as.numeric(!(syn.data$V21))
# syn.data$V21 = NULL
# 
# library(data.table)
# data <- data.frame(syn.data)
# #data <- scale_data(data)
# K = length(data)
# ground.subspaces = ground_truth(data = data, labels = data[K])
# initial.subspaces <- kde(features = data[1:(K-1)],labels = data[K], data = data,
#                          kde.th = 0.99)
# initial.subspaces$subid <- seq(1,length(initial.subspaces$lb))
# 
#     temp.orig.data = performance(data,initial.subspaces)
#     subs = return_subspaces(initial.subspaces,data = data,
#                                      d.th = 0.044,
#                                      p.th = 0)
#     temp.data <- performance(original.data = temp.orig.data[temp.orig.data$picked ==1,],
#                              subspaces = subs)
#     subs <- summary_subs(subs)
# # What should my d.th be ? It cannot be greater than the max(d.th) of initial.subspaces
# # To mine higher dimensional subspaces, it should ideally be lesser than the
# # min(d.th) of initial.subpsaces
# # I say this because we are not joining the subspaces in between. Since we have already
# # done KDE which gives non-overlapping initial subspaces.
# # In CLIQUE the d.th was set to 0.005
# # 
# # min.d.init = min(initial.subspaces$density)
# # max.d.init = max(initial.subspaces$density)
# # d.th.list = c(0.003,0.007,0.015)
# # 
# # orig.ratio = nrow(data[data$label ==0,])/nrow(data[data$label == 1,])
# # 
# # min.p.init = min(initial.subspaces$impurity)
# # max.p.init = max(initial.subspaces$impurity)
# # p.th.list = list()
# # for( i in (1:length(d.th.list)))
# # {
# #   p.th.list[[i]] = rep(d.th.list[[i]],3)*c(4*orig.ratio,1*orig.ratio,0.5*orig.ratio)
# # }
# # 
# # tot.subs = list()
# # init.subs = list()
# # data.list = list()
# # k = 1
# # for(i in (1:(length(d.th.list))))
# # {
# #   for(j in ((1:length(p.th.list[[1]]))))
# #   {
# #     #print(paste0(d.th.list[[i]],p.th.list[[i]][j]))
# #     K = length(data)
# #     # initial.subspaces <- kde(features = data[1:(K-1)],labels = data[K], data = data,
# #     #                          kde.th = 0.8, chunk = TRUE,factor = 30)
# #     # initial.subspaces$subid <- seq(1,length(initial.subspaces$lb))
# #     temp.orig.data = performance(data,initial.subspaces)
# #     tot.subs[[k]] = return_subspaces(initial.subspaces,data = data,
# #                                      d.th = d.th.list[[i]],
# #                                      p.th = p.th.list[[i]][j])
# #     init.subs[[k]] = initial.subspaces
# #     temp.data <- performance(original.data = temp.orig.data[temp.orig.data$picked ==1,],
# #                              subspaces = tot.subs[[k]])
# #     subs <- summary_subs(tot.subs[[k]])
# #     data.list[[k]] = temp.data
# #     k = k + 1
# #     
# #     # For 
# #     
# #   }
# # }
# # 

# 

