plotPerf <- function(measure,baselines,nbaselines,title,file,name)
{
  pdf(file=paste0(file,".pdf"))
  library(RColorBrewer)
  n <- length(baselines)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  col=sample(col_vector, n)
  model = "svmlinear"
  probs = read.csv(file = paste0(name,model,"probs.csv"))
  predictions = probs$prob0
  labels = probs$label
  roc_pred <- prediction(predictions,!labels)
  perf <- performance(roc_pred, measure[1], measure[2])
  plot(perf,col = col[2],main = title, add = F, lwd=5)
  print(model)
  
  library(PRROC)
  s0 = probs[probs$label ==0,]$prob0
  s1 = probs[probs$label ==1,]$prob0
  prcurve = pr.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
  print(paste0("AUPRC",prcurve$auc.integral))
  prcurve = roc.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
  print(paste0("AUC",prcurve$auc))
  
  for(i in 2:length(baselines))
  {
    if(i != 2)
    {
      base = baselines[[i]]
      if(base != "svdd")
      {
        if(base == "dtree")
        {
          probs = read.csv(file = paste0(name,base,"probs.csv"))
          predictions = probs$prob0
          labels = probs$label
          roc_pred <- prediction(predictions,!labels)
          perf <- performance(roc_pred, measure[1], measure[2])
          perf@y.values[[1]][1] = perf@y.values[[1]][2]
          plot(perf,col = col[i],main = title, add = T,lwd=5)
          print(base)
          library(PRROC)
          s0 = probs[probs$label ==0,]$prob0
          s1 = probs[probs$label ==1,]$prob0
          prcurve = pr.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
          print(paste0("AUPRC",prcurve$auc.integral))
          prcurve = roc.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
          print(paste0("AUC",prcurve$auc))
        }
        else
        {
          probs = read.csv(file = paste0(name,base,"probs.csv"))
          predictions = probs$prob0
          labels = probs$label
          roc_pred <- prediction(predictions,!labels)
          perf <- performance(roc_pred, measure[1], measure[2])
          plot(perf,col = col[i],main = title, add = T,lwd=5)
          print(base)
          library(PRROC)
          s0 = probs[probs$label ==0,]$prob0
          s1 = probs[probs$label ==1,]$prob0
          prcurve = pr.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
          print(paste0("AUPRC",prcurve$auc.integral))
          prcurve = roc.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
          print(paste0("AUC",prcurve$auc))
        }
        
      }
      else
      {
        probs = read.csv(file = paste0(name,base,"probs.csv"),header = F)
        predictions = probs[,length(probs)]
        labels = probs[,length(probs)-2]
        roc_pred <- prediction(predictions,!labels)
        perf <- performance(roc_pred, measure[1], measure[2])
        plot(perf,col = col[i],main = title, add = T, lwd=5)
        print(base)
        library(PRROC)
        s0 = predictions[which(labels ==0)]
        s1 = predictions[which(labels ==1)]
        prcurve = pr.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
        print(paste0("AUPRC",prcurve$auc.integral))
        prcurve = roc.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
        print(paste0("AUC",prcurve$auc))
      }
    }
  }
  model = "model"
  
  probs = read.csv(file = paste0(name,model,"probs.csv"))
  predictions = probs$prob0
  labels = probs$label
  roc_pred <- prediction(predictions,!labels)
  perf <- performance(roc_pred, measure[1], measure[2])
  plot(perf,col = "black",main = title, add = T, lwd=5)
  legend("bottomleft", legend= nbaselines,
         fill= c("black",col), ncol = 2, cex = 1)
  print(model)
  library(PRROC)
  s0 = probs[probs$label ==0,]$prob0
  s1 = probs[probs$label ==1,]$prob0
  prcurve = pr.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
  print(paste0("AUPRC",prcurve$auc.integral))
  prcurve = roc.curve(scores.class0 = s0,scores.class1 = s1,curve = T)
  print(paste0("AUC",prcurve$auc))
  dev.off()
}

setwd("/nfshome/SHARED/BreastCancerData/BaseLines/Model/Plots")
library(ROCR)
name = "breast"
nbaselines = c("xPacks","SVM-Lin","SVM-Radial","DTree-Bal","1-Gaussian","KDE","PCA-SVDD","NN")
baselines = c("model","svmlinear","svmradial","dtree","2gaussian","kde","svdd","nn")
plotPerf(measure = c("tpr","fpr"), baselines, nbaselines, "ROC curve","roc",name)
plotPerf(measure = c("prec","rec"), baselines, nbaselines, "Precision/Recall curve","prec",name)
plotPerf(measure = c("lift","rpp"), baselines, nbaselines, "Lift chart","lift",name)
plotPerf(measure = c("sens","spec"), baselines, nbaselines, "Senitivity/Specificity plot","senspec",name)

sdata = read.csv('breastmodelprobs.csv')
sscurves <- evalmod(scores = sdata$prob0, labels = !sdata$label)
sdata = read.csv('breastdtreeprobs.csv')
sscurves <- evalmod(scores = sdata$prob0, labels = !sdata$label)

probs = read.csv(file = paste0(name,"dtree","probs.csv"))
predictions = probs$prob0
labels = probs$label
roc_pred <- prediction(predictions,!labels)
perf <- performance(roc_pred, "prec", "rec")
plot(perf,col = "red", add = F, lwd=3)
