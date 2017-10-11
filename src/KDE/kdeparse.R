
setwd("/nfshome/SHARED/BreastCancerData/BaseLines/KDE/")
name = "breast"
model = "kde"
probs = read.csv(paste0(name,model,'probs.csv'),header = T) 
head(probs)

roc_pred <- prediction(1/probs$prob0,!probs$label)
perf <- ROCR::performance(roc_pred, "tpr", "fpr")
plot(perf,col = "red",main = "ROC", add = F)
abline(0,1,col="grey")
