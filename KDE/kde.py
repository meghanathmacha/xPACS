import numpy as np

from sklearn.datasets import load_digits
from sklearn.neighbors import KernelDensity
from sklearn.decomposition import PCA
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_curve
import pandas as pd


def kderun(outerfolds, innerfolds, model, name):
    for outerid in range(1,outerfolds+1):
        sets = list(set(range(1,outerfolds+1)) - set([outerid]))
        tdata = pd.read_csv(name+"trainouter"+str(sets[0])+".csv",  index_col=False)
        for id in sets[1:]:
            tdata = tdata.append(pd.read_csv(name+"trainouter"+str(id)+".csv", index_col = False))
        params = {'bandwidth': np.logspace(-1, 1, 20)}
        grid = GridSearchCV(KernelDensity(), params, cv = innerfolds)
        train = tdata[tdata['label'] == 1]
        # Running on only normal points
        train = tdata[tdata['label'] == 1]
        grid.fit(np.array(train.drop('label', axis=1)))
        bbw = grid.best_estimator_.bandwidth
        test = pd.read_csv(name+"trainouter"+str(outerid)+".csv",  index_col=False)
        kdemodel = KernelDensity(bandwidth=bbw)
        kdemodel.fit(np.array(train.drop('label', axis=1)))
        scores = np.exp(kdemodel.score_samples(np.array(test.drop('label', axis=1))))
        test['prob0'] = 1./scores
        test.to_csv(name+model+str(outerid)+"probs.csv",  index=False)
    probs = pd.read_csv(name+model+str(1)+"probs.csv",index_col = False)
    for id in range(2,outerfolds+1):
        probs = probs.append(pd.read_csv(name+model+str(id)+"probs.csv", index_col = False))
    probs.to_csv(name+model+"probs.csv", index = False)
        
        
# set the fold parameters
outerfolds = 3
innerfolds = 3
name = "breast"
import os
params = {'bandwidth': np.logspace(-1, 1, 20)}
os.chdir('/nfshome/SHARED/BreastCancerData/BaseLines/KDE/')
kderun(outerfolds,innerfolds, model = "kde", name = name)

# load the data
# digits = load_digits()
# data = digits.data
# 
# # project the 64-dimensional data to a lower dimension
# pca = PCA(n_components=15, whiten=False)
# data = pca.fit_transform(digits.data)
# 
# # use grid search cross-validation to optimize the bandwidth
# params = {'bandwidth': np.logspace(-1, 1, 5)}
# grid = GridSearchCV(KernelDensity(), params, cv = 5)
# grid.fit(data)
