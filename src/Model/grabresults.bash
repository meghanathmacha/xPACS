#!/bin/sh
cd Plots
cp ../../SVM/*radialprobs.csv .
cp ../../SVM/*linearprobs.csv .
cp ../../SVM/*polynomialprobs.csv .
cp ../../SVDD/*svddprobs.csv .
cp ../../Gaussian/*gaussianprobs.csv .
cp ../../DTree/*dtreeprobs.csv .
cp ../*modelprobs.csv .
cp ../../NN/*nnprobs.csv .
cp ../../KDE/*kdeprobs.csv .
