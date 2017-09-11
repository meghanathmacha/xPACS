#!/bin/sh

R --slave -f processbreast.R
echo "Part 1 done"
bash ellipse.bash
echo "Part 2 done"
R --slave -f parseEllipsoids.R
cd /nfshome/SHARED/BreastCancerData/BaseLines/Model/
echo "Part 3 done"
R --slave -f model.R
echo "Probabilities saved"