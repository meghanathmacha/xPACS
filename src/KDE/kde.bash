#!/bin/sh
cd /nfshome/SHARED/BreastCancerData/BaseLines/KDE
R --slave -f kdesplits.R
python kde.py