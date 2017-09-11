#!/bin/sh

R --slave -f processdata.R
echo "Part 1 done"
bash ellipse.bash
echo "Part 2 done"
R --slave -f parseEllipsoids.R
echo "Part 3 done"
cd Part2/
R --slave -f model.R
echo "Probabilities saved"
