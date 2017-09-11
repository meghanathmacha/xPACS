function [A,B,C] = filtering( A ,B,C,quant)
d = mahal(C',A');
nA = length(A);
nB = length(B);
nC = length(C);
% p = nB/(nB + nA);
p = 0.05; % This needs to be the proportion of total number of anomalies divided by the total number of normal cases.
dthre = quantile(d,quant);
C = C(:,d' <= dthre);




