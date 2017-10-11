function [A,B,C] = filtering_mod( A ,B,C,quant,lambda,diagonal)
[P,Q,r] = ellipses_filter(A,B,lambda,diagonal);
d = sum((C'*P).*C',2) + C'*Q + r;
nC = length(d(d < 0,:));
nA = length(A);
nB = length(B);
% p = nB/(nB + nA);
p = 0.05; % This needs to be the proportion of total number of anomalies divided by the total number of normal cases.
dthre = quantile(-1*d,quant);
C = C(:,d >= -1*dthre);