function [M,xc,n] = ellipses3compact( A ,B,C,lambda)
nA = length(A);
nB = length(B);
nC = length(C);
dims = size(A,1);
cvx_begin sdp
    variable P(dims,dims) symmetric
    variable Q(dims)
    variable r(1)
    variable errorsA(nA,1)
    variable errorsB(nB,1)
    variable errorsC(nC,1)
    minimize(10^-3*norm(P) + (1 - lambda)*(sum(errorsA)/nA ) +  ((lambda)/nB)*sum(errorsB) - 10^-15*sum(errorsC))
    subject to 
        sum((A'*P).*A',2) + A'*Q +  r + errorsA >= 1;
        sum((B'*P).*B',2) + B'*Q + r - errorsB <= -1;
        sum((C'*P).*C',2) + C'*Q +  r + errorsC >= 1;
        P <= -eye(dims);
        errorsB >= 0;
        errorsA >= 0;
        errorsC >= 0;
cvx_end

% nopts = 1000;
% angles = linspace(0,2*pi,nopts);
% ell = inv(sqrtm(P/c))*[cos(angles); sin(angles)] + repmat(xc,1,nopts);
% M = cov(ell');

fA = sum((A'*P).*A',2) + A'*Q +  r + errorsA;
perA = A(:,fA >=1);
fB = sum((B'*P).*B',2) + B'*Q + r - errorsB;
perB = B(:,fB >-1);
fC = sum((C'*P).*C',2) + C'*Q +  r + errorsC;
perC = C(:,fC >=1);     
per = vertcat(perA',perB',perC');
M = cov(per);
xc = mean(per',2);
fA = sum((A'*P).*A',2) + A'*Q +  r;
perA = A(:,fA >=1);
fB = sum((B'*P).*B',2) + B'*Q + r;
perB = B(:,fB >-1);
fC = sum((C'*P).*C',2) + C'*Q +  r;
perC = C(:,fC >=1); 
nP = size(perA,2) + size(perC,2);
nN = size(perB,2);
n = [nP nN];

% r = -r; P = -P; Q = -Q;
% xc = P \ Q;
% xc = -0.5*xc;
end


