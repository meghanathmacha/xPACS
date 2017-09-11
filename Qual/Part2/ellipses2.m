function [M,xc] = ellipses2( A ,B,lambda)
nA = length(A);
nB = length(B);
dims = size(A,1);
cvx_begin sdp
    variable P(dims,dims) symmetric diagonal
    variable Q(dims)
    variable r(1)
    variable errorsA(nA,1)
    variable errorsB(nB,1)
    minimize((((1 - lambda)/nA)*sum(errorsA)) +  (((lambda)/nB)*sum(errorsB)))
    subject to 
        sum((A'*P).*A',2) + A'*Q +  r + errorsA >= 1;
        sum((B'*P).*B',2) + B'*Q + r - errorsB <= -1;
        P <= -eye(dims);
        errorsB >= 0;
        errorsA >= 0;
cvx_end
margin = 0;
fA = sum((A'*P).*A',2) + A'*Q +  r ;
perA = A(:,fA >=margin);
fB = sum((B'*P).*B',2) + B'*Q + r;
perB = B(:,fB >=margin);
per = vertcat(perA',perB');
fA = sum((A'*P).*A',2) + A'*Q +  r;
perA = A(:,fA >=1);
fB = sum((B'*P).*B',2) + B'*Q + r;
perB = B(:,fB >-1); 
nP = size(perA,2);
nN = size(perB,2);
n = [nP nN];
M = cov(per);
M = M.* eye(dims);
xc = mean(per',2);
% r = -r; P = -P; Q = -Q;
% xc = P \ Q;
% xc = -0.5*xc;
% c = 0.25*Q'*inv(P)*Q - r;
% nopts = 1000;
% angles = linspace(0,2*pi,nopts);
% ell = inv(sqrtm(P/c))*[cos(angles); sin(angles)] + repmat(xc,1,nopts);
% M = cov(ell');



end


