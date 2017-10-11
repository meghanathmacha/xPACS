function [M,xc,n,status,rule] = ellipses3diag( A ,B,C,lambda,reward)
nA = size(A,2);
nB = size(B,2);
nC = size(C,2);
dims = size(A,1);
cvx_begin sdp 
    variable P(dims,dims) symmetric diagonal
    variable Q(dims)
    variable r(1)
    variable errorsA(nA,1) nonnegative
    variable errorsB(nB,1) nonnegative
    variable errorsC(nC,1) nonnegative
    minimize(sum(errorsA) +  (lambda)*sum(errorsB)  + reward*sum(errorsC))
    subject to 
        sum((A'*P).*A',2) + A'*Q +  r + errorsA >= 1;
        sum((B'*P).*B',2) + B'*Q + r - errorsB <= -1;
        sum((C'*P).*C',2) + C'*Q +  r + errorsC >= 1;
        P <= -eye(dims);
cvx_end

% nopts = 1000;
% angles = linspace(0,2*pi,nopts);
% ell = inv(sqrtm(P/c))*[cos(angles); sin(angles)] + repmat(xc,1,nopts);
% M = cov(ell');
margin = -1;
fA = sum((A'*P).*A',2) + A'*Q +  r ;
perA = A(:,fA >=-margin);
fB = sum((B'*P).*B',2) + B'*Q + r ;
perB = B(:,fB >=-margin);
fC = sum((C'*P).*C',2) + C'*Q +  r ;
perC = C(:,fC >=-margin);    
per = vertcat(perA',perB',perC');
M = cov(per);
M = M.* eye(dims);
xc = mean(per',2);
%[eigenvec, eigenval ] = eig(M);
eigenval = diag(M);
chisquare_val = sqrt(chi2inv(0.99,size(M,1)));
semaxes = chisquare_val*sqrt(eigenval);
disp(M);
disp(eigenval);
disp(size(semaxes));
disp(size(xc));
lb = xc-semaxes;
ub = xc+semaxes;

fA = sum((A'*P).*A',2) + A'*Q +  r ;
perA = A(:,fA >=-margin);
fB = sum((B'*P).*B',2) + B'*Q + r ;
perB = B(:,fB >=-margin);
fC = sum((C'*P).*C',2) + C'*Q +  r ;
perC = C(:,fC >=-margin);     
% M = inv(-P);
% xc = P\Q;
% xc = -0.5*xc;
%lb = min(per);
%ub = max(per);
rule = [lb';ub'];
disp(rule);
rule = rule';

n = [size(perA,2) size(perB,2) size(perC,2)];

status = 0;
if(strcmp(cvx_status,'Solved'))
    status = 1;
% r = -r; P = -P; Q = -Q;
% xc = P \ Q;
% xc = -0.5*xc;
end


