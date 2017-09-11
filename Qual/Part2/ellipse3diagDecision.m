function [decision] = ellipse3diagDecision( A ,B,C,lambda,reward,test)
nA = size(A,2);
nB = size(B,2);
nC = size(C,2);
dims = size(A,1);
cvx_begin sdp 
    variable P(dims,dims) symmetric
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
decision = sum((test'*P).*test',2) + test'*Q +  r ;


%margin = -1;
%fA = sum((A'*P).*A',2) + A'*Q +  r ;
%perA = A(:,fA >=-margin);
%fB = sum((B'*P).*B',2) + B'*Q + r ;
%perB = B(:,fB >=-margin);
%fC = sum((C'*P).*C',2) + C'*Q +  r ;
%perC = C(:,fC >=-margin); 
%perT = test(:,decision>=-margin); 
%disp(size(perB));
%disp(size(perT));  
%per = vertcat(perA',perB',perC');
%M = cov(per);
%M = M.* eye(dims);
%xc = mean(per',2);
%decision = mahal(test',per);
end


