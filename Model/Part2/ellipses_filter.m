function [P,Q,r] = ellipses_filter( A ,B,lambda, diagonal)
if diagonal == true
    nA = length(A);
    nB = length(B);
    dims = size(A,1);
    cvx_begin quiet sdp
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
else
    nA = length(A);
    nB = length(B);
    dims = size(A,1);
    cvx_begin quiet sdp
        variable P(dims,dims) symmetric
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
end
    