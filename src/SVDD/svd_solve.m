
%% SVDD Margin Primal form
function [d1, b, cSVDD,status]=svd_solve(dat,ldat,C)
nx = diag(dat*dat');
yi = ldat;
Xi = dat;
dims = size(dat,2);
ndat = length(dat);

cvx_begin quiet
   cvx_precision best
   variables b(1) cSVDD(dims,1) xi(ndat,1) 
   dual variables d1 p1
   minimize(  cSVDD'*cSVDD  + b + C*sum(xi) )
   subject to
      d1  : 2* diag(yi)*Xi*cSVDD >= yi.*(-b+nx) - xi;
      p1 :  xi >=0;
cvx_end
status = 0;
if(strcmp(cvx_status,'Solved'))
    status = 1;
end