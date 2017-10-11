function [] = writeellipse(xc,M,status,l,r,n,fileM4,fileC4,fileN4,fileP4)
    if(status && sum(n) > 0)
        xc = reshape( xc.' ,1,numel(xc));
        M = reshape( M.' ,1,numel(M));
        params = [status,l,r];
        dlmwrite(fileM4,M,'-append','delimiter',',')
        dlmwrite(fileC4,xc,'-append','delimiter',',')
        dlmwrite(fileN4,n,'-append','delimiter',',')
        dlmwrite(fileP4,params,'-append','delimiter',',')
    end
end 