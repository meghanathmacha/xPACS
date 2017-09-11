function [] = writerule(xc,M,status,l,r,n,densedims,filerule,rule,trainid)
    if(status && sum(n) > 0)
%         scale = sqrt(chi2inv(0.95,length(densedims)));
        xc = reshape( xc.' ,1,numel(xc));
        M = reshape( M.' ,1,numel(M));
        params = [status,l,r,n];
%         e = diag(M);
%         lb = xc - scale*sqrt(e');
%         ub = xc + scale*sqrt(e');
%         rule = [lb;ub];
%         rule = rule';
        rule = [rule densedims];
        params = params(ones(size(rule,1),1),:);
        rule = [rule params];
        path = strcat(pwd,'/Train',num2str(trainid),'/');
        dlmwrite([path filerule],rule,'-append','delimiter',',');
    end
end 