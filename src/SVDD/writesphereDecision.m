function [] = writesphereDecision(c,R,densedims,filerule,test,filedecision,status)
    xc = reshape( c.' ,1,numel(c));
    %M = reshape( M.' ,1,numel(M));
    lb = xc - R;
    ub = xc + R;
    rule = [lb;ub];
    rule = rule';
    rule = [rule densedims'];
    params = status;
    params = params(ones(size(rule,1),1),:);
    rule = [rule params];
    
    dlmwrite(filerule,rule,'delimiter',',');

    test = test(:,densedims);
    nx = diag(test*test');
    Xi = test;
    %decision = 2*Xi*c -(-b+nx);
    ndat = length(test);
    decision = sum((Xi - ones(ndat,1)*c').^2,2) - R*R;
    dlmwrite(filedecision,decision,'delimiter',',');
end 