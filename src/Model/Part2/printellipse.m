function [] = printellipse( A ,B, C, densedims, filerule,trainid)

power = -3:1:3;
reward = [10.^power 10.^power/2];
ratio = 100;
lmax = 10^3;
for r = reward
    disp(r)
    lmin = ratio*r;
    lmax = 10^6;
    if(lmin > lmax)
        %disp('Faulty')
        continue
    end
    [Mmax,xcmax,nmax,statusmax,rulemax] = ellipses3diag(A,B,C,lmax,r);
    [Mmin,xcmin,nmin,statusmin,rulemin] = ellipses3diag(A,B,C,lmin,r);
   % First we find the bounds lmax and lmin in which for a given r, the
   % solutions exist.
    while statusmax ==0 || statusmin == 0 && lmin < lmax
        if statusmax == 0
             lmax = lmax/2;
             [Mmax,xcmax,nmax,statusmax,rulemin] = ellipses3diag(A,B,C,lmax,r);
        elseif statusmin == 0
             lmin = 2*lmin;
            [Mmin,xcmin,nmin,statusmin,rulemax] = ellipses3diag(A,B,C,lmin,r);       
        end 
    end 
    if(lmin > lmax)
        %disp('Faulty')
        continue
    end
    % Found the bound in which solutions potentially exist - lmin and lmax.
    % Now we do a granular search in this bound.
    % First check for dominance in between the bounds. 
    display(nmin)
    display(nmax)
    if(dominance(nmin,nmax))
        writerule(xcmin,Mmin,statusmin,lmin,r,nmin,densedims,filerule,rulemin,trainid)
        continue
    elseif(dominance(nmax,nmin))
        writerule(xcmax,Mmax,statusmax,lmax,r,nmax,densedims,filerule,rulemax,trainid)
        continue
    else
        writerule(xcmin,Mmin,statusmin,lmin,r,nmin,densedims,filerule,rulemin,trainid)
        writerule(xcmax,Mmax,statusmax,lmax,r,nmax,densedims,filerule,rulemax,trainid)
    end
    elmin = lmin;
    elmax = lmax;
    while (lmax - lmin > 10)
        display(nmin)
        display(nmax)
        if(dominance(nmin,nmax))
            if elmin ~= lmin
                %display(lmin)
                writerule(xcmin,Mmin,statusmin,lmin,r,nmin,densedims,filerule,rulemin,trainid)
                elmin = lmin;
                lmax = 0;
                lmin = 0;         
                continue
            end
        elseif(dominance(nmax,nmin))
            if(elmax ~= lmax )
                %display(lmax)
                writerule(xcmax,Mmax,statusmax,lmax,r,nmax,densedims,filerule,rulemax,trainid)
                elmax = lmax;
                lmax = 0;
                lmin = 0;                
                continue
            end
        else
            if elmin ~= lmin 
                %display(lmin)
                writerule(xcmin,Mmin,statusmin,lmin,r,nmin,densedims,filerule,rulemin,trainid)
                elmin = lmin;
            elseif elmax~=lmax 
                display(lmax)
                writerule(xcmax,Mmax,statusmax,lmax,r,nmax,densedims,filerule,rulemax,trainid)
                elmax = lmax;
            end
            lmax = lmax/2;
            lmin = lmin*2;
            [Mmin,xcmin,nmin,statusmin,rulemin] = ellipses3diag(A,B,C,lmin,r);
            [Mmax,xcmax,nmax,statusmax,rulemax] = ellipses3diag(A,B,C,lmax,r);
        end
    end 
end
fclose('all');

end