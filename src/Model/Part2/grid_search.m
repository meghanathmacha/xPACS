[A,B,C] = readFile( 'final_2.csv' );
fileM4 = 'M4alt.csv';
fileC4 = 'C4alt.csv';
fileN4 = 'N4alt.csv';
fileP4 = 'P4alt.csv';

delete(fileM4,fileC4,fileN4,fileP4);
power = -5:1:4;
reward = [10.^power 10.^power/2];
ratio = 100;
lmax = 10^6;
for r = reward
    disp(r)
    lmin = ratio*r;
    lmax = 10^6;
    if(lmin > lmax)
        disp('Faulty')
        continue
    end
    [Mmax,xcmax,nmax,statusmax] = ellipses3diag(A,B,C,lmax,r);
    [Mmin,xcmin,nmin,statusmin] = ellipses3diag(A,B,C,lmin,r);
   % First we find the bounds lmax and lmin in which for a given r, the
   % solutions exist.
    while statusmax ==0 || statusmin == 0 && lmin < lmax
        if statusmax == 0
             lmax = lmax/2;
             [Mmax,xcmax,nmax,statusmax] = ellipses3diag(A,B,C,lmax,r);
        elseif statusmin == 0
             lmin = 2*lmin;
            [Mmin,xcmin,nmin,statusmin] = ellipses3diag(A,B,C,lmin,r);       
        end 
    end 
    if(lmin > lmax)
        disp('Faulty')
        continue
    end
    % Found the bound in which solutions potentially exist - lmin and lmax.
    % Now we do a granular search in this bound.
    % First check for dominance in between the bounds. 
    if(dominance(nmin,nmax))
        writeellipse(xcmin,Mmin,statusmin,lmin,r,nmin,fileM4,fileC4,fileN4,fileP4)
    elseif(dominance(nmax,nmin))
        writeellipse(xcmax,Mmax,statusmax,lmax,r,nmax,fileM4,fileC4,fileN4,fileP4)
    else
        writeellipse(xcmin,Mmin,statusmin,lmin,r,nmin,fileM4,fileC4,fileN4,fileP4)
        writeellipse(xcmax,Mmax,statusmax,lmax,r,nmax,fileM4,fileC4,fileN4,fileP4)
    end
    elmin = lmin;
    elmax = lmax;
    while (lmax - lmin > 10)
        if(dominance(nmin,nmax))
            if elmin ~= lmin
                display(lmin)
                writeellipse(xcmin,Mmin,statusmin,lmin,r,nmin,fileM4,fileC4,fileN4,fileP4)
                elmin = lmin;
            end
            lmax = (lmin + lmax)*0.5;
            [Mmax,xcmax,nmax,statusmax] = ellipses3diag(A,B,C,lmax,r);
        elseif(dominance(nmax,nmin))
            if(elmax ~= lmax )
                display(lmax)
                writeellipse(xcmax,Mmax,statusmax,lmax,r,nmax,fileM4,fileC4,fileN4,fileP4)
                elmax = lmax;
            end
            lmin = (lmin + lmax)*0.5;
            [Mmin,xcmin,nmin,statusmin] = ellipses3diag(A,B,C,lmin,r);
        else
            if elmin ~= lmin 
                display(lmin)
                writeellipse(xcmin,Mmin,statusmin,lmin,r,nmin,fileM4,fileC4,fileN4,fileP4)
                elmin = lmin;
            elseif elmax~=lmax 
                display(lmax)
                writeellipse(xcmax,Mmax,statusmax,lmax,r,nmax,fileM4,fileC4,fileN4,fileP4)
                elmax = lmax;
            end
            lmax = lmax/2;
            lmin = lmin*2;
        end
    end 
end
fclose('all');