%% Reading data
function []=singleSphere(outerfolds,innerfolds,model,name)
    Cgrid = 0.1:0.2:2;
    fprobs = [ ];
    for outerid = 1:outerfolds
        disp(outerid);
        sets = setdiff([1:outerfolds],outerid);
        delimiterIn = ',';
        headerlinesIn = 1;
        fdata = [ ]; 
        for id = 1:sets
            filename = strcat(name,'trainouter',num2str(id),'.csv');
            data = importdata(filename,delimiterIn,headerlinesIn);
            fdata = [fdata;data.data];
        end
        dlabels = fdata(:,end);
        indices = crossvalind('Kfold',size(fdata,1),innerfolds);
        % indices has 1,2,3 for the three folds.
        bauc= [ ];
        bC = [ ];
        for innerid = 1:innerfolds
            isets = setdiff([1:innerfolds],innerid);
            test = fdata(indices == innerid,1:end);
            train = fdata(indices ~= innerid,1:end);
            traindat = train(:,1:end-1);
            trainlabels = train(:,end);
            auc = [ ];
            innerauc = [ ];
            for grid = Cgrid
               [d, b, c, status] = svd_solve(traindat,trainlabels,grid);
               AUC = 0;
               
                R = b + c'*c; 
                testfeatures = test(:,1:end-1);
                testlabels = test(:,end);
                decision = sum((testfeatures - ones(length(testfeatures),1)*c').^2,2) - R*R;
                g = zeros(size(decision));
                g = 1.0 ./ ( 1.0 + exp(-decision));
                tlabels = test(:,end);
              if size(g,1) > 0 
                [X,Y,T,AUC] = perfcurve(~testlabels, 1 - g,false);
              end 
                auc =[auc,AUC];
            end
            bestC = Cgrid(find(auc == max(auc)));
            bC = [bC,bestC];
            bauc = [bauc,max(auc)]; 
        end
        hyperC = bC(find(bauc == max(bauc))); % Chose the best hyper C for that outer fold.
        filename = strcat(name,'trainouter',num2str(outerid),'.csv');
        testdata = importdata(filename,delimiterIn,headerlinesIn); % Outerid has the test data.
        [d, b, c, status] = svd_solve(fdata(:,1:end-1),fdata(:,end),hyperC);
        R = b + c'*c; 
        testfeatures = testdata.data(:,1:end-1);
        decision = sum((testfeatures - ones(length(testfeatures),1)*c').^2,2) - R*R;
        g = zeros(size(decision));
        g = 1.0 ./ ( 1.0 + exp(-decision));
        probs = [testdata.data (1-g) g];
        dlmwrite(strcat(name,'trainouter',model,num2str(outerid),'probs.csv'),probs,'delimiter',',');
        fprobs = [fprobs;probs];
    end
    dlmwrite(strcat(name,'trainouter',model,'probs.csv'),probs,'delimiter',',');
end
