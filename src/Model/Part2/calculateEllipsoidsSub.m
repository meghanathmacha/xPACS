function [] = calculateEllipsoidsSub(subid,datafile,subsfile,trainid)
%% Reading subspace candiates from Part - 1
    %filename = 'subs.csv';
    %filename = 'data.csv';
    %subid = str2num(subid);
    disp(subid);
    delimiterIn = ',';
    filename = datafile;
    headerlinesIn = 1;
    data = importdata(filename,delimiterIn,headerlinesIn);
    
    filename = subsfile;
    delimiterIn = ',';
    subs = importdata(filename,delimiterIn);

    %nfeatures = length(data.colheaders);
    %dlabels = data.data(:,end);

    %% Getting the ids of each of the subspaces in the set of subspaces
    t = data.data(:,(1:end -1));
    sub = subs.data(subs.data(:,9) == subid,:);
    disp(sub);
    for j = 1:size(sub,1)
        x = sub(j,:);
        dim = x(3); % 3 has the dimension of the subspace
        tmp_dat = t(:,dim); % Extracting the points in that dimension
        tmp = tmp_dat(tmp_dat >= x(1) & tmp_dat <= x(2));
        t = t(ismember(t(:,dim),tmp) == 1,:);        
    end
    label = data.data(:,end);
    ncol = zeros(size(data.data,1),1);
    ndata = [data.data ncol]; 
    ndata(ismember(data.data(:,1:end-1),t,'rows') == 1 & label ==0,end) = 1;
    ndata(ismember(data.data(:,1:end-1),t,'rows') == 1 & label ==1,end) = -1;
    sublabel = ndata(:,end);
    A = ndata(sublabel == 1,1:end-2);
    B = ndata(label == 1,1:end-2); % All the red points need to be considered.
    C = ndata(sublabel == 0 & label == 0,1:end-2); % All the other black points need to be considered.
    densedims = unique(sub(:,3));
    % Filtering the data to only the dense dimensions.
    %data.data = data.data(:,densedims);
    A = A(:,densedims);
    B = B(:,densedims);
    C = C(:,densedims);
    A = A';
    B = B';
    C = C';
    %fcount = randi([0,10000000000],1);
    filerule = strcat('MFile',num2str(subid),'.csv');
    printellipse(A,B,C,densedims,filerule,trainid);
end 