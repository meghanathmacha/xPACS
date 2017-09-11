%% Reading subspace candiates from Part - 1
filename = 'subs.csv';
delimiterIn = ',';
headerlinesIn = 1;
subs = importdata(filename,delimiterIn,headerlinesIn);

filename = 'data.csv';
data = importdata(filename,delimiterIn,headerlinesIn);

nfeatures = length(data.colheaders);
dlabels = data.data(:,end);

%% Getting the ids of each of the subspaces in the set of subspaces
numsubs = unique(subs.data(:,9)); % 3 is the column with the subspace id
%densedims = unique(subs.data(:,3));
% Filtering the data to only the dense dimensions.
% data.data = data.data(:,densedims);
for i = numsubs'
    t = data.data(:,(1:end -1));
    sub = subs.data(subs.data(:,9) == i,:); % Selecting the ith subspace
    densedims = unique(sub(:,3)); % 3 is the field with the dimension of the subspace
    % Filtering the data to only the dense dimensions.
    sdata = data.data(:,densedims);
    for j = 1:size(sub,1)
        x = sub(j,:);
        dim = x(3); % 3 has the dimension of the subspace
        tmp_dat = t(:,dim); % Extracting the points in that dimension
        tmp = tmp_dat(tmp_dat >= x(1) & tmp_dat <= x(2));
        t = t(ismember(t(:,dim),tmp) == 1,:);        
    end
    label = data.data(:,end);
    ncol = zeros(length(data.data),1);
    ndata = [sdata ncol]; 
    ndata(ismember(data.data(:,1:end-1),t,'rows') == 1 & label ==0,end) = 1;
    ndata(ismember(data.data(:,1:end-1),t,'rows') == 1 & label ==1,end) = -1;
    sublabel = ndata(:,end);
    A = ndata(sublabel == 1,1:end);
    B = ndata(sublabel == -1,1:end);
    C = ndata(sublabel == 0 & label == 0,1:end);
    A = A';
    B = B';
    C = C';
    %fcount = randi([0,10000000000],1);
    fcount = 1000;
    Mfile = strcat('MFile',num2str(fcount),'.csv');
    Cfile = strcat('CFile',num2str(fcount),'.csv');
    Nfile = strcat('NFile',num2str(fcount),'.csv');
    printellipse(A,B,C,Mfile,Cfile,Nfile)
end
calculateEllipses('subs.csv','data.csv');
%% Reading data into X and Y seperately
dat = csvread('ut23_simpleellipse.csv');
label = dat(:,3);
X = dat(:,[1,2]);
n = length(dat);
X = X';

A = dat(label == 1,:);
B = dat(label == -1,:);
C = dat(label == 0,:);
A = A(:,1:2);
B = B(:,1:2);
C = C(:,1:2);
lambda = 0.4;
A = A';
B = B';
C = C';
%% Experiment with 2 types of points
fileM2 = 'M2.csv';
fileC2 = 'C2.csv';
delete(fileM2,fileC2);
for lambda = 0.1:0.4:0.8
    [M,xc] = ellipses2(A,B,lambda);
    xc = reshape( xc.' ,1,numel(xc));
    M = reshape( M.' ,1,numel(M));
    dlmwrite(fileM2,M,'-append','delimiter',',')
    dlmwrite(fileC2,xc,'-append','delimiter',',')
end
fclose('all');

%% Experiment with 3 types of points
fileM3 = 'M3.csv';
fileC3 = 'C3.csv';
delete(fileM3,fileC3);
for lambda = 0.1:0.1:1
    [M,xc] = ellipses3(A,B,C,lambda);
    xc = reshape( xc.' ,1,numel(xc));
    M = reshape( M.' ,1,numel(M));
    dlmwrite(fileM3,M,'-append','delimiter',',')
    dlmwrite(fileC3,xc,'-append','delimiter',',')
end
fclose('all');

%% Experiment with 3 types of points alternate
[A,B,C] = readFile( 'final_5.csv' );
fileM3 = 'M3alt.csv';
fileC3 = 'C3alt.csv';
fileN3 = 'N3alt.csv';
fileP3 = 'P3alt.csv';
delete(fileM3,fileC3,fileN3,fileP3);
power = -6:1:6;
lambda = 10.^power;
reward = 10.^power;

for l = lambda
    for r = reward
        [M,xc,n,status] = ellipses3alt(A,B,C,l,r);
        xc = reshape( xc.' ,1,numel(xc));
        M = reshape( M.' ,1,numel(M));
        params = [status,l,r];
        dlmwrite(fileM3,M,'-append','delimiter',',')
        dlmwrite(fileC3,xc,'-append','delimiter',',')
        dlmwrite(fileN3,n,'-append','delimiter',',')
        dlmwrite(fileP3,params,'-append','delimiter',',')
    end
end
fclose('all');

%%
[A,B,C] = readFile( 'final_4.csv' );
fileM4 = 'M4alt.csv';
fileC4 = 'C4alt.csv';
fileN4 = 'N4alt.csv';
fileP4 = 'P4alt.csv';

delete(fileM4,fileC4,fileN4,fileP4);
power = -6:1:6;
lambda = 10.^power;
reward = 10.^power;

for l = lambda
    for r = reward
        [M,xc,n,status] = ellipses3diag(A,B,C,l,r);
        xc = reshape( xc.' ,1,numel(xc));
        M = reshape( M.' ,1,numel(M));
        params = [status,l,r];
        dlmwrite(fileM4,M,'-append','delimiter',',')
        dlmwrite(fileC4,xc,'-append','delimiter',',')
        dlmwrite(fileN4,n,'-append','delimiter',',')
        dlmwrite(fileP4,params,'-append','delimiter',',')
    end
end
fclose('all');
