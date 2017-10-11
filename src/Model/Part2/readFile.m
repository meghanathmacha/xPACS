function [A,B,C] = readFile( filename )
    dat = csvread(filename);
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
    A = A';
    B = B';
    C = C';
end