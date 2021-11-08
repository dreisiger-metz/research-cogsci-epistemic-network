function r = dr_barplots(filename)
%   Generates a series of barplots that show the distributed representations,
%   tailored for the output of the rogers-mcclelland training set
%   R = DR_BARPLOTS(filename) reads an m-by-n matrix of n-dimensional 
%           distributed representations from the specified file, and plots
%           their patterns of activation.
X = importdata(filename, ' ', 0);
subplot(1,1,1);

% the #objects are first arranged in column format
subplot(7,5,1), bar(X.data(1,:));
xlabel(X.textdata(1));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);
subplot(7,5,6), bar(X.data(5,:));
xlabel(X.textdata(5));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,5,2), bar(X.data(7,:));
xlabel(X.textdata(7));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);
subplot(7,5,7), bar(X.data(8,:));
xlabel(X.textdata(8));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,5,3), bar(X.data(3,:));
xlabel(X.textdata(3));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);
subplot(7,5,8), bar(X.data(4,:));
xlabel(X.textdata(4));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,5,4), bar(X.data(2,:));
xlabel(X.textdata(2));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);
subplot(7,5,9), bar(X.data(6,:));
xlabel(X.textdata(6));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,5,5), bar(X.data(10,:));
xlabel(X.textdata(10));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);
subplot(7,5,10), bar(X.data(9,:));
xlabel(X.textdata(9));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

for i = 11:max(size(X.data(:,1)))
  subplot(7,5,i), bar(X.data(i,:));
  xlabel(X.textdata(i));
  axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);
end



r = max(size(X.data(1,:)));
