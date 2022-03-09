z=[0 0 0;0 10 0;0 0 0;0 0 0];
[a,b,c,d] = imUtil.sources.mex_find_local_max_single(z,2.3,4,0.025)
%dsfsdf
Conn=4;
z2 = randn(5000);
z3 = zeros(size(z2)+2);
size(z3)
z3(2:(size(z2,1)+1),2:(size(z2,2)+1))= z2;
%z3=z2;
Thresh = 2.1;
AllocateFrac = 0.02;
tic
%[a1,b1,c1,d1] = imUtil.sources.mex_find_local_max_single(z3,Thresh,Conn,AllocateFrac);
[a1,b1,c1,d1] = imUtil.sources.findLocalMaxAboveThreshold_mex_double(z3,Thresh,Conn,AllocateFrac);
toc

tic
[a2,b2,c2,d2] = imUtil.sources.mex_find_local_max_single2(z3,Thresh,Conn,AllocateFrac);
toc

tic
[a3,b3,c3,d3] = imUtil.sources.findLocalMaxAboveThreshold(z3,Thresh,Conn,AllocateFrac);
toc

fprintf('comparison 1st Mex \n') 
all(a1(1:min(length(a1),length(a3)))==a3(1:min(length(a1),length(a3))))
all(b1(1:min(length(b1),length(b3)))==b3(1:min(length(b1),length(b3))))
all(c1(1:min(length(c1),length(c3)))==c3(1:min(length(c1),length(c3))))
all(all(d1==d3))
fprintf('comparison 1st Mex \n')
all(a2(1:min(length(a2),length(a3)))==a3(1:min(length(a1),length(a3))))
all(b2(1:min(length(b2),length(b3)))==b3(1:min(length(b1),length(b3))))
all(c2(1:min(length(c2),length(c3)))==c3(1:min(length(c1),length(c3))))
all(all(d2==d3))
