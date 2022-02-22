z2 = randn(8998);
z3 = zeros(size(z2)+2);
size(z3)
z3(2:(size(z2,1)+1),2:(size(z2,2)+1))= z2;
tic
[a1,b1,c1,d1] = imUtil.sources.mex_find_local_max_single(z3,2,4,0.025);
toc

tic
[a2,b2,c2,d2] = imUtil.sources.findLocalMaxAboveThreshold(z3,2,4,0.025);
toc


all(a2==a1(1:length(a2)))
all(b2==b1(1:length(a2)))
all(c2==c1(1:length(a2)))
all(all(d1==d2))
