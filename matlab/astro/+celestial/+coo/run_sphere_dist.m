data = randn(1,4);
%[a,b,c]=celestial.coo.mex_sphere_dist_fast_single(1,2,3,4);
[a,b,c]=celestial.coo.mex_sphere_dist_fast_single(data(1),data(2),data(3),data(4));
%[a1,b1,c1]=celestial.coo.sphere_dist_fast(1,2,3,4);
[a1,b1,c1]=celestial.coo.sphere_dist_fast(data(1),data(2),data(3),data(4));
fprintf("Dist mex % f matlab %f \n ",a(1),a1(1))
fprintf("Ang mex %f matlab %f \n",b(1),b1(1))
fprintf("PA mex %f matlab %f \n",c(1),c1(1))
dim = 500;
tic
[a,b,c]=celestial.coo.mex_sphere_dist_fast_single(randn(dim),randn(dim),randn(dim),randn(dim));
toc

tic
[a1,b1,c1]=celestial.coo.sphere_dist_fast(randn(dim),randn(dim),randn(dim),randn(dim));
toc