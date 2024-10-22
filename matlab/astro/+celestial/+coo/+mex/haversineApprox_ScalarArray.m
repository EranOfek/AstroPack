% Small distance approximation for spherical distance (fast mex)
%     This is only slightly faster than sphere_dist_fast
% Input  : - Longitude1 scalar [rad]
%          - Latitude1 scalar [rad]
%          - Longitude2 array [rad]
%          - Latitude2 array [rad]
% Output : - Approximate spherical disatnce [rad] If distance is larger
%            then about 0.01 radians, then it should be ignored.
% Author : Eran Ofek (2024 Oct) 
% Example: R3=rand(1e4,1).*2.*pi; R4=rand(1e4,1).*pi - pi./2;
%          tic; for I=1:1e4, dd=celestial.coo.mex.haversineApprox_ScalarArray(1,1,R3,R4); end,toc
%          tic; for I=1:1e4, d=celestial.coo.sphere_dist_fast(1,1,R3,R4); end,toc
%          F=d<0.01; max(abs(d(F)-dd(F)))