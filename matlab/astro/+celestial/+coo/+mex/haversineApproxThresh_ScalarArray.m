% Above threshold spherical distance using the taylor-approximated havesine formula (fastest mex)
%     Spherical distance will be calculated only for when
%     dlon/dlat>Threshold/cos(lat1) (should take care over wrapping).
%     This is ~x10 faster than sphere_dist_fast
% Input  : - Longitude1 scalar [rad]
%          - Latitude1 scalar [rad]
%          - Longitude2 array [rad]
%          - Latitude2 array [rad]
%          - Threshold distance [rad]. Default is 0.001.
% Output : - Spherical disatnce [rad]
% Author : Eran Ofek (2024 Oct) 
% Example: R3=rand(1e4,1).*2.*pi; R4=rand(1e4,1).*pi - pi./2;
%          tic; for I=1:1e4, dd=celestial.coo.mex.haversineApproxThresh_ScalarArray(1,1,R3,R4); end,toc
%          tic; for I=1:1e4, d=celestial.coo.sphere_dist_fast(1,1,R3,R4); end,toc
%          max(abs(d-dd),[],'omitnan')