% Find healpix indices in the cone search and optionally their RA, Dec
% Two search modes are supported (optional 5th argument):
%   'inclusive' (default) - query_disc_inclusive: all pixels whose borders
%                           overlap the cone, not only those whose centers lie within it.
%   'exclusive'           - query_disc: only pixels whose centers lie within the cone.
% Compiled with: mex coneSearch.cpp -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/Healpix_cxx \
%                    -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/cxxsupport \
%                    -L/home/kra/ExternalLib/Healpix_3.83/lib /home/kra/ExternalLib/Healpix_3.83/lib/libhealpix_cxx.a \
%                    -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
% Important: the Healpix library must be compiled with the -fPIX option
% Input  : - nside (a power of 2)
%          - RA (deg)
%          - Dec (deg)
%          - Cone radius (deg)
%          - Search mode (optional): 'inclusive' (default) or 'exclusive'
%          - Ordering scheme (optional): 'NEST' (default) or 'RING'
% Output : - Vector of indexes of the pixels in the cone
%          - Vectors of corresponding RA and Dec (optional)
% Author : A.M. Krassilchtchikov (2026 Mar)
% Example: ind = celestial.healpix.mex.coneSearch(1024,1.,1.,0.1);
%          [ind, pixlon, pixlat] = celestial.healpix.mex.coneSearch(1024,1.,1.,0.1);
%          ind = celestial.healpix.mex.coneSearch(1024,1.,1.,0.1,'exclusive');       % centers-only
%          ind = celestial.healpix.mex.coneSearch(1024,1.,1.,0.1,'inclusive','RING'); % RING scheme
%          tic;for i=1:1000;i=celestial.healpix.mex.coneSearch(256,10.+i/1e3,10.-i/1e3,0.5);end;toc
