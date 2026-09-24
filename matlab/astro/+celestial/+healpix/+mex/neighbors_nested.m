% Find all the neighbors of healpix pixel (NESTED scheme only!)
% Compiled with: mex neighbors_nested.cpp -I/home/sasha/ExternalLib/Healpix_3.83/src/cxx/Healpix_cxx -I/home/sasha/ExternalLib/Healpix_3.83/src/cxx/cxxsupport -L/home/sasha/ExternalLib/Healpix_3.83/lib /home/sasha/ExternalLib/Healpix_3.83/lib/libhealpix_cxx.a -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
% Input  : - Nside (must be 2 ^ N)
%          - a healpix nested index (must be int64)
% Output : - a vector of indexes of the neighboring pixels 
% Author : A.M. Krassilchtchikov (2026 Feb) 
% Example: ne = celestial.healpix.mex.neighbors_nested(256,int64(20567)) 
