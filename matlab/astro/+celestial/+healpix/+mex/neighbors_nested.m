% Find all the neighbors of healpix pixel (NESTED scheme only!)
% Compiled with: mex neighbors_nested.cpp -I/home/sasha/Downloads/Healpix_3.83/src/cxx/Healpix_cxx -I/home/sasha/Downloads/Healpix_3.83/src/cxx/cxxsupport -L/home/sasha/Downloads/Healpix_3.83/lib /home/sasha/Downloads/Healpix_3.83/lib/libhealpix_cxx.a -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
% Input  : - N: a power of 2 to be used for Nside  = 2 ^ N
%          - a healpix nested index
% Output : - A vector of indexes of the neighboring pixels 
% Author : A.M. Krassilchtchikov (2026 Feb) 
% Example: ne = celestial.healpix.mex.neighbors_nested(8,20567) // NB: here 8 means Nside = 2^8
