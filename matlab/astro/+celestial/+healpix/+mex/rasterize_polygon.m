% Rasterize a sky polygon into healpix for a given resolution 
% Compiled with: mex rasterize_polygon.cpp -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/Healpix_cxx \
%                    -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/cxxsupport \
%                    -L/home/kra/ExternalLib/Healpix_3.83/lib /home/kra/ExternalLib/Healpix_3.83/lib/libhealpix_cxx.a \
%                    -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
% Important: the Healpix library must be compiled with the -fPIX option
% Input  : - a sky polygin as a 2 x N matrix of RA, Dec
%          - resolution (arcsec)
%          - healpix scheme (optional, def. NESTED)
% Output : - vector of indexes of the raster pixels of resolution Nside
%          - the value of Nside calculated from the input resolution in arcsec
% Author : A.M. Krassilchtchikov (2026 Mar) 
% Example: P0 = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70]; 
%          [Ind, Nside] = celestial.healpix.mex.rasterize_polygon(P0, 3);
%          log(double(Nside))/log(2)
%          numel(Ind)
