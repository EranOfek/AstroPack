% Rasterize a sky polygon into healpix for a given resolution 
% Compiled with: mex rasterize_polygon.cpp -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/Healpix_cxx \
%                    -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/cxxsupport \
%                    -L/home/kra/ExternalLib/Healpix_3.83/lib /home/kra/ExternalLib/Healpix_3.83/lib/libhealpix_cxx.a \
%                    -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
% Important: the Healpix library must be compiled with the -fPIX option
% Input  : - A sky polygon as a 2 x N matrix of RA, Dec
%          - A resolution value: either the Healpix Nside (default) or an
%            angular resolution in arcsec, depending on the mode argument below
%          - Mode (optional, def. 'Nside'): 'Nside' interprets the second
%            argument as the Healpix Nside (must be a positive power of 2);
%            'arcsec' interprets it as an angular resolution in arcsec, from
%            which the matching Nside is computed
%          - Healpix scheme (optional, def. NESTED): 'NEST' or 'RING'
% Output : - vector of indexes of the raster pixels of resolution Nside
%          - the value of Nside used (echoed when given, or computed from the
%            input resolution in arcsec)
% Author : A.M. Krassilchtchikov (2026 Mar)
% Example: P0 = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
%          % second argument as Nside (default):
%          [Ind, Nside] = celestial.healpix.mex.rasterize_polygon(P0, 2^16);
%          % second argument as resolution in arcsec:
%          [Ind, Nside] = celestial.healpix.mex.rasterize_polygon(P0, 3, 'arcsec');
%          log(double(Nside))/log(2)
%          numel(Ind)