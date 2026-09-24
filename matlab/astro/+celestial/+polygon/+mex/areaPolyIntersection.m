% Mex for calculating the intersection area of convex spherical polygons.
%   See also: celestial.polygon.areaPolyIntersection
%   Calculate the intersection area between a single reference polygon
%   and one or more polygons on the celestial sphere. Polygon edges are
%   assumed to be minor great-circle arcs. The calculation is performed
%   using three-dimensional unit vectors and therefore correctly treats
%   longitude zero crossing and polygons containing a celestial pole.
%
%   The reference polygon is specified by vectors of longitude and
%   latitude. The polygons to test may be specified by vectors, in which
%   case a single polygon is assumed, or by matrices in which every
%   column contains the vertices of one polygon. NaN-padded polygon
%   columns are supported.
%
%   All polygons must be convex and should normally be contained within
%   a hemisphere. Consecutive vertices are connected using the shorter
%   great-circle arc. Consecutive antipodal vertices are not allowed.
%
% Input  : - Vector of longitudes of the reference polygon vertices.
%          - Vector of latitudes of the reference polygon vertices.
%          - Vector or matrix of polygon vertex longitudes. If this is a
%            matrix, every column represents one polygon.
%          - Vector or matrix of polygon vertex latitudes, with the same
%            size as the polygon-longitude input.
%          * ...,key,val,...
%            'CooUnits' - Coordinate units of all input coordinates:
%                   'rad' - Radians. Areas are returned in steradians.
%                   'deg' - Degrees. Areas are returned in square
%                           degrees.
%                   Default is 'rad'.
%            'TolInside' - Numerical tolerance used for spherical
%                   half-space inclusion tests. Default is 1e-12.
%            'TolParallel' - Tolerance for identifying parallel or
%                   coincident great circles. Default is 1e-14.
%            'TolDuplicate' - Euclidean chord-distance tolerance for
%                   identifying duplicate vertices on the unit sphere.
%                   Default is 1e-10.
% Output : - Row vector containing the intersection area between the
%            reference polygon and every polygon. The output is in
%            steradians for CooUnits='rad', and in square degrees for
%            CooUnits='deg'.
%          - Area of the reference polygon, in the same area units as
%            the first output.
% Compilation: mex -R2018a CXX=g++-9 CXXFLAGS="$CXXFLAGS -O3 -march=native" areaPolyIntersection.cpp
% Author : ChatGPT + Eran Ofek (2026 Jul)
% Example:
%{
    RefLon = [350; 10; 10; 350];
    RefLat = [-10; -10; 10; 10];
    Lon    = [355; 15; 15; 355];
    Lat    = [-5; -5; 15; 15];
    [Area, AreaRefPoly] = celestial.polygon.areaPolyIntersection(RefLon, RefLat, Lon, Lat, 'CooUnits','deg');
%}