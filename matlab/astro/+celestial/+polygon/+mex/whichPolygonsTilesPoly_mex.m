% Determine a minimal subset of spherical convex polygons Poly2(:,I) whose union fully covers the spherical convex polygon Poly1.
%   See also non-mex version: celestial.polygon.whichPolygonsTilesPoly
%   The two functions are not identical and may return slightly different
%   results.
%   Algorithm:
%   1) Project the sphere to a plane using a gnomonic projection centered
%      inside Poly1. Great circles become straight lines.
%   2) Convert Poly1 and all Poly2 polygons to planar polyshape objects.
%   3) Intersect each Poly2 with Poly1.
%   4) Test whether the union of all intersections covers Poly1.
%   5) Search subsets in increasing size until a full cover is found.
%
%   Assumptions:
%   All relevant vertices must lie in the same open hemisphere as Poly1
%   with respect to the chosen projection center. This is the standard case
%   for convex spherical polygons smaller than a hemisphere.
%
% Input  : - (LonPoly1) A vector of Poly1 longitude verteces [radians].
%          - (LatPoly1) A vector of Poly1 latitude verteces [radians].
%          - A matrix of Poly2 longitude vertces (column per polygon).
%            [radians].
%          - A matrix of Poly2 latitude vertces (column per polygon).
%            [radians].
%            NaNs are allowed as row padding in each column
%
% Output : - (PolyFlag) logical row vector, length = size(LonPoly2,2)
%            true for polygons selected in one minimum-cardinality
%            exact cover of Poly1.
%            If full tiling is impossible, PolyFlag = [].
%
% Compilation: mex -O CXXFLAGS='$CXXFLAGS -O3 -DNDEBUG' whichPolygonsTilesPoly_mex.m
% Author : ChatGPT + Eran Ofek (Mar 2026)
% Example: [LonPoly1, LatPoly1] = localPlaneRectToSphere(-0.10, 0.10, -0.10, 0.10, CenterLon, CenterLat);
%          LonPoly2 = nan(numel(LonPoly1), 1); LatPoly2 = nan(numel(LatPoly1), 1);
%          LonPoly2(:,1) = LonPoly1;  LatPoly2(:,1) = LatPoly1;
%          LonPoly2 = LonPoly2 + [0.05, 0.05, -0.05, -0.05, 1, 1, 0.01];
%          LatPoly2 = LatPoly2 + [-0.05 0.05 -0.05 0.05, 1 1, 0.01];
%          tic; for i=1:1e2, PolyFlag = whichPolygonsTilesPoly_mex(LonPoly1, LatPoly1, LonPoly2, LatPoly2); end, T1=toc;