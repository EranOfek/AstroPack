% Fast mex for test intersection of convex spherical polygons.
%   The function test if one polygon (in set 1) intersects all the polygons
%   in set 2. If set 1 and set 2 has the same number of polygons, then test
%   polygons one to one.
%   The polygons must be convex and the function doesn't verify this.
% Description:
%       See also and see: celestial.polygon.isSpherePolyIntersect
% Input  : - Matrix of longitudes for polygon set 1. Size is [Nvert1,Npoly1].
%            Each column contains one polygon.
%          - Matrix of latitudes  for polygon set 1. Size is [Nvert1,Npoly1].
%          - Matrix of longitudes for polygon set 2. Size is [Nvert2,Npoly2].
%            Each column contains one polygon.
%          - Matrix of latitudes  for polygon set 2. Size is [Nvert2,Npoly2].
%          * ...,key,val,... 
%            'IsDeg' - Logical scalar indicating if inputs are in degrees.
%                  Default is true.
%            'IncludeEdge' - Logical scalar indicating if touching at an edge or
%                        a vertex counts as intersection.
%                        Default is true.
%
% Output : Logical column vector of length Ncmp, where:
%          Ncmp = max(Npoly1,Npoly2), provided one of them is 1 or both are equal.
%
% Author : ChatGPT + Eran Ofek (Mar 2026)
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -march=native -ffast-math' isSpherePolyIntersect_mex.cpp
% Example:
%    LonPoly1=[1;1;0;0]; LatPoly1=[1;0;1;0]; LonPoly2=rand(4,100); LatPoly2=rand(4,100);           
%    Flag = celestial.polygon.mex.isSpherePolyIntersect_mex(LonPoly1, LatPoly1, LonPoly2, LatPoly2);