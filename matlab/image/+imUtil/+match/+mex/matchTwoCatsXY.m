% Match two catalogs of [X, Y] planar coordinates using a radius search
%   See also: 
% Description: Match sources in catalog 2 against catalog 1 on the plane.
%              Catalog 1 is assumed to be sorted by Y, so a binary search
%              on Y is used in order to identify candidate matches.
%              The function returns the nearest match in catalog 1 for each
%              source in catalog 2, the distance to the nearest match, the
%              number of matches within the search radius, and optionally
%              the list of all matches and their distances.
%              The function is designed to be very fast, supports single
%              and double precision coordinates, and skips NaN/Inf
%              coordinates in the input catalogs.
% Input  : - Vector of X of catalog 1. Catalog 1 must be sorted by Y1.
%          - Vector of Y of catalog 1.
%          - Vector of X of catalog 2.
%          - Vector of Y of catalog 2.
%          - Search radius. Scalar. 
%          - A logical that is ignored (to be consistent with the input of
%            imUtil.match.mex.matchTwoCats
%          - A logical indicating if to check that catalog 1 is sorted by
%            Dec after coordinate normalization.
%            Default is false.
%          - A logical indicating if to sort the IndAll output by distance
%            for each source in catalog 2.
%            If false, then the nearest match is placed first and the rest
%            remain in scan order.
%            Default is false.
% Output : - Vector of indices in catalog 1 of the nearest match to each
%            source in catalog 2. The output length is equal to the length
%            of catalog 2. If no match is found, then NaN is returned.
%          - Vector of angular distances to the nearest match. Same length
%            as catalog 2. Units follow IsUnitsDeg. If no match is found,
%            then NaN is returned.
%          - Vector containing the number of matches within the search
%            radius for each source in catalog 2.
%          - A structure array with the same length as catalog 2 and with
%            the fields:
%            'Ind'  - Indices in catalog 1 of all matches within the
%                     search radius.
%            'Dist' - Distances of all matches. Units follow IsUnitsDeg.
%            This output is calculated only if requested.
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Compilation: mex -O CXXFLAGS='$CXXFLAGS -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' matchTwoCatsXY.cpp
% Example: 
%{
            RA1  = rand(10000,1).*2.*pi;
            Dec1 = sort((rand(10000,1)-0.5).*pi);
            RA2  = rand(100,1).*2.*pi;
            Dec2 = (rand(100,1)-0.5).*pi;
            [IndNearest2to1, DistNearest, Nmatch] = ...
                      imUtil.match.mex.matchTwoCatsXY(RA1, Dec1, RA2, Dec2, 1e-3);
            [IndNearest2to1, DistNearest, Nmatch, IndAll] = ...
                      imUtil.match.mex.matchTwoCatsXY(RA1, Dec1, RA2, Dec2, 1e-3, false, false, true);
%}