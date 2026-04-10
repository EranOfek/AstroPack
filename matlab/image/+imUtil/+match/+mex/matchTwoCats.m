% Match two catalogs of [RA, Dec] spherical coordinates using a radius search
% Description: Match sources in catalog 2 against catalog 1 on the sphere.
%              Catalog 1 is assumed to be sorted by Dec, so a binary search
%              on Dec is used in order to identify candidate matches.
%              The function returns the nearest match in catalog 1 for each
%              source in catalog 2, the distance to the nearest match, the
%              number of matches within the search radius, and optionally
%              the list of all matches and their distances.
%              The function is designed to be very fast, supports single
%              and double precision coordinates, handles RA wrap-around at
%              0/360 deg (or 0/2pi), corrects Dec values outside the legal
%              range by reflecting across the poles, and skips NaN/Inf
%              coordinates in the input catalogs.
% Input  : - Vector of RA of catalog 1. Catalog 1 must be sorted by Dec1.
%            Units are radians by default, or degrees if IsUnitsDeg=true.
%          - Vector of Dec of catalog 1.
%          - Vector of RA of catalog 2.
%          - Vector of Dec of catalog 2.
%          - Search radius. Scalar. Units are radians by default, or
%            degrees if IsUnitsDeg=true.
%          - A logical indicating if coordinates/radius units are degrees.
%            false/0 means radians, true/1 means degrees.
%            Default is false.
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
% Compilation: mex -O CXXFLAGS='$CXXFLAGS -fopenmp -std=c++17' LDFLAGS='$LDFLAGS -fopenmp' matchTwoCats.cpp
% Example: 
%{
            RA1  = rand(10000,1).*2.*pi;
            Dec1 = sort((rand(10000,1)-0.5).*pi);
            RA2  = rand(100,1).*2.*pi;
            Dec2 = (rand(100,1)-0.5).*pi;
            [IndNearest2to1, DistNearest, Nmatch] = ...
                      imUtil.match.mex.matchTwoCats(RA1, Dec1, RA2, Dec2, 1e-3);
            [IndNearest2to1, DistNearest, Nmatch, IndAll] = ...
                      imUtil.match.mex.matchTwoCats(RA1, Dec1, RA2, Dec2, 1e-3, false, false, true);
%}