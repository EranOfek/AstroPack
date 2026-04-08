% (matchCatalogsXY) Fast mex for match two planar-coo catalogs using nearest-neighbor search.
%   See also: imUtil.match.mex.matchCatalogs (for spherical coordinates)
% Package: +imUtil/+match/+mex
% Description: Given two catalogs of planar coordinates, find the
%              closest match to each source in catalog 1 within a search
%              radius in catalog 2, and vice versa. The matching is
%              non-symmetric: the best match of source I1 in catalog 1 may
%              point to source I2 in catalog 2, while the best match of I2
%              may point to another source in catalog 1. For each source,
%              the function also returns the number of matches found within
%              the search radius.
% Input  : - Vector of X of catalog 1.
%          - Vector of Y of catalog 1.
%          - Vector of X of catalog 2 (sorted by Y)
%          - Vector of Y of catalog 2 (Sorted by Y).
%          - Search radius.
%   
%          - Units flag. Ignored. Kept here for consistency with
%            imUtil.match.mex.matchCatalogs
%
%          - Logical/numeric vector indicating which sources in catalog 1
%            to use. If empty or omitted, then all sources are used.
%            If empty, all true.
%            Default is all true.
%          - Logical/numeric vector indicating which sources in catalog 2
%            to use. If empty or omitted, then all sources are used.
%            If empty all true.
%            Default is all true.
%
%          - A logical indicating if to test that list 2 is sorted.
%            Default is false.
% Output : - Ind1: Vector of length N1. For each source I1 in catalog 1,
%            contains the index I2 of the closest match in catalog 2, or
%            NaN if no match was found.
%          - Dist1: Vector of length N1 containing the angular distance to
%            the closest match in catalog 2, or NaN if no match was found.
%          - Nmatch1: Vector of length N1 containing the number of matches
%            found within the search radius in catalog 2 for each source in
%            catalog 1.
%
%          - Ind2: Vector of length N2. For each source I2 in catalog 2,
%            contains the index I1 of the closest match in catalog 1, or
%            NaN if no match was found.
%          - Dist2: Vector of length N2 containing the angular distance to
%            the closest match in catalog 1, or NaN if no match was found.
%          - Nmatch2: Vector of length N2 containing the number of matches
%            found within the search radius in catalog 1 for each source in
%            catalog 2.
% Author  : ChatGPT + Eran Ofek (Apr 2026)
% 
% Comments: 1. Matching is non-symmetric.
%           2. Output indices refer to the original input vectors.
%           3. Sources with NaN coordinates are ignored.
%           4. Sources with Use1/Use2==false are ignored.
%           5. If Use1 or Use2 are empty, then all sources are used.
%           6. Distances are returned in the input angular units.
%           7. RA is normalized internally to the range [0,2*pi).
%           8. If List2Sorted is true, then catalog 2 must be sorted by Dec
%              in ascending order.
%           9. If List2Sorted is false, the code does not sort catalog 2
%              internally.
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -std=c++11 -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' matchCatalogsXY.cpp
%
% Example:
%{
        RA1  = [10; 20; 30];
        Dec1 = [ 0;  1;  2];
        RA2  = [10.1; 19.9; 40];
        Dec2 = [ 0.0;  1.1;  5];
        [Dec2, SI] = sort(Dec2);
        RA2 = RA2(SI);
        [Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, 0.5, true, [], [], true, true);
%}
