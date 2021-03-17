function [Density,Area]=surface_density(Cat,HalfSize)
% Estimate surface density in catalog, optionally using convex hull
% Package: +imUtil.cat
% Description: Estimate the surface density and area that a catalog is
%              covering. The area is estimated using either the box size,
%              circle radius, and if not provided using the convex hull.
% Input  : - A catalog (row per source). If convex hull is used then the
%            first two columns of the catalog are [Long, Lat].
%          - Either radius or [HalfWidth, HalfHeight] that the catalog
%            covers. If not provided, then will use the convex hull
%            algorithm to estimate he area.
% Output : - Surface density of sources in catalog.
%          - Area that is being covered by the catalog. The arae unist are
%            the radius/HalfSize area units or the units of the Long/Lat
%            coordinates.
%      By: Eran O. Ofek                         Apr 2020
% Example: [Density,Area]=imUtil.cat.surface_density(rand(1000,2))

if nargin<2
    HalfSize = [];
end

if isempty(HalfSize)
    % estimate area using convexhull
    
    [~,Area] = convhull(Cat(:,1:2));    
    
else
    % area is provided
    if numel(HalfSize)==2
        % box
        Area = prod(2.*HalfSize);
    elseif numel(HalfSize)==1
        % circ
        Area = pi.*Size.^2;
    else
        error('Unknown HalfSize format - must contain 1 or 2 elements');
    end
    
end

N = size(Cat,1);
Density = N./Area;
