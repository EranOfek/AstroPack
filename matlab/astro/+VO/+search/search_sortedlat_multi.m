function [Ind,FlagUnique,FlagFound]=search_sortedlat_multi(Cat,Long,Lat,Radius,FlagUnique,DistFun,Args)
% Search a single long/lat in a catalog sorted by latitude
% Package: VO.search
% Description: A low level function for a single cone search
%              in a [Long, Lat] array.
% Input  : - An array of [Long, Lat] in radians, sorted by Lat.
%            The program doesnot verify that the array is sorted.
%          - Longitude [radians] to search.
%          - Latitude [radians] to search.
%          - Radius [radians] to search.
%            If radius is negative then will add .Dist to the output index
%            structure.
%          - A vector of false with the length of the catalog,
%            after first iteration provide the output.
%          - A function handle for calculating distances Fun(X1,Y1,X2,Y2).
%            Default is @celestial.coo.sphere_dist_fast.
%          * ...,key,val,...
%            'UseMex' - A logical indicating if to use the binarySearch mex
%                   program instead of tools.find.mfind_bin
%                   Default is false.
% Output : - A strucure array in which the number of elements equal to the
%            number of searched coordinates (Lat), and with the following
%            fields:
%            'Ind' - Vector of indices of matched sources.
%            'Nmatch' - Number of matched sources.
%            'Dist' - Distance between sources. This is provided only if
%                   the input search radius is negative.
%            'Ind1' - If Dist is requested (negative search radius)
%                   then will also calculate Ind1 -
%                   containing the index for the nearest match only,
%                   and NaN if no matches.
%          - A logical vector of length equal to the number of searched
%            coordinates (Cat) which flag the first unique source.
%          - If Cat and Long/Lat have the same length and are the same
%            catalog, then this is a flag indicating if a source was
%            already found as another source.
%     By : Eran O. Ofek                    Feb 2017
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Cat=sortrows(rand(10000,2),2);
%          Ind=VO.search.search_sortedlat_multi(Cat,0.5,0.5,0.01)
% Reliable: 2

arguments
    Cat
    Long
    Lat
    Radius
    FlagUnique                   = [];
    DistFun function_handle      = @celestial.coo.sphere_dist_fast; % @celestial.coo.sphere_dist_fast_threshDist; %@celestial.coo.sphere_dist_fast;
    Args.DistFunArgs cell        = {};  %{4.8481e-5};   % 10 [arcsec] in radians
    Args.UseMex logical          = false;
end

if Radius<0
    CalcDist = true;
    Radius = abs(Radius);
else
    CalcDist = false;
end

if isempty(FlagUnique)
    FlagUnique = false(size(Cat,1),1);
end

Col.Lon = 1;
Col.Lat = 2;

% somewhat slower version:
% Ilow  = tools.find.bin_sear(Cat(:,Col.Lat),Lat-Radius);
% Ihigh = tools.find.bin_sear(Cat(:,Col.Lat),Lat+Radius);

Lat   = Lat(:).';  % convert Lat to a row vector
Nlat  = numel(Lat); % number of latitudes to search
Ilat  = [(1:1:Nlat).', (1:1:Nlat).'+Nlat];

Ncat  = size(Cat,1);
if Args.UseMex
    Inear = uint32(binarySearch(Cat(:,Col.Lat),[Lat-Radius, Lat+Radius]));
else
    Inear = tools.find.mfind_bin(Cat(:,Col.Lat),[Lat-Radius, Lat+Radius]);
end


% Inear(Ilat) is a two column matrix [low, high] index for each latitud
% search
Ilowhigh = double(Inear(Ilat));
Ilow     = Ilowhigh(:,1);
Ihigh    = min(Ncat,Ilowhigh(:,2)+1); % add 1 because of the way mfind_bin works

Ind = tools.struct.struct_def({'Ind','Nmatch','Dist','Ind1'},Nlat,1);

FlagFound    = false(Nlat,1);
%CopyOfUnique = false(Nlat,1);


for I=1:1:Nlat
    %Dist  = celestial.coo.sphere_dist_fast(Long(I),Lat(I), Cat(Ilow(I):Ihigh(I),Col.Lon), Cat(Ilow(I):Ihigh(I),Col.Lat));
    Dist  = DistFun(Long(I),Lat(I), Cat(Ilow(I):Ihigh(I),Col.Lon), Cat(Ilow(I):Ihigh(I),Col.Lat), Args.DistFunArgs{:});
    FlagDist = Dist <= Radius;
    Ind(I).Ind    = Ilow(I)-1+find(FlagDist);
    %Ind(I).Ind    = Ilow(I)-1+find(Dist <= Radius);
    Ind(I).Nmatch = numel(Ind(I).Ind);
    
    if CalcDist
        Ind(I).Dist   = Dist(FlagDist);
        
        switch numel(Ind(I).Ind)
            case 0
                Ind(I).Ind1 = NaN;
            case 1
                Ind(I).Ind1 = Ind(I).Ind;
            otherwise
                [~,MinI] = min(Ind(I).Dist);
                Ind(I).Ind1 = Ind(I).Ind(MinI);
        end
        
    end
    if ~any(FlagUnique(Ind(I).Ind))
        % a new unique source
        FlagUnique(Ind(I).Ind) = true;
        %FlagFound(I) = true;
    end
    
    if ~FlagFound(I)
        FlagFound(Ind(I).Ind) = true;
        FlagFound(I) = false;
    end
    
end