function [IndTable,CatFlagNearest,CatFlagAll,IndInRef]=search_sortedlat_multiNearest(Cat,Long,Lat,Radius,DistFun, Args)
% Search a single long/lat in a catalog sorted by latitude
% Package: VO.search
% Description: A low level function for a single cone search
%              in a [Long, Lat] array.
% Input  : - An array of [Long, Lat] in radians, sorted by Lat.
%            The program doesnot verify that the array is sorted.
%          - Longitude [radians] to search.
%          - Latitude [radians] to search.
%          - Radius [radians] to search.
%          - A function handle for calculating distances Fun(X1,Y1,X2,Y2).
%            Default is @celestial.coo.sphere_dist_fast.
%          * ...,key,val,...
%            'DistFunArgs' - A cell array of additional arguments to pass to the DistFun
%                   (after the 4th position).
%            'UseMex' - A logical indicating if to use the binarySearch mex
%                   program instead of tools.find.mfind_bin
%                   Default is true.
% Output : - A three column matrix with, one line per line in Long,Lat.
%            Columns are [Index of nearest source, within search radius, in
%            Cat;
%            Distance; Total number of matches within radius].
%          - A vector of logical (length as Cat), which indicate the object
%            in Cat that were identified as the nearest object to a source in
%            Long, Lat.
%          - The same as the previous output, but for all the sources
%            within the search radius.
%          - A vector of the indices of the sources in the ref image. NaN
%            if the source is not apperas in the ref image.
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
    DistFun function_handle      = @celestial.coo.sphere_dist_fast; %@celestial.coo.sphere_dist_fast_threshDist; %@celestial.coo.sphere_dist_fast;
    Args.DistFunArgs cell        = {}; %{4.8481e-5}; %{};
    Args.UseMex logical          = false;
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

% Inear(Ilat) is a two column matrix [low, high] index for each latitude
% search
if isempty(Inear)
    
    IndTable = nan(Nlat,3);
    IndTable(:,3) = 0;
    
    CatFlagNearest = [];
    CatFlagAll = [];
    IndInRef = [];
else

    Ilowhigh = double(Inear(Ilat));
    Ilow     = Ilowhigh(:,1);
    Ihigh    = min(Ncat,Ilowhigh(:,2)+1); % add 1 because of the way mfind_bin works
    
    IndTable = [nan(Nlat,2), zeros(Nlat,1)]; % [Index, Dist, Nmatch]
    %tools.struct.struct_def({'Ind','Nmatch','Dist'},Nlat,1);
    
    CatFlagNearest  = false(Ncat,1);
    CatFlagAll      = false(Ncat,1);
    
    
    for I=1:1:Nlat
        %Dist  = celestial.coo.sphere_dist_fast(Long(I),Lat(I), Cat(Ilow(I):Ihigh(I),Col.Lon), Cat(Ilow(I):Ihigh(I),Col.Lat));

        %DiffLong = (Long(I) - Cat(Ilow(I):Ihigh(I),Col.Lon).*cos(Lat(I)));
        %if any(DiffLong<Radius) || any(abs(DiffLong)>(2.*pi-Radius))
            Dist  = DistFun(Long(I),Lat(I), Cat(Ilow(I):Ihigh(I),Col.Lon), Cat(Ilow(I):Ihigh(I),Col.Lat), Args.DistFunArgs{:});
            FlagDist = Dist <= Radius;
        %else
        %    Dist  = nan(size(DiffLong));
        %    FlagDist = false(size(Dist));
        %end
        
        IndI  = Ilow(I)-1+find(FlagDist);
        DistI = Dist(FlagDist);
        if ~isempty(DistI)
            [MinDist, MinInd] = min(DistI);
            %IndTable(I,1) = IndI(MinInd);   % VERIFY THIS???
            %IndTable(I,2) = MinDist;
            %IndTable(I,3) = numel(IndI);
            IndTable(I,:) = [IndI(MinInd), MinDist, numel(IndI)];
            
            CatFlagNearest(IndTable(I,1)) = true;
            CatFlagAll(IndI)              = true;
            
        end
        
    end
    
    
    if nargout>3
        IndRef = (1:1:Nlat).';
        NewIndTable = IndTable(:,1);
    
        FlagNN = ~isnan(NewIndTable);
        IndRef = IndRef(FlagNN);
        NewIndTable = NewIndTable(FlagNN);
    
        IndInRef = nan(Ncat,1);
    
        IndInRef(NewIndTable) = IndRef;
    end
end