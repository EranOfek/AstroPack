function [Flag, Obj] = flagSrcWithNeighbors(Obj, Args)
    % Flag sources in AstroCatalog which have neighbors within a radius
    %   Optionaly, remove sources with neighboors.
    % Input  : - A multi-element AstroCatalog object.
    %            The object must be sorted, by 'Y'.
    %          * ...,key,val,...
    %            'CooType' - ['pix'] | 'spere'.
    %                   'pix' will work on cartesian coordinates, while 'sphere',
    %                   on spherical coordinates.
    %            'Radius' - Search radius. Default is 10.
    %            'RadiusUnits' - Search radius units (for
    %                   'CooType'='sphere'). Default is 'arcsec'.
    %            'ColNamesX' - A cell array of dictionary names for the X
    %                   coordinates (first to appear will be selected).
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColNamesY' - A cell array of dictionary names for the Y
    %                   coordinates (first to appear will be selected).
    %                   Default is AstroCatalog.DefNamesY.
    %            'ColNamesRA' - A cell array of dictionary names for the RA
    %                   coordinates (first to appear will be selected).
    %                   Default is AstroCatalog.DefNamesRA.
    %            'ColNamesDec' - A cell array of dictionary names for the
    %                   Dec coordinates (first to appear will be selected).
    %                   Default is AstroCatalog.DefNamesDec.
    % Outout : - A vector of logical indicating sources with neighboors
    %            within search radius. If multi-element AstroCatalog, then
    %            only the vector corresponding to last object is returned.
    %          - Modified object (original copy is modified!).
    %            The AstroCatalog object after removing sources with
    %            neighboors. If nargout<2 then the original object is not
    %            modified.
    % Author : Eran Ofek (Jul 2021)
    % Example: AC = AstroCatalog({rand(100,2).*1024},'ColNames',{'X','Y'});
    %          Flag = imProc.match.flagSrcWithNeighbors(AC)
    
    arguments
        Obj AstroCatalog
        Args.CooType               = 'pix';
        Args.Radius                = 10;
        Args.RadiusUnits           = 'arcsec';
        
        Args.ColNamesX             = AstroCatalog.DefNamesX;
        Args.ColNamesY             = AstroCatalog.DefNamesY;
        Args.ColNamesRA            = AstroCatalog.DefNamesRA;
        Args.ColNamesDec           = AstroCatalog.DefNamesDec;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
    
        switch lower(Args.CooType)
            case 'sphere'
                DistFun     = @celestial.coo.sphere_dist_fast;
                DistFunArgs = {};
                [ColInd1] = colnameDict2ind(Obj(Iobj), Args.ColNamesRA);
                [ColInd2] = colnameDict2ind(Obj(Iobj), Args.ColNamesDec);
                if ~Obj(Iobj).IsSorted
                    % sort by Y/Dec
                    error('Obj must be sorted by Dec');
                    Obj(Iobj).sortrows(ColInd2);
                end
                Coo     = getLonLat(Obj(Iobj), 'rad');

                RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
            case 'pix'
                DistFun     = @tools.math.geometry.plane_dist;
                DistFunArgs = {};
                [ColInd1] = colnameDict2ind(Obj(Iobj), Args.ColNamesX);
                [ColInd2] = colnameDict2ind(Obj(Iobj), Args.ColNamesY);
                if ~Obj(Iobj).IsSorted
                    % sort by Y/Dec
                    error('Obj must be sorted by Y');
                    Obj(Iobj).sortrows(ColInd2);
                end
                Coo    = getXY(Obj(Iobj));

                RadiusRad = Args.Radius;
            otherwise
                error('Unknown CooType option');
        end   

        % perform the search
        if ~issorted(Coo(:,2))
            error('Coo is not sorted');
            
            [~,IndSort] = sort(Coo(:,2));
            Coo         = Coo(IndSort,:);
        end
        % Only "has this source a neighbour within the radius" is needed here,
        % while VO.search.search_sortedlat_multi calls DistFun once per source
        % and builds a full Ind/Nmatch/Dist/Ind1 struct array. That is 4.2e6
        % DistFun calls in one visit reduction, dominated by call overhead
        % (issue #1257, observation 4). The local pass below searches the same
        % declination bands in batches and returns the same flags.
        Flag = localFlagNeighbors(Coo, RadiusRad, DistFun, DistFunArgs);

        if nargout>1
            % select sources
            Obj(Iobj).Catalog = Obj(Iobj).Catalog(~Flag,:);
        end
    end
end

function Flag = localFlagNeighbors(Coo, Radius, DistFun, DistFunArgs)
    % Flag sources having at least one other source within Radius.
    %   Batched equivalent of
    %     Ind  = VO.search.search_sortedlat_multi(Coo, Coo(:,1), Coo(:,2), Radius, [], DistFun);
    %     Flag = [Ind.Nmatch]>1;
    %   using the same declination bands, so the result is identical.
    % Input  : - A two column [Lon, Lat] matrix, sorted by the second column.
    %          - Search radius, in the units of Coo.
    %          - Distance function handle.
    %          - A cell array of additional arguments for the distance function.
    % Output : - A logical column, true for a source with a neighbour.
    % Author : Alexander Gioffe (Sep 2026)

    Nsrc = size(Coo,1);
    Flag = false(Nsrc,1);
    if Nsrc==0
        return;
    end
    Lon = Coo(:,1);
    Lat = Coo(:,2);

    % The batched pass trades per-source call overhead for a flattened pair
    % list, so it only pays while the declination bands are narrow. Measured
    % crossover (R2020b, random fields): with a mean band below ~200 candidates
    % it runs 1.1-2.0 times faster, above ~350 it is 1.4-3 times SLOWER than the
    % per-source loop, which never materialises the pair list. Estimate the mean
    % band from the declination span - cheap, and it avoids computing the bands
    % twice on the dense branch (issue #1257, observation 4).
    MaxMeanBand = 200;
    DecSpan     = Lat(end) - Lat(1);
    if DecSpan<=0
        MeanBandEst = Nsrc;
    else
        MeanBandEst = min(Nsrc, Nsrc.*2.*Radius./DecSpan);
    end
    if MeanBandEst>MaxMeanBand
        % dense field: keep the original per-source path
        Ind  = VO.search.search_sortedlat_multi(Coo, Lon, Lat, Radius, [], DistFun, 'DistFunArgs',DistFunArgs);
        Flag = ([Ind.Nmatch]>1).';
        Flag = Flag(:);
        return;
    end

    % the declination band of each source, exactly as search_sortedlat_multi
    % derives it (the +1 on the upper index is how mfind_bin reports)
    Ilat     = [(1:1:Nsrc).', (1:1:Nsrc).'+Nsrc];
    Inear    = tools.find.mfind_bin(Lat, [Lat(:).'-Radius, Lat(:).'+Radius]);
    Ilowhigh = double(Inear(Ilat));
    Ilow     = max(1,    Ilowhigh(:,1));
    Ihigh    = min(Nsrc, Ilowhigh(:,2)+1);

    Counts = Ihigh - Ilow + 1;
    Counts(Counts<0) = 0;

    % Work in chunks so that the flattened pair list stays bounded: a dense
    % field with a wide radius can otherwise ask for a very large index vector.
    % Kept modest on purpose - this runs inside parfor workers, where large
    % transient allocations cost more than the batching saves
    MaxPairs = 1e6;
    Istart   = 1;
    while Istart<=Nsrc
        CumCount = cumsum(Counts(Istart:end));
        Nchunk   = find(CumCount<=MaxPairs, 1, 'last');
        if isempty(Nchunk)
            Nchunk = 1;    % a single source already exceeds the budget
        end
        Iend  = Istart + Nchunk - 1;
        Isrc  = (Istart:1:Iend).';
        Cnt   = Counts(Isrc);
        Npair = sum(Cnt);

        if Npair>0
            % flatten the bands: Irep repeats each source index, Jidx runs
            % over that source's band
            Irep = repelem(Isrc, Cnt);
            Offs = repelem(cumsum([0; Cnt(1:end-1)]), Cnt);
            Jidx = (1:1:Npair).' - Offs + repelem(Ilow(Isrc), Cnt) - 1;

            Dist   = DistFun(Lon(Irep), Lat(Irep), Lon(Jidx), Lat(Jidx), DistFunArgs{:});
            Nmatch = accumarray(Irep - Istart + 1, Dist<=Radius, [numel(Isrc), 1]);
            Flag(Isrc) = Nmatch>1;
        end

        Istart = Iend + 1;
    end
end
