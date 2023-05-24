function [MatchedObj, UnMatchedObj, TruelyUnMatchedObj] = matchOld(Obj1, Obj2, Args)
    % Match two catalogs in AstroCatalog objects
    %       This function returens: a matched source catalog, and an
    %       unmatched source catalog.
    %       The matched catalog result has the same number of
    %       sources as in the Obj2 catalog, and for each Obj2 source,
    %       the nearest source in Obj1 is listed. If there is no
    %       source within the search radius, then the entire line
    %       contains NaNs.
    %       The sources in Obj1 that doesn't have counterparts in
    %       Obj2 are listed in the unmatched catalog.
    %       The angular distance and index of the source in Obj1 may be
    %       added to the matched catalog.
    %       Also return a catalog of TruelyUnMatchedObj.
    %       This exclude not only the nearest source within the
    %       search radius, but all the sources in Obj1 which have
    %       counterparts within the search radius.
    % Input  : - An AstroCatalog/AstroImage object.
    %            If multiple elements then each element will be
    %            matched against the corresponding element (or a
    %            single element) in the second object. 
    %            If this object is not sorted, then the object will be
    %            sorted (and modified).
    %          - A second AstroCatalog object - The function will
    %            attempt to match every source in this catalog with
    %            objects in the first input catalog.
    %          * ..., key, val,..
    %            'Radius'  - Search radius. Default is 5.
    %            'RadiusUnits' - Search radius units (if spherical
    %                   coordinates search). Default is 'arcsec'.
    %            'AddIndInRef' - A logical indicating if to add a
    %                   column to Obj1 that include the index of
    %                   the source in the reference catalog (Obj2).
    %                   Default is true.
    %            'IndInRefColName' - The column name of the Index
    %                   in reference. Default is 'IndInRef'.
    %            'IndInRefColPos' - Position of the IndInRef column
    %                   name to add to Obj1. Default is Inf (i.e.,
    %                   last column).
    %            'DeleteExistIndInRef' - A logical indicating if to
    %                   delete existing IndRefColName, before
    %                   inserting the new column.
    %                   Default is true.
    %            'AddDistCol' - Add column distance to outout
    %                   catalog. Default is true.
    %            'DistUnits' - Distance units. Default is 'arcsec'.
    %            'DistColName' - Distance column name.
    %                   Default is 'Dist'.
    %            'DistColPos' - Position of Distance column in
    %                   output catalog. Default is Inf (i.e., last
    %                   column).
    %            'CooType' - CooType (i.e., 'pix','sphere').
    %                   If empty, will use what is available in the catalog
    %                   with preference for 'sphere'. Default is empty.
    %            'ColCatX' - If CooType is not empty, this is the column
    %                   names/index from which to select the catalog X
    %                   coordinate. Default is [].
    %            'ColCatY' - Like 'ColCatX', but for the Y coordinate.
    %            'ColRefX' - Like 'ColCatX', but for te ref catalog.
    %            'ColRefY' - Like 'ColRefX', but for the Y coordinate.
    %            'CreateNewObj' - Create a new ouput object.
    %                   Default is true.
    % Output : - An AstroCatalog object of matched sources.
    %            Numeber of sources equal to that of the number
    %            of sources in the second object (Obj2).
    %            Data is taken from Obj1.
    %            Entire line is NaN if no source found.
    %          - An AstroCatalog object of unmatched sources.
    %            Include all the sources in the first object that
    %            are not matched.
    %          - An AstroCatalog object of truely unmatched
    %            sources. 
    %            Include all the sources in the first object that
    %            are not found in the search radius.
    % Author : Eran Ofek (Apr 2021)
    % Example : AC = AstroCatalog;
    %           AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
    %           AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
    %           AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]
    %           AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    %           [MC,UM,TUM] = imProc.match.matchOld(AC,AC2,'Radius',0.01,'RadiusUnits','rad')

    arguments
        Obj1
        Obj2
        Args.Radius                      = 5;
        Args.RadiusUnits                 = 'arcsec';
        Args.AddIndInRef(1,1) logical    = true;
        Args.IndInRefColName char        = 'IndInRef';
        Args.IndInRefColPos              = Inf;
        Args.DeleteExistIndInRef         = true;
        Args.AddDistCol(1,1) logical     = true;
        Args.DistUnits                   = 'arcsec';
        Args.DistColName char            = 'Dist';
        Args.DistColPos                  = Inf;
        Args.AddNmatchCol(1,1) logical   = true;
        Args.NmatchColPos                = Inf;
        Args.NmatchColName char          = 'Nmatch';
        
        Args.CreateNewObj logical        = true;
        
        % if given will override ColX/ColY
        Args.CooType                     = '';  % pix' | 'sphere'
        Args.ColCatX                     = [];
        Args.ColCatY                     = [];
        Args.ColRefX                     = [];
        Args.ColRefY                     = [];
        
        
    end    
    
    % convert AstroImage to AstroCatalog: Obj1
    if isa(Obj1,'AstroImage')
        Obj1 = astroImage2AstroCatalog(Obj1,'CreateNewObj',Args.CreateNewObj);
    elseif isa(Obj1,'AstroCatalog')
        % do nothing
    elseif isnumeric(Obj1)
        error('Input Obj1 is of unsupported class');
    else
        error('Input Obj1 is of unsupported class');
    end

     % convert AstroImage to AstroCatalog: Obj2
    if isa(Obj2,'AstroImage')
        Obj2 = astroImage2AstroCatalog(Obj2,'CreateNewObj',Args.CreateNewObj);
    elseif isa(Obj2,'AstroCatalog')
        % do nothing
    elseif isnumeric(Obj2)
        error('Input Obj2 is of unsupported class');
    else
        error('Input Obj2 is of unsupported class');
    end

    Nobj1 = numel(Obj1);
    Nobj2 = numel(Obj2);
    Nmax  = max(Nobj1, Nobj2);
    
    % select CooType
    if isempty(Args.CooType)
        % attempt to select automatically
        [~, ~, CommonCooType] = getCommonCooType(Obj1, Obj2);
    else
        [CommonCooType{1:Nmax}] = deal(Args.CooType);
    end

    MatchedObj         = AstroCatalog([Nmax,1]);
    UnMatchedObj       = AstroCatalog([Nmax,1]);
    TruelyUnMatchedObj = AstroCatalog([Nmax,1]);
    for Imax=1:1:Nmax
        Iobj1 = min(Imax, Nobj1);
        Iobj2 = min(Imax, Nobj2);

        if isempty(CommonCooType{Imax})
            error('CooType is not consistent while matching: Iobj1=%d, Iobj2=%d',Iobj1,Iobj2);
        end
        
        % Match Obj1(Iobj1) against Obj2(Iobj2)
        if ~Obj1(Iobj1).IsSorted
            [~, ColY] = getColCooForCooType(Obj1(Iobj1), CommonCooType{Imax});
            Obj1(Iobj1).sortrows(ColY);
        end
       
        switch lower(CommonCooType{Imax})
            case 'sphere'
                DistFun = @celestial.coo.sphere_dist_fast;
                Coo1    = getLonLat(Obj1(Iobj1), 'rad');
                Coo2    = getLonLat(Obj2(Iobj2), 'rad');

                RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
                ConvertDist = true;
            case 'pix'
                DistFun = @tools.math.geometry.plane_dist;
                Coo1    = getXY(Obj1(Iobj1));
                Coo2    = getXY(Obj2(Iobj2));

                RadiusRad = Args.Radius;
                ConvertDist = false;
            otherwise
                error('Unknown CooType option');
        end   
        % match

        [IndTable, CatFlagNearest, CatFlagAll, IndInRef] = VO.search.search_sortedlat_multiNearest(Coo1,...
                                                    Coo2(:,1), Coo2(:,2), RadiusRad, DistFun);

        % Columns of IndTable:
        % 1. Index of nearest source, within search radius, in Cat;
        % 2. Distance;
        % 3. Total number of matches within radius.

        % add a column to the Cat (Obj1) with the index of the
        % source in the Ref (Obj2)
        if Args.AddIndInRef
            if isColumn(Obj1(Iobj1), Args.IndInRefColName) && Args.DeleteExistIndInRef
                % the IndInRef column already exist
                deleteCol(Obj1(Iobj1), Args.IndInRefColName);
            end

            insertCol(Obj1(Iobj1), IndInRef, Args.IndInRefColPos , Args.IndInRefColName, {''});
        end

        if nargout>0
            % select nearest from each Ind
            %Result1(Imax).Catalog = Obj1(Iobj1).Catalog ...
            FlagNN = ~isnan(IndTable(:,1));
            [Nrow2,Ncol2]   = size(Obj2(Iobj2).Catalog);
            [Nrow1,Ncol1]   = size(Obj1(Iobj1).Catalog);
            MatchedObj(Imax).Catalog = nan(Nrow2,Ncol1);
            MatchedObj(Imax).Catalog(FlagNN,:) = Obj1(Iobj1).Catalog(IndTable(FlagNN,1),:);

            % copy the common properties from Obj2
            Obj1(Iobj1).copyProp(MatchedObj(Imax), {'ColNames','ColUnits','ColDesc','SortByCol','IsSorted'});

            % add Dist column:
            if Args.AddDistCol
                if ConvertDist
                    Dist = convert.angular('rad', Args.DistUnits, IndTable(:,2));
                    DistUnits = Args.DistUnits;
                else
                    Dist = IndTable(:,2);
                    DistUnits = '';
                end
                insertCol(MatchedObj(Imax), Dist, Args.DistColPos , Args.DistColName, {DistUnits});
            end

            if Args.AddNmatchCol
                insertCol(MatchedObj(Imax), IndTable(:,3), Args.NmatchColPos , Args.NmatchColName, {''});
            end

            % UnMatchedObj
            if nargout>1
                UnMatchedObj(Imax).Catalog = Obj1(Iobj1).Catalog(~CatFlagNearest,:);
                UnMatchedObj(Imax) = copyProp(Obj1(Iobj2), UnMatchedObj(Imax), {'ColNames','ColUnits','ColDesc','SortByCol','IsSorted'});

                if nargout>2
                    TruelyUnMatchedObj(Imax).Catalog = Obj1(Iobj1).Catalog(~CatFlagAll,:);
                    TruelyUnMatchedObj(Imax) = copyProp(Obj1(Iobj2), TruelyUnMatchedObj(Imax), {'ColNames','ColUnits','ColDesc','SortByCol','IsSorted'});
                end
            end
        end
    end

end
