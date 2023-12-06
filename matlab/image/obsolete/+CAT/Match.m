
classdef Match < Component
    properties
        Cat
        Ref
        Coo
        CatName
        
        CooUnits                    = 'deg';
        Radius                      = 3;
        RadiusUnits                 = 'arcsec';
        
        CatRadius                   = 3600;     % radius for catsHTM
        CatRadiusUnits              = 'arcsec';
        
        Shape                       = 'circle';
        Con                         = [];  % additional constraints for catsHTM
        catsHTMisRef(1,1) logical   = false;
        
        AddDistCol(1,1) logical     = true;
        DistUnits                   = 'arcsec';
        DistColName char            = 'Dist';
        DistColPos                  = Inf;
        AddNmatchCol(1,1) logical   = true;
        NmatchColPos                = Inf;
        NmatchColName               = 'Nmatch';
        
        Cols                        = {};
        CreateNewObj                = [];
    end
    
    
    
    methods
        function Obj = Match(Args)
            % Constructor for a Match object
            % Input  : * ...,key,val,...
            %            Can be any Match object property name followed by
            %            its new value.
            % Output : - A Match object
            % Author : Eran Ofek (Apr 2021)
            % Example: M = imProc.cat.Match('Radius',5,'RadiusUnits','arcsec');
           
            arguments
                Args.Cat              % catalaog
                Args.Ref              % ref catalog
                Args.Coo              % Search coordinates - used by coneSearch, match_catsHTM
                Args.CatName          % catsHTM name
                
                Args.CooUnits
                Args.Radius
                Args.RadiusUnits
                Args.Shape
                Args.AddDistCol
                Args.DistUnits
                Args.DistColName
                Args.DistColPos
                
                Args.OutIsSelfClass
                
                Args.CreateNewObj
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
    
    
    methods % search by coordinates/name
        function [Result, Flag, AllDist] = coneSearch(Obj, CatObj, Coo, Args)
            % cone search(s) on AstroCatalog/AstroImage object
            % Input  : - A Match object.
            %          - An AstroCatalog/AstroImage object. If multiple elements
            %            then will perform the cone search on any one of
            %            the elements.
            %          - A two column matrix of [RA, Dec] or [X, Y]
            %            to search.
            %            If more then one row, then the results of the
            %            search will be merged.
            %          * ...,key,val,...
            %            'Radius'  - Search radius. Default is 5.
            %            'RadiusUnits' - Search radius units (if spherical
            %                   coordinates search). Default is 'arcsec'.
            %            'Shape' - Shape search. Default is 'circle'.
            %            'CooUnits' - Units of (spherical) coordinates
            %                   (second input argument). Default is 'deg'.
            %            'AddDistCol' - Add column distance to outout
            %                   catalog. Default is true.
            %            'DistUnits' - Distance units. Default is 'arcsec'.
            %            'DistColName' - Distance column name.
            %                   Default is 'Dist'.
            %            'DistColPos' - Position of Distance column in
            %                   output catalog. Default is Inf (i.e., last
            %                   column).
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - An AstroCatalog object with the found sources.
            %          - A vector of logicals with the same length as the
            %            number of rows in the input object. True if object
            %            is in cone. If the input is an object with
            %            multiple elements, then this vector corresponds to
            %            the last element in the object array.
            %          - A vector of distances of the found objects from
            %            the cone search center. If the input is an object with
            %            multiple elements, then this vector corresponds to
            %            the last element in the object array.
            % Author : Eran Ofek (Apr 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          M = imProc.cat.Match;
            %          [NC, Flag, Dist] = M.coneSearch(AC,[1 1],'Radius',3600)
            %          [NC, Flag, Dist] = M.coneSearch(AC,[1 1; 0 0],'Radius',3600);  % search around two positions (merged results).
            
            arguments
                Obj(1,1)
                CatObj
                Coo
                Args.Radius                      = [];
                Args.RadiusUnits                 = [];
                Args.Shape char                  = '';
                Args.CooUnits char               = '';
                Args.AddDistCol                  = [];
                Args.DistUnits                   = [];
                Args.DistColName                 = [];
                Args.DistColPos                  = [];
                Args.CreateNewObj                = [];
            end
            
            if isempty(CatObj(1).CooType)
                CatObj.getCooTypeAuto;
            end
            
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(Obj, Args);
            if isempty(CatObj)
                CatObj = Obj.Coo;
            end
            if isempty(Coo)
                Coo = Obj.Coo;
            end
                
            % Convert Coo to radians
            CooRad = convert.angular(Args.CooUnits,'rad',Coo);
            
            if isempty(Args.CreateNewObj)
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
            end
            
            % convert AstroImage to AstroCatalog
            if isa(CatObj,'AstroImage')
                CatObj = astroImage2AstroCatalog(CatObj,'CreateNewObj',Args.CreateNewObj);
            elseif isa(CatObj,'AstroCatalog')
                % do nothing
            elseif isnumeric(CatObj)
                error('Input CatObj is of unsupported class');
            else
                error('Input CatObj is of unsupported class');
            end
            
            RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
            if Args.AddDistCol || nargout>2
                RadiusRad = -abs(RadiusRad);
            end
            
            if Args.CreateNewObj
                Result = CatObj.copy();
				Result.Catalog = [];
            else 
                Result = CatObj;
            end
            
            Nobj = numel(CatObj);
            for Iobj=1:1:Nobj
                if ~CatObj(Iobj).IsSorted
                    CatObj(Iobj).sortrows(CatObj(Iobj).ColY);
                end
                switch lower(CatObj(Iobj).CooType)
                    case 'sphere'
                        [Ind,Flag] = VO.search.search_sortedlat_multi(getCoo(CatObj(Iobj),'rad'),...
                                                                    CooRad(:,1), CooRad(:,2), RadiusRad, [],...
                                                                    @celestial.coo.sphere_dist_fast);
                    case 'pix'
                        [Ind,Flag] = VO.search.search_sortedlat_multi(getCoo(CatObj(Iobj),'rad'),...
                                                                    Coo(:,1), Coo(:,2), RadiusRad, [],...
                                                                    @tools.math.geometry.plane_dist);
                        
                    otherwise
                        error('Unknown CooType option');
                end
                        
                
                % what to do with the found objects
                Ncoo = numel(Ind);
                Out     = zeros(0, size(CatObj(Iobj).Catalog,2));
                AllDist = zeros(0,1);
                for Icoo=1:1:Ncoo
                    Out     = [Out; CatObj(Iobj).Catalog(Ind(Icoo).Ind,:)];
                    AllDist = [AllDist; Ind(Icoo).Dist]; 
                end
                AllDist  = convert.angular('rad', Args.DistUnits, AllDist);
                Result(Iobj).Catalog = Out;
                if Args.AddDistCol
                    Result(Iobj).insertCol(AllDist, Args.DistColPos, Args.DistColName);
                end
            end
        end
        
        function [Result, Flag] = inPolygon(MObj, CatObj, Coo, Args)
            % Return sources inside polygon
            % Input  : - A Match object.
            %          - An AstroCatalog/AstroImage object. If multiple elements
            %            then will perform the inPolygon search on any one of
            %            the elements.
            %          - A two column matrix of [Long, Lat] or [X,Y] that
            %            defines the verteces of the polygon.
            %          * ...,key,val,...
            %            'CooUnits' - Units of (spherical) coordinates
            %                   (second input argument). Default is 'deg'.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - An AstroCatalog object with the found sources.
            %          - A vector of logicals with the same length as the
            %            number of rows in the input object. True if object
            %            is in polygon. If the input is an object with
            %            multiple elements, then this vector corresponds to
            %            the last element in the object array.
            % Author : Eran Ofek (Apr 2021)
            % very similar to coneSearch
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          M = imProc.cat.Match;
            %          [InP, Flag] = M.inPolygon(AC,[1 1; 1.1 1.1; 0.5 0.1],'CooUnits','rad')
            
            arguments
                MObj(1,1)
                CatObj
                Coo
                
                Args.CooUnits char               = '';
                Args.CreateNewObj                = [];
            end
            
            if isempty(CatObj(1).CooType)
                CatObj.getCooTypeAuto;
            end
            
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(MObj, Args);
            if isempty(CatObj)
                CatObj = MObj.Coo;
            end
            if isempty(Coo)
                Coo = MObj.Coo;
            end
                
            % Convert Coo to radians
            CooRad = convert.angular(Args.CooUnits,'rad',Coo);
            
            if isempty(Args.CreateNewObj)
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
            end
            
            % convert AstroImage to AstroCatalog
            if isa(CatObj,'AstroImage')
                CatObj = astroImage2AstroCatalog(CatObj,'CreateNewObj',Args.CreateNewObj);
            elseif isa(CatObj,'AstroCatalog')
                % do nothing
            elseif isnumeric(CatObj)
                error('Input CatObj is of unsupported class');
            else
                error('Input CatObj is of unsupported class');
            end
            
            if Args.CreateNewObj
                Result = CatObj.copy();
				Result.Catalog = [];
            else 
                Result = CatObj;
            end
            
            Nobj = numel(CatObj);
            for Iobj=1:1:Nobj
                %if ~CatObj(Iobj).IsSorted
                %    CatObj(Iobj).sortrows(CatObj(Iobj).ColY);
                %end
                
                switch lower(CatObj(Iobj).CooType)
                    case 'sphere'
                        Flag = celestial.htm.in_polysphere(getCoo(CatObj(Iobj),'rad'),CooRad);
                    case 'pix'
                        Pos = getCoo(CatObj(Iobj),'rad');  % here 'rad' is ignored
                        Flag = inpolygon(Pos(:,1),Pos(:,2), Coo(:,1), Coo(:,2));
                    otherwise
                        error('Unknown CooType option');
                end
                Result(Iobj).Catalog = CatObj(Iobj).Catalog(Flag,:);
                
            end
            
            
        end
        
        
    end
    
    methods % match two AstroCatalog
        
        function [MatchedObj, UnMatchedObj, TruelyUnMatchedObj] = match(MObj, Obj1, Obj2, Args)
            % Match two catalogs in AstroCatalog objects
            %       This functin returens: a matche source catalog, and an
            %       unmatched source catalog.
            %       The matched catalog result has the same number of
            %       sources as in the Obj2 catalog, and for each Obj2 source,
            %       the nearest source in Obj1 is listed. If there is no
            %       source within the search radius, then the entire line
            %       contains NaNs.
            %       The sources in Obj1 that doesn't have counterparts in
            %       Obj2 are listed in the unmatched catalog.
            %       Also return a catalog of TruelyUnMatchedObj.
            %       This exclude not only the neaest source within the
            %       search radius, but all the sources in Obj1 which have
            %       counterparts within the search radius.
            % Input  : - An AstroCatalog/AstroImage object.
            %            If multiple elements then each element will be
            %            matched against the corresponding element (or a
            %            single element) in the second object.
            %          - A second AstroCatalog object - The function will
            %            attempt to match every source in this catalog with
            %            objects in the first input catalog.
            %          * ..., key, val,..
            %            'Radius'  - Search radius. Default is 5.
            %            'RadiusUnits' - Search radius units (if spherical
            %                   coordinates search). Default is 'arcsec'.
            %            'AddIndInRef' - A logical indicating if to add a
            %                   column to  Obj1 that include the index of
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
            %           AC.getCooTypeAuto
            %           AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]
            %           AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
            %           AC2.getCooTypeAuto
            %           M = imProc.cat.Match;
            %           [MC,UM,TUM] = M.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad')
            
            arguments
                MObj
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
            end
                    
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(MObj, Args);
            if isempty(Obj1)
                Obj1 = MObj.Cat;
            end
            if isempty(Obj2)
                Obj2 = MObj.Ref;
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
            MatchedObj         = AstroCatalog([Nmax,1]);
            UnMatchedObj       = AstroCatalog([Nmax,1]);
            TruelyUnMatchedObj = AstroCatalog([Nmax,1]);
            for Imax=1:1:Nmax
                Iobj1 = min(Imax, Nobj1);
                Iobj2 = min(Imax, Nobj2);
               
                % Match Obj1(Iobj1) against Obj2(Iobj2)
                if ~Obj1(Iobj1).IsSorted
                    Obj1(Iobj1).sortrows(Obj1(Iobj1).ColY);
                end
                if ~strcmp(Obj1(Iobj1).CooType, Obj2(Iobj2).CooType)
                    error('CooType is not consistent while matching: Iobj1=%d, Iobj2=%d',Iobj1,Iobj2);
                end
                switch lower(Obj1(Iobj1).CooType)
                    case 'sphere'
                        DistFun = @celestial.coo.sphere_dist_fast;
                        Coo1    = getCoo(Obj1(Iobj1), 'rad');
                        Coo2    = getCoo(Obj2(Iobj2), 'rad');
                        
                        RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
                        ConvertDist = true;
                    case 'pix'
                        DistFun = @tools.math.geometry.plane_dist;
                        Coo1    = getCoo(Obj1(Iobj1));
                        Coo2    = getCoo(Obj2(Iobj2));
                        
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
                    copyProp(Obj1(Iobj1), MatchedObj(Imax), {'CooType','ColX','ColY','ColNames','ColUnits','ColDesc','SortByCol','IsSorted'});

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
                        UnMatchedObj(Imax) = copyProp(Obj1(Iobj2), UnMatchedObj(Imax), {'CooType','ColX','ColY','ColNames','ColUnits','ColDesc','SortByCol','IsSorted'});

                        if nargout>2
                            TruelyUnMatchedObj(Imax).Catalog = Obj1(Iobj1).Catalog(~CatFlagAll,:);
                            TruelyUnMatchedObj(Imax) = copyProp(Obj1(Iobj2), TruelyUnMatchedObj(Imax), {'CooType','ColX','ColY','ColNames','ColUnits','ColDesc','SortByCol','IsSorted'});
                        end
                    end
                end
            end
            
        end
        
    end
    
    methods % match against external catalog
        function [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = match_catsHTM(MObj, Obj, CatName, Args)
            % Match an AstroCatalog object with catsHTM catalog
            % Input  : - A Match object.
            %          - An AstroCatalog or an AstroImage object (multi
            %            elements supported). The AStroCatalog object will
            %            be matched against a catsHTM catalog.
            %          - catsHTM catalog name (e.g., 'GAIADR2').
            %            See catsHTM.catalogs for possible options.
            %          * ...,key,val,...
            %            'Coo' - [RA, Dec] of coordinates to search.
            %                   If empty, then will attempt to find this
            %                   from the catalog itself. DEfault is [].
            %            'CooUnits' - Units of coordinates. Object default
            %                   is 'deg'.
            %            'Radius' - Matching radius. Default is 3.
            %            'RadiusUnits' - Matchin radius units.
            %                   Default is 'arcsec'.
            %            'CatRadius' - The search radius of the catsHTM
            %                   catalog. If not given this is taken as the
            %                   bounding circle radius of the inout
            %                   AStroCatalog. Default is [].
            %            'CatRadiusUnits' - CatRadius units.
            %                   Default is 'arcsec'.
            %            'Con' - A cell array of additional
            %                  constraints to apply to output catalog.
            %                  See catsHTM.cone_search for options.
            %                  E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}
            %            'catsHTMisRef' - A logical indicating if the
            %                   catsHTM catalog is treated as the reference
            %                   catalog. Default is false.
            %                   If true, then the number of sources of the matched
            %                   catalog is like the catsHTM object, while
            %                   false the size is like the input object.
            % Output : - A matched AstroCatalog object. See Match.match.
            %          - An unatched AstroCatalog object. See Match.match.
            %          - A truely unatched AstroCatalog object. See Match.match.
            %          - The catsHTM AstroCatalog object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          M = imProc.cat.Match;
            %          M.coneSearch(AC,[1 1],'Radius',3600);
            %          [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = M.match_catsHTM(AC,'GAIADR2')
            
            arguments
                MObj(1,1)
                Obj
                CatName char
                Args.Coo                 = [];
                Args.CooUnits            = [];
                Args.Radius              = [];
                Args.RadiusUnits         = [];
                Args.CatRadius           = [];
                Args.CatRadiusUnits      = [];
                Args.Con                 = [];
                Args.catsHTMisRef        = [];
            end
            
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(MObj, Args);
            if isempty(Obj)
                Obj = MObj.Cat;
            end
            if isempty(CatName)
                CatName = MObj.CatName;
            end
                
            % convert AstroImage to AstroCatalog
            if isa(Obj,'AstroImage')
                Obj = astroImage2AstroCatalog(Obj,'CreateNewObj',Args.CreateNewObj);
            elseif isa(Obj,'AstroCatalog')
                % do nothing
            elseif isnumeric(Obj)
                error('Input Obj is of unsupported class');
            else
                error('Input Obj is of unsupported class');
            end

            
            if isempty(Args.Coo) || isempty(Args.Radius)
                UseUserCoo = true;
            else
                UseUserCoo = false;
            end
            
            
            Nobj = numel(Obj);
            MatchedObj = AstroCatalog(size(Obj));

            CatH = AstroCatalog(size(Obj));  % output of catsHTM
            for Iobj=1:1:Nobj
                if isempty(Args.Coo) || isempty(Args.CatRadius)
                    % get coordinates using boundingCircle
                    [CircX, CircY, CircR] = Obj(Iobj).boundingCircle('OutUnits','rad');
                    Args.Coo                 = [CircX, CircY];
                    Args.CatRadius      = CircR;
                    Args.CooUnits       = 'rad';
                    Args.CatRadiusUnits = 'rad';
                else
                    Args.Coo = convert.angular(Args.CooUnits,'rad',Args.Coo);
                end
                Icoo = 1;
                CatH(Iobj)  = catsHTM.cone_search(CatName, Args.Coo(Icoo,1), Args.Coo(Icoo,2), Args.CatRadius, 'RadiusUnits',Args.CatRadiusUnits, 'Con',Args.Con, 'OutType','astrocatalog');
                CatH.getCooTypeAuto;
                % match sources
                if Args.catsHTMisRef
                    [MatchedObj, UnMatchedObj, TruelyUnMatched] = MObj.match(Obj, CatH, 'Radius',Args.Radius, 'RadiusUnits',Args.RadiusUnits);
                else
                    [MatchedObj, UnMatchedObj, TruelyUnMatched] = MObj.match(CatH, Obj, 'Radius',Args.Radius, 'RadiusUnits',Args.RadiusUnits);
                end
                
            end
        end
        
    end
    
    
    methods % pattern matching
        function matchPattern(MObj, Obj1, Obj2)
            %
        end
        
        
    end
    
    methods
        function [Res, Summary, N_Ep] = matched2matrix(MObj, MatchedObj, Cols, IsEpochByInd)
            %A matched AstroCatalog object into a matrix of epochs by index
            % AstCat object to a matrix of matched sources.
            % Description: Given an AstroCatalog object containing multiple elements, in
            %              which each element containing the same number of rows
            %              (e.g., the output of Match/match.m), return a matrix
            %              that contains, for selected columns, the requested column
            %              in all the AstroCatalog elements. E.g., the number of columns
            %              in the matrix is equal to the number of AstroCatalog elements
            %              (column per element) and the number of rows is equal to
            %              the number of rows in each AstroCatalog element.
            % Input  : - A Match object.
            %          - An AstroCatalog object containing multiple element, in
            %            which each element containing the same number of rows
            %            (e.g., the output of AstCat/match.m)
            %          - Column indices or column names (string or a cell array of
            %            strings) for which to prepare the array.
            %          - A logical indicating if the ouput matrix is
            %            Epoch by Ind (true), or Ind by Epoch (false).
            %            Default is true.
            % Output : - A structure in which each field name corresponds to a
            %            requested column name and contains the matrix of all the
            %            column entries in all the AStroCatalog object elements.
            %          - A structure array containing a summary.
            %            The following fields are available:
            %            .Nsrc - Number of sources (size of the 1st dimension of the
            %                    output matrix).
            %            .Nepoch - Number of epochs (size of the 2nd dimension of the
            %                    output matrix).
            %            .Nnn - number of not NaNs for each source in the first Column.
            %          - Vector of length equal to the number of epochs. The value in
            %            each element is the number of stars that appears in exactly
            %            I epochs.
            % License: GNU general public license version 3
            % Tested : Matlab R2015b
            %     By : Eran O. Ofek                    May 2016
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: 
            %          FFU : MODIFY!!! and TEST!!!
            
            %          M=match(Ref,AstC);
            %          Res=astcat2matched_array(M,{'XWIN_IMAGE','YWIN_IMAGE','MAG_APER'});
            %          % step by step application:
            %          S=images2sim('PTF_2015*.fits');
            %          S=gain_correct(S);
            %          S=background(S,'Block',[64 64]);
            %          S=mextractor(S);
            %          [~,I]=max(sizecat(S)); 
            %          Sref = S(I);
            %          [M,UM]=match(S,Sref);
            %          [Res,Summary]=astcat2matched_array(M,{'MAG_PSF'});
            %          II=find(Summary.Nnn==Summary.Nepoch); % find sources that appears in all epochs
            %          Res.MAG_PSF(:,II)
            % Reliable: 2
            %--------------------------------------------------------------------------

            arguments
                MObj(1,1)
                MatchedObj   % FFU:  what about AstroImage???
                Cols
                IsEpochByInd(1,1) logical           = true;
            end
             
            % use object default arguments if not supplied by user
            if isempty(MatchedObj)
                MatchedObj = MObj.Cat;
            end
            if isempty(Cols)
                Cols = MObj.Cols;
            end
            
            CatField   = 'Catalog';

            Ncat        = numel(MatchedObj);
            [CatRow,~]  = sizecat(MatchedObj);
            if all(CatRow(1)==CatRow)
                Nrow = CatRow(1);
            else
                error('Number of rows in all MatchedObj elemnts must be equal');
            end

            if char(Cols)
                Cols = {Cols};
            end

            % get column names and indices
            ColInd  = colname2ind(MatchedObj(1),Cols);
            ColName = ind2colname(MatchedObj(1),Cols);
            Ncol    = numel(ColInd);

            % Initiate Res
            for Icol=1:1:Ncol
                Res.(ColName{Icol}) = zeros(Nrow,Ncat);
            end

            if IsEpochByInd
                DimSrc   = 2;
                DimEpoch = 1;
            else
                DimSrc   = 1;
                DimEpoch = 2;
            end

            % For each selected column
            for Icol=1:1:Ncol
                % for each catalog
                for Icat=1:1:Ncat
                    % E.g., Res.XWIN_IMAGE(:,10) = MatchedObj(10).Cat(:,Column)
                    Res.(ColName{Icol})(:,Icat) = MatchedObj(Icat).(CatField)(:,ColInd(Icol));
                end
                if IsEpochByInd
                    % transpose the matrix
                    Res.(ColName{Icol}) = Res.(ColName{Icol}).';
                end
            end


            if (nargout>1)
                % calculate summary
                Icol = 1;
                Summary.Nsrc   = size(Res.(ColName{Icol}),DimSrc);
                Summary.Nepoch = size(Res.(ColName{Icol}),DimEpoch);
                Summary.Nnn    = sum(~isnan(Res.(ColName{Icol})),DimEpoch);

                if (nargout>2)
                    % calculate the number of stars that appears in N images
                    N_Ep = zeros(Summary.Nepoch,1);
                    for Iep=1:1:Summary.Nepoch
                        % Summary.Nnn is the number of epochs in each stars appears
                        % N_Ep is the number of stars that appears in exactly Iep
                        % epochs.
                        N_Ep(Iep) = sum(Summary.Nnn==Iep);
                    end
                end
            end

        end
    end
        
    methods  % projections and transformations : PUT in seperate class
        
    end      
    
    methods (Static) % unitTest
        function Result = unitTest
            % Example: imProc.cat.Match.unitTest
            
            % coneSearch
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            M = imProc.cat.Match;
            [NC, Flag, Dist] = M.coneSearch(AC,[1 1],'Radius',3600);
            [NC, Flag, Dist] = M.coneSearch(AC,[1 1; 0 0],'Radius',3600);  % search around two positions (merged results).

            % inPolygon
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            M = imProc.cat.Match;
            [InP, Flag] = M.inPolygon(AC,[1 1; 1.1 1.1; 0.5 0.1],'CooUnits','rad');
            InP.plotMapFun
            
            % match
            AC = AstroCatalog;
            AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
            AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
            AC.getCooTypeAuto
            AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]
            AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
            AC2.getCooTypeAuto
            M = imProc.cat.Match;
            [MC,UM,TUM] = M.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad')

            % match against catsHTM
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            M = imProc.cat.Match;
            M.coneSearch(AC,[1 1],'Radius',3600);
            [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = M.match_catsHTM(AC,'GAIADR2');
            % catsHTM is the ref catalog:
            [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = M.match_catsHTM(AC,'GAIADR2','catsHTMisRef',true);

            

            Result = true;
        end
    end
    
end