
classdef Match < Component
    properties
        Cat
        Ref
        Coo
        CatName
        
        CooUnits                    = 'deg';
        Radius                      = 3;
        RadiusUnits                 = 'arcsec';
        Shape                       = 'circle';
   
        AddDistCol(1,1) logical     = true;
        DistUnits                   = 'arcsec';
        DistColName char            = 'Dist';
        DistColPos                  = Inf;
                
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
                Args.Cat
                Args.Ref
                Args.Coo
                Args.CatName
                
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
    
    methods % aux functions
        function Args = selectDefaultArgsFromProp(Obj, Args)
            % Given an Args structure, go over fields - if empty, take
            % value from object property. Otherwise, use value.
            
            ArgNames = fieldnames(Args);
            for Ian=1:1:numel(ArgNames)
                if isempty(Args.(ArgNames{Ian}))
                    Args.(ArgNames{Ian}) = Obj.(ArgNames{Ian});
                end
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
                Result = copyObject(CatObj, 'ClearProp',{'Catalog'});
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
            %
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
                Result = copyObject(CatObj, 'ClearProp',{'Catalog'});
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
            %            'AddDistCol' - Add column distance to outout
            %                   catalog. Default is true.
            %            'DistUnits' - Distance units. Default is 'arcsec'.
            %            'DistColName' - Distance column name.
            %                   Default is 'Dist'.
            %            'DistColPos' - Position of Distance column in
            %                   output catalog. Default is Inf (i.e., last
            %                   column).
            % Output : - An AstroCatalog object of matched sources.
            %            Numeber of sources eqyual to that of the number
            %            of sources in the second object.
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
                Args.AddDistCol                  = true;
                Args.DistUnits                   = 'arcsec';
                Args.DistColName                 = 'Dist';
                Args.DistColPos                  = Inf;
            end
            
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(MObj, Args);
            if isempty(Obj1)
                Obj1 = Obj.Cat;
            end
            if isempty(Obj2)
                Obj2 = Obj.Ref;
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
                
                [IndTable, CatFlagNearest, CatFlagAll] = VO.search.search_sortedlat_multiNearest(Coo1,...
                                                            Coo2(:,1), Coo2(:,2), RadiusRad, DistFun);

                % select nearest from each Ind
                %Result1(Imax).Catalog = Obj1(Iobj1).Catalog ...
                FlagNN = ~isnan(IndTable(:,1));
                [Nrow2,Ncol2]   = size(Obj2(Iobj2).Catalog);
                MatchedObj(Imax).Catalog = nan(Nrow2,Ncol2);
                MatchedObj(Imax).Catalog(FlagNN,:) = Obj1(Iobj1).Catalog(IndTable(FlagNN,1),:);

                % copy the common properties from Obj2
                copyProp(Obj2(Iobj2), MatchedObj(Imax), {'CooType','ColX','ColY','ColNames','ColUnits','ColDesc','SortByCol','IsSorted'});

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
        
        function matchPattern(Obj1, Obj2, ~)
            %
        end
        
        
    end
    
    methods % match against external catalog
        function [MatchedObj, UnMatchedObj] = match_catsHTM(MObj, Obj, CatName, Args)
            %
            
            %'not ready'
            
            arguments
                MObj
                Obj
                CatName char
                Args.Coo                 = [];
                Args.CooUnits            = 'deg';
                Args.Radius              = [];
                Args.RadiusUnits         = 'arcsec';
            end
            
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(MObj, Args);
            if isempty(Obj)
                Obj = Obj.Cat;
            end
            if isempty(CatName)
                CatName = Obj.CatName;
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

            % UNDER CONSTRUCTION
            for Iobj=1:1:Nobj
                switch lower(Obj(Iobj).CooType)
                    case 'sphere'
                        
                    case 'pix'
                        
                    otherwise
                        error('Unknown CooType=%s option',Obj(Iobj).CooType);
                end
            end
            
            
            
        end
        
    end
    
    
    
    
    methods (Static) % unitTest
        function Result = unitTest
            %
            
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
            

            
            Result = true;
        end
    end
    
end