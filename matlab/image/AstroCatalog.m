

classdef AstroCatalog < AstroTable
    properties
        ColX                                                            = [];
        ColY                                                            = [];
        CooType char    {mustBeMember(CooType,{'pix','sphere',''})}     = '';
        CooUnits char   {mustBeMember(CooUnits,{'deg','rad','pix',''})} = 'deg';
    end
    
    properties (Hidden, SetAccess=private)
        CooUnitsKnown(1,1) logical                                      = false;
    end
    
    properties (Hidden, Constant)
        DefNamesX cell                   = {'X','X_IMAGE','XWIN_IMAGE','X1','X_PEAK'};
        DefNamesY cell                   = {'Y','Y_IMAGE','YWIN_IMAGE','Y1','Y_PEAK'};
        DefNamesRA cell                  = {'RA','ALPHA','ALPHAWIN_J2000','ALPHA_J2000','RA_J2000','RAJ2000','RightAsc'};
        DefNamesDec cell                 = {'Dec','DELTA','DELTAWIN_J2000','DELTA_J2000','DEC_J2000','DEJ2000','Declination'};
        DefNamesPMRA cell                = {'PMRA'};
        DefNamesPMDec cell               = {'PMDec'};
        DefNamesPlx cell                 = {'Plx'};
        DefNamesMag cell                 = {'Mag','PSF_MAG','MAG_PSF'};
    end
  
    
    
    methods % constructor
        function Obj = AstroCatalog(varargin)
            % Constructor for Astrocatalog 
            %
            % Example: AC=AstroCatalog({'asu.fit','asu.fit'},'HDU',2)
            
            % construct AstroTable
            AT   = AstroTable(varargin{:});
            FN   = fieldnames(AT);
            Nfn  = numel(FN);
            Nobj = numel(AT);
            for Iobj=1:1:Nobj
                for Ifn=1:1:Nfn
                    Obj(Iobj).(FN{Ifn}) = AT(Iobj).(FN{Ifn});
                end
            end
                
        end
    end
    
    methods % setters/getters

      
        function Units = get.CooUnits(Obj)
            % getter for CooUnits - if empty, attempt to obtain from
            % catalog.
            % Note this is not cleared when the catalog is changed.
           
%             if Obj.CooUnitsKnown
%                 Units = '';
                
            if isempty(Obj.CooUnits)
                ColIndX = colname2ind(Obj, Obj.ColX);
                ColIndY = colname2ind(Obj, Obj.ColY);
                CooUnitsX = Obj.ColUnits{ColIndX};
                CooUnitsY = Obj.ColUnits{ColIndY};
                if ~strcmp(CooUnitsX,CooUnitsY)
                    error('CooUnits for columns %d and %d are inconsistent',ColIndX, ColIndY);
                end
                Units        = CooUnitsX;
                Obj.CooUnits = Units;
            else
                Units = Obj.CooUnits;
            end

            
            
        end
        
        function set.CooUnits(Obj, Val)
            % setter for CooUnits and coordinates unit conversion
            % If ColNames doesn't contains units, this will just update the
            % ColNames and CooUnits according to the input value.
            % If ColName do contain units, this will update the units and
            % convert the coordinate columns to the requested Units.
            % If ColX and ColY are empty, then will attempt to populate
            % them.
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2)
            %          AC.CooUnits = 'rad';
            %          AC.CooUnits = 'deg';                     
            
            if isempty(Obj.ColX) || isempty(Obj.ColY)
                Obj.getCooTypeAuto;
            end
            ColIndX = colname2ind(Obj, Obj.ColX);
            ColIndY = colname2ind(Obj, Obj.ColY);
            CooUnitsX = Obj.ColUnits{ColIndX};
            CooUnitsY = Obj.ColUnits{ColIndY};
            if isempty(CooUnitsX)
                % set coordinate units without converting the values
                Obj.ColUnits{ColIndX} = Val;
            else
                if ~strcmp(CooUnitsX,'pix')
                    ConvFactor = convert.angular(CooUnitsX, Val, 1);
                    Obj.Catalog(:,ColIndX) = Obj.Catalog(:,ColIndX).*ConvFactor;
                    Obj.ColUnits{ColIndX} = Val;
                end
            end
            if isempty(CooUnitsY)
                % set coordinate units without converting the values
                Obj.ColUnits{ColIndY} = Val;
            else
                if ~strcmp(CooUnitsY,'pix')
                    ConvFactor = convert.angular(CooUnitsY, Val, 1);
                    Obj.Catalog(:,ColIndY) = Obj.Catalog(:,ColIndY).*ConvFactor;
                    Obj.ColUnits{ColIndY} = Val;
                end
            end
                
            Obj.CooUnits = Val;
        end
    end
    
    methods % column names
        function [CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(Obj, Args)
            % Attempt to get CooType and RA/Dec X/Y column names automatically from Catalog
            % Input  : - An AstroCatalog object.
            %          * ...,key,val,...
            %            'CaseSens' - Case senstive column name search.
            %                   Default is false.
            %            'UpdateProp' - Indicating if to update the
            %            CooType, ColX and ColY properties.
            %            Default is true.
            % Output : - CooType: 'sphere' | 'pix', for the last element in
            %            the AstroCatalog object.
            %          - X column name.
            %          - Y column name.
            %          - X column index.
            %          - Y column index.
            % Author : Eran Ofek (Mar 2021)
            % Example: [CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(AC(1));
           
            arguments
                Obj
                Args.CaseSens(1,1) logical            = false;
                Args.UpdateProp(1,1) logical          = true;
            end
            
            
            Nobj = numel(Obj);
            % search synonyms in config file
            warning('Search synonym in config file does not operational yet');
            
            for Iobj=1:1:Nobj
                SynonymCell_RA  = Obj(Iobj).DefNamesRA;
                SynonymCell_Dec = Obj(Iobj).DefNamesDec;

                [Name_RA,  IndInCell_RA,  IndInSynonym_RA]  = AstroTable.searchSynonym(Obj(Iobj).ColNames, SynonymCell_RA,  'CaseSens', Args.CaseSens);
                [Name_Dec, IndInCell_Dec, IndInSynonym_Dec] = AstroTable.searchSynonym(Obj(Iobj).ColNames, SynonymCell_Dec, 'CaseSens', Args.CaseSens);
                if isempty(IndInCell_RA) || isempty(IndInCell_Dec)
                    CooType = 'pix';
                else
                    % spherical coordinates found in AstroCatalog
                    CooType = 'sphere';
                end

                switch lower(CooType)
                    case 'pix'
                        SynonymCell_X = Obj(Iobj).DefNamesX;
                        SynonymCell_Y = Obj(Iobj).DefNamesY;

                        [NameX, IndInCellX, IndInSynonymX] = AstroTable.searchSynonym(Obj(Iobj).ColNames, SynonymCell_X, 'CaseSens', Args.CaseSens);
                        [NameY, IndInCellY, IndInSynonymY] = AstroTable.searchSynonym(Obj(Iobj).ColNames, SynonymCell_Y, 'CaseSens', Args.CaseSens);
                    case 'sphere'
                        SynonymCell_RA  = Obj(Iobj).DefNamesRA;
                        SynonymCell_Dec = Obj(Iobj).DefNamesDec;

                        [NameX, IndInCellX, IndInSynonymX] = AstroTable.searchSynonym(Obj(Iobj).ColNames, SynonymCell_RA,  'CaseSens', Args.CaseSens);
                        [NameY, IndInCellY, IndInSynonymY] = AstroTable.searchSynonym(Obj(Iobj).ColNames, SynonymCell_Dec, 'CaseSens', Args.CaseSens);
                    otherwise
                        error('Unknown/illegal CooType option');
                end
                NameX = NameX{1};
                NameY = NameY{1};
                CooUnitsX = Obj(Iobj).ColUnits{IndInCellX};
                CooUnitsY = Obj(Iobj).ColUnits{IndInCellY};
                
                if Args.UpdateProp
                    Obj(Iobj).CooType = CooType;
                    Obj(Iobj).ColX    = IndInCellX;
                    Obj(Iobj).ColY    = IndInCellY;
                    if ~strcmp(CooUnitsX, CooUnitsY)
                        error('Coo units must be identical in both axes');
                    end
                    Obj(Iobj).CooUnits = CooUnitsX;
                end
            end
            
            
        end
        
        function Obj = convertCooUnits(Obj,Units)
            % convert all coordinates Units for multiple element object
            % Example: AC=AstroCatalog({'asu.fit','asu.fit'},'HDU',2)
            %          AC.convertCooUnits('deg')
            %          Obj.convertCooUnits('deg')
            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).CooUnits = Units;
            end            
            
        end
        
        function [varargout]=getCoo(Obj, Units)
            % get coordinates columns from a single element AstroCatalog object
            % Input  : - A single element AstroCatalog object
            %          - Units of output coordinates (for no spherical
            %            coordinates will return them as they are).
            % Output : * If two argument are requested then these are X and
            %            Y, or RA and Dec. If a single argument is
            %            requested then this is a two column matrix of
            %            [X, Y] or [RA, Dec].
            % Author : Eran Ofek (Mar 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          [RA, Dec] = AC.getCoo('deg');
            %          [RADec]   = AC.getCoo('rad');
            
            arguments
                Obj(1,1)
                Units char                = 'rad';
            end
           
            if isempty(Obj.ColX) || isempty(Obj.ColY)
                Obj.getCooTypeAuto;
            end
            
            X = getCol(Obj, Obj.ColX);
            Y = getCol(Obj, Obj.ColY);
            
            if strcmp(Obj.CooType, 'sphere') && ~strcmp(Obj.CooUnits, Units)
                ConvFactor = convert.angular(Obj.CooUnits, Units, 1);
                X          = X.*ConvFactor;
                Y          = Y.*ConvFactor;
            end
            
            if nargout>1
                varargout{1} = X;
                varargout{2} = Y;
            else
                varargout{1} = [X, Y];
            end
            
        end
        
    end
    
    
    
    methods % search by coordinates/name
        function [Result, Flag, AllDist] = coneSearch(Obj, Coo, Args)
            % cone search(s) on AstroCatalog object
            % Input  : - An AstroCatalog object. If multiple elements
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
            %            'CreateNewObj' - Create a new object (true), or
            %                   store the data in the input object (false).
            %                   Default is true.
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
            %          AC.getCooTypeAuto
            %          [NC, Flag, Dist] = coneSearch(AC,[1 1],'Radius',3600)
            %          [NC, Flag, Dist] = coneSearch(AC,[1 1; 0 0],'Radius',3600);  % search around two positions (merged results).
            
            
            arguments
                Obj
                Coo
                Args.Radius                      = 5;
                Args.RadiusUnits                 = 'arcsec';
                Args.Shape char                  = 'circle';
                Args.CooUnits char               = 'deg';
                Args.AddDistCol                  = true;
                Args.DistUnits                   = 'arcsec';
                Args.DistColName                 = 'Dist';
                Args.DistColPos                  = Inf;
                Args.CreateNewObj(1,1) logical   = true;
            end
            
            RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
            if Args.AddDistCol || nargout>2
                RadiusRad = -abs(RadiusRad);
            end
            
            
            if Args.CreateNewObj
                Result = copyObject(Obj, 'ClearProp',{'Catalog'});
            else 
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if ~Obj(Iobj).IsSorted
                    Obj(Iobj).sortrows(Obj(Iobj).ColY);
                end
                switch lower(Obj(Iobj).CooType)
                    case 'sphere'
                        [Ind,Flag] = VO.search.search_sortedlat_multi(getCoo(Obj(Iobj),'rad'),...
                                                                    Coo(:,1), Coo(:,2), RadiusRad, [],...
                                                                    @celestial.coo.sphere_dist_fast);
                    case 'pix'
                        [Ind,Flag] = VO.search.search_sortedlat_multi(getCoo(Obj(Iobj),'rad'),...
                                                                    Coo(:,1), Coo(:,2), RadiusRad, [],...
                                                                    @tools.math.geometry.plane_dist);
                        
                        error('Pixel coneSearch is not yet supported');
                    otherwise
                        error('Unknown CooType option');
                end
                        
                
                % what to do with the found objects
                Ncoo = numel(Ind);
                Out     = zeros(0, size(Obj(Iobj).Catalog,2));
                AllDist = zeros(0,1);
                for Icoo=1:1:Ncoo
                    Out     = [Out; Obj(Iobj).Catalog(Ind(Icoo).Ind,:)];
                    AllDist = [AllDist; Ind(Icoo).Dist]; 
                end
                AllDist  = convert.angular('rad', Args.DistUnits, AllDist);
                Result(Iobj).Catalog = Out;
                if Args.AddDistCol
                    Result(Iobj).insertCol(AllDist, Args.DistColPos, Args.DistColName);
                end
            end
            
            
            
            
        end
        
        function [Obj, Flag] = inPolygon(Obj, PolyCoo, Args)
            %
            % very similar to coneSearch
            
            
        end
        
        
    end
    
    methods % match two AstroCatalog
        function [MatchedObj, UnMatchedObj, TruelyUnMatchedObj] = match(Obj1, Obj2, Args)
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
            % Input  : - An AstroCatalog object.
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
            %           [M,UM,TUM] = match(AC,AC2,'Radius',0.01,'RadiusUnits','rad')
            
            arguments
                Obj1
                Obj2
                Args.Radius                      = 5;
                Args.RadiusUnits                 = 'arcsec';
                Args.AddDistCol                  = true;
                Args.DistUnits                   = 'arcsec';
                Args.DistColName                 = 'Dist';
                Args.DistColPos                  = Inf;
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
        
        function matchPattern(Obj1, Obj2, Args)
            %
        end
        
        
    end
    
    methods % match against external catalog
        function [MatchedObj, UnMatchedObj] = match_catsHTM(Obj, CatName, Args)
            %
        end
        
    end
    
    methods % plotting
        function varargin = plotMapFun(Obj, Projection, PlotFun, AddCol, varargin)
            % A general map (RA/Dec) plotting function for AstroCatalog object
            % Input  : - An AstroCatalog object.
            %          - Map projection. See axesm for options.
            %            Default is 'aitoff'. 
            %          - Plot function handle. e.g., @plotm.
            %          - Cell array of additional columns (to Dec, RA) to pass to the
            %            plot function (e.g., in the case of @scatterm).
            %            Default is {}.
            %          * Additional arguments to pass to the plot function.
            % Output : - 
            % Author : Eran Ofek (Apr 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          AC.getCooTypeAuto
            %          AC.plotMapFun('aitoff',@plotm,{},'.','MarkerSize',1)
            %          AC.plotMapFun('aitoff',@scatterm,{'mag1','sep1'},'.','MarkerSize',1)
            
            if nargin<2
                Projection = [];
            end
            if isempty(Projection)
                Projection = 'aitoff';
            end
               
            if ~iscell(AddCol)
                AddCol = num2cell(AddCol);
            end
            
            % first time
            A=axesm(Projection, 'Frame', 'on', 'Grid', 'on');

            CooUnits = Obj.CooUnits;
            Obj.CooUnits = 'deg';
            [varargin{1:nargout}] = plotFun(Obj, PlotFun, {Obj.ColY, Obj.ColX, AddCol{:}}, varargin{:});
            Obj.CooUnits = CooUnits;

            %[varargin{1:1:nargin}] = plot(Obj)
             
            
        end
        
    end
    
    
    methods (Static) % unitTest
        function Result = unitTest
            % unitTest for the AstroCatalog class
            
            AC = AstroCatalog({'asu.fit','asu.fit'}, 'HDU',2);
            [CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(AC);
            if AC(1).ColX~=AC(2).ColX
                error('ColX in the two elements should have been identical');
            end
            if AC(1).ColY~=AC(2).ColY
                error('ColY in the two elements should have been identical');
            end
            
            % sort
            AC(1).SortByCol = AC(1).ColY;
            AC(2).SortByCol = 'DEJ2000';
            AC.sortrows(AC(1).ColY);
            if ~issorted(AC(1).Catalog(:,AC(1).ColY))
                error('catalog is not sorted');
            end
            
            % sort using the SortByCol property
            AC = AstroCatalog({'asu.fit','asu.fit'}, 'HDU',2);
            AC.getCooTypeAuto;
            AC(1).SortByCol = AC(1).ColY;
            AC(2).SortByCol = 'DEJ2000';
            AC.sortrows;
            if ~issorted(AC(1).Catalog(:,AC(1).ColY))
                error('catalog is not sorted');
            end
            
            
            % match (spherical)
            AC = AstroCatalog;
            AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
            AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
            AC.getCooTypeAuto
            AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
            AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
            AC2.getCooTypeAuto
            [M,UM,TUM] = match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
            if ~(sizeCatalog(M)==5 && sizeCatalog(UM)==3 && sizeCatalog(TUM)==2)
                error('Size of matched/unmatched catalog is wrong');
            end
            
            % match (pixel)
            AC = AstroCatalog;
            AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
            AC.ColNames = {'X','Y'}; AC.ColUnits = {'pix','pix'};
            AC.getCooTypeAuto
            AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
            AC2.ColNames = {'X','Y'}; AC2.ColUnits = {'pix','pix'};
            AC2.getCooTypeAuto
            [M,UM,TUM] = match(AC,AC2,'Radius',0.02);
            if ~(sizeCatalog(M)==5 && sizeCatalog(UM)==3 && sizeCatalog(TUM)==2)
                error('Size of matched/unmatched catalog is wrong');
            end
            
            
        end
    end
    
end
     