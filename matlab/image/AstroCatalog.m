% AstroCatalog class - A container of AstroTable with coordinates


classdef AstroCatalog < AstroTable
    properties
        ColX                                                            = [];
        ColY                                                            = [];
        CooType char    {mustBeMember(CooType,{'pix','sphere',''})}     = '';
        CooUnits char   {mustBeMember(CooUnits,{'deg','rad','pix',''})} = 'deg';
    end
    
    properties (Hidden, SetAccess=private)
        %CooUnitsKnown(1,1) logical                                      = false;
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
            % Constructor for AstroCatalog 
            % For parameters input see: AstroTable
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
            %warning('Search synonym in config file does not operational yet');
            
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
        
        function [varargout] = getCoo(Obj, Units)
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
        
        function [varargout] = getLonLat(Obj, Units, Args)
            % Get Lon/Lat columns from AstroCatalog.
            % Input - A single element AstroCatalog object.
            %       - Units of output Lon/Lat columns.
            %         Default is 'deg'.
            %       * ...,key,val,...
            %         'ColLon' - A cell array of Lon column names. Will
            %               select the first exitsing column name.
            %               Default is Obj(1).DefNamesRA.
            %         'ColLat' - A cell array of Lat column names. Will
            %               select the first exitsing column name.
            %               Default is Obj(1).DefNamesDec.
            % Output : * If two argument are requested then these are RA and Dec.
            %            If a single argument is
            %            requested then this is a two column matrix of
            %            [RA, Dec].
            % Author : Eran Ofek (May 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          [Lon,Lat] = getLonLat(AC);
            %          [Lon,Lat] = getLonLat(AC,'rad');
            
            arguments
                Obj(1,1)
                Units            = 'deg';
                Args.ColLon      = Obj(1).DefNamesRA;
                Args.ColLat      = Obj(1).DefNamesDec;
            end
            
            ColLon = colnameDict2ind(Obj, Args.ColLon);
            ColLat = colnameDict2ind(Obj, Args.ColLat);
            
            if isempty(ColLon) || isempty(ColLat)
                Lon = [];
                Lat = [];
            else
                [Lon, LonUnits] = getCol(Obj, ColLon);
                [Lat, LatUnits] = getCol(Obj, ColLat);

                LonUnits = LonUnits{1};
                LatUnits = LatUnits{1};
                if ~isempty(LonUnits)
                    Lon = convert.angular(LonUnits, Units, Lon);
                end
                if ~isempty(LatUnits)
                    Lat = convert.angular(LatUnits, Units, Lat);
                end
            end
            
            if nargout>1
                varargout{1} = Lon;
                varargout{2} = Lat;
            else
                varargout{1} = [Lon, Lat];
            end
            
            
        end
        
        function [varargout] = getXY(Obj, Args)
            % Get X/Y columns from AstroCatalog.
            % Input - A single element AstroCatalog object.
            %       * ...,key,val,...
            %         'ColX' - A cell array of X column names. Will
            %               select the first exitsing column name.
            %               Default is Obj(1).DefNamesX.
            %         'ColY' - A cell array of Y column names. Will
            %               select the first exitsing column name.
            %               Default is Obj(1).DefNamesY.
            % Output : * If two argument are requested then these are X and Y.
            %            If a single argument is
            %            requested then this is a two column matrix of
            %            [X, Y].
            % Author : Eran Ofek (May 2021)
            % Example:
            % AC=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});
            %          [X,Y] = getXY(AC);
            
            arguments
                Obj(1,1)
                Args.ColX      = Obj(1).DefNamesX;
                Args.ColY      = Obj(1).DefNamesY;
            end
            
            ColX = colnameDict2ind(Obj, Args.ColX);
            ColY = colnameDict2ind(Obj, Args.ColY);
            
            if isempty(ColX) || isempty(ColY)
                X = [];
                Y = [];
            else
                X = getCol(Obj, ColX);
                Y = getCol(Obj, ColY);
            end
            
            if nargout>1
                varargout{1} = X;
                varargout{2} = Y;
            else
                varargout{1} = [X, Y];
            end
            
            
        end
        
    end
    
    methods % coordinates and bounding box
        function [CircleX, CircleY, CircleRadius] = boundingCircle(Obj, Args)
            % Fit a bounding circle position and radius to a catalog
            % Input  : - An AstroCatalog object (multi elements supported).
            %            All elements must have the same CooType.
            %          * ...,key,val,...
            %            'OutUnits' - Output units. Default is 'deg'.
            % Output : - The best fit circle X/Long
            %          - The best fit circle Y/Lat
            %          - The best fit circle radius
            % Author : Eran Ofek (Apr 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          M = imProc.cat.Match;
            %          [Result] = M.coneSearch(AC, [1 1], 'Radius',3600.*10)
            %          [CircleX, CircleY, CircleRadius] = boundingCircle(Result);
            
            arguments
                Obj
                Args.OutUnits char       = 'deg';
            end
            
            Nobj         = numel(Obj);
            CircleX      = nan(size(Obj));
            CircleY      = nan(size(Obj));
            CircleRadius = nan(size(Obj));
            
            for Iobj=1:1:Nobj
                [X, Y] = getCoo(Obj(Iobj),'rad');
                switch lower(Obj(Iobj).CooType)
                    case 'sphere'
                        [BestCoo, BestRadius] = celestial.coo.boundingCircle(X, Y);   % [radians]
                    case 'pix'
                        [BestCoo, BestRadius] = tools.math.geometry.boundingCircle(X, Y);  % [radians]
                    otherwise
                        error('Unknown CooType=%s option',Obj(Iobj).CooType);
                end
                CircleX      = BestCoo(1);
                CircleY      = BestCoo(2);
                CircleRadius = BestRadius;
            end
            % convert output to Args.OutUnits
            if strcmp(Obj(Iobj).CooType,'sphere')
                ConvFactor   = convert.angular('rad',Args.OutUnits);
                CircleX      = CircleX.*ConvFactor;
                CircleY      = CircleY.*ConvFactor;
                CircleRadius = CircleRadius.*ConvFactor;
            end
        end
            
    end
    
    methods % cut, projection, and transform
        function Result = cropXY(Obj, CCDSEC, Args)
            % crop AstroCatalog object by X/Y coordinates.
            %       Including updateing the X/Y coordinates.
            %       The function may operate on multiple AstroCatalog
            %       and/or multiple sections.
            %       For example, it can produce multiple crops of a single
            %       catalog.
            % Example : AC = AstroCatalog({rand(100,3).*100}, 'ColNames',{'XWIN','YWIN','Flux'});
            %           Result = cropXY(AC, [1 50 1 50])
            %           Result = cropXY(AC, [81 100 41 70],'AddX',{'Flux'})
            %           Result = cropXY(AC, [81 100 41 70; 1 50 1 50]); % multiple crops of a single catalog
            
            arguments
                Obj
                CCDSEC
                Args.ColX                          = {'X','XWIN_IMAGE','XWIN','XPEAK','X_PEAK'};
                Args.ColY                          = {'Y','YWIN_IMAGE','YWIN','YPEAK','Y_PEAK'};
                Args.AddX                          = {};  % additional X-coo to update
                Args.AddY                          = {};
                Args.UpdateXY(1,1) logical         = true;
                Args.CreateNewObj                  = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            if Args.CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end
            
            Nsec = size(CCDSEC,1);
            Nobj = numel(Obj);
            Nmax = max(Nsec, Nobj);
            
            for Imax=1:1:Nmax
                Isec = min(Imax, Nsec);
                Iobj = min(Imax, Nobj);
                
                [ColIndX] = colnameDict2ind(Obj(Iobj),Args.ColX);
                [ColIndY] = colnameDict2ind(Obj(Iobj),Args.ColY);
                
                Flag = Obj(Iobj).Catalog(:,ColIndX) >= CCDSEC(Isec,1) & ...
                       Obj(Iobj).Catalog(:,ColIndX) <= CCDSEC(Isec,2) & ...
                       Obj(Iobj).Catalog(:,ColIndY) >= CCDSEC(Isec,3) & ...
                       Obj(Iobj).Catalog(:,ColIndY) <= CCDSEC(Isec,4);
                   
                Result(Imax).Catalog = Obj(Iobj).Catalog(Flag,:);
                
                if Args.UpdateXY
                    % update XY coordinates
                    Result(Imax).Catalog(:,ColIndX) = Result(Imax).Catalog(:,ColIndX) - CCDSEC(Isec,1) + 1;
                    Result(Imax).Catalog(:,ColIndY) = Result(Imax).Catalog(:,ColIndY) - CCDSEC(Isec,3) + 1;
                    
                    % update additional coordinates
                    if ~isempty(Args.AddX)
                        [ColIndX] = colnameDict2ind(Obj(Iobj), Args.AddX);
                        Result(Imax).Catalog(:,ColIndX) = Result(Imax).Catalog(:,ColIndX) - CCDSEC(Isec,1) + 1;
                    end
                    
                    if ~isempty(Args.AddY)
                        [ColIndY] = colnameDict2ind(Obj(Iobj), Args.AddY);
                        Result(Imax).Catalog(:,ColIndY) = Result(Imax).Catalog(:,ColIndY) - CCDSEC(Isec,3) + 1;
                    end
                end
            end
        end
        
        function Result = projection(Obj, IsInv, Projection, Args)
            %
           
%             arguments
%                 Obj
%                 IsInv(1,1) logical       = false;  % false - lon/lat->x,y
%                 Projection               = 'tan';
%                 Args
%             end
%             
%             Nobj = numel(Obj);
%             for Iobj=1:1:Nobj
%                 if IsInv
%                     % inverse projection
%                     % get X/Y
%                 
%                 else
%                     % regular projection
%                     % get Long/Lat
%                     
%                 
%                 
%                 end
%             end
            
        end
    end
    
    methods % plotting
        function varargin = plotMapFun(Obj, Projection, PlotFun, AddCol, varargin)
            % A general map (RA/Dec) plotting function for AstroCatalog object
            % Input  : - An AstroCatalog object.
            %          - Map projection. See axesm for options.
            %            Default is 'aitoff'. 
            %          - Plot function handle. e.g., @plotm.
            %            Default is @plotm.
            %          - Cell array of additional columns (to Dec, RA) to pass to the
            %            plot function (e.g., in the case of @scatterm).
            %            Default is {}.
            %          * Additional arguments to pass to the plot function.
            % Output : - 
            % Author : Eran Ofek (Apr 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          AC.getCooTypeAuto
            %          AC.plotMapFun('aitoff',@plotm,{},'.','MarkerSize',1)
            %          
            %          AC.plotMapFun('aitoff',@scatterm,{'mag1','sep1'},'.','MarkerSize',1)
            
            if nargin<4
                AddCol = {};
                if nargin<3
                    PlotFun = @plotm;
                    if nargin<2
                        Projection = [];
                    end
                end
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
            
            DataSampleDir = tools.os.getTestDataDir;
            PWD = pwd;
            cd(DataSampleDir);
            
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
            
            % bounding circle
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [Result] = imProc.match.coneSearch(AC, [1 1], 'Radius',3600.*10);
            [CircleX, CircleY, CircleRadius] = boundingCircle(Result);
            if abs(CircleX-1)>0.1 ||  abs(CircleY-1)>0.1 || abs(CircleRadius-10)>0.5
                error('Problem with catalogBoundingCircle');
            end
            
            % cropXY
            AC = AstroCatalog({rand(100,3).*100}, 'ColNames',{'XWIN','YWIN','Flux'});
            Result = cropXY(AC, [1 50 1 50]);
            Result = cropXY(AC, [81 100 41 70],'AddX',{'Flux'});
            Result = cropXY(AC, [81 100 41 70; 1 50 1 50]); % multiple crops of a single catalog

            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [Lon,Lat] = getLonLat(AC);
            [Lon,Lat] = getLonLat(AC,'rad');
            
            AC=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});
            [X,Y] = getXY(AC);

            
%             % TRANSFERED!!
%             % match (spherical)
%             AC = AstroCatalog;
%             AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
%             AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
%             AC.getCooTypeAuto
%             AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
%             AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
%             AC2.getCooTypeAuto
%             [M,UM,TUM] = match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
%             if ~(sizeCatalog(M)==5 && sizeCatalog(UM)==3 && sizeCatalog(TUM)==2)
%                 error('Size of matched/unmatched catalog is wrong');
%             end
%             
%             % match (pixel)
%             AC = AstroCatalog;
%             AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
%             AC.ColNames = {'X','Y'}; AC.ColUnits = {'pix','pix'};
%             AC.getCooTypeAuto
%             AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
%             AC2.ColNames = {'X','Y'}; AC2.ColUnits = {'pix','pix'};
%             AC2.getCooTypeAuto
%             [M,UM,TUM] = match(AC,AC2,'Radius',0.02);
%             if ~(sizeCatalog(M)==5 && sizeCatalog(UM)==3 && sizeCatalog(TUM)==2)
%                 error('Size of matched/unmatched catalog is wrong');
%             end
            
            
            % plot
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            AC.getCooTypeAuto
            AC.plotMapFun('aitoff',@plotm,{},'.','MarkerSize',1);

            cd(PWD);
            Result = true;
        end
    end
    
end
     