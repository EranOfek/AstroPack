% AstroCatalog class - A container of AstroTable with coordinates


classdef AstroCatalog < AstroTable
    properties
        ColX                                                            = [];
        ColY                                                            = [];
        CooType char    {mustBeMember(CooType,{'pix','sphere',''})}     = '';
        CooUnits char   {mustBeMember(CooUnits,{'deg','rad','pix',''})} = '';
    end
    
    properties (Hidden, SetAccess=private)
        %CooUnitsKnown(1,1) logical                                      = false;
    end
    
    properties (Hidden, Constant)
        DefNamesX cell                   = {'X','X_IMAGE','XWIN_IMAGE','X1','X_PEAK','XPEAK'};
        DefNamesY cell                   = {'Y','Y_IMAGE','YWIN_IMAGE','Y1','Y_PEAK','YPEAK'};
        DefNamesRA cell                  = {'RA','ALPHA','ALPHAWIN_J2000','ALPHA_J2000','RA_J2000','RAJ2000','RightAsc'};
        DefNamesDec cell                 = {'Dec','DELTA','DELTAWIN_J2000','DELTA_J2000','DEC_J2000','DEJ2000','Declination'};
        DefNamesPMRA cell                = {'PMRA'};
        DefNamesPMDec cell               = {'PMDec'};
        DefNamesRV cell                  = {'RV'};
        DefNamesPlx cell                 = {'Plx'};
        DefNamesMag cell                 = {'Mag','PSF_MAG','MAG_PSF','Mag_BP','Mag_G','Mag_RP'};
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
                if isempty(ColIndX) || isempty(ColIndY)
                    Units        = '';
                    Obj.CooUnits = Units;
                else
                    CooUnitsX = Obj.ColUnits{ColIndX};
                    CooUnitsY = Obj.ColUnits{ColIndY};
                    if ~strcmp(CooUnitsX,CooUnitsY)
                        error('CooUnits for columns %d and %d are inconsistent',ColIndX, ColIndY);
                    end
                    Units        = CooUnitsX;
                    Obj.CooUnits = Units;
                end
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
        function [Result] = isCooPix(Obj, DicCooX, DicCooY)
            % Check if X/Y (pixel) coordinates exist in an AstroCatalog
            % Input  : - An AstroCatalog object.
            %          - Cell array of dictionary for X column names.
            %            Default is AstroCatalog.DefNamesX.
            %          - Cell array of dictionary for Y column names.
            %            Default is AstroCatalog.DefNamesY.
            % Output : - An array of logical indicating if each one of the
            %            Astrocatalog elements contains an X/Y columns.
            % Author : Eran Ofek (Aug 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          [Result] = isCooPix(AC);
            
            arguments
                Obj
                DicCooX       = AstroCatalog.DefNamesX;
                DicCooY       = AstroCatalog.DefNamesY;
            end
            
            Nobj   = numel(Obj);
            Result = true(size(Obj));
            for Iobj=1:1:Nobj
                [~, UnitsX, IndX] = getColDic(Obj, DicCooX);
                [~, UnitsY, IndY] = getColDic(Obj, DicCooY);
                
                if ~strcmp(UnitsX, UnitsY)
                    error('X and Y columns have different units');
                end
                
                if isempty(IndX) || isempty(IndY)
                    Result(Iobj) = false;
                end
            end
        end
        
        function [Result, Units] = isCooSphere(Obj, DicCooRA, DicCooDec)
            % Check if RA/Dec (units) coordinates exist in an AstroCatalog
            % Input  : - An AstroCatalog object.
            %          - Cell array of dictionary for RA column names.
            %            Default is AstroCatalog.DefNamesRA.
            %          - Cell array of dictionary for Dec column names.
            %            Default is AstroCatalog.DefNamesDec.
            % Output : - An array of logical indicating if each one of the
            %            Astrocatalog elements contains an RA/Dec columns.
            %          - A cell array of units per AstroCatalog element.
            % Author : Eran Ofek (Aug 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          [Result, Units] = isCooSphere(AC);
            
            arguments
                Obj
                DicCooRA       = AstroCatalog.DefNamesRA;
                DicCooDec      = AstroCatalog.DefNamesDec;
            end
            
            Nobj   = numel(Obj);
            Result = true(size(Obj));
            Units  = cell(size(Obj));
            for Iobj=1:1:Nobj
                [~, UnitsRA,  IndRA]  = getColDic(Obj, DicCooRA);
                [~, UnitsDec, IndDec] = getColDic(Obj, DicCooDec);
                
                if ~strcmp(UnitsRA, UnitsDec)
                    error('RA and Dec columns have different units');
                end
                Units{Iobj} = UnitsRA;
                
                if isempty(IndRA) || isempty(IndDec)
                    Result(Iobj) = false;
                end
            end
        end
            
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
        
        function [RA, Dec, PMRA, PMDec, Plx, RV] = getRADecPM(Obj, Args)
            % get RA/Dec/PM/Plx/RV from astrometric catalog
            % Input  : - A single element AstroCatalog object.
            %          * ...,key,val,...
            %            'OutCooUnits' - Output coo units. Default is 'rad'
            %            'OutPMUnits' - Output PM units. Default is 'mas/yr'
            %            'OutPlxUnits' - Output Plx units. Default is 'mas'
            %            'OutRVUnits' - Output RV units. Default is 'km/s'
            % Output : - RA
            %          - Dec
            %          - PM RA
            %          - PM Dec
            %          - Plx
            %          - RV
            % Author : Eran Ofek (May 2021)
            % Example: C=catsHTM.cone_search('GAIADR2',1,1,100,'OutType','astrocatalog');
            %          [RA, Dec, PM_RA, PM_Dec, Plx, RV] = getRADecPM(C)
            
            arguments
                Obj(1,1)
                Args.OutCooUnits     = 'rad';
                Args.OutPMUnits      = 'mas/yr';
                Args.OutPlxUnits     = 'mas';
                Args.OutRVUnits      = 'km/s';
            end
            
            % RA
            ColInd_RA = colnameDict2ind(Obj, Obj.DefNamesRA);
            [RA, Units]  = getCol(Obj, ColInd_RA);
            RA = convert.angular(Units{1}, Args.OutCooUnits, RA);
            
            % Dec
            ColInd_Dec = colnameDict2ind(Obj, Obj.DefNamesDec);
            [Dec, Units]  = getCol(Obj, ColInd_Dec);
            Dec = convert.angular(Units{1}, Args.OutCooUnits, Dec);
            
            % PM_RA
            ColInd_PMRA = colnameDict2ind(Obj, Obj.DefNamesPMRA);
            [PMRA, Units]  = getCol(Obj, ColInd_PMRA);
            PMRA = convert.proper_motion(Units{1}, Args.OutPMUnits, PMRA);
            
            % PM_Dec
            ColInd_PMDec = colnameDict2ind(Obj, Obj.DefNamesPMDec);
            [PMDec, Units]  = getCol(Obj, ColInd_PMDec);
            PMDec = convert.proper_motion(Units{1}, Args.OutPMUnits, PMDec);
            
            % Plx
            ColInd_Plx = colnameDict2ind(Obj, Obj.DefNamesPlx);
            [Plx, Units]  = getCol(Obj, ColInd_Plx);
            Plx = convert.angular(Units{1}, Args.OutPlxUnits, Plx);
            
            % RV
            ColInd_RV = colnameDict2ind(Obj, Obj.DefNamesRV);
            [RV, Units]  = getCol(Obj, ColInd_RV);
            RV = convert.velocity(Units{1}, Args.OutRVUnits, RV);
            
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
            %            'CooType' - Coordinate type {'sphere'|'pix'}.
            %                   If empty, use the AstroCatalog CooType.
            %                   Default is empty.
            % Output : - The best fit circle X/Long
            %          - The best fit circle Y/Lat
            %          - The best fit circle radius
            % Author : Eran Ofek (Apr 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          [Result] = imProc.match.coneSearch(AC, [1 1], 'Radius',3600.*10)
            %          [CircleX, CircleY, CircleRadius] = boundingCircle(Result);
            
            arguments
                Obj
                Args.OutUnits char       = 'deg';
                Args.CooType             = [];
            end
            
            Nobj         = numel(Obj);
            CircleX      = nan(size(Obj));
            CircleY      = nan(size(Obj));
            CircleRadius = nan(size(Obj));
            
            for Iobj=1:1:Nobj
                if isempty(Args.CooType)
                    CooType = Obj(Iobj).CooType;
                else
                    CooType = Args.CooType;
                end
                %[X, Y] = getCoo(Obj(Iobj),'rad');
                switch lower(CooType)
                    case 'sphere'
                        [X, Y] = getLonLat(Obj(Iobj),'rad');
                        [BestCoo, BestRadius] = celestial.coo.boundingCircle(X, Y);   % [radians]
                    case 'pix'
                        [X, Y] = getXY(Obj(Iobj));
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
            
        function [Dist, PA] = sphere_dist(Obj, Lon, Lat, LonLatUnits, OutUnits)
            % Calculate the spherical distance and PA between Lon,Lat in
            % Astrocatalog and a Lon, Lat in array.
            % Input  : - A single-element AstroCatlog Object.
            %          - Lon (scalar, or column vector with the same length
            %            as the AstroCatalog catalog, or a row vector of
            %            arbitrary length).
            %          - Lat
            %          - Lon/Lat units. Default is 'deg'.
            %          - Output units of Dist and PA. Default is 'deg'.
            % Output : - Angular distance.
            %          - Position angle, as calculated by celestial_sphere.
            % Author : Eran Ofek (Jul 2021)
            % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
            %          [Dist, PA] = sphere_dist(AC,1,1);
            
            arguments
                Obj(1,1)
                Lon
                Lat
                LonLatUnits      = 'deg';
                OutUnits         = 'deg';
            end
           
            [ObjLon, ObjLat] = getLonLat(Obj, 'rad');
            ConvertFactor    = convert.angular(LonLatUnits,'rad');
            Lon              = Lon.*ConvertFactor;
            Lat              = Lat.*ConvertFactor;
            
            [Dist, PA]    = celestial.coo.sphere_dist(ObjLon, ObjLat, Lon, Lat);
            ConvertFactor = convert.angular('rad',LonLatUnits);
            Dist          = Dist.*ConvertFactor;
            PA            = PA.*ConvertFactor;
            
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
            % Input  : - An AstroCatalog object.
            %          - CCDSEC [Xmin, Xmax, Ymin, Ymax]
            %          * ...,key,val,...
            %            'ColX' - Name, or cell array (from which to select
            %                   first that appears) of X column name on
            %                   which to operate the cropping.
            %                   Default is Obj.DefNamesX
            %            'ColY' - Name, or cell array (from which to select
            %                   first that appears) of Y column name on
            %                   which to operate the cropping.
            %                   Default is Obj.DefNamesY
            %            'AddX' - A cell array of additional columns which
            %                   contains X coordinates and requires updating
            %                   after cropping. Default is {}.
            %            'AddY' - Like 'AddX', but for Y coordinates.
            %            'UpdateXY' - A logical indicating if to update the
            %                   X/Y  coordinates, such that the new
            %                   coordinates will refer to the new cropped
            %                   image. Default is true.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - An AstroCatalog object with the updated catalog.
            % Author : Eran Ofek (Jul 2021)
            % Example: AC = AstroCatalog({rand(100,3).*100}, 'ColNames',{'XWIN','YWIN','Flux'});
            %          Result = cropXY(AC, [1 50 1 50])
            %          Result = cropXY(AC, [81 100 41 70],'AddX',{'Flux'})
            %          Result = cropXY(AC, [81 100 41 70; 1 50 1 50]); % multiple crops of a single catalog
            
            arguments
                Obj
                CCDSEC
                Args.ColX                          = Obj.DefNamesX; %{'X','XWIN_IMAGE','XWIN','XPEAK','X_PEAK'};
                Args.ColY                          = Obj.DefNamesY; %{'Y','YWIN_IMAGE','YWIN','YPEAK','Y_PEAK'};
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
        
    end
    
    methods % interact with Mask images
        function Result = insertFlagColFromMask(Obj, Mask, Args)
            % Insert/replace FLAGS column in AstroCatalog as populated from a MaskImage object.
            % Description: Given an AstroCatalog and a MaskImage objects,
            %       for each source in the AstroCatalog, search the FLAGS
            %       in the MaskImage (bitwise or/and) within a user
            %       provided radius (default is 3 pix). These FLAGS will be
            %       inserted (or replaced) in the AstroCatalog object as a
            %       new column.
            %       Note that by default, unless the Catalog is a table
            %       type, the FLAGS typw will be casted to have the same
            %       type as the Catalog data.
            % Input  : - A single-element AstroCatalog object.
            %          - A single-element MaskImage object.
            %          * ...,key,val,...
            %            'CastToType' - A logical indicating if to cast the
            %                   type of the inserted Flags column to be the
            %                   same as the AstroCatalog object Catalog
            %                   data (if Catalog is of table type, then
            %                   this is ignored). Default is true.
            %            'ColX' - A cell array of X column names. Will
            %                   select the first exitsing column name.
            %                   Default is AstroCatalog.DefNamesX.
            %            'ColY' - - A cell array of Y column names. Will
            %                   select the first exitsing column name.
            %                   Default is AstroCatalog.DefNamesY.
            %            'FlagColName' - The flags column name.
            %                   Default is 'FLAGS'.
            %            'ColPos' - If Flags column doesn't exist, then
            %                   this is the position in which to insert the
            %                   new column. Default is Inf.
            %            'CreateNewObj' - - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            %            'BitWiseOperator' - Bit-wise Operator: ['or'] | 'and'
            %            'HalfSize' - Cutout half size (actual size will be
            %                   1+2*HalfSize. Default is 3.
            %            'CutAlgo' - Algorithm: ['mex'] | 'wmat'.            
            %            'IsCircle' - If true then will pad each cutout
            %                   with NaN outside the HalfSize radius.
            %                   Default is false.
            %            'DataProp' - Data property from which to extract
            %                   the cutouts. Default is 'Image'.
            % Example: AC = AstroCatalog({rand(100,2).*1024}, 'ColNames',{'X','Y'});
            %          MI = MaskImage({uint32(ones(1024,1024).*5)});
            %          insertFlagColFromMask(AC, MI)
            
            arguments
                Obj(1,1)
                Mask(1,1) MaskImage
                Args.CastToType(1,1) logical    = true;
                Args.ColX                       = AstroCatalog.DefNamesX;
                Args.ColY                       = AstroCatalog.DefNamesY;
                Args.FlagColName char           = 'FLAGS';
                Args.ColPos                     = Inf;
                Args.CreateNewObj               = [];
                Args.BitWiseOperator            = 'or';
                % bitwise_cutouts arguments
                Args.HalfSize                   = 3;
                Args.CutAlgo                    = 'wmat';  % 'mex' | 'wmat'
                Args.IsCircle(1,1) logical      = false;
                Args.DataProp                   = 'Image';
                
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
            % get X, Y from AstroCatalog
            XY = getXY(Obj, 'ColX',Args.ColX, 'ColY',Args.ColY);
            
            % get the Flags from the mask image
            FlagColData = bitwise_cutouts(Mask, XY, Args.BitWiseOperator, 'HalfSize',Args.HalfSize,...
                                                                          'CutAlgo',Args.CutAlgo,...
                                                                          'IsCircle',Args.IsCircle,...
                                                                          'DataProp',Args.DataProp);
            % cast the Type of the new column into the type of the Catalog
            % (if array). 
            % If catalog is table then ignore
            if Args.CastToType && ~istable(Obj.Catalog)
                FlagColData = cast(FlagColData, class(Obj.Catalog));
            end
            
            % add the flags to the AstroCatalog
            Obj.replaceCol(FlagColData, Args.FlagColName, Args.ColPos);
            
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
    
    
    methods (Static)
        function help
            % show mlx help file for AstroCatalog
            open manuals.AstroCatalog
        end
    end

    
    methods (Static) % perfTest
        function Result = perfTest
            % perfTest for the AstroCatalog class
            io.msgStyle(LogLevel.Test, '@start', 'AstroCatalog perfTest started')
            
            DataSampleDir = tools.os.getTestDataDir;
            PWD = pwd;
            cd(DataSampleDir);
            
            

            cd(PWD);           
            io.msgStyle(LogLevel.Test, '@passed', 'AstroCatalog perfTest passed')
            Result = true;
        end
    end
    
    
    methods (Static) % unitTest
        function Result = unitTest
            % unitTest for the AstroCatalog class
            io.msgStyle(LogLevel.Test, '@start', 'AstroCatalog test started')
            
            DataSampleDir = tools.os.getTestDataDir;
            PWD = pwd;
            cd(DataSampleDir);
            
            % constructor
            
            % @FIX - @Eran - Where is this table? We need it also on
            % Windows - Need instructions where from to take it and where
            % to put it
            
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [Result] = isCooPix(AC);
            
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [Result, Units] = isCooSphere(AC);
            
            
            io.msgLog(LogLevel.Test, 'testing AstroCatalog constructor');
            AC = AstroCatalog({'asu.fit','asu.fit'}, 'HDU',2);
            [CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(AC);
            if AC(1).ColX~=AC(2).ColX
                error('ColX in the two elements should have been identical');
            end
            if AC(1).ColY~=AC(2).ColY
                error('ColY in the two elements should have been identical');
            end
            
            % sort
            io.msgLog(LogLevel.Test, 'testing AstroCatalog sort');
            AC(1).SortByCol = AC(1).ColY;
            AC(2).SortByCol = 'DEJ2000';
            AC.sortrows(AC(1).ColY);
            if ~issorted(AC(1).Catalog(:,AC(1).ColY))
                error('catalog is not sorted');
            end
            
            % sort using the SortByCol property
            io.msgLog(LogLevel.Test, 'testing AstroCatalog SortByCol');
            AC = AstroCatalog({'asu.fit','asu.fit'}, 'HDU',2);
            AC.getCooTypeAuto;
            AC(1).SortByCol = AC(1).ColY;
            AC(2).SortByCol = 'DEJ2000';
            AC.sortrows;
            if ~issorted(AC(1).Catalog(:,AC(1).ColY))
                error('catalog is not sorted');
            end
            
            % bounding circle
            io.msgLog(LogLevel.Test, 'testing AstroCatalog boundingCircle');
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [Result] = imProc.match.coneSearch(AC, [1 1], 'Radius',3600.*10);
            [CircleX, CircleY, CircleRadius] = boundingCircle(Result);
            if abs(CircleX-1)>0.1 ||  abs(CircleY-1)>0.1 || abs(CircleRadius-10)>0.5
                error('Problem with catalogBoundingCircle');
            end
            
            % cropXY
            io.msgLog(LogLevel.Test, 'testing AstroCatalog cropXY');
            AC = AstroCatalog({rand(100,3).*100}, 'ColNames',{'XWIN','YWIN','Flux'});
            Result = cropXY(AC, [1 50 1 50]);
            Result = cropXY(AC, [81 100 41 70],'AddX',{'Flux'});
            Result = cropXY(AC, [81 100 41 70; 1 50 1 50]); % multiple crops of a single catalog

            % getLonLat
            io.msgLog(LogLevel.Test, 'testing AstroCatalog getLonLat');
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [Lon,Lat] = getLonLat(AC);
            [Lon,Lat] = getLonLat(AC,'rad');
            
            % getXY
            io.msgLog(LogLevel.Test, 'testing AstroCatalog getXY');
            AC=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});
            [X,Y] = getXY(AC);

            % insertFlagColFromMask
            io.msgLog(LogLevel.Test, 'testing AstroCatalog insertFlagColFromMask');
            AC = AstroCatalog({rand(100,2).*1024}, 'ColNames',{'X','Y'});
            MI = MaskImage({uint32(ones(1024,1024).*5)});
            insertFlagColFromMask(AC, MI);
            
            % cone_search? what is this?
%             io.msgLog(LogLevel.Test, 'testing AstroCatalog/catsHTM cone_search?');
%             C=catsHTM.cone_search('GAIADR2',1,1,100,'OutType','astrocatalog'); % <--- doesn't work
%             [RA, Dec, PM_RA, PM_Dec, Plx, RV] = getRADecPM(C)
            
            
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
            io.msgLog(LogLevel.Test, 'testing AstroCatalog getCooTypeAuto');
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            AC.getCooTypeAuto;
            io.msgLog(LogLevel.Test, 'testing AstroCatalog plotMapFun');
            AC.plotMapFun('aitoff',@plotm,{},'.','MarkerSize',1);
            
            % convertCooUnits
            io.msgLog(LogLevel.Test, 'testing AstroCatalog convertCooUnits');
            AC=AstroCatalog({'asu.fit','asu.fit'},'HDU',2);
            AC.convertCooUnits('deg');
            
            % getCoo
            io.msgLog(LogLevel.Test, 'testing AstroCatalog getCoo');
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [RA, Dec] = AC.getCoo('deg');
            [RADec]   = AC.getCoo('rad');

            % sphere_dist
            AC=AstroCatalog({'asu.fit'},'HDU',2);
            [Dist, PA] = sphere_dist(AC,1,1);
                                   
            cd(PWD);           
            io.msgStyle(LogLevel.Test, '@passed', 'AstroCatalog test passed')
            Result = true;
        end
    end
    
end
     