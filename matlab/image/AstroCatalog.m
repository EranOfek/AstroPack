

classdef AstroCatalog < AstroTable
    properties
        ColX                                                            = [];
        ColY                                                            = [];
        CooType char    {mustBeMember(CooType,{'pix','sphere',''})}     = '';
        CooUnits char   {mustBeMember(CooUnits,{'deg','rad'})}          = 'deg';
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
%         function set.CooType(Obj, CooType)
%             % setter for CooType
%             % Obj.CooType = 'auto' will automatically deduce the CooType
%             % and set the ColX, ColY properties.
%             % If RA/Dec exist in ColNames than will set CooType to 'sphere'
%             
%             % search synonyms in config file
% %             switch lower(CooType)
% %                 case 'auto'
% %                     [CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(Obj, 'CaseSens', false);
% %                     
% %                     Obj.CooType = CooType;
% %                     Obj.ColX    = IndInCellX;
% %                     Obj.ColY    = IndInCellY;
% %                     
% %                 otherwise
% %                     Obj.CooType = CooType;
% %             end
%         end
        
%         function Obj = get.CooType(Obj)
%             %
%            
%             
%             CooType = Obj.CooType;
%             switch lower(CooType)
%                 case 'auto'
%                     [CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(Obj, 'CaseSens', false);
%                     
%                     Obj.CooType = CooType;
%                     if isempty(Obj.ColX)
%                         Obj.ColX    = IndInCellX;
%                     end
%                     if isempty(Obj.ColY)
%                         Obj.ColY    = IndInCellY;
%                     end
%                     
%                 otherwise
%                     %Obj.CooType = CooType;
%             end
%             
%         end
%         
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
                ConvFactor = convert.angular(CooUnitsX, Val, 1);
                Obj.Catalog(:,ColIndX) = Obj.Catalog(:,ColIndX).*ConvFactor;
                Obj.ColUnits{ColIndX} = Val;
            end
            if isempty(CooUnitsY)
                % set coordinate units without converting the values
                Obj.ColUnits{ColIndY} = Val;
            else
                ConvFactor = convert.angular(CooUnitsY, Val, 1);
                Obj.Catalog(:,ColIndY) = Obj.Catalog(:,ColIndY).*ConvFactor;
                Obj.ColUnits{ColIndY} = Val;
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
                                                                    Coo(:,1), Coo(:,2), RadiusRad);
                    case 'pix'
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
        function [MatchedObj, UnMatchedObj] = match(Obj1, Obj2, Args)
            %
            
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
        function H = plot(Obj, Projection, varargin)
            %
        end
        
        function H = scatter(Obj, Projection, varargin)
            %
        end
        
    end
    
    
end
     