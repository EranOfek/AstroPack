

classdef AstroCatalog < AstroTable
    properties
        ColX                                                            = [];
        ColY                                                            = [];
        CooType char    {mustBeMember(CooType,{'pix','sphere','auto'})} = 'auto';
        CooUnits char   {mustBeMember(CooUnits,{'deg','rad'})}          = 'deg';
    end
    
    properties (Hidden, Constant)
        DefNamesX cell                   = {'X','X_IMAGE','XWIN_IMAGE','X1','X_PEAK'};
        DefNamesY cell                   = {'Y','Y_IMAGE','YWIN_IMAGE','Y1','Y_PEAK'};
        DefNamesRA cell                  = {'RA','ALPHA','ALPHAWIN_J2000','ALPHA_J2000','RA_J2000','RAJ2000','RightAsc'};
        DefNamesDec cell                 = {'Dec','DELTA','DELTAWIN_J2000','DELTA_J2000','DEC_J2000','DECJ2000','Declination'};
        DefNamesPMRA cell                = {'PMRA'};
        DefNamesPMDec cell               = {'PMDec'};
        DefNamesPlx cell                 = {'Plx'};
        DefNamesMag cell                 = {'Mag','PSF_MAG','MAG_PSF'};
    end
    
    
    methods % constructor
        function Obj = AstroCatalog(varargin)
            % Constructor for Astrocatalog 
            
            % construct AstroTable
            Obj = AstroTable(varargin{:});
        end
    end
    
    methods % setters/getters
        function set.CooType(Obj, CooType)
            % setter for CooType
            % Obj.CooType = 'auto' will automatically deduce the CooType
            % and set the ColX, ColY properties.
            % If RA/Dec exist in ColNames than will set CooType to 'sphere'
            
            % search synonyms in config file
            warning('Search synonym in config file does not operational yet');
            switch lower(CooType)
                case 'auto'
                    SynonymCell_RA  = Obj.DefNamesRA;
                    SynonymCell_Dec = Obj.DefNamesDec;
                    
                    [Name_RA,  IndInCell_RA,  IndInSynonym_RA]  = AstroTable.searchSynonym(Obj.ColNames, SynonymCell_RA,  'CaseSens', Args.CaseSens);
                    [Name_Dec, IndInCell_Dec, IndInSynonym_Dec] = AstroTable.searchSynonym(Obj.ColNames, SynonymCell_Dec, 'CaseSens', Args.CaseSens);
                    if isempty(IndInCell_RA) || isempty(IndInCell_Dec)
                        CooType = 'pix';
                    else
                        % spherical coordinates found in AstroCatalog
                        CooType = 'sphere';
                    end
                
                    switch lower(CooType)
                        case 'pix'
                            SynonymCell_X = Obj.DefNamesX;
                            SynonymCell_Y = Obj.DefNamesY;

                            [NameX, IndInCellX, IndInSynonymX] = AstroTable.searchSynonym(Obj.ColNames, SynonymCell_X, 'CaseSens', Args.CaseSens);
                            [NameY, IndInCellY, IndInSynonymY] = AstroTable.searchSynonym(Obj.ColNames, SynonymCell_Y, 'CaseSens', Args.CaseSens);
                        case 'sphere'
                            SynonymCell_RA  = Obj.DefNamesRA;
                            SynonymCell_Dec = Obj.DefNamesDec;

                            [NameX, IndInCellX, IndInSynonymX] = AstroTable.searchSynonym(Obj.ColNames, SynonymCell_RA,  'CaseSens', Args.CaseSens);
                            [NameY, IndInCellY, IndInSynonymY] = AstroTable.searchSynonym(Obj.ColNames, SynonymCell_Dec, 'CaseSens', Args.CaseSens);
                        otherwise
                            error('Unknown/illegal CooType option');
                    end

                    Obj.CooType = 'sphere';
                    Obj.ColX    = IndInCellX;
                    Obj.ColY    = IndInCellY;
                    
                otherwise
                    Obj.CooType = CooType;
            end
        end
        
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
                    error('CooUnits for columns %d and %d are inconsistent,'ColIndX, ColIndY);
                end
                Units        = CooUnitsX;
                Obj.CooUnits = Units;
            else
                Units = Obj.CooUnits;
            end

            
            
        end
        
        function set.CooUnits(Obj, Val)
            % setter for CooUnits (only if ColUnits for coordinates in Catalog are empty
            
            ColIndX = colname2ind(Obj, Obj.ColX);
            ColIndY = colname2ind(Obj, Obj.ColY);
            CooUnitsX = Obj.ColUnits{ColIndX};
            CooUnitsY = Obj.ColUnits{ColIndY};
            if isempty(CooUnitsX) && isempty(CooUnitsY)
                Obj.ColUnits(ColIndX) = Val;
                Obj.ColUnits(ColIndY) = Val;
            else
                warning('Coordinates are already set to %s and %s',CooUnitsX, CooUnitsY);
            end
            
            
        end
    end
    
    methods % column names
        function Obj = convertCooUnits(Obj,Units)
            % convert coordinates Units (only if in angular units)
            % Example: Obj.convertCooUnits('deg')
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if strcmp(Obj(Iobj).CooType,'sphere') && ~strcmp(Obj(Iobj).CooUnits,Units)
                    ColIndX = colname2ind(Obj(Iobj), Obj(Iobj).ColX);
                    ColIndY = colname2ind(Obj(Iobj), Obj(Iobj).ColY);

                    ConvFactor = convert.angular(Obj(Iobj).CooUnits,Units);
                    Obj(Iobj).Catalog(:,ColIndX) = Obj(Iobj).Catalog(:,ColIndX).*ConvFactor;
                    Obj(Iobj).Catalog(:,ColIndY) = Obj(Iobj).Catalog(:,ColIndY).*ConvFactor;
                    Obj(Iobj).ColUnits{ColIndX}  = Units;
                    Obj(Iobj).ColUnits{ColIndY}  = Units;
                    Obj(Iobj).CooUnits           = Units;

                end
                % Otherwise: Coordinates are already in requested Units
            end
                
            
            
        end
    end
    
    methods % search by coordinates/name
        function Obj = coneSearch(Obj, Coo, Args)
            %
            
            arguments
                Obj
                Coo
                Args.Radius
                Args.Shape char                  = 'circle';
                Args.CooUnits char               = 'deg';
                Args.OutIsObj(1,1) logical       = true;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                
                [Ind,FlagUnique,FlagFound] = VO.search.search_sortedlat_multi(Obj(Iobj).Catalog ,0.5,0.5,0.01)
                ...
                
            end
            
            
            
            
        end
        
        function [Obj, Flag] = inPolygon(Obj, PolyCoo, Args)
            %
            
        end
        
        
    end
    
    methods % match two AstroCatalog
        function [MatchedObj, UnMatchedObj] = match(Obj1, Obj2, Args)
            %
            
        end
        
        
    end
    
    methods % match against external catalog
        
    end
    
    methods % plotting
        
    end
    
    
end
     