

classdef AstroCatalog < AstroTable
    properties
        ColX                                                            = [];
        ColY                                                            = [];
        CooType char    {mustBeMember(CooType,{'pix','sphere','auto'})} = 'auto';
        CooUnits char   {mustBeMember(CooUnits,{'deg','rad'})}          = 'deg';
    end
    
    properties (Hidden, constant)
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
    
    methods % column names
        function [ColRA, ColDec] = getColCoo(Obj, ReturnName)
            % get coordinates (x/y or ra/dec) column names or indices
            
            arguments
                Obj(1,1)
                ReturnName(1,1) logical       = false;
            end
            
            % search synonyms in config file
            warning('Search synonym in config file does not operational yet');
            switch lower(Obj.CooType)
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
                otherwise
                    CooType = Obj.CooType;
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
                    
            
            
        end
    end
    
    methods % search by coordinates/name
        function [Obj, Flag] = coneSearch(Obj, Coo, Args)
            %
            
            arguments
                Obj
                Coo
                Args.Radius
                Args.Shape char                  = 'circle';
                Args.CooUnits char               = 'deg';
                Args.OutIsObj(1,1) logical       = true;
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
        