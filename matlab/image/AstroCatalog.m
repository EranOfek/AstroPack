

classdef AstroCatalog < AstroTable
    properties
        ColX
        ColY
        CooType char    {mustBeMember(CooType,{'pix','sphere','cosd','auto'})} = 'auto';
        CooUnits char   {mustBeMember(CooUnits,{'deg','rad'})} = 'deg';
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
    
    methods % search by coordinates/name
        
    end
    
    methods % match two AstroCatalog
        
    end
    
    methods % match against external catalog
        
    end
    
    methods % plotting
        
    end
    
    
end
        