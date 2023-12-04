% MatchedSources class - A container for matrices of matched sources
% Properties:
% Methods:

%
% #functions (autogen)
%
% #/functions (autogen)
%

classdef MagCalib < Component
    properties
        Inst AstroCatalog   % e.g., RA, Dec, Mags, X, Y,...
        Cat AstroCatalog   % e.g., RA, Dec, Mags, Err,...
        
        IsFlux logical    = false;
        
        % PhotSol
        
    end
    
    properties (Dependent)
    end
    properties (Dependent)
        
    end
    
    properties (Constant)
       
    end
   
    methods % constructor
        
    end
    
    methods % setters/getters
        
    end
    
    methods (Static)
        function Result = getCalibCat(AC, Args)
            % Given AstroCatalog with instrumental mag and coo, get calib cat
            
        end
                
    end
    
    methods
        
    end
    
    methods (Static) % unitTest
        Result = unitTest()
            % unitTest for MatchedSources class
                    
    end
end
