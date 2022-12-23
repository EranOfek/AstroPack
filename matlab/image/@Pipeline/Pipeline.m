% Pipeline - 
% Properties :
%       
% Functionality :
%

classdef Pipeline < Component
    properties (SetAccess = public)
        CI CalibImages    
        StampSize         = [];
    end
    
    methods % Constructor
       
    end
    
    methods % Setters/Getters
    
    end
    
    methods (Static)  % static methods
     
        
    end
    
    methods % Prepare pipeline for executation
        function Obj = loadCalibImages(Obj, Args)
            %
            
            arguments
                Obj
                Args.CalibDir = [];
            end
            
        end
        
    end
    
    methods % generic pipelines
        % prep dark/bias
        function Obj=createBias(Obj, Args)
            %
           
            arguments
                Obj
                Args.CI                     = [];  % pass a CalibImages object
                Args.ImagesPath             = @pipeline.last.constructCamDir;  % bias images are in this dir (empty=current dir)
                Args.ImagePathArgs          = {1,'Node',1, 'SubDir','new', 'ProjNamebase','LAST'};
                Args.FileNameType           = 'dark';
                Args.UseFileNames logical   = true;
                Args.UseConfigArgs logical  = true;
                Args.Args                   = {};
            end
            
            if ~isempty(Args.CI)
                Obj.CI = Args.CI;
            else
                
            
            
        end
        
        % prep flat
        
        
        % getSources (bias, flat,..., back, sources, astrometry)
        
        % match sources
        
        % coadd
    end

    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
