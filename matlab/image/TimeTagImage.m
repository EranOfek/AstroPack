% Class for time-tagged events table/images

classdef TimeTagImage < Component
    properties (Dependent)
        Image
    end
    properties
        ImageData(1,1) ImageComponent
        EventsTable(1,1) AstroCatalog
        Header(1,1) AstroHeader         % maybe redundent if part of AstroImage
        BadTimes(:,2)                   = zeros(0,2);
        GoodTimeFlag(:,1) logical       = true(0,1);
        UseFlag(:,1) logical            = true(0,1);
    end
    
    methods % constructor
        function Obj = TimeTagImage(varargin)
            % what to read?
            
            Obj.ImageData   = ImageComponent;
            Obj.EventsTable = AstroCatalog;
            
        end
        
    end
    
    methods % setters/getters
        function Result = get.Image(Obj)
            % getter for Image
            Result = Obj.ImageData.Image;
        end
   
        function Obj = set.Image(Obj, Val)
            % setter for Image
            Obj.ImageData.Image = Val;
        end
                
    end
    
    methods (Static)    % static functions
        function Result = evnts2image(Table, Args)
            %
            
            
        end
        
    end
    
    
    methods % good times and selections
        function findGoodTimes
            
        end
        
        function selectEnergy
            
        end
        
        
        
    end
    
    methods % astrometry
        function pix2coo
            
        end
        
        function coo2pix
            
        end
        
    end
        
    
end
            