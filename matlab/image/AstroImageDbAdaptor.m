% AstroImage database adaptor

classdef AstroImageDbAdaptor < Component
    
    properties (Hidden, SetAccess = public)

    
    end
    

    methods
    
        % Constructor
        function Obj = AstroImageDbAdaptor           
            
        end

    end
    
 
    
    % Setters/Getters
    methods
        function Obj = set.Image(Obj, Data)
            Obj.BImage.setData(Data); %#ok<MCSUP>
        end
        
        function Data = get.Image(Obj)
            Data = Obj.BImage.getData();
        end        
    end
    
    % static methods
    methods (Static)
       
    end
    
    % 
    
    % setters/getters
    methods
        
    end
    
    % static methods
    methods (Static)

        function Result = unitTest()
            Astro = AstroImage;
            
        end
    end
    
    

end

            
