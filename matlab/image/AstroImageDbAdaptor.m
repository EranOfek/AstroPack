% AstroImage database adaptor

classdef AstroImageDbAdaptor < Component
    
    properties (Hidden, SetAccess = public)

    
    end
    

    methods
    
        % Constructor
        function Obj = AstroImageDbAdaptor           
            
        end

    end
    
 
    methods
    
        function Result = writeImage(Image, WriteData)
            % 
        end
        
        
        function Result = writeHeader(Image, WriteData)
            % 
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
        
        
        function Result = getTableName(TableId)
           % Convert table enum to string 
           
        end
        
        
        function Result = getTableId(TableName)
            % Convert table name to enum
            
            
        end
    end
    
    
    % static methods
    methods (Static)

        function Result = unitTest()
            Astro = AstroImage;
            
        end
    end
    
    

end

            
