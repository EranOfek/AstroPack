
classdef MyClass < handle
% Summary
% Detailed description
   
    properties (Dependent) % Access image data directly        
    end
    
    properties (SetAccess = public)
	
		% Classes
        
        % Data
        Data
    end

    
    methods % Constructor       
        function Obj = MyClass()
            Obj.Data = 0;
        end
        
        function delete(Obj)
        end
        
    end
 
 
    methods % Setters/Getters
        function Obj = set.Data(Obj, Data)            
            Obj.Data = Data;
        end
        
        function Result = get.Data(Obj)            
            Result = Obj.Data;
        end        
    end
    
    
    methods (Static)  % Static methods
       
    end

    
    methods % Unit-Test
        function Result = unitTest()
            Obj = MyClass();
            Result = true;
        end
    end
  

end
