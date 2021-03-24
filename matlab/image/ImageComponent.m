
classdef ImageComponent < Component
    
    
    properties (Hidden, SetAccess = public)
        Data                                    % e.g., Image matrix
        Scale {mustBeNumeric(Scale)} = [];      %
        ScaleMethod = 'lanczos3';               %
        
        %DataProp cell = {'Data'};              % a cell of properties on which the fun_* methods will be applied
        Virt VirtImage                          % Actual image data
        
        % Storage 
    end

    
    properties (Hidden, SetAccess = public)
  
    end
    
    
    methods
       
        function Obj = ImageComponent
       
            
        end

    end
    
     
    
    % 
    methods
        
    end
    
end



