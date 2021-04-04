% NoisyImage handle class 
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: 
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

% "Component" is in folder ../base/

classdef NoisyImage < Component
    
    properties (Hidden, SetAccess = public)
        ImageData ImageComponent
        BackData ImageComponent
        VarData ImageComponent
        
        IsBackSub(1,1) logical              = false;
        BackVarOriginMap
        UseOrigin
        
    end
    
    
    methods % Constructor       
        function Obj = NoisyImage
            % class constructor
            
            
        end
    end 
    
        
    methods % Setters/getters
     
    end
    
    methods (Static) % static methods
       
    end
    
    methods % basic fununctions 
        function Obj = fun_unary(Obj, Operator, OpArgs, Args)
            %
            
        end
        
        function Obj = fun_binary(Obj1, Obj2, Operator, OpArgs, Args)
            %
            
        end
    end
     

    
    
    methods % Unit-Test
        function Result = unitTest()
            Result = true;
        end
    end
    
end



