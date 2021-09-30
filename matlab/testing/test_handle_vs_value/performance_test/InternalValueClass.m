
classdef InternalValueClass
    
    properties
        InMat
        InMat1
        InMat2
        InMat3
        InSmall
        Uuid
    end

    
    methods
        function Obj = InternalValueClass(varargin)
            % Constructor
            if numel(varargin) > 0
                Obj.InMat  = ones(1000, 10000);
                Obj.InMat1 = ones(1000, 10000);
                Obj.InMat2 = ones(1000, 10000);
                Obj.InMat3 = ones(1000, 10000);
                Obj.InSmall = ones(100, 100);
            end
            
            % Generate Uuid using java package
            Temp = java.util.UUID.randomUUID;
            Obj.Uuid = string(Temp.toString()).char;
            fprintf('InternalValueClass created: %s\n', Obj.Uuid);
        end
        
        
        function delete(Obj)
            fprintf('InternalValueClass deleted: %s\n', Obj.Uuid);            
        end
       
        
        function Obj = sin(Obj)
            Obj.InMat = sin(Obj.InMat.^2).^2;
        end                
    end
end

