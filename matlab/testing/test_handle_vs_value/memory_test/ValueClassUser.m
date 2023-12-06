
classdef ValueClassUser
    
    properties
        MatCls
        Uuid
    end

    
    methods
        function Obj = ValueClass(varargin)
            % Constructor
            if numel(varargin) > 0
                Obj.MatCls  = HandleClass();
            end
            
            % Generate Uuid using java package
            Temp = java.util.UUID.randomUUID;
            Obj.Uuid = string(Temp.toString()).char;
            fprintf('ValueClassUser created: %s\n', Obj.Uuid);
        end
        
        
        function delete(Obj)
            fprintf('ValueClassUser deleted: %s\n', Obj.Uuid);            
        end
                
        
        function Obj = Func(Obj)
            if isempty(Obj.MatCls)
                fprintf('ValueClassUser: MatCls not allocated\n');
            else
                fprintf('ValueClassUser: MatCls is allocated\n');
            end
        end                
    end
end

