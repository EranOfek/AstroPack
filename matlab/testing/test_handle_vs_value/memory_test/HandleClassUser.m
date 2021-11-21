
classdef HandleClassUser < handle
    
    properties
        MatCls
        Uuid
    end

    
    methods
        function Obj = HandleClassUser(varargin)
            % Constructor
            if numel(varargin) > 0
                Obj.MatCls = HandleClass();
            end
            
            % Generate Uuid using java package
            Temp = java.util.UUID.randomUUID;
            Obj.Uuid = string(Temp.toString()).char;
            fprintf('HandleClassCopy created: %s\n', Obj.Uuid);            
        end
       
        
        function Obj = Func(Obj)
            if isempty(Obj.MatCls)
                fprintf('HandleClassUser: MatCls not allocated\n');
            else
                fprintf('HandleClassUser: MatCls is allocated\n');
            end
        end   
    end
   
end

