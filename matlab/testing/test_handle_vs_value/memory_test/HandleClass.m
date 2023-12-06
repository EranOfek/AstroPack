
classdef HandleClass < handle
    
    properties
        BigMat
        Uuid
    end

    
    methods
        function Obj = HandleClass(varargin)
            % Constructor
            Obj.BigMat  = ones(100000, 10000);
            
            % Generate Uuid using java package
            Temp = java.util.UUID.randomUUID;
            Obj.Uuid = string(Temp.toString()).char;
            fprintf('HandleClass created: %s\n', Obj.Uuid);            
        end
        
        
        function delete(Obj)
            fprintf('HandleClass deleted: %s\n', Obj.Uuid);            
        end

       
        function Obj = Func(Obj)
            if isempty(Obj.BigMat)
                fprintf('HandleClass: BigMat not allocated\n');
            else
                fprintf('HandleClass: BigMat: %d', numel(Obj.BigMat));
            end
        end   
        
    end
end

