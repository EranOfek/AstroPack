
classdef HandleClass < matlab.mixin.Copyable % handle
    
    properties
        Mat
        Mat1
        Mat2
        Mat3
        Small
        Uuid
    end

    
    methods
        function Obj = HandleClass(varargin)
            % Constructor
            if numel(varargin) > 0
                Obj.Mat  = ones(1000, 10000);
                Obj.Mat1 = ones(1000, 10000);
                Obj.Mat2 = ones(1000, 10000);
                Obj.Mat3 = ones(1000, 10000);                
                Obj.Small = ones(100, 100);
            end
            
            % Generate Uuid using java package
            %Temp = java.util.UUID.randomUUID;
            %Obj.Uuid = string(Temp.toString()).char;
            %fprintf('ValueClass created: %s\n', Obj.Uuid);            
        end
        
        
        
        function delete(Obj)
            %fprintf('ValueClass deleted: %s\n', Obj.Uuid);            
        end

        
        function NewObj = serCopy(Obj)
            ObjByteArray = getByteStreamFromArray(Obj);
            NewObj       = getArrayFromByteStream(ObjByteArray);
        end
          
        
        function Obj = sin(Obj)
            Obj.Mat = sin(Obj.Mat.^2).^2;
        end        
        
    end
end

