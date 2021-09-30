
classdef HandleClassCopy < matlab.mixin.Copyable % handle
    
    properties
        Mat
        Mat1
        Mat2
        Mat3
        Small
        InObj
        Uuid
    end

    
    methods
        function Obj = HandleClassCopy(varargin)
            % Constructor
            if numel(varargin) > 0
                Obj.Mat  = ones(1000, 10000);
                Obj.Mat1 = ones(1000, 10000);
                Obj.Mat2 = ones(1000, 10000);
                Obj.Mat3 = ones(1000, 10000);                
                Obj.Small = ones(100, 100);
                Obj.InObj = InternalHandleClass(1);
            end
            
            % Generate Uuid using java package
            Temp = java.util.UUID.randomUUID;
            Obj.Uuid = string(Temp.toString()).char;
            fprintf('HandleClassCopy created: %s\n', Obj.Uuid);            
        end
        
        
        
        function delete(Obj)
            fprintf('HandleClassCopy deleted: %s\n', Obj.Uuid);            
        end

        
        function NewObj = serCopy(Obj)
            ObjByteArray = getByteStreamFromArray(Obj);
            NewObj       = getArrayFromByteStream(ObjByteArray);
        end
        
        
        function Obj = sin(Obj)
            Obj.Mat = sin(Obj.Mat.^2).^2;
        end        
        
    end
    
    
    methods(Access = protected)
      
        function NewObj = copyElement(Obj)
            cprintf('Magenta', 'HandleClassCopy.copyElement started\n');
            tic
            
            % Make a shallow copy of all properties
            % This copies all properties which are NOT classes
            NewObj = copyElement@matlab.mixin.Copyable(Obj);
                               
            % Make a deep copy of the DeepCp object
            NewObj.InObj = copy(Obj.InObj);
            
            Temp = java.util.UUID.randomUUID;
            NewObj.Uuid = string(Temp.toString()).char;
            fprintf('Created new HandleClassCopy.Uuid: %s\n', NewObj.Uuid);                                    
            
            cprintf('Magenta', 'HandleClassCopy.copyElement: %0.4f\n\n', toc);
        end
   end
end

