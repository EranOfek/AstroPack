
classdef InternalHandleClass < matlab.mixin.Copyable % handle
    
    properties
        InMat
        InMat1
        InMat2
        InMat3
        InSmall
        Uuid
    end

    
    methods
        function Obj = InternalHandleClass(varargin)
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
            fprintf('InternalHandleClass created: %s\n', Obj.Uuid);            
        end
        
        
        function delete(Obj)
            fprintf('InternalHandleClass deleted: %s\n', Obj.Uuid);            
        end

        
        function NewObj = serCopy(Obj)
            ObjByteArray = getByteStreamFromArray(Obj);
            NewObj       = getArrayFromByteStream(ObjByteArray);
        end
        
        
        function Obj = sin(Obj)
            Obj.InMat = sin(Obj.InMat.^2).^2;
        end        
        
    end
    
    
    methods(Access = protected)    
        function NewObj = copyElement(Obj)
            cprintf('Magenta', 'InternalHandleClass.copyElement started\n');
            tic
            
            % Make a shallow copy of all properties
            NewObj = copyElement@matlab.mixin.Copyable(Obj);
         
            % Make a deep copy of the DeepCp object
            %NewObj.Mat1 = copy(Obj.Mat1);
            
            % Make a deep copy of the DeepCp object
            % Generate Uuid using java package
            Temp = java.util.UUID.randomUUID;
            NewObj.Uuid = string(Temp.toString()).char;
            fprintf('Created new InternalHandleClass.Uuid: %s\n', NewObj.Uuid);                        
            
            cprintf('Magenta', 'InternalHandleClass.copyElement: %0.4f\n\n', toc);
        end
    end
end

