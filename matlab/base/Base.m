% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Base < handle
    % Properties
    properties (SetAccess = public)
        UserData
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Base()
        end
    end
    
    methods % copy
        function NewObj = copyObject(Obj, Args)
            %
            
            arguments
                Obj
                Args.DeepCopy(1,1) logical          = true;
                Args.ClearProp cell                 = {};
            end
            
            if Args.DeepCopy
                ObjByteArray = getByteStreamFromArray(Obj);
                NewObj       = getArrayFromByteStream(ObjByteArray);
            else
                error('Non deep copy are not supported yet');
            end
            
            Nobj  = numel(Obj);
            Nprop = numel(Args.ClearProp);
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    NewObj(Iobj).(Args.ClearProp{Iprop}) = [];
                end
            end
            
            
        end
    end
    
end

