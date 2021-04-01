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
            % By value copy of an object ans its content
            % Input  : - Any object that inherits from Base
            %          * ...,key,val,...
            %            'ClearProp' - A cell array of properties which
            %                   will be cleared after the copy operation.
            %                   default is {}.
            % Output : - A copy of the original object.
            % Example: NC=AC.copyObject('ClearProp',{'Catalog'});
            
            arguments
                Obj
                Args.DeepCopy(1,1) logical          = true;
                Args.ClearProp                      = {};
            end
            
            if ~iscell(Args.ClearProp) && ~isstring(Args.ClearProp)
                Args.ClearProp = {Args.ClearProp};
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

