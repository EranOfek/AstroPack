% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Base < handle
    % Properties
    properties (SetAccess = public)
        UserData    % Optional user data (any type)
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Base()
        end
    end

    
    methods % Copy
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
        
        
        function Obj2 = copyProp(Obj1, Obj2, PropList)
            % Copy the content of properties from object1 into object2.
            % Input  : - Obj1 (from which to copy)
            %          - Obj2
            %          - A cell array or a string array of properties to
            %            copy.
            % Output : - Obj2
            % Author : Eran Ofek (Apr 2021)
            % Example: Obj2 = copyProp(Obj1, Obj2, {'UserData'})
           
            if ischar(PropList)
                PropList = {PropList};
            end
            Nprop = numel(PropList);
            Nobj1 = numel(Obj1);
            Nobj2 = numel(Obj2);
            for Iobj2=1:1:Nobj2
                Iobj1 = min(Iobj2, Nobj1);
                for Iprop=1:1:Nprop
                    Obj2(Iobj2).(PropList{Iprop}) = Obj1(Iobj1).(PropList{Iprop});
                end
            end
        end
    end
    
    %----------------------------------------------------------------------   
    methods(Static) % Unit test
        
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'Base test started');
            a = Base();
            a.UserData = 123;            
            b = a.copyObject();
            assert(a.UserData == b.UserData);
            b.UserData = 0;
            assert(a.UserData ~= b.UserData);
            
            io.msgLog(LogLevel.Test, 'Base test passed');
            Result = true;
        end
    end    
    
end

