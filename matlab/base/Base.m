% Component base class
%       This is the base class from which all the classes in AstroPack
%       hinerits.
% Functionality:
%       copyObject - by value deep copy of an object/
%       copyProp - Copy specific properyies from one object to another
%--------------------------------------------------------------------------

% Making a DEEP Copy: Copy each property value and assign it to the new 
% (copied) property. Recursively copy property values that reference handle 
% objects to copy all of the underlying data.
%
% Making a SHALLOW Copy: Copy each property value and assign it to the new 
% (copied) property. If a property value is a handle, copy the handle but 
% not the underlying data.
%
% https://www.mathworks.com/help/matlab/ref/matlab.mixin.copyable-class.html
% < matlab.mixin.Copyable

classdef Base < handle
    % Base class for all objects 
    
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
                
            % Deep copy
            if Args.DeepCopy
                % Copy using serializing/deserializing (@FFU - Is there better/faster way?)                
                ObjByteArray = getByteStreamFromArray(Obj);
                NewObj       = getArrayFromByteStream(ObjByteArray);
                
                % @Chen @Todo: Generate unique UUID?
                if isprop(Obj, 'Uuid')
                    
                    % Generate new uuid, note that makeUuid() is a method
                    % of the Component class
                    if ~isempty(Obj(1).Uuid)
                        NewObj.makeUuid();                        
                    end
                    
                    % Set MapKey
                    if ~isempty(Obj(1).MapKey)
                        NewObj.MapKey = NewObj.Uuid;
                    end
                end
                    
            % Shallow copy
            else
                error('Base.copyObject: Shally copy is not supported yet');
            end
            
            % Optionally clear specified properties
            Nobj  = numel(Obj);
            Nprop = numel(Args.ClearProp);
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    NewObj(Iobj).(Args.ClearProp{Iprop}) = [];
                end
            end                   
        end
        
        
        function Target = copyProp(Obj, Target, PropList)
            % Copy the content of properties from object1 into object2.
            % Input  : - Obj1 (from which to copy)
            %          - Target object
            %          - A cell array or a string array of properties to copy.
            % Output : - Target object
            % Author : Eran Ofek (Apr 2021)
            % Example: Obj2 = copyProp(Obj1, Obj2, {'UserData'})
           
            % Convert to cellarray
            if ischar(PropList)
                PropList = {PropList};
            end
            Nprop = numel(PropList);
            Nobj1 = numel(Obj);
            Nobj2 = numel(Target);
            for Iobj2 = 1:1:Nobj2
                Iobj1 = min(Iobj2, Nobj1);
                for Iprop=1:1:Nprop
                    Target(Iobj2).(PropList{Iprop}) = Obj(Iobj1).(PropList{Iprop});
                end
            end
        end
    end
    
    %----------------------------------------------------------------------   
    methods(Static) % Unit test
        
        function Result = unitTest()
            % unitTest for Base class
            io.msgLog(LogLevel.Test, 'Base test started');
            
            % Test copyObject()
            a = Base();
            a.UserData = 123;            
            b = a.copyObject();
            assert(a.UserData == b.UserData);
            b.UserData = 0;
            assert(a.UserData ~= b.UserData);
            
            % Test copyProp()
            c = Base();
            a.copyProp(c, {'UserData'});
            assert(a.UserData == c.UserData);
            
            io.msgLog(LogLevel.Test, 'Base test passed');
            Result = true;
        end
    end    
    
end

