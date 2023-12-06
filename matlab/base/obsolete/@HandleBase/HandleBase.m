% Component base class
%   This is the base class from which all the classes in AstroPack hinerits.
%
% Functionality:
%   copyObject - by value deep copy of an object/
%   copyProp - Copy specific properyies from one object to another
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
%
% Notes:
%    Singleton: In software engineering, the singleton pattern is a software 
%               design pattern that restricts the instantiation of a class to one 
%               "single" instance. This is useful when exactly one object is needed 
%               to coordinate actions across the system.
%

% 
% Condier using (HandleCompatible)
% https://www.mathworks.com/help/matlab/matlab_oop/supporting-both-handle-and-value-subclasses-handlecompatible.html
% https://www.mathworks.com/help/matlab/matlab_oop/how-to-define-handle-compatible-classes-1.html

classdef HandleBase < matlab.mixin.Copyable  % <handle
    % Base class for all objects

    % Properties
    properties (SetAccess = public)
        UserData    % Optional user data (any type)
    end

    %--------------------------------------------------------
    methods
        function Obj = HandleBase()
            % Constructor
        end
    end


    methods % Copy
        function NewObj = copyObject(Obj, Args)
            % Copy by value an object and its content
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
                    for i=1:1:numel(NewObj)
                        if ~isempty(Obj(1).Uuid)
                            NewObj(i).makeUuid();
                        end
                    end

                    % Set MapKey
                    for i=1:1:numel(NewObj)
                        if ~isempty(Obj(1).MapKey)
                            NewObj(i).MapKey = NewObj(i).Uuid;
                        end
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

        function NewObj = copyRec(Obj)
            % doesn't work
            arguments
                Obj
            end

            PropList = metaclass(Obj);
            
            FN  = {PropList.PropertyList.Name};
            CopyProp = ~[PropList.PropertyList.Dependent] & ~[PropList.PropertyList.Transient];
            Nfn = numel(FN);
            for Ifn=1:1:Nfn
                if CopyProp(Ifn)
                    if isobject(Obj.(FN{Ifn}))
                        if isa(Obj.(FN{Ifn}), 'Configuration') || isa(Obj.(FN{Ifn}), 'MsgLogger') 
                            NewObj.(FN{Ifn}) = Obj.(FN{Ifn});
                        else
                            NewObj.(FN{Ifn}) = copyRec(Obj.(FN{Ifn}));
                        end
                    else
                        NewObj.(FN{Ifn}) = Obj.(FN{Ifn});
                    end
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


        function [Result, CreateNewObj] = createNewObj(Obj, CreateNewObj, Nargout, MinNargout)
            % A utility function for creation of an object new copy based
            % on nargout
            % Input  : - An object
            %          - [], true, false.
            %            If true, create new deep copy
            %            If false, return pointer to object
            %            If [] and Nargout==0 then do not create new copy.
            %            Otherwise, create new copy.
            %          - nargout of function.
            %          - Number of nargout above (>) which to create new
            %            object. Default is 0.
            % Output : - The object (pointer or deep copy).
            %          - The new value of the CreateNewObj argument
            % Author : Eran Ofek (Jul 2021)
            % Example: [Result, CreateNewObj] = createNewObj(Obj, CreateNewObj, Nargout)

            if nargin<4
                MinNargout = 0;
            end

            if isempty(CreateNewObj)
                if Nargout>MinNargout
                    CreateNewObj = true;
                else
                    CreateNewObj = false;
                end
            end
            if CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end
        end

    end


    methods

        function Result = setProps(Obj, Args)
            % Copy fields of struct Args to class properties, non-existing properties are ignored
            % Return number of fields copied
            Result = 0;
            fn = fieldnames(Args);
            for i = 1:numel(fn)
                if isprop(Obj, fn{i})
                    Obj.(fn{i}) = Args.(fn{i});
                    Result = Result + 1;
                end
            end
        end
    end

    %----------------------------------------------------------------------
    methods(Static) % Unit test

        Result = unitTest()
            % unitTest for Base class
    end

end
