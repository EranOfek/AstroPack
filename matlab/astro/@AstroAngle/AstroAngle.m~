% Component base class
%   This is the base class from which all the classes in AstroPack hinerits.
%
% Authors: Chen Tishler & Eran Ofek (Apr 2021)
%
% Functionality:
% #functions (autogen)
% Base - Constructor
% copy - Create a deep copy of the object (and any object that inherits
%       from Base).
% copyProp - Copy the content of properties from Obj into Target.
% setProps - Copy fields of struct Args to class properties, non-existing properties are ignored Return number of fields copied
%
% copyElement - (Internal) Custom copy of object properties Called from copy() of matlab.mixin.Copyable decendents
% createNewObj - A utility function for creation of an object new copy based on nargout
% openMLX - added by O.S., opens the MLX of the class, Run by using: classname.empty.openMLX
% #/functions (autogen)
%
%
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
%--------------------------------------------------------------------------
%
% Consider using HandleCompatible to allow inheritace by both handle
% and value classes:
%
% https://www.mathworks.com/help/matlab/matlab_oop/supporting-both-handle-and-value-subclasses-handlecompatible.html
% https://www.mathworks.com/help/matlab/matlab_oop/how-to-define-handle-compatible-classes-1.html

classdef Base < matlab.mixin.Copyable
    % Base class for all objects

    % Properties
    properties (SetAccess = public)
        UserData    % Optional user data (any type)
    end

    %--------------------------------------------------------
    methods
        function Obj = Base()
            % Constructor
        end
    end

    
    methods % Copy
        function Target = copyProp(Obj, Target, PropList)
            % Copy the content of properties from Obj into Target.
            % Input  : - Obj (from which to copy)
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

            if nargin < 4
                MinNargout = 0;
            end

            if isempty(CreateNewObj)
                if Nargout > MinNargout
                    CreateNewObj = true;
                else
                    CreateNewObj = false;
                end
            end
            if CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
        end

    end


    methods

        function Result = setProps(Obj, Args)
            % Copy fields of struct Args to class properties, non-existing properties are ignored
            % Return number of fields copied
            % Input:    Args - 'arguments' struct of the caller function
            % Example:  Obj.setProps(Args)
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
    methods(Access = protected)
        function NewObj = copyElement(Obj)
            % Custom copy of object properties, internally used by matlab.mixin.Copyable
            % Called from copy() of matlab.mixin.Copyable decendents
            % See: https://www.mathworks.com/help/matlab/ref/matlab.mixin.copyable-class.html
            
            % Make shallow copy of all properties
            NewObj = copyElement@matlab.mixin.Copyable(Obj);

            % Make deep copy
            if ~isempty(Obj.UserData)
                if isobject(Obj.UserData) % @Todo: How to check that it is derived from matlab.mixin.Copyable???
                    % o.s - try: isa(Obj.UserData, 'Base')
                    % i would also run a loop on properties(obj) to do deep
                    % copy for every derived obj by default, and inherit
                    % the class matlab.mixin.SetGetExactNames to allow useing
                    % get(obj, 'property name')
                    NewObj.UserData = Obj.UserData.copy();
                end
            end
        end
    end

    %----------------------------------------------------------------------
    %Todo: Chen how to open the manual of the actual class
    %Check if there is somethink like @classmethod of python:
    %https://www.geeksforgeeks.org/classmethod-in-python/
    % methods (Static)
    %     function help
    %         % show mlx help file for AstroCatalog
    %         open manuals.AstroCatalog
    %     end
    % end

    % https://www.mathworks.com/matlabcentral/answers/525877-link-to-section-in-another-live-script
    % function open_local_mlx(mlxname, lineNum)
    %     whence = mfilename('fullpath');
    %     [filedir, basename] = fileparts(whence);
    %     mlxname = fullfile(filedir, [mlxname '.mlx']);
    %     matlab.desktop.editor.openAndGoToLine(mlxname, lineNum);
    % end

    methods (Sealed)
        function openMLX(Obj)
            % added by O.S., opens the MLX of the class, Run by using:
            % classname.empty.openMLX
            cls = class(Obj);
            filename = fullfile('manuals', 'class', cls);
            open(filename)
        end
    end

    %----------------------------------------------------------------------
    methods(Static) % Unit test
        Result = unitTest()
            % unitTest for Base class
    end

end
