% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

% Component is in folder ../base/

classdef BaseImage < Component
    properties (Hidden, SetAccess = public)
        Data
        Config
        UserData
        DataProp        = {};   % a cell of properties on which the fun_* methods will be applied
        Virt VirtImage
    end
    
    
    %-------------------
    %--- Constructor ---
    %-------------------
    methods
       
        function Obj = BaseImage
            % Base class constructor
            % Package: @Base
            
            
        end

    end
    
 
    methods
        function Data = getData(Obj)
            Data = Obj.Virt.getData();
        end
        
        function setData(Obj, NewData)
            Obj.Virt.setData(NewData);
        end
        
        % Read from file
        function read(Obj, FileName)
            Db = ImageDbManager.getDbByFileExt(FileName);
            if notempty(Db)
                Db.
        end
        
        function readDb(Obj, Db, Path)
        end
        
        
        % Write to file
        function write(Obj, FileName)
        end
        
        function writeDb(Obj, Db, Path)
        end
        
    end
    
    
    % setters/getters
    methods
        function set.Data(Obj, NewData)
            Obj.setData(NewData);
        end
        

        function Result = get.Data(Obj)
            Result = Obj.getData(NewData);
        end

    end
    
    % static methods
    methods (Static)
       
    end
    
    % 
    methods
        function Result=fun_unary(Obj,Fun,FunArg,DataProp,OutType)
            % Apply a unary function on some fields and store on object
            % Input  : - (Obj) An object
            %          - (Fun) A function handle of the form Y=F(Data,Arg)
            %          - (FunArg) A cell array of additional arguments to pass to
            %            the function. Default is {}.
            %          - (DataProp) A string or a cell array of strings.
            %            These are the properties on which the unary
            %            function will br applied.
            %            If [], then will use the list of properties in the
            %            DataProp property of the Base class.
            %            Default is [].
            %          - (OutType) Indicate the type of the output:
            %            ['object'] - The output is an object of the same
            %                       type.
            %            'matrix' - matri output (single element only)
            %            'cell' - cell output
            %            'struct' - structure output
            % Output : - The output after applying the function.
            % Author : Eran Ofek (Mar 2021)
            % Example: Result=fun_unary(Obj,@median,{'all'},'Im','matrix')
            
            if nargin<5
                OutType = 'object';
                if nargin<4
                    DataProp = [];
                    if nargin<3
                        FunArg = {};
                    end
                end
            end
            
            if isempty(DataProp)
                DataProp = Obj.DataProp;
            end
            
            if ~iscell(DataProp)
                DataProp = {DataProp};
            end
            Ndp = numel(DataProp);
            if Ndp==0
                error('DataProp is empty - must be specified');
            end
            
            Nobj = numel(Obj);
            
            switch lower(OutType)
                case 'object'
                    Result = Obj;
                    for Iobj=11:1:Nobj
                        for Idp=1:1:Ndp
                            Result(Iobj).(DataProp{Idp}) = Fun(Obj(Iobj).(DataProp{Idp}),FunArg{:});
                        end
                    end
                    
                case 'matrix'
                    if Nobj~=1
                        error('For OutType=matrix number of objects must be 1');
                    end
                    if Ndp~=1
                        error('For OutType==matrix number of DataProp must be 1');
                    end
                    Result = Fun(Obj(Iobj).(DataProp{Idp}),FunArg{:});
                case 'cell'
                    if ~(xor(Nobj>1, Ndp>1) || (Nobj==1 && Ndp==1))
                        error('For OutType=cell either number of objects or DataProp must be 1');
                    end
                    
                    Result = cell(1,max(Ndp,Nobj));
                    Ind = 0;
                    for Iobj=11:1:Nobj
                        for Idp=1:1:Ndp
                            Ind = Ind + 1;
                            Result{Ind} = Fun(Obj(Iobj).(DataProp{Idp}),FunArg{:});
                        end
                    end
                    
                case 'struct'
                    
                    Result = struct();
                    for Iobj=11:1:Nobj
                        for Idp=1:1:Ndp
                            Result(Iobj).(DataProp{Idp}) = Fun(Obj(Iobj).(DataProp{Idp}),FunArg{:});
                        end
                    end
                    
                otherwise
                    error('Unknown OutType option');
            end
            
        end % fun_unary
        
        function Result=fun_binary(Obj1,Obj2,Fun,FunArg,DataProp,OutType)
            % Apply a binary function on some fields
            % Descruption: This function apply a binary function on
            %           an object (Obj1) and another object (Obj2),
            %           where Obj2 may be of the same class as Obj1, an
            %           array or a cell array.
            %           If the two args are of the same Obj class, the
            %           operation is conducted between corresponding
            %           elements of the objects (e.g., Obj1(2) and
            %           Obj2(2)). However, if one of the object contains a
            %           single element then the operation is between this
            %           element and all the other elements in the second
            %           object.
            %           If the second object is a array, than it is
            %           applied as the second arg for all the images in the
            %           first arg.
            %           If the second object is a cell, than each element
            %           in the cell corresponds to an element (with the
            %           same index) in the object.
            %           The output of this function may be an object of the
            %           same type, an array, a cell array, or a structure.
            %
            % Input  : - (Obj1) The first binary argument. An object
            %            (Obj2) - The second binary argument. This is
            %            either an object of the same size as Obj1, or with
            %            numel=1, a cell array in which the number of
            %            elements is equal to the number of elements in the
            %            first object (Obj1), or a matrix which size is
            %            equal to the images in Obj1.
            %          - (Fun) A function handle of the form Y=F(Data,Arg)
            %          - (FunArg) A cell array of additional arguments to pass to
            %            the function. Default is {}.
            %          - (DataProp) A string or a cell array of strings.
            %            These are the properties on which the unary
            %            function will br applied.
            %            If [], then will use the list of properties in the
            %            DataProp property of the Base class.
            %            Default is [].
            %          - (OutType) Indicate the type of the output:
            %            ['object'] - The output is an object of the same
            %                       type.
            %            'matrix' - matri output (single element only)
            %            'cell' - cell output
            %            'struct' - structure output
            % Output : - The output after applying the function.
            % Author : Eran Ofek (Mar 2021)
            % Example: Result=fun_binary(Obj,@plus,{},{'Im','Back','Var'},'object')
            
            
            if nargin<6
                OutType = 'object';
                if nargin<5
                    DataProp = [];
                    if nargin<4
                        FunArg = {};
                    end
                end
            end
            
            if isempty(DataProp)
                DataProp = Obj.DataProp;
            end
            
            if ~iscell(DataProp)
                DataProp = {DataProp};
            end
            Ndp = numel(DataProp);
            if Ndp==0
                error('DataProp is empty - must be specified');
            end
            
            Nobj = numel(Obj);
            
            ClassObj1 = class(Obj1);
            if isnumeric(Obj2)
                Nobj2     = 1;
            elseif iscell(Obj2)
                Nobj2 = numel(Obj2);
            elseif isa(Obj1, ClassObj1)
                Nobj2 = numel(Obj2);
                if ~((Nobj==Nobj2) || Nobj==1 || Nobj2==1)
                    error('Numeber of elements in objects must be the same or one of them equal to 1');
                end
            else
                error('Obj2 must be an array, a cell array or of the same type as Obj1');
            end
                
            
            switch lower(OutType)
                case 'object'
                    Result = Obj;
                    
                    for Iobj=1:1:Nobj
                        Iobj1 = min(Iobj,Nobj);
                        Iobj2 = min(Iobj,Nobj2);
                        for Idp=1:1:Ndp
                            if isnumeric(Obj2)
                                % If Obj2 is an array - second argument on
                                % each other array
                                Result(Iobj).(DataProp{Idp}) = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2, FunArg{:});
                            elseif iscell(Obj2)
                                % If Obj2 is a cell array than each element
                                % in the cell corresponds to all the
                                % properties in one object element.
                                Result(Iobj).(DataProp{Idp}) = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2{Iobj2}, FunArg{:});
                            else
                                % both args are an object of the same type
                                Result(Iobj).(DataProp{Idp}) = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2(Iobj2).(DataProp{Idp}), FunArg{:});
                            end
                        end
                    end
                    
                case 'matrix'
                    if Nobj~=1
                        error('For OutType=matrix number of objects must be 1');
                    end
                    if Ndp~=1
                        error('For OutType==matrix number of DataProp must be 1');
                    end
                    
                    if isnumeric(Obj2)
                        Result = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2, FunArg{:});
                    elseif iscell(Obj2)
                        Result = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2{Iobj2}, FunArg{:});
                    else
                        Result = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2(Iobj2).(DataProp{Idp}), FunArg{:});
                    end    
                    
                case 'cell'
                    if ~(xor(Nobj>1, Ndp>1) || (Nobj==1 && Ndp==1))
                        error('For OutType=cell either number of objects or DataProp must be 1');
                    end
                    
                    Result = cell(1,max(Ndp,Nobj));
                    Ind = 0;
                    for Iobj=11:1:Nobj
                        Iobj1 = min(Iobj,Nobj);
                        Iobj2 = min(Iobj,Nobj2);
                        for Idp=1:1:Ndp
                            Ind = Ind + 1;
                            if isnumeric(Obj2)
                                Result{Ind} = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2, FunArg{:});
                            elseif iscell(Obj2)
                                Result{Ind} = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2{Iobj2}, FunArg{:});
                            else
                                Result{Ind} = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2(Iobj2).(DataProp{Idp}), FunArg{:});
                            end    
                        end
                    end
                    
                case 'struct'
                    
                    Result = struct();
                    for Iobj=11:1:Nobj
                        Iobj1 = min(Iobj,Nobj);
                        Iobj2 = min(Iobj,Nobj2);
                        for Idp=1:1:Ndp
                            if isnumeric(Obj2)
                                Result(Iobj).(DataProp{Idp}) = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2, FunArg{:});
                            elseif iscell(Obj2)
                                Result(Iobj).(DataProp{Idp}) = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2{Iobj2}, FunArg{:});
                            else
                                Result(Iobj).(DataProp{Idp}) = Fun(Obj1(Iobj1).(DataProp{Idp}), Obj2(Iobj2).(DataProp{Idp}), FunArg{:});
                            end    
                        end
                    end
                    
                otherwise
                    error('Unknown OutType option');
            end
        end
    end
    
 
    
    % setters/getters
    methods
        
    end
    
    % static methods
    methods (Static)
       
    end
    
    % 
    methods
        
    end
    
end

            
