
classdef ImageComponent < handle %Component
    properties (Dependent)
        Image                                   % return the rescaled image
    end
    
    properties (SetAccess = public)
        Data                                    % e.g., Image matrix
        Scale {mustBeNumeric(Scale)} = [];      %
        ScaleMethod = 'lanczos3';               %
        
        %DataProp cell = {'Data'};              % a cell of properties on which the fun_* methods will be applied
        Virt VirtImage                          % Actual image data
        
        % Storage 
    end

    
    properties (Hidden, SetAccess = public)
  
    end
    
    
    methods % Constructor
        function Obj = ImageComponent(ImageData,Size)
            % ImageComponent constructor
            % Input  : - Image data. Default is [].
            %          - Size of ImageComponent. Default is [1 1].
            %            Will copy the image data to each element.
            % Output : - An ImageComponent object
            % Example: IC = ImageComponent(rand(10,10),[2 2]);
            
            arguments
                ImageData                   = [];
                Size                        = [1 1];
            end
            
            Nobj = prod(Size);
            for Iobj=1:1:Nobj
                Obj(Iobj).Data = ImageData;
            end
            Obj = reshape(Obj,Size);
            
        end

    end
    
    % 
    methods % getters/setters
        function Result = get.Image(Obj)
            % getter for Image (rescale Data)
            
            Result = imresize(Obj,[],'UpdateObj',false,'Method',Obj.ScaleMethod);
        end
        
        function set.Image(Obj, ImageData)
            % setter for image - store in Data and set Scale to []
            Obj.Data  = ImageData;
            Obj.Scale = [];
        end
        
        function set.Data(Obj, ImageData)
            % setter for Data - store in Data and set Scale to []
            Obj.Data  = ImageData;
            Obj.Scale = [];
        end
    end
    
    methods % function on images
        
        % consider moving to BaseImage??
        
        function Result=fun_unary(Obj,Fun,FunArg,OutType,DataProp)
            % Apply a unary function on a single fields and store on object
            % Input  : - (Obj) An object
            %          - (Fun) A function handle of the form Y=F(Data,Arg)
            %          - (FunArg) A cell array of additional arguments to pass to
            %            the function. Default is {}.
            %          - (OutType) Indicate the type of the output:
            %            ['object'] - The output is an object of the same
            %                       type.
            %            'matrix' - matri output (single element only)
            %            'cell' - cell output
            %            'struct' - structure output with field named
            %          - (DataProp) A string conatining a single property
            %            on which the operation will be executed.
            %            See fun_unary_MP for multi properties.
            %            Default is 'Data'.
            % Output : - The output after applying the function.
            % Author : Eran Ofek (Mar 2021)
            % Example: Result=fun_unary(Obj,@median,{'all'},'matrix')
            
            arguments
                Obj
                Fun function_handle                                      = [];
                FunArg cell                                              = {};
                OutType char   {mustBeMember(OutType,{'obj','object','matrix','cell','struct'})} = 'obj';                                     
                DataProp                                                 = 'Data';
                
            end
              
            Nobj = numel(Obj);
            
            switch lower(OutType)
                case {'obj','object'}
                    Result = Obj;
                    for Iobj=11:1:Nobj
                        Result(Iobj).(DataProp) = Fun(Obj(Iobj).(DataProp), FunArg{:});
                    end
                    
                case 'matrix'
                    if Nobj~=1
                        error('For OutType=matrix number of objects must be 1');
                    end
                    
                    Result = Fun(Obj(Iobj).(DataProp), FunArg{:});
                case 'cell'
                    Result = cell(size(Obj));
                    for Iobj=11:1:Nobj
                        Result{Iobj} = Fun(Obj(Iobj).(DataProp), FunArg{:});
                    end
                    
                case 'struct'
                    Result = struct();
                    for Iobj=11:1:Nobj
                        Result(Iobj).(DataProp) = Fun(Obj(Iobj).(DataProp), FunArg{:});
                    end
                    
                otherwise
                    error('Unknown OutType option');
            end
            
        end % fun_unary
        
        function Result=fun_unaryMP(Obj,Fun,FunArg,OutType,DataProp)
            % Apply a unary function on some fields and store on object
            % Input  : - (Obj) An object
            %          - (Fun) A function handle of the form Y=F(Data,Arg)
            %          - (FunArg) A cell array of additional arguments to pass to
            %            the function. Default is {}.
            %          - (OutType) Indicate the type of the output:
            %            ['object'] - The output is an object of the same
            %                       type.
            %            'matrix' - matri output (single element only)
            %            'cell' - cell output
            %            'struct' - structure output
            %          - (DataProp) A string or a cell array of strings.
            %            These are the properties on which the unary
            %            function will br applied.
            %            If [], then will use the list of properties in the
            %            DataProp property of the Base class.
            %            Default is {}.
            % Output : - The output after applying the function.
            % Author : Eran Ofek (Mar 2021)
            % Example: Result=fun_unaryMP(Obj,@median,{'all'},'matrix','data')
            
            arguments
                Obj
                Fun function_handle                                      = [];
                FunArg cell                                              = {};
                OutType char   {mustBeMember(OutType,{'obj','object','matrix','cell','struct'})} = 'obj';       
                DataProp       {mustBeA(DataProp,{'char','cell'})}       = {};
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
                case {'obj','object'}
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
            
        end % fun_unaryMF
        
        function Result=fun_binary(Obj1,Obj2,Fun,FunArg,OutType,DataProp1,DataProp2,DataPropOut)
            % Apply a binary function on a single field
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
            %          - (OutType) Indicate the type of the output:
            %            ['object'] - The output is an object of the same
            %                       type.
            %            'matrix' - matri output (single element only)
            %            'cell' - cell output
            %            'struct' - structure output
            %          - (DataProp1) A string containing the property name
            %            in Obj1 on which to execute the operation.
            %            Default is 'Data'.
            %          - (DataProp2) A string containing the property name
            %            in Obj1 on which to execute the operation.
            %            Default is like DataProp1.
            %          - (DataPropOut) A string containing the property
            %            name in which the ouput will be stored.
            %            Default is like DataProp1.
            % Output : - The output after applying the function.
            % Author : Eran Ofek (Mar 2021)
            % Example:
            % Result=fun_binaryMP(Obj,@plus,{},'object','Back');
            
            arguments
                Obj1
                Obj2           {mustbeA(Obj2,{'BaseImage','numeric','cell'})} = [];
                Fun function_handle                                           = [];
                FunArg cell                                                   = {};
                OutType char   {mustBeMember(OutType,{'obj','object','matrix','cell','struct'})} = 'obj';   
                DataProp1                                                     = 'Data';
                DataProp2                                                     = [];
                DataPropOut                                                   = [];
            end
            
            if isempty(DataProp2)
                DataProp2 = DataProp1;
            end
            if isempty(DataPropOut)
                DataPropOut = DataProp1;
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
                case {'obj','object'}
                    Result = Obj;
                    
                    for Iobj=1:1:Nobj
                        Iobj1 = min(Iobj,Nobj);
                        Iobj2 = min(Iobj,Nobj2);
                        for Idp=1:1:Ndp
                            if isnumeric(Obj2)
                                % If Obj2 is an array - second argument on
                                % each other array
                                Result(Iobj).(DataPropOut{Idp}) = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2, FunArg{:});
                            elseif iscell(Obj2)
                                % If Obj2 is a cell array than each element
                                % in the cell corresponds to all the
                                % properties in one object element.
                                Result(Iobj).(DataPropOut{Idp}) = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2{Iobj2}, FunArg{:});
                            else
                                % both args are an object of the same type
                                Result(Iobj).(DataPropOut{Idp}) = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2(Iobj2).(DataProp2{Idp}), FunArg{:});
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
                    
                    Iobj1=1;
                    Iobj2=1;
                    Idp=1;
                    
                    if isnumeric(Obj2)
                        Result = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2, FunArg{:});
                    elseif iscell(Obj2)
                        Result = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2{Iobj2}, FunArg{:});
                    else
                        Result = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2(Iobj2).(DataProp2{Idp}), FunArg{:});
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
                                Result{Ind} = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2, FunArg{:});
                            elseif iscell(Obj2)
                                Result{Ind} = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2{Iobj2}, FunArg{:});
                            else
                                Result{Ind} = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2(Iobj2).(DataProp2{Idp}), FunArg{:});
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
                                Result(Iobj).(DataPropOut{Idp}) = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2, FunArg{:});
                            elseif iscell(Obj2)
                                Result(Iobj).(DataPropOut{Idp}) = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2{Iobj2}, FunArg{:});
                            else
                                Result(Iobj).(DataPropOut{Idp}) = Fun(Obj1(Iobj1).(DataProp1{Idp}), Obj2(Iobj2).(DataProp2{Idp}), FunArg{:});
                            end    
                        end
                    end
                    
                otherwise
                    error('Unknown OutType option');
            end
        end
        
        function Result=fun_binaryMP(Obj1,Obj2,Fun,FunArg,OutType,DataProp)
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
            %          - (OutType) Indicate the type of the output:
            %            ['object'] - The output is an object of the same
            %                       type.
            %            'matrix' - matri output (single element only)
            %            'cell' - cell output
            %            'struct' - structure output
            %          - (DataProp) A string or a cell array of strings.
            %            These are the properties on which the unary
            %            function will br applied.
            %            If [], then will use the list of properties in the
            %            DataProp property of the Base class.
            %            Default is [].
            % Output : - The output after applying the function.
            % Author : Eran Ofek (Mar 2021)
            % Example:
            % Result=fun_binaryMP(Obj,@plus,{},'object',{'Im','Back','Var'});
            
            arguments
                Obj1
                Obj2           {mustbeA(Obj2,{'BaseImage','numeric','cell'})} = [];
                Fun function_handle                                           = [];
                FunArg cell                                                   = {};
                OutType char   {mustBeMember(OutType,{'obj','object','matrix','cell','struct'})} = 'obj';   
                DataProp       {mustBeA(DataProp,{'char','cell'})}            = {};
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
                case {'obj','object'}
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
                    
                    Iobj1=1;
                    Iobj2=1;
                    Idp=1;
                    
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
        
        function [Obj,ObjReplaced] = replace(Obj, Range, NewVal)
            % Replace values in range in image with a new value and generate a flag image of replaced pixels
            % Input  : - An ImageComponent object.
            %          - A two column matrix of min/max ranges, or a single
            %            column vector of values.
            %            Any value in the image in this range <=/>= will be
            %            replaced.
            %          - A vector of new values that will replace the old
            %            values. If scalar, then will use the same value
            %            for all ranges.
            %            For vector, value can be NaN.
            %          - A value to subtract to the lower range and add to
            %            the upper range prior to comparison. This is
            %            useful in order to avoid problems with comparison
            %            of floating point numbers. Default is 0.
            % Output : - The same object, but with the updated values.
            %          - A new ImageComponent object which contains a
            %            matrix of logicals. True if value was replaced.
            % Author : Eran Ofek (Mar 2021)
            % Example: [Obj,ObjReplaced] = replace(Obj, Range, NewVal);
            
            arguments
                Obj
                Range      {mustBeNumeric(Range)}
                NewVal     {mustBeNumeric(NewVal)}
                Eps        {mustBeNumeric(Eps)}        = 0;
            end
            
            if nargout>1
                ObjReplaced = ImageComponent(size(Obj));
            end
            
            Nnewval = numel(NewVal); 
            Nrange  = size(Range,1);
            Nobj    = numel(Obj);
            for Iobj=1:1:Nobj
                FlagAll = false(numel(Obj(Iobj).Image),1);
                for Irange=1:1:Nrange
                    if any(isnan(Range(Irange,:)))
                        % Replace NaN
                        Flag = isnan(Obj(Iobj).Image(:));
                    else
                        % Replace values (not NaN)
                        MinVal = min(Range(Irange,:),[],2) - Eps;
                        MaxVal = max(Range(Irange,:),[],2) + Eps;

                        Flag   = Obj(Iobj).Image(:)>=MinVal & Obj(Iobj).Image(:)<=MaxVal;
                        Inew   = min(Irange, NnewVal);
                    end
                    
                    Obj(Iobj).Image(Flag) = NewVal(Inew);
                    
                    if nargout>1
                        FlagAll = FlagAll | Flag;
                    end
                end
                    
                if nargout>1
                    % flag the MaskImage
                    ObjReplaced(Iobj).Image = false(size(Obj(Iobj).Image));
                    ObjReplaced(Iobj).Image(FlagAll) = true;
                end
            end
        end
        
        function Result = imresize(Obj, Scale, Args)
            % resize image data using matlab imresize function
            % Input  : - An ImageComponent object
            %          - Rescaling factor or [rows, columns] in the ouput
            %            image. If empty, then will attempt to atke this
            %            parameter from the ImageComponent object Scale
            %            property, and if this is also empty, then will do
            %            nothing.
            %          * ...,key,val,...
            %            'Method' - see imresize for methods options.
            %                   Default is 'lanczos3'.
            %            'UpdateObj' - If true then will store the resized
            %                   image also in the Data propery and set the
            %                   scale to []. Default is false.
            % Output : - A resized image (matrix).
            % Example: Result = imresize(Obj, Scale)
            
            
            arguments
                Obj
                Scale                             = [];
                Args.Method char                  = 'lanczos3'; % see imresize for option
                Args.UpdateObj(1,1) logical       = false;
            end
            
            NewScale = Scale;
            
            Nobj  = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(Scale)
                   NewScale = Obj(Iobj).Scale;
                end
                
                if ~isempty(NewScale)
                    Result = imresize(Obj(Iobj).Data, NewScale, 'Method', Args.Method);
                    
                    if Args.UpdateObj
                        Obj(Iobj).Data  = Result;
                        Obj(Iobj).Scale = [];
                    end
                else
                    Result = Obj(Iobj).Data;
                end
                
            end
        end
        
    end
    
    methods % break/rejoin image to smaller images
        
        
        function [Result,ListEdge,ListCenter] = image2subimages(Obj,BlockSize,Args)
            % break an image in a single element ImageComponent into sub images.
            % Input  : - An ImageComponent object with a single element.
            %            If the image is scaled, then will rescale the
            %            image (i.e., will use the 'Image' property).
            %          - Size [X, Y] of sub images. or [X] (will be copied as [X, X]).
            %            Alternatively, if this is empty then will use ListEdge and
            %            ListCenter parameters.
            %          * ...,key,val,...
            %            'ListEdge' - [xmin, xmax, ymin, ymax] as returned by
            %                   imUtil.partition.subimage_boundries.
            %                   This is used only if BlockSize is empty.
            %                   Default is empty.
            %            'ListCenter' - [xcenter,ycenter] as returned by
            %                   imUtil.partition.subimage_boundries.
            %                   This is used only if BlockSize is empty.
            %                   Default is empty.
            %            'Overlap' - Overlapping buffer. Default is 10 pix.
            % Output : - A multiple element ImageComponent object. Each
            %            element contains one sub image.
            % Author : Eran Ofek (Mar 2021)
            % Example: IC=ImageComponent; IC.Image=rand(1000,1000);
            %          [Result,ListEdge,ListCenter] = image2subimages(IC,[256 256]);
            
            arguments
                Obj(1,1)               % must be a single element object
                BlockSize             = [256 256];
                Args.ListEdge         = [];
                Args.ListCenter       = [];
                Args.Overlap          = 10;
            end
            
            [Sub,ListEdge,ListCenter]=imUtil.partition.image_partitioning(Obj.Image, BlockSize, 'ListEdge',Args.ListEdge,...
                                                                                                'ListCenter',Args.ListCenter,...
                                                                                                'Overlap',Args.Overlap);
            Nsub   = numel(Sub);
            Result = ImageComponent([1,Nsub]);
            for Isub=1:1:Nsub
                Result(Isub).Data  = Sub.Im;
                Result(Isub).Scale = [];
            end
            
        end
        
        function Result = subimages2image(Obj)
            % break image to sub images
            
        end
        
        function Result = cutouts(Obj)
            % break image to cutouts around positions
            
        end
        
        function [varargout] = funCutouts(Obj)
            % Apply function on image cutouts
            
        end
        
    end
    
    
    methods (Static)  % unitTest
        function Result = unitTest
            % unitTest for ImageComponent
            
            IC = ImageComponent;
            Data     = [1 2;1 2];
            IC.Image = Data;
            IC.Scale = [10 10];
            if ~all(size(IC.Data)==Data)
                error('Inconsistent size stored in Data');
            end
            if ~all(size(IC.Image)==IC.Scale)
                error('resized image has the wrong size');
            end
            
            Result = true;
            
        end
        
    end
    
end



