
classdef ImageComponent < Component
    
    
    properties (Hidden, SetAccess = public)
        Data                                    % e.g., Image matrix
        Scale {mustBeNumeric(Scale)} = [];      %
        ScaleMethod = 'lanczos3';               %
        
        %DataProp cell = {'Data'};              % a cell of properties on which the fun_* methods will be applied
        Virt VirtImage                          % Actual image data
        
        % Storage 
    end

    
    properties (Hidden, SetAccess = public)
  
    end
    
    
    methods
       
        function Obj = ImageComponent
       
            
        end

    end
    
    
     
    
    % 
    methods
        
    end
    
    methods % function on images
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
                OutType char   {mustBeMember(OutType,{'object','matrix','cell','struct'})} = 'object';                                     
                DataProp                                                 = 'Data';
                
            end
              
            Nobj = numel(Obj);
            
            switch lower(OutType)
                case 'object'
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
                OutType char   {mustBeMember(OutType,{'object','matrix','cell','struct'})} = 'object';                                     
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
                OutType char   {mustBeMember(OutType,{'object','matrix','cell','struct'})} = 'object';   
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
                case 'object'
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
                OutType char   {mustBeMember(OutType,{'object','matrix','cell','struct'})} = 'object';   
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
        
        function Obj=imresize(Obj,NewScale,Args)
            %
            
            arguments
                Obj
                NewScale double                   = 1;
                Args.Method                       = 'lanczos3'; % see imresize for option
                Args.RelativeScale(1,1) logical   = true;
                Args.OutputSuze                   = [];    
                Args.DataProp cell                = {};
            end
            
            Nobj  = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(Args.DataProp)
                    DataProp = Obj(Iobj).DataProp;
                else
                    DataProp = Args.DataProp;
                end
            
                Nprop = numel(DataProp);
                    
                if Args.RelativeScale
                    Scale = NewScale./Obj(Iobj).Scale;
                    if Scale~=1

                        for Iprop=1:1:Nprop
                            Obj(Iobj).(DataProp{Iprop}) = imresize(Obj(Iobj).(DataProp{Iprop}),'Method',Args.Method,'OutputSize',Args.OutputSize);
                        end
                    end
                end
            end
            
            
        end
    end
    
end



