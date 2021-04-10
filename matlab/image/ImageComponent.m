
classdef ImageComponent < Component
    properties (Dependent)
        Image                                   % return the rescaled image
    end
    
    properties (SetAccess = public)
        Data                                    % e.g., Image matrix
        Scale {mustBeNumeric(Scale)} = [];      %
        ScaleMethod = 'lanczos3';               %
        
        %DataProp cell = {'Data'};              % a cell of properties on which the fun_* methods will be applied
        FileName                     = '';
        Virt VirtImage                          % Actual image data
        
        % Storage 
    end

    
    properties (Hidden, SetAccess = public)
  
    end
    
    
    methods % Constructor
        function Obj = ImageComponent(FileName, Args)
            % ImageComponent constructor
            % Input  : - Either: Image name to read; image name to read
            %            with wild cards or regular expressions;
            %            a cell array of image names; a cell array of
            %            image matrices, or a vector of ImageComponent size
            %            (creates an empty array).
            %            Default is [1 1].
            %          * ...,key,val,...
            %            'Scale' - Image scaling, which will be used to
            %                   rescale the Data to the full image.
            %                   Default is [].
            %            'HDU' - FITS HDU. Default is 1.
            %            'UseRegExp' - Logical indicating if to use regexp
            %                   when using io.files.filelist.
            %                   Default is false.
            % Output : - An ImageComponent object
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent;
            %          IC = ImageComponent([2 2]);
            %          IC = ImageComponent({rand(10,10), rand(2,2)});
            %          IC = ImageComponent({rand(10,10), rand(2,2)},'Scale',5);
            %          IC = ImageComponent('*.fits')
            
            arguments
                FileName                    = [1 1];
                Args.Scale                  = [];
                Args.HDU                    = 1;
                Args.FileType               = [];
                Args.UseRegExp(1,1) logical = false;
            end
            
            if isempty(FileName)
                % define the object
                Obj.Data = [];
            else
                if isa(FileName,'ImageComponent')
                    Obj = FileName;
                elseif isa(FileName,'SIM') || isa(FileName,'imCl')
                    Nobj = numel(FileName);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = ImageComponent;
                        Obj(Iobj).Data  = FileName(Iobj).Im;
                        Obj(Iobj).Scale = Args.Scale;
                    end
                else
                    ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                             'FileType',Args.FileType,...
                                             'IsTable',false,...
                                             'UseRegExp',Args.UseRegExp);
                                         
                    Nobj = numel(ImIO);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = ImageComponent([]);
                        if ~isempty(ImIO(Iobj).Data)
                            % otherwise generate an empty object
                            Obj(Iobj).Data  = ImIO(Iobj).Data;
                            Obj(Iobj).Scale = Args.Scale;
                        end
                    end
                    Obj = reshape(Obj, size(ImIO));
                end
            end % end if isempty...
        end % end ImageComponent
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
            % setter for Data - store in Data 
            Obj.Data  = ImageData;
            %Obj.Scale = [];
        end
    end
    
    methods % function on images
                
        function Result = funUnary(Obj, Operator, Args)
            % funUnary on ImageComponent
            % Input  : - An ImageComponent object (multi elemenets supported).
            %          - Unary operator (e.g., @median, @sin)
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CreateNewObj' - Logical indicatinf if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false)
            %                   Default is false (i.e., input object will
            %                   be modified).
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'OutOnlyCCDSEC' - A logical indicating if the
            %                   output include only the CCDSEC region, or
            %                   it is the full image (where the opeartor,
            %                   operated only on the CCDSEC region).
            %            'DataPropIn' - Data property in which the operator
            %                   will be operated. Default is 'Data'.
            %            'DataPropOut' - Data property in which the result
            %                   will be stored. Default is 'Data'.
            % Output : - An ImageComponent object.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5)
            %          IC.funUnary(@sin);
            %          IC.funUnary(@median,'OpArgs',{'all','omitnan'});
            %          IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5)
            %          IC.funUnary(@median,'OpArgs',{'all','omitnan'},'CCDSEC',[1 2 1 3]);
            %          IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5)
            %          IC.funUnary(@tanh,'CCDSEC',[1 2 1 3]);
            %          IC.funUnary(@tanh,'CCDSEC',[1 2 1 3],'OutOnlyCCDSEC',true);
            %          
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.CreateNewObj(1,1) logical  = false;
                Args.CCDSEC                     = [];
                Args.OutOnlyCCDSEC(1,1) logical = false;
                Args.DataPropIn                 = 'Data';
                Args.DataPropOut                = 'Data';
            end
            
            if ~isempty(Args.CCDSEC)
                % If CCDSEC is given, must operate on the Image
                Args.DataPropIn  = 'Image';
                Args.DataPropOut = 'Image';
            end
            
            if isempty(Args.DataPropOut)
                Args.DataPropOut = Args.DataPropIn;
            end
                        
            if Args.CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                
                if isempty(Args.CCDSEC)
                    Result(Iobj).(Args.DataPropOut) = Operator(Obj(Iobj).(Args.DataPropIn), Args.OpArgs{:});
                else
                    Tmp = Operator(Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), Args.OpArgs{:});
                    if numel(Tmp)==1
                        Result(Iobj).(Args.DataPropOut) = Tmp;
                    else
                        if Args.OutOnlyCCDSEC
                            Result(Iobj).(Args.DataPropOut) = Tmp;
                        else
                            Result(Iobj).(Args.DataPropOut)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)) = Tmp;
                        end
                    end
                end
            end
            
        end
        
        function Result = funBinary(Obj1, Obj2, Operator, Args)
            % Apply a binary operator to ImageComponent objects
            % Input  : - The 1st ImageComponent object.
            %            Number of elements must be equal or larger than
            %            the number of elements in the 2nd input.
            %          - The 2nd ImageComponent object, or alternatively
            %            a cell array of images,
            %            or an array of numerical values.
            %            If the array size is equal to the size of the 1st
            %            object, then it is assumed that each element in
            %            the array is a scalar image. If not, the array
            %            size must be consistent with the image size in the
            %            first object.
            %          - Binary operator (e.g., @plus)
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CCDSEC1' - [Xmin Xmax Ymin Ymax] CCDSEC for the
            %                   1st oprand. The Operator will be applied
            %                   only on this section.
            %                   If empty, use all image. Default is [].
            %            'CCDSEC2' - The same as CCDSEC1, but for the 2nd
            %                   operand. Default is [].
            %            'OutOnlyCCDSEC' - A logical indicating if the
            %                   output include only the CCDSEC, or the
            %                   entire image (in which only the CCDSEC
            %                   region was modified).
            %                   Default is true.
            %            'DataPropIn1' - Data property for 1st operand.
            %                   Default is 'Data'.
            %                   If CCDSEC is not empty, then this will be
            %                   modified into 'Image'.
            %            'DataPropIn2' - Like DataPropIn1, but for the 2nd
            %                   operand. If empty, use the value in
            %                   DataPrope1'. Default is ''.
            %            'DataPropOut' - Data property for the output
            %                   object. If empty, use the value in
            %                   DataPrope1'. Default is ''.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC  = ImageComponent({rand(10,10)})
            %          R   = funBinary(IC,IC,@plus,'CreateNewObj',true)
            %          R   = funBinary([IC,IC],1,@plus,'CreateNewObj',true)
            %          IC1 = ImageComponent({rand(2,2)});
            %          IC2 = ImageComponent({rand(2,2)});
            %          R   = funBinary([IC1,IC2],{1 2},@plus,'CreateNewObj',true)
            %          IC  = ImageComponent({rand(10,10), rand(2,2)},'Scale',5)
            %          R   = funBinary(IC,3,@times,'CreateNewObj',true) 
            %          IC  = ImageComponent({rand(10,10), rand(2,2)},'Scale',5)
            %          R   = funBinary(IC,3,@times,'CreateNewObj',false) 
            %          IC  = ImageComponent({rand(10,10), rand(10,10)},'Scale',5)
            %          IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true)
            %          IC  = ImageComponent({rand(10,10), rand(10,10)},'Scale',5)
            %          IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 3 5])
            %          IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 2 4],'OutOnlyCCDSEC',true)
            %          IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 2 4],'OutOnlyCCDSEC',false)
            
            arguments
                Obj1
                Obj2
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.CreateNewObj(1,1) logical  = true;
                Args.CCDSEC1                    = [];
                Args.CCDSEC2                    = [];
                Args.OutOnlyCCDSEC(1,1) logical = true;
                Args.DataPropIn1                = 'Data';
                Args.DataPropIn2                = '';
                Args.DataPropOut                = '';
            end
            
            if ~isempty(Args.CCDSEC1) || ~isempty(Args.CCDSEC2)
                % If CCDSEC1/2 is given, must operate on the Image
                Args.DataPropIn1 = 'Image';
                Args.DataPropIn2 = 'Image';
                Args.DataPropOut = 'Image';
            end
            
            if isempty(Args.DataPropOut)
                Args.DataPropOut = Args.DataPropIn1;
            end
            if isempty(Args.DataPropIn2)
                Args.DataPropIn2 = Args.DataPropIn1;
            end
                        
            
            % make sure Obj2 is in the right format
            if isnumeric(Obj2)
                % If Obj2 is an array with the same size as Obj1, then
                % convert into a cell array of scalars.
                if all(size(Obj1)==size(Obj2))
                    Obj2 = num2cell(Obj2);
                else
                    % otherwise a single element cell
                    Obj2 = {Obj2};
                end
            end
            % at this stage Obj2 must be a cell or an ImageComponent
            if iscell(Obj2)
                Obj2IsCell = true;
            else
                Obj2IsCell = false;
            end
            if ~(Obj2IsCell || isa(Obj2,'ImageComponent'))
                error('Obj2 must be a cell, and image component or an numeric array');
            end
            
            Nobj1 = numel(Obj1);
            Nobj2 = numel(Obj2);
            Nres  = max(Nobj1, Nobj2);
            if ~(Nobj2==1 || Nobj2==Nobj1)
                error('number of elements in Obj2 must be 1 or equal to the number in Obj1');
            end
                
            if Args.CreateNewObj
                Result = Obj1.copyObject;
            else
                Result = Obj1;
            end
                
            for Ires=1:1:Nres
                Iobj1 = min(Ires, Nobj1);
                Iobj2 = min(Ires, Nobj2);
                if isempty(Args.CCDSEC2)
                    if Obj2IsCell
                        Tmp = Obj2{Iobj2};
                    else
                        Tmp = Obj2(Iobj2).(Args.DataPropIn2);
                    end
                else
                    if Obj2IsCell
                        Tmp = Obj2{Iobj2}(Args.CCDSEC2(3):Args.CCDSEC2(4), Args.CCDSEC2(1):Args.CCDSEC2(2));
                    else
                        Tmp = Obj2(Iobj2).(Args.DataPropIn2)(Args.CCDSEC2(3):Args.CCDSEC2(4), Args.CCDSEC2(1):Args.CCDSEC2(2));
                    end
                end
                
                if isempty(Args.CCDSEC1)
                    % operate on full images
                    Result(Ires).(Args.DataPropOut) = Operator(Obj1(Iobj1).(Args.DataPropIn1), Tmp,               Args.OpArgs{:});
                else
                    if Args.OutOnlyCCDSEC
                        Result(Ires).(Args.DataPropOut) = [];
                        Result(Ires).(Args.DataPropOut) = Operator(Obj1(Iobj1).(Args.DataPropIn1)(Args.CCDSEC1(3):Args.CCDSEC1(4), Args.CCDSEC1(1):Args.CCDSEC1(2)), Tmp, Args.OpArgs{:});
                    else
                        Result(Ires).(Args.DataPropOut)(Args.CCDSEC1(3):Args.CCDSEC1(4), Args.CCDSEC1(1):Args.CCDSEC1(2)) = Operator(Obj1(Iobj1).(Args.DataPropIn1)(Args.CCDSEC1(3):Args.CCDSEC1(4), Args.CCDSEC1(1):Args.CCDSEC1(2)), Tmp, Args.OpArgs{:});
                    end                    
                end
            end
        end
        
        
        function Result = funUnaryScalar(Obj, Operator, Args)
            % funUnary on ImageComponent returning an array of scalars (one
            %       scalar per image)
            % Input  : - An ImageComponent object (multi elemenets supported).
            %          - Unary operator (e.g., @median, @sin)
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'DataPropIn' - Data property in which the operator
            %                   will be operated. Default is 'Data'.
            % Output : - An ImageComponent object.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5)
            %          R = IC.funUnaryScalar(@median,'OpArgs',{'all','omitnan'});
            %          IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5)
            %          R = IC.funUnaryScalar(@median,'OpArgs',{'all','omitnan'},'CCDSEC',[1 2 1 3]);
            %          
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.CCDSEC                     = [];
                Args.DataPropIn                 = 'Data';
            end
            
            if ~isempty(Args.CCDSEC)
                % If CCDSEC is given, must operate on the Image
                Args.DataPropIn  = 'Image';
            end
            
            Result = nan(size(Obj));
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                
                if isempty(Args.CCDSEC)
                    Result(Iobj) = Operator(Obj(Iobj).(Args.DataPropIn), Args.OpArgs{:});
                else
                    Tmp = Operator(Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), Args.OpArgs{:});
                    if numel(Tmp)==1
                        Result(Iobj) = Tmp;
                    else
                        error('funUnaryScalar operator must return a scalar');
                    end
                end
            end
            
        end
        
        
        
        function Cube = images2cube(Obj, Args)
            % Generate a cube of images from ImageComponent object
            % Input  : - An ImageComponent object (multi elements
            %            supported).
            %          * ...,key,val,...
            %            'CCDSEC' - A 4 column matrix of CCDSEC of each
            %                   image in ImageComponent to insert into the
            %                   cube [Xmin, Xmax, Ymin, Ymax].
            %                   If single line, then use the same CCDSEC
            %                   for all images. If empty, use entore image.
            %                   Default is [].
            %            'DataPropIn' - Data property from which to take
            %                   the image. Default is 'Image'.
            %            'DimIndex' - Cube dimension of the image index.
            %                   Either 1 or 3. Default is 3.
            % Output : - A cube of images.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC=ImageComponent({ones(5,5),2.*ones(5,5),3.*ones(5,5)});
            %          Cube = images2cube(IC)
            %          Cube = images2cube(IC,'CCDSEC',[1 2 2 4])
            %          IC=ImageComponent({ones(5,5),2.*ones(7,7),3.*ones(8,8)},'Scale',5);
            %          Cube = images2cube(IC,'CCDSEC',[1 2 2 4])
            %          Cube = images2cube(IC,'CCDSEC',[1 2 2 4; 1 2 2 4; 2 3 3 5])
            
            arguments
                Obj
                Args.CCDSEC                        = [];  % empty for the entire image
                Args.DataPropIn                    = 'Image';
                Args.DimIndex                      = 3;
            end
            
            
            Nobj      = numel(Obj);
            Nsec      = size(Args.CCDSEC,1);
            % size of images
            Iobj = 1;
            if isempty(Args.CCDSEC)
                Size = size(Obj(Iobj).(Args.DataPropIn));
            else
                Size = [Args.CCDSEC(Iobj,4)-Args.CCDSEC(Iobj,3)+1, Args.CCDSEC(Iobj,2)-Args.CCDSEC(Iobj,1)+1];
            end
            % allocate cube
            Cube = zeros(Size(1), Size(2), Nobj);
                
            if isempty(Args.CCDSEC)
                % generate cube from full images
                for Iobj=1:1:Nobj
                    Cube(:,:,Iobj) = Obj(Iobj).(Args.DataPropIn);
                end
            else
                % generate cube from sub images (CCDSEC)
                for Iobj=1:1:Nobj
                    Isec = min(Iobj, Nsec);
                    Cube(:,:,Iobj) = Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(Isec,3):Args.CCDSEC(Isec,4), Args.CCDSEC(Isec,1):Args.CCDSEC(Isec,2));
                end
            end
                
            % change dimension
            if Args.DimIndex==3
                % do nothing
            elseif Args.DimIndex==1
                Cube = permute(Cube, [3 1 2]);
            else
                error('DimIndex must be 1 or 3');
            end
            
        end
        
        % got here
        % funStack (including scaling and zero subtracting)
        % funTransform
        
        
        
        function [Obj,ObjReplaced] = replace(Obj, Range, NewVal, Eps)
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
       
        function Result = imrotate(Obj, RotationAng, Args)
            % Apply imrotate (image rotation) in ImageComponent object
            % Input  : - An ImageComponent object.
            %          - Rotation angle [deg]. 
            %          * ...,key,val,...
            %            'Method' - imrotate interpolation method.
            %                   Default is 'bicubic'.
            %            'BBox' - imrotate bounding box ['loose'] | 'crop'.
            %            'DataProp' - Data prop on which to apply the
            %                   rotation. Default is 'Image'.
            %            'CreateNewObj' - Create new object (true), oir
            %                   update input (false). Default is true.
            %          * Additional parameters to pass to imrotate:
            %            Angle(deg), Method, BBOX.
            %            Default is 'nearest', 'loose'
            % Output : - An ImageComponent object with the rotated images.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC=ImageComponent({rand(10,10)});
            %          IC.imrotate(10)

            arguments
                Obj
                RotationAng
                Args.Method                     = 'bicubic';
                Args.BBox                       = 'loose';
                Args.DataProp                   = 'Image';
                Args.CreateNewObj(1,1) logical  = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj).(Args.DataProp) = imrotate(Obj(Iobj).(Args.DataProp), RotationAng, Args.Method, Args.BBox);
            end
            
        end
          
    end
    
    methods % break/rejoin image to smaller images
        
        function [Result] = trim(Obj, CCDSEC, Args)
            % Trim an ImageComponent object. Either apply multiple trims to
            %       a single image, or a single trime to multiple images,
            %       or multiple trims to multiple images (one to one).
            % Input  : - An ImageComponent object. 
            %          - Either [minX, maxX, minY, maxY] (Type='ccdsec')
            %            or [Xcenter, Ycenter, Xhalfsize, Yhalfsize] (Type = 'center')
            %            or [Xhalfsize, Yhalfsize] (Type = 'center').
            %          * ..., key, val,...
            %            'Type' - ['ccdsec'] | 'center'
            %            'CreateNewObj' - Create a new object (true), or
            %                   write result over input object (false).
            %                   Default is false.
            % Output : - An ImageComponent object with the trimed images.
            % Author : Eran Ofek (Apr 2021)
            % Example: 
            
            arguments
                Obj
                CCDSEC
                Args.Type char                   = 'ccdsec';
                Args.CreateNewObj(1,1) logical   = false;
            end
            
            Nobj = numel(Obj);
            Nsec = size(CCDSEC,1);
            if Nobj==Nsec || Nobj==1 || Nsec==1
                Nmax = max(Nobj, Nsec);
                if Args.CreateNewObj
                    Result = ImageComponent(Nmax,1);
                else
                    Result = Obj;
                end
                for Imax=1:1:Nmax
                    Iobj = min(Imax, Nobj);
                    Isec = min(Imax, Nsec);
                    
                    Result(Imax).Data = imUtil.image.trim(Obj(Iobj).Data, CCDSEC(Isec,:), Args.Type);
                end
            else
                error('trim function works on a single ImageComponent, or a single CCDSEC or number of images equal to number of sections');
            end
            
            
        end
        
        function [Result,EdgesCCDSEC,ListCenters,NoOverlapCCDSEC] = image2subimages(Obj, BlockSize, Args)
            % break an image in a single element ImageComponent into sub images.
            % Input  : - An ImageComponent object with a single element.
            %            If the image is scaled, then will rescale the
            %            image (i.e., will use the 'Image' property).
            %          - BlockSize [X, Y] of sub images. or [X] (will be copied as [X, X]).
            %            If empty, will use imUtil.image.subimage_grid
            %          * ...,key,val,...
            %            'CCDSEC' - A 4 column matrix of CCDSEC
            %                   [Xmin, Xmax, Ymin, Ymax]. Each line
            %                   represent one output sub image boundries.
            %                   If empty, will use BlockSize.
            %                   If given, override BlockSize.
            %                   Default is [].
            %            'Nxy' - Number of sub images in each axis
            %                   (e.g., [10 10]). If given this will
            %                   override BlckSize.
            %                   Default is [].
            %            'OverlapXY' - Overlapping buffer [X, Y].
            %                   Default is 10 pix.
            % Output : - A multiple element ImageComponent object. Each
            %            element contains one sub image. The output is
            %            always a new ImageCompinent object (i.e., the
            %            input ImageComponent is not modified).
            %          - EdgesCCDSEC : CCDSEC matrix of the ouput subimages.
            %          - ListCenters : list of [X,Y] centers of sub images.
            %          - NoOverlapCCDSEC: CCDSEC matrix of the non
            %               overlapping regions of the sub images.
            % Author : Eran Ofek (Mar 2021)
            % Example: IC=ImageComponent; IC.Image=rand(1000,1000);
            %          [Result,EdgesCCDSEC,ListCenters,NoOverlapCCDSEC] = image2subimages(IC,[256 256]);
            
            arguments
                Obj(1,1)               % must be a single element object
                BlockSize             = [256 256];   % If empty, will use imUtil.image.subimage_grid
                Args.CCDSEC           = [];   % If given, override BlockSize
                Args.Nxy              = [];   % If empty then use SubSizeXY. Default is [].
                Args.OverlapXY        = 10;
                
                %Args.ListEdge         = [];
                %Args.ListCenter       = [];
                
            end
            
            [Sub,EdgesCCDSEC,ListCenters,NoOverlapCCDSEC] = imUtil.image.partition_subimage(Obj.Image, Args.CCDSEC,...
                                                                                       'Output','struct',...
                                                                                       'FieldName','Im',...
                                                                                       'SubSizeXY',BlockSize,...
                                                                                       'Nxy',Args.Nxy,...
                                                                                       'OverlapXY',Args.OverlapXY);
            
            % old code
            %[Sub,ListEdge,ListCenter]=imUtil.partition.image_partitioning(Obj.Image, BlockSize, 'ListEdge',Args.ListEdge,...
            %                                                                                    'ListCenter',Args.ListCenter,...
            %                                                                                    'Overlap',Args.OverlapXY);
            
            Nsub   = numel(Sub);
            Result = ImageComponent([1,Nsub]);
            for Isub=1:1:Nsub
                Result(Isub).Data  = Sub.Im;
                Result(Isub).Scale = [];
            end
            
        end
        
        function Result = subimages2image(Obj)
            % break image to sub images
            
            % consider writing a new version of:
            % [FullImage]=imUtil.image.subimages2image(SubImage,CCDSEC);
        end
        
        function [CutoutCube, ActualXY] = cutouts(Obj, XY, Args)
            % Break a single image to a cube of cutouts around given positions
            %       including optional sub pixel shift.
            % Input  : - A single element ImageComponent object.
            %          - A two column matrix of [X, Y] positions around
            %            which to generate the cutouts.
            %          * ...,key,val,...
            
            %            'HalfSize' - Cutout half size (actual size will be
            %                   1+2*HalfSize. Default is 8.
            
            %            'CutAlgo' - Algorithm: ['mex'] | 'wmat'.
            
            %            'IsCircle' - If true then will pad each cutout
            %                   with NaN outside the HalfSize radius.
            %                   Default is false.
            %            'DataProp' - Data property from which to extract
            %                   the cutouts. Default is 'Image'.
            % Outout : - A cube of size 1+2*HalfSize X 1+2*HalfSize X
            %               numberOfCutouts. each layer contain a cutout
            %               and 3rd dim is for cutout index.
            %          - A two column matrix of the actual positions
            %            [X,Y], around which the cutouts are extracted.
            %            These may be rounded if 'RoundPos' is set to true.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent({rand(1000,1000)});
            %          XY = rand(10000,2).*900 + 50;
            %          Cube = cutouts(IC, XY);
            
            arguments
                Obj(1,1)
                XY(:,2)                     = zeros(0,2);
                Args.HalfSize               = 8;
                Args.PadVal                 = NaN;
                
                Args.CutAlgo                = 'mex';  % 'mex' | 'wmat'
                Args.Shift(1,1) logical     = false;
                Args.ShiftAlgo              = 'fft';  % 'fft' | 'lanczos2' | 'lanczos3' | ...
                
                Args.IsCircle               = false;
                Args.DataProp               = 'Image';
            end
            
            CutoutSize = 1+2.*Args.HalfSize;
            
            RoundXY    = round(XY);
            
            Iobj = 1;
            switch lower(Args.CutAlgo)
                case 'mex'
                    [CutoutCube] = imUtil.image.mexCutout(Obj(Iobj).(Args.DataProp), RoundXY, CutoutSize, Args.PadVal, 0, 0, 1); 
                    CutoutCube   = squeeze(CutoutCube);
                case 'wmat'
                    [CutoutCube] = imUtil.image.find_within_radius_mat(Obj(Iobj).(Args.DataProp), RoundXY(:,1), RoundXY(:,2), Args.HalfSize, Args.IsCircle);
                otherwise
                    error('Unknown Algo option');
            end
            
            % shift cutouts 
            if Args.Shift
                
                switch lower(Args.ShiftAlgo)
                    case 'fft'
                        Ncut = size(XY,1);
                        DXY   = XY - RoundXY;
                        
                        % FFU: I suspect the loop can be removed
                        if Ncut>0
                            Icut = 1;
                            [CutoutCube(:,:,1), NY,NX,Nr,Nc] = imUtil.trans.shift_fft(squeeze(CutoutCube(:,:,Icut)), DXY(Icut,1), DXY(Icut,2));
                        end
                        for Icut=2:1:Ncut
                            [CutoutCube(:,:,Icut), NY,NX,Nr,Nc] = imUtil.trans.shift_fft(squeeze(CutoutCube(:,:,Icut)), DXY(Icut,1), DXY(Icut,2), NY,NX,Nr,Nc);
                        end
                        
                        % FFT: add lanczos2/3
                        %ShiftedImage=imUtil.trans.shift_lanczos(G,[1.22,-2.1],3,'circ');
                        
                    otherwise
                        error('Unknown ShiftAlgo option');
                end
                
                
            end
            
        end
        
        function [varargout] = funCutouts(Obj)
            % Apply function on image cutouts
            
        end
        
    end
    
    
    methods (Static)  % unitTest
        function Result = unitTest
            % unitTest for ImageComponent
            
            Size = [2 2];
            IC = ImageComponent(Size);
            if ~all(size(IC)==Size)
                error('ImageComponent size is not consistent');
            end
            Npix  = 10;
            IC = ImageComponent({rand(Npix,Npix), rand(2,2)});
            Scale = 5;
            IC = ImageComponent({rand(Npix,Npix), rand(2,2)},'Scale',Scale);
            if ~all(size(IC(1).Data)==[Npix Npix])
                error('Image data is not consistent');
            end
            if ~all(size(IC(1).Image)==[Npix Npix].*Scale)
                error('Image rescaling is not consistent');
            end
            
            IC = ImageComponent('*.fits');
            
            % funUnary
            IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
            IC.funUnary(@sin);
            IC.funUnary(@median,'OpArgs',{'all','omitnan'});
            IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
            IC.funUnary(@median,'OpArgs',{'all','omitnan'},'CCDSEC',[1 2 1 3]);
            IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
            IC.funUnary(@tanh,'CCDSEC',[1 2 1 3]);
            IC.funUnary(@tanh,'CCDSEC',[1 2 1 3],'OutOnlyCCDSEC',true);
                 
            % funBinary
            IC  = ImageComponent({rand(10,10)});
            R   = funBinary(IC,IC.copyObject,@plus,'CreateNewObj',true);
            if ~all(abs(R.Image-2.*IC(1).Image)<1e-8,'all')
                error('')
            end
            R   = funBinary([IC,IC],1,@plus,'CreateNewObj',true);
            IC1 = ImageComponent({rand(2,2)});
            IC2 = ImageComponent({rand(2,2)});
            R   = funBinary([IC1,IC2],{1 2},@plus,'CreateNewObj',true);
            IC  = ImageComponent({rand(10,10), rand(2,2)},'Scale',5);
            R   = funBinary(IC,3,@times,'CreateNewObj',true);
            IC  = ImageComponent({rand(10,10), rand(2,2)},'Scale',5);
            R   = funBinary(IC,3,@times,'CreateNewObj',false); 
            IC  = ImageComponent({rand(10,10), rand(10,10)},'Scale',5);
            IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true);
            IC  = ImageComponent({rand(10,10), rand(10,10)},'Scale',5);
            % operation between different regions in the 1st and 2nd image
            IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 3 5]);
            IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 2 4],'OutOnlyCCDSEC',true);
            if ~all(size(IB(1).Image)==[3 2])
                error('Problem with output size in funBinary');
            end
            IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 2 4],'OutOnlyCCDSEC',false);
            if ~all(size(IB(1).Image)==size(IC(1).Image))
                error('Problem with output size in funBinary');
            end
            % operate against a scalar for each image
            R   = funBinary([IC1, IC2],[1 3],@plus,'CreateNewObj',true);
            if ~all(abs(R(1).Image - IC1.Image - 1)<1e-8,'all') || ~all(abs(R(2).Image - IC2.Image - 3)<1e-8,'all')
                error('Problem with arithmatics');
            end
            R   = funBinary([IC1],[1 3],@plus,'CreateNewObj',true);

            % In both these case funBinary regards [1 3] as a single image
            % and the opeation is done column wise
            R   = funBinary([IC1],[1 3],@plus,'CreateNewObj',true);  
            R1   = funBinary([IC1],{[1 3]},@plus,'CreateNewObj',true);
            if ~all(abs(R.Image-R1.Image)<1e-8,'all')
                error('Should return the same result');
            end
            
            % funUnaryScalar
            IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
            R = IC.funUnaryScalar(@median,'OpArgs',{'all','omitnan'});
            IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
            R = IC.funUnaryScalar(@median,'OpArgs',{'all','omitnan'},'CCDSEC',[1 2 1 3]);
            
            
            IC=ImageComponent({ones(5,5),2.*ones(5,5),3.*ones(5,5)});
            Cube = images2cube(IC);
            images2cube(IC,'CCDSEC',[1 2 2 4]);
            IC=ImageComponent({ones(5,5),2.*ones(7,7),3.*ones(8,8)},'Scale',5);
            Cube = images2cube(IC,'CCDSEC',[1 2 2 4]);
            % shift and stack using the CCDSEC atgument
            images2cube(IC,'CCDSEC',[1 2 2 4; 1 2 2 4; 2 3 3 5]);
            
            
            Result = true;
            
        end
        
    end
    
end



