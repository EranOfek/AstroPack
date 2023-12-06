% ImageComponent
%

% #functions (autogen)
% ImageComponent - ImageComponent constructor
% background - Estimate the background and variance of an ImageComponent
% cast - Cast the image data in ImageComponent (transform to a new type)
% coadd - coadd images in an ImageComponent object including pre/post normalization and variance
% crop - Crop an ImageComponent object. Either apply multiple trims to a single image, or a single trime to multiple images, or multiple trims to multiple images (one to one). Update also the CCDSEC propery.
% cutouts - Break a single image to a cube of cutouts around given positions including optional sub pixel shift.
% funBinary - Apply a binary operator to ImageComponent objects CCDSEC is updated according to CCDSEC1.
% funCutouts - Apply function (that returns a scalar) on image cutouts
% funStack - Simple stack (coadd) images in an ImageComponent object without pre/post normalization
% funUnary - funUnary on ImageComponent Update CCDSEC accordingly.
% funUnaryScalar - funUnary on ImageComponent returning an array of scalars (one scalar per image)
% get.Cols -
% get.Image - getter for Image (rescale Data)
% get.Rows -
% getImageByKey -
% image2subimages - break an image in a single element ImageComponent into sub images.
% imageComponent2AstroImage - Convert an ImageComponet to AstroImage
% images2cube - Generate a cube of images from ImageComponent object
% imresize - resize image data using matlab imresize function
% imrotate - Apply imrotate (image rotation) in ImageComponent object Set CCDSEC to [].
% isemptyImage - Check if images data in ImageComponent are empty (true if empty). Result is an array (element per image element). Example: IC = ImageComponent({rand(0,1), rand(10,12)}); isemptyImage(IC)
% replace - Replace values in range in image with a new value and generate a flag image of replaced pixels
% set.Data - setter for Data - store in Data
% set.Image - setter for image - store in Data and set Scale to []
% sizeImage - get size of all images/data
% subimages2image - break image to sub images
% subtractBack - subtract background from an ImageComponent If this is a SciImage, then also set the IsBackSubtracted to true.
% #/functions (autogen)
%

classdef ImageComponent < Component

    % Parent class for all images
    
    properties (Dependent)
        Image                                   % return the rescaled image
        Rows
        Cols
    end
    
    properties (SetAccess = public)
        Data                                    % e.g., usually Image matrix
        Scale {mustBeNumeric(Scale)} = [];      %
        ScaleMethod = 'lanczos3';               %
        VecX                         = [];      % specify the X ordinates of the image in Data. If empty, use scale.
        VecY                         = [];
        SizeIJ                       = [];
        
        CCDSEC                       = [];      % [Xmin, Xmax, Ymin, Ymax] from previous image. [] - unknown or full
        
        %DataProp cell = {'Data'};              % a cell of properties on which the fun_* methods will be applied
        FileName                     = '';      % @FFU
        
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
                    % FFU: I uncommented this line
                    % Obj = FileName;   % the constructor must preserve the class of the returned object
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
                        Obj(Iobj) = ImageComponent(Obj);   % Tailored in order to avoid problems in the superclass
                        if ~isempty(ImIO(Iobj).Data)
                            % otherwise generate an empty object
                            Obj(Iobj).Data  = ImIO(Iobj).Data;
                            Obj(Iobj).Scale = Args.Scale;
                        end
                    end
                    Obj = reshape(Obj, size(ImIO));
                end
            end % end if isempty...
            
            % Removed 24/04/2023 by @Chen - according to Eran it consumes
            % too much time and is not used at the moment.
            % Obj.needMapKey();
            
        end % end ImageComponent
    end
    
    
    methods % getters/setters
        function Result = get.Image(Obj)
            % getter for Image (rescale Data)
            
            %if isempty(Obj.VecX) || isempty(Obj.VecY) || isempty(Obj.SizeIJ)
                Result = imresize(Obj, [], 'UpdateObj', false, 'Method', Obj.ScaleMethod);
            %else
                % use a different method for rescaling
                % FFU
            %    error('Rescaling with VecX/VecY/SizeIJ is not yet implemented');
            %end
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

        
        function Result = get.Rows(Obj)
            Result = size(Obj.Image, 1);
        end
        
        function Result = get.Cols(Obj)
            Result = size(Obj.Image, 2);
        end
            
        
    end
    
    methods % conversion
        function AI = imageComponent2AstroImage(Obj, DataProp, CreateNewObj)
            % Convert an ImageComponet to AstroImage
            % Input  : - An ImageComponent object
            %          - Data property in the AstroImage in which to store the
            %            ImageComponent object. Default is 'ImageData'.
            %          - CreateNewObj. Default is true.
            % Output : - An AstroImage object
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent({ones(3,3), zeros(3,3)});
            %          AI = IC.imageComponent2AstroImage;
            
            arguments
                Obj
                DataProp char                  = 'ImageData';
                CreateNewObj(1,1) logical      = true;
            end
            
            if CreateNewObj
                IC = Obj.copy();
            else
                IC = Obj;
            end
                
            AI = AstroImage(size(Obj));
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                AI(Iobj).(DataProp).Data        = IC(Iobj).Data;
                AI(Iobj).(DataProp).ScaleMethod = IC(Iobj).ScaleMethod;
                AI(Iobj).(DataProp).Scale       = IC(Iobj).Scale;
                AI(Iobj).(DataProp).CCDSEC      = IC(Iobj).CCDSEC;
                AI(Iobj).(DataProp).Config      = IC(Iobj).Config;
                AI(Iobj).(DataProp).Logger      = IC(Iobj).Logger;
            end
        end
    end
    
    methods % data size
        function [Ny, Nx] = sizeImage(Obj, Prop)
            % get size of all images/data
            % Input  : - An ImageComponent object (multi elements supported).
            %          - Property name for wich the size is requested.
            %            Default is 'Image'.
            % Output : arrays [Nx] and [Ny] of all images.
            % Example: IC = ImageComponent({rand(10,12)});
            %          [Nx, Ny] = IC.sizeImage
           
            arguments
                Obj
                Prop       = 'Image';
            end
            
            Nobj = numel(Obj);
            Nx   = zeros(size(Obj));
            Ny   = zeros(size(Obj));
            for Iobj=1:1:Nobj
               [Nx(Iobj), Ny(Iobj)] = size(Obj(Iobj).(Prop));
            end
        end
        
        function Result = isemptyImage(Obj)
            % Check if images data in ImageComponent are empty (true if
            % empty). Result is an array (element per image element).
            % Example: IC = ImageComponent({rand(0,1), rand(10,12)});
            %          isemptyImage(IC)
           
            Result = false(size(Obj));
            Nobj   = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj) = isempty(Obj(Iobj).Data);
            end
            
        end
    end
    
    methods % function on images
        
        function Result = cast(Obj, NewClass, CreateNewObj)
            % Cast the image data in ImageComponent (transform to a new type)
            %  Input  : - An ImageComponent object.
            %           - A char array containing the new type.
            %             Default is 'single'.
            %           - Indicating if the output
            %             is a new copy of the input (true), or an
            %             handle of the input (false).
            %             If empty (default), then this argument will
            %             be set by the number of output args.
            %             If 0, then false, otherwise true.
            %             This means that IC.fun, will modify IC,
            %             while IB=IC.fun will generate a new copy in
            %             IB.
            % Output : - An ImageComponent object in which the image 'Data'
            %            is transformed into the new type.
            % Author : Eran Ofek (May 2021)
            % Example: IC = ImageComponent({rand(10,10)});
            %          cast(IC,'single')
            
            arguments
                Obj
                NewClass        = 'single';
                CreateNewObj    = [];
            end
            
            if isempty(CreateNewObj)
                if nargout==0
                    CreateNewObj = false;
                else
                    CreateNewObj = true;
                end
            end
            if CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj).Data = cast(Result(Iobj).Data,NewClass);
            end
            
        end
        
        function Result = funUnary(Obj, Operator, Args)
            % funUnary on ImageComponent
            %       Update CCDSEC accordingly.
            % Input  : - An ImageComponent object (multi elemenets supported).
            %          - Unary operator (e.g., @median, @sin)
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'OutOnlyCCDSEC' - A logical indicating if the
            %                   output include only the CCDSEC region, or
            %                   it is the full image (where the opeartor,
            %                   operated only on the CCDSEC region).
            %                   Default is true.
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
                Args.CreateNewObj               = [];
                Args.CCDSEC                     = [];
                Args.OutOnlyCCDSEC(1,1) logical = true;
                Args.DataPropIn                 = 'Data';
                Args.DataPropOut                = 'Data';
            end
            
            if isempty(Args.CreateNewObj)
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
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
                Result = Obj.copy();
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
                            Result(Iobj).CCDSEC             = Args.CCDSEC;
                        else
                            Result(Iobj).(Args.DataPropOut)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)) = Tmp;
                        end
                    end
                end
            end
            
        end
        
        function Result = funBinary(Obj1, Obj2, Operator, Args)
            % Apply a binary operator to ImageComponent objects
            %       CCDSEC is updated according to CCDSEC1.
            % Input  : - The 1st ImageComponent object.
            %            Number of elements must be equal or larger than
            %            the number of elements in the 2nd input.
            %          - The 2nd ImageComponent object, or alternatively
            %            a cell array of images,
            %            or an array of numerical values.
            %            The array
            %            size must be consistent with the image size in the
            %            first object.
            %          - Binary operator (e.g., @plus)
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
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
                Args.CreateNewObj               = [];
                Args.CCDSEC1                    = [];
                Args.CCDSEC2                    = [];
                Args.OutOnlyCCDSEC(1,1) logical = true;
                Args.DataPropIn1                = 'Data';
                Args.DataPropIn2                = '';
                Args.DataPropOut                = '';
            end
            
            if isempty(Args.CreateNewObj)
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
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
                %if all(size(Obj1)==size(Obj2))
                %    Obj2 = num2cell(Obj2);
                %else
                    % otherwise a single element cell
                    Obj2 = {Obj2};
                %end
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
                Result = Obj1.copy();
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
                        Result(Ires).CCDSEC             = Args.CCDSEC1;
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
            % Output : - An array in which each element corresponds to the operator applied
            %            to an element in the ImageComponent object.
            %            If operator returns empty, then this function will
            %            return NaN.
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
                Args.DataPropIn char            = 'Data';
            end
            
            if ~isempty(Args.CCDSEC)
                % If CCDSEC is given, must operate on the Image
                Args.DataPropIn  = 'Image';
            end
            
            Result = nan(size(Obj));
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                
                if isempty(Args.CCDSEC)
                    Tmp = Operator(Obj(Iobj).(Args.DataPropIn), Args.OpArgs{:});
                    if ~isempty(Tmp)
                        Result(Iobj) = Tmp;
                    end
                else
                    Tmp = Operator(Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), Args.OpArgs{:});
                    if numel(Tmp)==1
                        Result(Iobj) = Tmp;
                    elseif numel(Tmp)==0
                        % do nothing - already NaN
                    else
                        error('funUnaryScalar operator must return a scalar');
                    end
                end
            end
            
        end
                
        function Cube = images2cubeIC(Obj, Args)
            % Generate a cube of images from ImageComponent object and return an ImageComponent with a cube
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
            %            'Class'    - Force thr output to be of some class.
            %                   If empty will use the first object class.
            %                   Default is empty.
            %            'Cube' - Pre allocated cube. This can be a
            %                   single-element ImageComponent object  with
            %                   pre-allocated cube with the exact same size
            %                   needed by the function. If provided, this
            %                   will be used instaed of allocating new
            %                   memory using the zeros command.
            %                   If empty, then the Cube will be allocated.
            %                   Default is [].
            % Output : - An Image component containing a cube of images.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC=ImageComponent({ones(5,5),2.*ones(5,5),3.*ones(5,5)});
            %          Cube = images2cubeIC(IC)
            
            arguments
                Obj
                Args.CCDSEC                        = [];  % empty for the entire image
                Args.DataPropIn                    = 'Data';
                Args.DimIndex                      = 3;
                Args.Class                         = [];
                Args.Cube                          = [];
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
            if isempty(Args.Class)
                Args.Class = class(Obj(Iobj).(Args.DataPropIn));
            end
            if isempty(Args.Cube)
                if Args.DimIndex==1
                    Cube = ImageComponent({Nobj, zeros(Size(1), Size(2), Args.Class)});    % faster
                    %Cube = zeros(Nobj, Size(1), Size(2), Args.Class);   % slower
                elseif Args.DimIndex==3
                    Cube = ImageComponent({zeros(Size(1), Size(2), Nobj, Args.Class)});
                else
                    error('DimINdex must be 1 or 3');
                end
            else
                % use previously allocated cube
                % assuming Cube is an ImageComponent
                Cube = Args.Cube;
            end
                
            if isempty(Args.CCDSEC)
                % generate cube from full images
                if Args.DimIndex==3
                    for Iobj=1:1:Nobj
                        Cube.(Args.DataPropIn)(:,:,Iobj) = Obj(Iobj).(Args.DataPropIn);
                    end
                else
                    % DimIndex = 1
                    
                    for Iobj=1:1:Nobj
                        Cube.(Args.DataPropIn)(Iobj,:,:) = Obj(Iobj).(Args.DataPropIn);
                    end
                    %Cube.(Args.DataPropIn) = permute(Cube.(Args.DataPropIn),[3 1 2]);

                end
                        
            else
                % generate cube from sub images (CCDSEC)
                if Args.DimIndex==3
                    for Iobj=1:1:Nobj
                        Isec = min(Iobj, Nsec);
                        Cube.(Args.DataPropIn)(:,:,Iobj) = Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(Isec,3):Args.CCDSEC(Isec,4), Args.CCDSEC(Isec,1):Args.CCDSEC(Isec,2));
                    end
                else
                    % DimIndex = 1
                    for Iobj=1:1:Nobj
                        Isec = min(Iobj, Nsec);
                        Cube.(Args.DataPropIn)(Iobj,:,:) = Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(Isec,3):Args.CCDSEC(Isec,4), Args.CCDSEC(Isec,1):Args.CCDSEC(Isec,2));
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
            %            'Class'    - Force thr output to be of some class.
            %                   If empty will use the first object class.
            %                   Default is empty.
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
                Args.Class                         = [];
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
            if isempty(Args.Class)
                Args.Class = class(Obj(Iobj).(Args.DataPropIn));
            end
            if Args.DimIndex==1
                Cube = zeros(Size(1), Size(2), Nobj, Args.Class);    % faster
                %Cube = zeros(Nobj, Size(1), Size(2), Args.Class);   % slower
            elseif Args.DimIndex==3
                Cube = zeros(Size(1), Size(2), Nobj, Args.Class);
            else
                error('DimINdex must be 1 or 3');
            end
            
            if isempty(Args.CCDSEC)
                % generate cube from full images
                if Args.DimIndex==3
                    for Iobj=1:1:Nobj
                        Cube(:,:,Iobj) = Obj(Iobj).(Args.DataPropIn);
                    end
                else
                    % DimIndex = 1
                    
                    for Iobj=1:1:Nobj
                        Cube(:,:,Iobj) = Obj(Iobj).(Args.DataPropIn);
                    end
                    Cube = permute(Cube,[3 1 2]);

                    % slower
                    %for Iobj=1:1:Nobj
                    %   Cube(Iobj,:,:) = Obj(Iobj).(Args.DataPropIn);
                    %end
                    
                end
                        
            else
                % generate cube from sub images (CCDSEC)
                if Args.DimIndex==3
                    for Iobj=1:1:Nobj
                        Isec = min(Iobj, Nsec);
                        Cube(:,:,Iobj) = Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(Isec,3):Args.CCDSEC(Isec,4), Args.CCDSEC(Isec,1):Args.CCDSEC(Isec,2));
                    end
                else
                    % DimIndex = 1
                    for Iobj=1:1:Nobj
                        Isec = min(Iobj, Nsec);
                        Cube(Iobj,:,:) = Obj(Iobj).(Args.DataPropIn)(Args.CCDSEC(Isec,3):Args.CCDSEC(Isec,4), Args.CCDSEC(Isec,1):Args.CCDSEC(Isec,2));
                    end
                end
                    
            end
                
            % change dimension
%             if Args.DimIndex==3
%                 % do nothing
%             elseif Args.DimIndex==1
%                 Cube = permute(Cube, [3 1 2]);
%             else
%                 error('DimIndex must be 1 or 3');
%             end
            
        end
                
    end
    
    methods % background and variance
        
        function [Back, Var] = background(Obj, varargin)
            % Estimate the background and variance of an ImageComponent
            % Input  : - An ImageComponent (multi element suppoted)
            %            The back/var are estimated from the 'Image'
            %            property.
            %          * Additional arguments to pass to:
            %            imUtil.background.background
            %            Default is no parameters.
            % Output : - A BackImage with the estimated background in the
            %            Data property.
            %          - A VarImage with the estimated variance in the
            %            Data property.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent({100+randn(1000,1000), 50+randn(100,100)})
            %          [B,V] = background(IC);
            
            Nobj = numel(Obj);
            Back = BackImage(size(Obj));
            Var  = VarImage(size(Obj));
            for Iobj=1:1:Nobj
                [Back(Iobj).Data, Var(Iobj).Data] = imUtil.background.background(Obj(Iobj).Image, varargin{:});
                % FFU: set scale parameters if needed
            end
            
        end
        
        function Obj = subtractBack(Obj, Back, varargin)
            % subtract background from an ImageComponent
            %       If this is a SciImage, then also set the IsBackSubtracted
            %       to true.
            % Input  : - An ImageComponet object.
            %          - Background, either in ImageComponent, a matrix or
            %            a cell array of matrices. If empty, cal back.
            %            Default is [].
            %          * Additional arguments to pass to the background
            %            function. Default is no arguments.
            % Output : - A background subtracted ImageComponent
            % Author : Eran Ofek (Apr 2021)
            % Example:
                
        
            if nargin<2
                Back = [];
            end
            
            Nobj = numel(Obj);
            
            if isempty(Back)
                % estimate background
                Back = background(Obj, varargin{:});
            end
            
            if isnumeric(Back)
                Back = ImageComponent({Back});
            elseif iscell(Back)
                Back = ImageComponent(Back);
            else
                % do nothing
            end
            
            Nback = numel(Back);
            
            if isa(Obj,'SciImage')
                % SciImage has a IsBackSubtracted prop
                [Obj(1:1:Nobj).IsBackSubtracted] = deal(true);
            end
            
            N = max(Nobj, Nback);
            for I=1:1:N
                Iobj  = min(I, Nobj);
                Iback = min(I, Nback);
                
                Obj(I).Data = Obj(Iobj).Image - Back(Iback).Image;
                if isa(Obj(Iobj),'SciImage')
                    Obj(Iobj).IsBackSubtracted = true;
                end
            end
            
            
        end
        
        % funStack (including scaling and zero subtracting)
        function [ResCoadd, ResCoaddVarEmpirical, ResCoaddVar, ResCoaddN] = funStack(Obj, Args)
            % Simple stack (coadd) images in an ImageComponent object without pre/post normalization
            % Input  : - An ImageComponent object.
            %          * ...,key,val,...
            %            'CCDSEC' - [Xmin Xmax Ymin Ymax] to stack.
            %                       If empty, use entire image. Default is
            %                       [].
            %            'VarImage' - An ImageComponent of the variance
            %                   images. Default is [].
            %            'StackMethod' - Stacking method. Options are:
            %                   'sum'
            %                   ['mean']
            %                   'median'
            %                   'var'
            %                   'rvar'
            %                   'min'
            %                   'max'
            %                   'range'
            %                   'quantile' - rquires a quqntile argument.
            %                   'wmean'
            %                   'sigmaclip' - for arguments see: imUtil.image.mean_sigclip
            %                   'wsigmaclip' - for arguments see: imUtil.image.wmean_sigclip
            %                   'bitor' - bit-wise or operation. Return only Coadd.
            %                   'bitand' - bit-wise and operation. Return only Coadd.
            %                   'bitnot' - bit-wise not operation. Return only Coadd.
            %              'StackArgs' - A cell array of arguments to pass to the
            %                   method function. Default is {}.
            %              'EmpiricalVarFun' - Default is @var.
            %              'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.
            %              'MedianVarCorrForEmpirical' - A logical indicating if to
            %                   correct the variance calculation by the ratio between
            %                   the variance of the median and variance of the mean.
            %                   Default is false.
            %              'DivideEmpiricalByN' - A logical indicating if to divide
            %                   CoaddVarEmpirical by N. Default is false.
            %              'DivideVarByN' - A logical indicating if to divide
            %                   CoaddVar by N. Default is false.
            %              'CalcCoaddVarEmpirical' - Logical indicating if to calc the
            %                   CoaddVarEmpirical. Default is true.
            %              'CalcCoaddVar' - Logical indicating if to calc the
            %                   CoaddVar. Default is true.
            %              'CalcCoaddN' - Logical indicating if to calc the
            %                   CoaddN. Default is true.
            %              'DataPropIn' - Data property on which to
            %                   operate in the ImageComponent.
            %                   Default is 'Data'.
            %              'OutIsMat' - A logical indicating if the output
            %                   is matrix or some image object.
            %                   Default is false.
            %              'CoaddClass' - Class for the coadd image (if
            %                   OutIsMat=false). Default is @SciImage.
            %              'CoaddVarEmpiricalClass' - Default is @VarImage.
            %              'CoaddVarClass' - Default is @VarImage.
            %              'CoaddNClass' - Default is @VarImage.
            % Output : - Coadd image.
            %          - CoaddVarEmpirical - This is the empirical variance of the
            %               data. In case the variance of the mean is needed set DivideEmpiricalByN
            %               to true.
            %          - CoaddVar - This is the variance of the
            %               data. In case the variance of the mean is needed set DivideByN
            %               to true.
            %          - CoaddN - This is the number of images used in the stacking of
            %               each pixel.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent({2.*randn(100,100), 2.*randn(100,100), 2.*randn(100,100)})
            %          Var = ImageComponent({4.*ones(100,100), 4.*ones(100,100), 4.*ones(100,100)})
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = funStack(IC);
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'StackMethod','wmean','VarImage',Var);
            
            arguments
                Obj
                Args.CCDSEC                          = [];
                
                Args.VarImage                        = [];
                Args.StackMethod                     = 'mean';
                Args.StackArgs                       = {};
                Args.EmpiricalVarFun                 = @var;
                Args.EmpiricalVarFunArgs             = {[],3,'omitnan'};
                
                Args.MedianVarCorrForEmpirical(1,1) logical = false;
                Args.DivideEmpiricalByN(1,1) logical        = false;
                Args.DivideVarByN(1,1) logical              = false;
                Args.CalcCoaddVarEmpirical(1,1) logical     = true;
                Args.CalcCoaddVar(1,1) logical              = true;
                Args.CalcCoaddN(1,1) logical                = true;
                
                Args.DataPropIn                      = 'Data';
                Args.OutIsMat(1,1) logical           = false;
                Args.CoaddClass                      = @SciImage;
                Args.CoaddVarEmpiricalClass          = @VarImage;
                Args.CoaddVarClass                   = @VarImage;
                Args.CoaddNClass                     = @VarImage;
            end
            
            
            % generate a cube
            Cube = images2cube(Obj, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DimIndex',3);
            
            % generate a cube of variance
            if ~isempty(Args.VarImage)
                VarCube = images2cube(Args.VarImage, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DimIndex',3);
            else
                VarCube = [];
            end
            
            [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube, 'StackMethod',Args.StackMethod,...
                                                                                        'StackArgs',Args.StackArgs,...
                                                                                        'EmpiricalVarFun',Args.EmpiricalVarFun,...
                                                                                        'EmpiricalVarFunArgs',Args.EmpiricalVarFunArgs,...
                                                                                        'VarCube',VarCube,...
                                                                                        'MedianVarCorrForEmpirical',Args.MedianVarCorrForEmpirical,...
                                                                                        'DivideEmpiricalByN',Args.DivideEmpiricalByN,...
                                                                                        'DivideVarByN',Args.DivideVarByN,...
                                                                                        'CalcCoaddVarEmpirical',Args.CalcCoaddVarEmpirical,...
                                                                                        'CalcCoaddVar',Args.CalcCoaddVar,...
                                                                                        'CalcCoaddN',Args.CalcCoaddN);
          
            if Args.OutIsMat
                ResCoadd             = Coadd;
                ResCoaddVarEmpirical = CoaddVarEmpirical;
                ResCoaddVar          = CoaddVar;
                ResCoaddN            = CoaddN;
            else
                % convert output matrices to ImageComponent
                ResCoadd             = Args.CoaddClass({Coadd});
                ResCoaddVarEmpirical = Args.CoaddVarEmpiricalClass({CoaddVarEmpirical});
                ResCoaddVar          = Args.CoaddVarClass({CoaddVar});
                ResCoaddN            = Args.CoaddNClass({CoaddN});
            end
        end
        
        function [ResCoadd, ResCoaddVarEmpirical, ResCoaddVar, ResCoaddN] = coadd(Obj, Args)
            % coadd images in an ImageComponent object including pre/post
            % normalization and variance
            % Input  : - An ImageComponent object.
            %          * ...,key,val,...
            %            'CCDSEC' - [Xmin Xmax Ymin Ymax] to stack.
            %                       If empty, use entire image. Default is
            %                       [].
            %            'SubBack' - A logical indicating if to subtract
            %                   background using the background command.
            %                   Default is false.
            %            'BackArgs' - A cell array of background arguments.
            %                   Default is {}.
            %            'SubMethod' - Either a vector of numbers to
            %                   subtract from each image, or a function
            %                   handle which is gettug a cube, and
            %                   returining a new cube.
            %                   Default is empty (do nothing).
            %            'SubArgs' - A cell array of additional arguments
            %                   to pass to the 'SubMethod' function.
            %                   Default is {}.
            %            'NormMethod' - Eiher a vector of numbrers (one per
            %                   image) for normalizing the images after
            %                   subtraction and prior to stacking,
            %                   or a function handle that get a cube and
            %                   return a vector of numbers.
            %                   If empty do nothing (default).
            %            'NormArgs' - A cell array of additional arguments
            %                   to pass to the 'NormMethod' function.
            %                   Default is {}.
            %            'NormOperator' - The operator that will used with
            %                   the normalization values. Default is
            %                   @times.
            %            'VarImage' - An ImageComponent of the variance
            %                   images. Default is [].
            %            'StackMethod' - Stacking method. Options are:
            %                   'sum'
            %                   ['mean']
            %                   'median'
            %                   'var'
            %                   'rvar'
            %                   'min'
            %                   'max'
            %                   'range'
            %                   'quantile' - rquires a quqntile argument.
            %                   'wmean'
            %                   'sigmaclip' - for arguments see: imUtil.image.mean_sigclip
            %                   'wsigmaclip' - for arguments see: imUtil.image.wmean_sigclip
            %                   'bitor' - bit-wise or operation. Return only Coadd.
            %                   'bitand' - bit-wise and operation. Return only Coadd.
            %                   'bitnot' - bit-wise not operation. Return only Coadd.
            %              'StackArgs' - A cell array of arguments to pass to the
            %                   method function. Default is {}.
            %              'EmpiricalVarFun' - Default is @var.
            %              'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.
            %              'MedianVarCorrForEmpirical' - A logical indicating if to
            %                   correct the variance calculation by the ratio between
            %                   the variance of the median and variance of the mean.
            %                   Default is false.
            %              'DivideEmpiricalByN' - A logical indicating if to divide
            %                   CoaddVarEmpirical by N. Default is false.
            %              'DivideVarByN' - A logical indicating if to divide
            %                   CoaddVar by N. Default is false.
            %              'CalcCoaddVarEmpirical' - Logical indicating if to calc the
            %                   CoaddVarEmpirical. Default is true.
            %              'CalcCoaddVar' - Logical indicating if to calc the
            %                   CoaddVar. Default is true.
            %              'CalcCoaddN' - Logical indicating if to calc the
            %                   CoaddN. Default is true.
            %              'NormEnd' - Eiher a vector of numbrers (one per
            %                   image) for normalizing the images after
            %                   stacking,
            %                   or a function handle that get a cube and
            %                   return a vector of numbers.
            %                   If empty do nothing (default).
            %              'NormEndArgs' - A cell array of additional arguments
            %                   to pass to the 'NormEnd' function.
            %                   Default is {}.
            %              'NormEndOperator' - The operator that will used with
            %                   the normalization values. Default is
            %                   @rdivide.
            %              'DataPropIn' - Data property on which to
            %                   operate in the ImageComponent.
            %                   Default is 'Data'.
            %              'OutIsMat' - A logical indicating if the output
            %                   is matrix or some image object.
            %                   Default is false.
            %              'CoaddClass' - Class for the coadd image (if
            %                   OutIsMat=false). Default is @SciImage.
            %              'CoaddVarEmpiricalClass' - Default is @VarImage.
            %              'CoaddVarClass' - Default is @VarImage.
            %              'CoaddNClass' - Default is @VarImage.
            % Output : - Coadd image.
            %          - CoaddVarEmpirical - This is the empirical variance of the
            %               data. In case the variance of the mean is needed set DivideEmpiricalByN
            %               to true.
            %          - CoaddVar - This is the variance of the
            %               data. In case the variance of the mean is needed set DivideByN
            %               to true.
            %          - CoaddN - This is the number of images used in the stacking of
            %               each pixel.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC = ImageComponent({2.*randn(100,100), 2.*randn(100,100), 2.*randn(100,100)})
            %          Var = ImageComponent({4.*ones(100,100), 4.*ones(100,100), 4.*ones(100,100)})
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC);
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'StackMethod','wmean','VarImage',Var);
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] =
            %          coadd(IC,'SubMethod',[10 10 10]);
            %          %mean(CoaddVarEmpirical.Image,'all') should be close
            %          to 4
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] =
            %          coadd(IC,'SubMethod',[10 10 10],'NormMethod',[0.5
            %          0.5 0.5]); % mean(CoaddVarEmpirical.Image,'all')
            %          should be close to 1
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'SubMethod',[10 10 10],'NormMethod',[0.5 0.5 0.5],'NormEnd',5); % mean(CoaddVarEmpirical.Image,'all')
            %          % mean(Coadd.Image,'all') should be near -25
            %          mean(CoaddVarEmpirical.Image,'all') should be near
            %          25
            %          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN]=coadd(IC,'SubMethod',-100,'NormEnd',@median,'NormEndArgs',{'all'},'NormEndOperator',@rdivide);
            %          % mean(Coadd.Image,'all') should be near 1
            
            
            %%%% THIS FUNCTION REQUIRES FURTHER TESTING!!!!
            
            arguments
                Obj
                Args.CCDSEC                          = [];
                Args.SubBack(1,1) logical            = false;
                Args.BackArgs cell                   = {};
                Args.SubMethod                       = [];  % function_handle or vector of numbers
                Args.SubArgs                         = {};
                Args.NormMethod                      = [];  % function_handle or vector of numbers
                Args.NormArgs                        = {};
                Args.NormOperator function_handle    = @times;
                
                Args.EmpiricalVarFun                 = @var;
                Args.EmpiricalVarFunArgs cell        = {[],3,'omitnan'};
                
                Args.VarImage                        = [];
                Args.StackMethod                     = 'mean';
                Args.StackArgs                       = {};
                
                Args.MedianVarCorrForEmpirical(1,1) logical = false;
                Args.DivideEmpiricalByN(1,1) logical        = false;
                Args.DivideVarByN(1,1) logical              = false;
                Args.CalcCoaddVarEmpirical(1,1) logical     = true;
                Args.CalcCoaddVar(1,1) logical              = true;
                Args.CalcCoaddN(1,1) logical                = true;
    
                Args.NormEnd                         = [];  % function_handle or vector of numbers
                Args.NormEndArgs                     = {};
                Args.NormEndOperator                 = @rdivide;
                Args.DataPropIn                      = 'Data';
                Args.OutIsMat(1,1) logical           = false;
                Args.CoaddClass                      = @SciImage;
                Args.CoaddVarEmpiricalClass          = @VarImage;
                Args.CoaddVarClass                   = @VarImage;
                Args.CoaddNClass                     = @VarImage;
            end
            
            if nargout<4
                Args.CalacCoaddN = false;
                if nargout<3
                    Args.CalcCoaddVar = false;
                    if nargout<2
                        Args.CalcCoaddVarEmpirical = false;
                    end
                end
            end
            
            if Args.SubBack
                % subtract background (only if not subtracted)
                Obj = subtractBack(Obj, [], Args.BackArgs{:});
            end
            
            % generate a cube
            Cube = images2cube(Obj, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DimIndex',3);
            
            % generate a cube of variance
            if ~isempty(Args.VarImage)
                VarCube = images2cube(Args.VarImage, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DimIndex',3);
            else
                VarCube = [];
            end
            
            % subtract additional value
            if ~isempty(Args.SubMethod)
                if isa(Args.SubMethod,'function_handle')
                    Cube = Args.SubMethod(Cube, Args.SubArgs{:});
                elseif isnumeric(Args.SubMethod)
                    Cube = Cube - reshape(Args.SubMethod,[1 1 numel(Args.SubMethod)]);
                else
                    error('Unknown SubMethod option');
                end
            end
            
            % normalize
            if ~isempty(Args.NormMethod)
                if isa(Args.NormMethod,'function_handle')
                    PreNormFactor = Args.NormMethod(Cube, Args.NormArgs{:});
                    Cube = Args.NormOperator(Cube, reshape(PreNormFactor,[1 1 numel(PreNormFactor)]));
                    VarCube = Args.NormOperator(VarCube, reshape(PreNormFactor,[1 1 numel(PreNormFactor)]).^2);
                elseif isnumeric(Args.NormMethod)
                    Cube = Args.NormOperator(Cube, reshape(Args.NormMethod,[1 1 numel(Args.NormMethod)]));
                    VarCube = Args.NormOperator(Cube, reshape(Args.NormMethod,[1 1 numel(Args.NormMethod)]).^2);
                else
                    error('Unknown SubMethod option');
                end
            end
            
            [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube, 'StackMethod',Args.StackMethod,...
                                                                                        'StackArgs',Args.StackArgs,...
                                                                                        'VarCube',VarCube,...
                                                                                        'EmpiricalVarFun',Args.EmpiricalVarFun,...
                                                                                        'EmpiricalVarFunArgs',Args.EmpiricalVarFunArgs,...
                                                                                        'MedianVarCorrForEmpirical',Args.MedianVarCorrForEmpirical,...
                                                                                        'DivideEmpiricalByN',Args.DivideEmpiricalByN,...
                                                                                        'DivideVarByN',Args.DivideVarByN,...
                                                                                        'CalcCoaddVarEmpirical',Args.CalcCoaddVarEmpirical,...
                                                                                        'CalcCoaddVar',Args.CalcCoaddVar,...
                                                                                        'CalcCoaddN',Args.CalcCoaddN);
          
                
            % normalize
            if ~isempty(Args.NormEnd)
                if isa(Args.NormEnd,'function_handle')
                    NormFactor = Args.NormEnd(Coadd, Args.NormEndArgs{:});
                    Coadd = Args.NormEndOperator(Coadd, NormFactor);
                    CoaddVar = Args.NormEndOperator(CoaddVar, NormFactor.^2);
                    CoaddVarEmpirical = Args.NormEndOperator(CoaddVarEmpirical, NormFactor.^2);
                    
                else
                    Coadd = Args.NormEndOperator(Coadd, Args.NormEnd);
                    CoaddVar = Args.NormEndOperator(CoaddVar, Args.NormEnd.^2);
                    CoaddVarEmpirical = Args.NormEndOperator(CoaddVarEmpirical, Args.NormEnd.^2);
                end
                
            end
            
            
            if Args.OutIsMat
                ResCoadd             = Coadd;
                ResCoaddVarEmpirical = CoaddVarEmpirical;
                ResCoaddVar          = CoaddVar;
                ResCoaddN            = CoaddN;
            else
                % convert output matrices to ImageComponent
                ResCoadd             = Args.CoaddClass({Coadd});
                ResCoaddVarEmpirical = Args.CoaddVarEmpiricalClass({CoaddVarEmpirical});
                ResCoaddVar          = Args.CoaddVarClass({CoaddVar});
                ResCoaddN            = Args.CoaddNClass({CoaddN});
            end
            
            
        end
        
        
        % funTransform
    end
    
    methods % replace, rotate, resize
        
        function [Result, ObjReplaced] = replace(Obj, Range, NewVal, Args)
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
            %          * ...,key,val,...
            %            'Eps' - A value to subtract to the lower range and add to
            %                   the upper range prior to comparison. This is
            %                   useful in order to avoid problems with comparison
            %                   of floating point numbers. Default is 0.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - The same object, but with the updated values.
            %          - An ImageComponent in which the image contains
            %            logical indicating which pixels were replaced.
            % Author : Eran Ofek (Mar 2021)
            % Example: [Obj,ObjReplaced] = replace(Obj, Range, NewVal);
            
            arguments
                Obj
                Range      {mustBeNumeric(Range)}
                NewVal     {mustBeNumeric(NewVal)}
                Args.Eps   {mustBeNumeric(Args.Eps)}   = 0;
                Args.CreateNewObj                      = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
            end
            
            Nnewval = numel(NewVal);
            Nrange  = size(Range,1);
            Nobj    = numel(Obj);
            
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            if nargout>1
                ObjReplaced = ImageComponent(size(Obj));
            end
            
            for Iobj=1:1:Nobj
                FlagAll = false(numel(Obj(Iobj).Image),1);
                for Irange=1:1:Nrange
                    Inew   = min(Irange, Nnewval);
                    if any(isnan(Range(Irange,:)))
                        % Replace NaN
                        Flag = isnan(Obj(Iobj).Image(:));
                    else
                        % Replace values (not NaN)
                        MinVal = min(Range(Irange,:),[],2) - Args.Eps;
                        MaxVal = max(Range(Irange,:),[],2) + Args.Eps;

                        Flag   = Obj(Iobj).Image(:)>=MinVal & Obj(Iobj).Image(:)<=MaxVal;
                    end
                    
                    Result(Iobj).Image(Flag) = NewVal(Inew);
                    
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
            %            image. If empty, then will attempt to take this
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
                
                if NewScale==1
                    NewScale = [];
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
            %       Set CCDSEC to [].
            % Input  : - An ImageComponent object.
            %          - Rotation angle [deg].
            %          * ...,key,val,...
            %            'Method' - imrotate interpolation method.
            %                   Default is 'bicubic'.
            %            'BBox' - imrotate bounding box ['loose'] | 'crop'.
            %            'DataProp' - Data prop on which to apply the
            %                   rotation. Default is 'Image'.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
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
                Args.CreateNewObj               = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
            end
            
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj).(Args.DataProp) = imrotate(Obj(Iobj).(Args.DataProp), RotationAng, Args.Method, Args.BBox);
                Result(Iobj).CCDSEC          = [];
            end
            
        end
          
    end
    
    methods % break/rejoin image to smaller images
        
        function [Result] = crop(Obj, CCDSEC, Args)
            % Crop an ImageComponent object. Either apply multiple trims to
            %       a single image, or a single trime to multiple images,
            %       or multiple trims to multiple images (one to one).
            %       Update also the CCDSEC propery.
            % Input  : - An ImageComponent object.
            %          - Either [minX, maxX, minY, maxY] (Type='ccdsec')
            %            or [Xcenter, Ycenter, Xhalfsize, Yhalfsize] (Type = 'center')
            %            or [Xhalfsize, Yhalfsize] (Type = 'center').
            %          * ..., key, val,...
            %            'Type' - ['ccdsec'] | 'center'
            %            'DataPropIn' - Data property on which to operate.
            %                   Default is 'Data'.
            %            'CreateNewObj' - Copy to new object.
            %                   Default is false.
            % Output : - An ImageComponent object with the cropped images.
            % Author : Eran Ofek (Apr 2021)
            % Example: IC=ImageComponent({rand(5,5),2.*rand(5,5),3.*ones(5,5)});
            %          Res = crop(IC,[1 2 1 3])
            
            arguments
                Obj
                CCDSEC                  % [xmin xmax ymin ymax]
                Args.Type char                   = 'ccdsec';
                Args.DataPropIn char             = 'Data';
                Args.CreateNewObj logical        = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            Nsec = size(CCDSEC,1);
            if Nobj==Nsec || Nobj==1 || Nsec==1
                Nmax = max(Nobj, Nsec);
                if Args.CreateNewObj
                    Result = ImageComponent([Nmax,1]);
                else
                    Result = Obj;
                end
                for Imax=1:1:Nmax
                    Iobj = min(Imax, Nobj);
                    Isec = min(Imax, Nsec);
                    
                    [Result(Imax).Data, UsedCCDSEC]   = imUtil.cut.trim(Obj(Iobj).(Args.DataPropIn), CCDSEC(Isec,:), Args.Type);
                    Result(Imax).CCDSEC = UsedCCDSEC;
                end
            else
                error('crop function works on a single ImageComponent, or a single CCDSEC or number of images equal to number of sections');
            end
            
            
        end
        
        function [Result,EdgesCCDSEC,ListCenters,NoOverlapCCDSEC] = image2subimages(Obj, BlockSize, Args)
            % break an image in a single element ImageComponent into sub images.
            % Input  : - An ImageComponent object with a single element.
            %            If the image is scaled, then will rescale the
            %            image (i.e., will use the 'Image' property).
            %          - BlockSize [X, Y] of sub images. or [X] (will be copied as [X, X]).
            %            If empty, will use imUtil.cut.subimage_grid
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
                BlockSize             = [256 256];   % If empty, will use imUtil.cut.subimage_grid
                Args.CCDSEC           = [];   % [xmin xmax ymin ymax] If given, override BlockSize
                Args.Nxy              = [];   % If empty then use SubSizeXY. Default is [].
                Args.OverlapXY        = 10;   % Optionally [overlapX overlapY]
                
                %Args.ListEdge         = [];
                %Args.ListCenter       = [];
            end
            
            
            [Sub,EdgesCCDSEC,ListCenters,NoOverlapCCDSEC] = ...
                    imUtil.cut.partition_subimage(Obj.Image, Args.CCDSEC,...
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
                Result(Isub).Data   = Sub.Im;
                Result(Isub).Scale  = [];
                Result(Isub).CCDSEC = EdgesCCDSEC(Isub,:);
            end
            
        end
        
        
        function Result = subimages2image(Obj)
            % break image to sub images
            
            % consider writing a new version of:
            % [FullImage]=imUtil.cut.subimages2image(SubImage,CCDSEC);
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
            %            'PadVal' - padding value for cutouts near edge or
            %                   without circular shifts.
            %            'CutAlgo' - Algorithm: ['mex'] | 'wmat'.
            %            'IsCircle' - If true then will pad each cutout
            %                   with NaN outside the HalfSize radius.
            %                   Default is false.
            %            'Shift' - A logical indicating if to shift
            %            'ShiftAlgo' - Shift algorithm ['lanczos3'] |
            %                   'lanczos2' | 'fft'.
            %            'IsCircFilt' - While using lanczos, is circshift
            %                   is circular or not. Default is false.
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
            %          Cube = cutouts(IC, XY,'Shift',true);
            %          Cube = cutouts(IC, XY,'Shift',true,'IsCircFilt',true);
            
            arguments
                Obj(1,1)
                XY(:,2)                     = zeros(0,2);
                Args.HalfSize               = 8;
                Args.PadVal                 = NaN;
                Args.CutAlgo                = 'mex';  % 'mex' | 'wmat'
                Args.IsCircle               = false;
                Args.Shift(1,1) logical     = false;
                Args.ShiftAlgo              = 'lanczos3';  % 'fft' | 'lanczos2' | 'lanczos3' | ...
                Args.IsCircFilt(1,1) logical = true;
                Args.DataProp               = 'Image';
            end
            
            CutoutSize = 1+2.*Args.HalfSize;
            
            RoundXY    = round(XY);
            
            Iobj = 1;
            switch lower(Args.CutAlgo)
                case 'mex'
                    [CutoutCube] = imUtil.cut.mex.mex_cutout(Obj(Iobj).(Args.DataProp), RoundXY, CutoutSize, Args.PadVal, 0, 0, 1);
                    CutoutCube   = squeeze(CutoutCube);
                case 'wmat'
                    [CutoutCube] = imUtil.cut.find_within_radius_mat(Obj(Iobj).(Args.DataProp), RoundXY(:,1), RoundXY(:,2), Args.HalfSize, Args.IsCircle);
                otherwise
                    error('Unknown Algo option');
            end
            
            % shift cutouts
            if Args.Shift
                ActualXY  = XY;
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
                        
                    case 'lanczos2'
                        CutoutCube = imUtil.trans.shift_lanczos(CutoutCube, XY, 2, Args.IsCircFilt, Args.PadVal);
                    case 'lanczos3'
                        CutoutCube = imUtil.trans.shift_lanczos(CutoutCube, XY, 3, Args.IsCircFilt, Args.PadVal);
                    otherwise
                        error('Unknown ShiftAlgo option');
                end
            
            else
                ActualXY  = RoundXY;
            end
        end
        
        
        function Result = funCutouts(Obj, XY, Fun, Args)
            % Apply function (that returns a scalar) on image cutouts
            % Input  : - A single-element ImageComponent object.
            %          - A two column matrix of [X, Y] positions.
            %            Positions will be rounded.
            %          - A function handle to operate on each cutout.
            %            The function must returns a scalar.
            %          * ...,key,val,...
            %            'FunArgs' - A cell array of additional arguments
            %                   to pass to the function. Default is {}.
            %            'HalfSize' - Cutout half size (actual size will be
            %                   1+2*HalfSize. Default is 8.
            %            'PadVal' - Padding value. Default is NaN.
            %            'CutAlgo' - Algorithm: ['mex'] | 'wmat'.
            %            'IsCircle' - If true then will pad each cutout
            %                   with NaN outside the HalfSize radius.
            %                   Default is false.
            %            'DataProp' - Data property from which to extract
            %                   the cutouts. Default is 'Image'.
            % Output : - A column vector of function output per each
            %            cutout.
            % Author : Eran Ofek (Jul 2021)
            % Example: IC=ImageComponent({uint16(ones(100,100))});
            %          Result = funCutouts(IC, [1 1; 2 2; 10 10; 30 30], @tools.array.bitor_array)
            %          IC=ImageComponent({uint32(ones(100,100))});
            %          Result = funCutouts(IC, [1 1; 2 2; 10 10; 30 30], @tools.array.bitor_array,'CutAlgo','wmat')
            
            arguments
                Obj(1,1)
                XY
                Fun function_handle
                Args.FunArgs cell           = {};
                
                Args.PadVal                 = NaN;
                Args.HalfSize               = 3;
                Args.CutAlgo                = 'mex';  % 'mex' | 'wmat'
                Args.IsCircle               = false;
                Args.DataProp               = 'Image';
                
            end
            
            Ncut = size(XY,1);
            [CutoutCube] = cutouts(Obj, XY, 'HalfSize',Args.HalfSize,...
                                                      'PadVal',Args.PadVal,...
                                                      'CutALgo',Args.CutAlgo,...
                                                      'IsCircle',Args.IsCircle,...
                                                      'DataProp',Args.DataProp);
                                                  
            Array = reshape(CutoutCube, (1+2.*Args.HalfSize).^2, Ncut);
            
            Result = Fun(Array, Args.FunArgs{:});
            Result = Result(:);
           
        end
        
    end
    
    
    methods(Static) % getDbDriver
        
        function Result = getImageByKey(Key)
            persistent Map
            if isempty(Map)
                Map = ComponentMap('ImageComponent');
            end
                      
            Image = Map.find(Key);
            if isempty(Comp)
            else
            end
            
            Result = Image;

        end
    end

    
    
    methods (Static)  % unitTest
        Result = unitTest
            % unitTest for ImageComponent

    end
end
