% CubeComponent - A container class for Cubes of images
% Properties :
%
% Functionality :
%
%



classdef CubeComponent < Component
    properties (Dependent)
        Image
    end
    
    properties (SetAccess = public)
        Data(:,:,:)
        CCDSEC(:,4)      = zeros(0,4)
        XY(:,2)          = zeros(0,2);
        HalfSizeXY
    end
    
    properties
        Dim      = 3;
    end
    
    methods % Constructor
       

    end
    
    methods (Static) % images to cube
        function Obj=images2cube(Images, Args)
            % Create a CubeComponent object with cube from a set of images
            % Input  : - A cell array of images, or an AstroImage object
            %            array.
            %          * ...,key,val,...
            %            'Dim' - Dimension of the image index in the cube.
            %                   Default is 3.
            %            'CCDSEC' - An optional CCDSEC of the input images
            %                   to which to copy to the cube.
            %                   This is a 4-cilumn matrix with line per
            %                   image, or a single line for all images.
            %                   If empty, use the entire image.
            %                   Default is [].
            %            'DataPropIn' - Data property, in ImageComponent,
            %                   from which to take
            %                   the image. Default is 'Image'.
            %            'DataProp' - The data properties for which the
            %                   cubes will be calculated.
            %                   Default is {'ImageData','BackData',
            %                   'VarData', 'MaskData'}.
            %            'Cube' - Pre allocated cube. This can be a
            %                   pre-allocated cube with the exact same size
            %                   needed by the function. If provided, this
            %                   will be used instaed of allocating new
            %                   memory using the zeros command.
            %                   If empty, then the Cube will be allocated.
            %                   Default is [].
            % Output : - An CubeComponent object with the images converted
            %            into a cube.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im{1}=rand(100,100); Im{2}=rand(100,100);
            %          C=CubeComponent.images2cube(Im);
            %          Im{1}=rand(100,100); Im{2}=rand(102,100);
            %          C=CubeComponent.images2cube(Im, 'CCDSEC',[1 90 1 90]);
            %          Im = AstroImage({rand(100,100), rand(100,100)});
            %          C=CubeComponent.images2cube(Im);
            %          C=CubeComponent.images2cube(Im, 'CCDSEC',[11 90 1 100]);
                        
            arguments
                Images
                Args.Dim                           = 3;
                
                Args.CCDSEC                        = [];
                Args.DataPropIn                    = 'Image';
                Args.DataProp                      = 'ImageData';
                Args.Cube                          = [];
            end
            
            Nccd = size(Args.CCDSEC,1);
            
            Obj = CubeComponent;
            
            if iscell(Images)
                Nim  = numel(Images);
                if isempty(Args.CCDSEC)
                    Size = size(Images{1});
                else
                    Size = [Args.CCDSEC(4)-Args.CCDSEC(3)+1, Args.CCDSEC(2)-Args.CCDSEC(1)+1];
                end
                Obj.Data = zeros([Size Nim]);
                for Iim=1:1:Nim
                    if isempty(Args.CCDSEC)
                        Obj.Data(:,:,Iim) = Images{Iim};
                    else
                        Iccd = min(Iim,Nccd);
                        Obj.Data(:,:,Iim) = Images{Iim}(Args.CCDSEC(Iccd,3):Args.CCDSEC(Iccd,4), Args.CCDSEC(Iccd,1):Args.CCDSEC(Iccd,2));
                    end
                end
                Obj.shiftDim(Args.Dim);
                
            elseif isa(Images, 'AstroImage')
                Obj.Data = imProc.image.images2cube(Images, 'DimIndex',Args.Dim,...
                                               'CCDSEC',Args.CCDSEC,...
                                               'DataPropIn',Args.DataPropIn,...
                                               'DataProp',Args.DataProp,...
                                               'Cube',Args.Cube);
            else
                error('Uknown Images class');
            end
            Obj.CCDSEC = Args.CCDSEC;
        end
        
        function Obj=cutouts(Image, X, Y, Radius, Args)
            % Given an image, generate an CubeComponent object with cutouts around [X,Y] positions.
            % Input  : - A single element AstroImage object or a 2D matrix image.
            %            If a cube, then the 3rd dimension is the image index.
            %          - Vector of X positions.
            %            If empty, and the input is cube, then will set X to the
            %            center of cutout.
            %          - Vector of Y coordinates.
            %          - Radius of cutouts. Default is 12.
            %          * ...,key,val,...
            %            'mexCutout' - use imUtil.cut.mex.mex_cutout.m (true) or
            %                   imUtil.cut.find_within_radius_mat (false).
            %                   Default is true.
            %            'Circle' - If true, then will set all points outside the radius to NaN.
            %                   Default is false.
            % Output : - A single element CubeComponent object with the cube of stamps
            %            around the requested positions.
            % Author : Eran Ofek (Jun 2023)
            % Example: C = CubeComponent.cutouts(rand(100,100),[1 16 17]',[45 17 98]');
            %          C = CubeComponent.cutouts(AstroImage({rand(100,100)}),[1 16 17]',[45 17 98]');
            
            arguments
                Image
                X
                Y
                Radius                    = 12;
                Args.mexCutout logical    = true;
                Args.Circle logical       = false;
            end
            
            Obj = CubeComponent;
            if isa(Image, 'AstroImage')
                % Assume Image is a single elemnt AstroImage object
                [Obj.Data, RoundX, RoundY] = imUtil.cut.image2cutouts(Image.Image, X, Y, Radius, 'mexCutout',Args.mexCutout,...
                                                                                                 'Circle',Args.Circle);
            else
                % assume a image matrix
                [Obj.Data, RoundX, RoundY] = imUtil.cut.image2cutouts(Image, X, Y, Radius, 'mexCutout',Args.mexCutout,...
                                                                                           'Circle',Args.Circle);
            end
            Obj.XY         = [RoundX, RoundY];
            Obj.HalfSizeXY = Radius;
            Obj.CCDSEC     = [RoundX(:) - Radius, RoundX(:) + Radius, RoundY(:) - Radius, RoundY(:) + Radius];
            
        end
    end
    
    methods % setter/getter
        function set.Image(Obj, Val)
            % setter for dependent property Image
            Obj.Data = Val;
        end
        function Result=get.Image(Obj)
            % getter for dependent property Image
            Result = Obj.Data;
        end
        
    end
    
    methods % utilities
        function N=numImages(Obj)
            % Return the number of images in each cube of images
            % Input  : - self.
            % Output : - An array with the number if images in each cube
            %           (e.g.., 3rd dim). One result per element.
            % Author : Eran Ofek (Jun 2023)
            % Example: C=CubeComponent; C.Data = rand(3,3,2); C.numImages
            
            Nobj = numel(Obj);
            N    = zeros(size(Obj));
            for Iobj=1:1:Nobj
                N(Iobj) = size(Obj(Iobj).Data, Obj(Iobj).Dim);
            end
        end
        
        function Result=dimXY(Obj)
            % Return the dimension indices of the images
            %   I.e., If image index Dim=3, return [1 2]
            % Input  : - A singlel element CubeComponent object.
            % Output : - The [X, Y] dimensions indices.
            %            I.e., the dims complementory to the Dim property.
            % Author : Eran Ofek (Jun 2023)
            % Example: C=CubeComponent; C.Data = rand(3,3,2); C.dimXY
            
            arguments
                Obj(1,1)
            end
            
            VecDim = [1 2 3];
            Result = VecDim(~ismember(VecDim, Obj.Dim));
            
        end
    
        function Obj=shiftDim(Obj, NewDim)
            % Shift the dimension of the image index in the cube.
            % Input  : - self.
            %          - The required new dimension of the image index in the cube.
            %            For example, if the new dimension is 1 and Dim=3,
            %            then will shift by -2.
            % Output : - The CubeComponent object with the shifted
            %            image index dimension.
            % Author : Eran Ofek (Jun 2023)
            % Example: C=CubeComponent; C.Data = rand(3,3,2);
            %          D = C.copy;
            %          C.shiftDim(1); C.shiftDim(3);
            %          all(C.Data==D.Data)
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                ShiftDim = NewDim - Obj(Iobj).Dim;
                if ShiftDim~=0
                    Obj(Iobj).Data = squeeze(shiftdim(Obj(Iobj).Data, ShiftDim));
                end
            end
        end
        
        
        
    end
    
    methods % create images
        function Result = cutouts2image(Obj, Image, Args)
            % Insert stamps in a cube into an image.
            %   The stamps will be inserted according to the positions in
            %   the XY property and using the imUtil.cut.cutouts2image
            %   function.
            % Input  : - self.
            %          - Image to which the cutouts will be inserted.
            %            This can be a matrix, AstroImage, or
            %            ImageComponent.
            %            If this is a two element vector, then this is
            %            assumed to be the [X,Y] size of a zeros matrix to
            %            generate.
            %          * ...,key,val,...
            %            'SubCubeX' - [start end] of stamps X coordinates to insert
            %                   to image. If empty, then will use [1 StampXsize].
            %                   Default is [].
            %            'SubCubeY' - Like 'SubCubeX', but for the Y coordinates.
            %                   Default is [].
            %            'DataProp' - Data property from which to get the
            %                   image, and in which to save the output.
            %                   Default is 'Image'.
            %            'OutType' - Output class:
            %                   'matrix' | 'ImageComponent' | 'AstroImage'
            %                   Default is 'AstroImage'.
            % Output : - The image with the inserted cutouts.
            % Author : Eran Ofek (Jan 2022)
            % Example: Im = AstroImage({rand(10,10), rand(10,10)});        
            %          C=CubeComponent.images2cube(Im);
            %          C.XY = [30 30; 60 70];
            %          Result = C.cutouts2image(zeros(100,100));
            %          Result = C.cutouts2image([100 100]);


            
            arguments
                Obj
                Image
                Args.SubCubeX = []; % [start, end]
                Args.SubCubeY = []; % [start, end]
                Args.DataProp = 'Image';
                Args.OutType  = 'AstroImage';  % 'matrix' | 'ImageComponent' | 'AstroImage'
            end
            
            if numel(Image)==2
                Image = zeros(Image(2), Image(1));
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                OrigDim = Obj(Iobj).Dim;
                Obj(Iobj).shiftDim(3);
               
                if isa(Image, 'AstroImage') || isa(Image, 'ImageComponent')
                    Iim = min(Iobj, numel(Image));
                    InputImage = Image(Iim).(Args.DataProp);
                elseif ismatrix(Image)
                    InputImage = Image;
                else
                    error('Unknown Image (second input) class');
                end
                    
                Output = imUtil.cut.cutouts2image(Obj(Iobj).Data, InputImage, Obj(Iobj).XY(:,1), Obj(Iobj).XY(:,2), 'SubCubeX',Args.SubCubeX,...
                                                                                                        'SubCubeY',Args.SubCubeY);
                Obj(Iobj).shiftDim(OrigDim);   
                
                switch lower(Args.OutType)
                    case 'matrix'
                        Result = Output;
                    case 'astroimage'
                        if Iobj==1
                            Result = AstroImage(size(Obj));
                        end
                        Result(Iobj).(Args.DataProp) = Output;
                    case 'imagecomponent'
                        if Iobj==1
                            Result = ImageComponent(size(Obj));
                        end
                        Result(Iobj).(Args.DataProp) = Output;
                    otherwise
                        error('Unknown OutType option');
                end
                        
            end
        end
        
    end
    
    methods % operators
        function Result=unaryFunImage(Obj, Fun, Args)
            % Apply a unary function on cube returning a vector with one element per image
            % Input  : - A single element CubeComponent object
            %          - A function handle for the function to execute on
            %            each image in the cube. The function on each image
            %            should return a scalar.
            %            This function must be of the form:
            %            Fun(Cube, ..., Dim,...), where Cube is the cube on
            %            which we operate and Dim is the dimensions on
            %            which to operate (e.g., for image index Dim=3,
            %            this is [1 2]).
            %          * ...,key,val,...
            %            'FunPreDimArgs' - A cell array of arguments to
            %                   pass to the function after the cube
            %                   argument, but before the Dim argument.
            %                   Default is {}.
            %                   For some functions like @std, @min, @max
            %                   this should be {[]}.
            %            'FunPostDimArgs' - A cell array of arguments to
            %                   pass to the function after the Dim
            %                   argument.
            %                   Default is {'omitnan'}.
            %            'Squeeze' - A logical indicating if to squeeze the
            %                   result.
            %                   Default is false.
            % Output : - A vector. Each element in the vector corresponds
            %            to the output of the function on one image layer
            %            in the cube.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im = AstroImage({ones(10,10), 2.*ones(10,10)}); 
            %          C=CubeComponent.images2cube(Im);             
            %          unaryFunImage(C, @sum)
            %          unaryFunImage(C, @sum, 'Squeeze',1)==[100;200]
            %          unaryFunImage(C, @std, 'FunPreDimArgs',{[]})
            
            arguments
                Obj(1,1)
                Fun function_handle   % e.g., @sum
                Args.FunPreDimArgs   = {};  % for some functions (e.g., @std this need to be {[]})
                Args.FunPostDimArgs  = {'omitnan'};
                Args.Squeeze logical = false;
            end
            
            %Nobj = numel(Obj);
            Iobj = 1;
            DimXY = Obj(Iobj).dimXY;
            
            FunArgs = [Args.FunPreDimArgs, DimXY, Args.FunPostDimArgs];
            Result  = Fun(Obj(Iobj).Data, FunArgs{:});
                        
            if Args.Squeeze
                Result = squeeze(Result);
            end
        end
        
        function Result=unaryFunCollapse(Obj, Fun, Args)
            % Apply a unary function on cube returning a matrix corresponding to the collapse alonng Dim
            % Input  : - A CubeComponent object
            %          - A function handle for the function to execute on
            %            each image in the cube. The function on the cube
            %            should return an image, by collapse operator along
            %            Dim (Dim property in the CubeComponent object).
            %            This function must be of the form:
            %            Fun(Cube, ..., Dim,...), where Cube is the cube on
            %          * ...,key,val,...
            %            'FunPreDimArgs' - A cell array of arguments to
            %                   pass to the function after the cube
            %                   argument, but before the Dim argument.
            %                   Default is {}.
            %                   For some functions like @std, @min, @max
            %                   this should be {[]}.
            %            'FunPostDimArgs' - A cell array of arguments to
            %                   pass to the function after the Dim
            %                   argument.
            %                   Default is {'omitnan'}.
            %            'Squeeze' - A logical indicating if to squeeze the
            %                   result.
            %                   Default is false.
            %            'OutType' - Output type:
            %                   'matrix' - A 2-D matrix containing the
            %                       collapsed image.
            %                       If CubeComponent has multi element then
            %                       this will contain only the last element
            %                       result.
            %                   'cube' - A cube of a 2-D matrices per
            %                           CubeComponent element.
            %                   'AstroImage' - An AstroImage object of the
            %                           collapsed images per CubeComponent element.
            %                   'ImageComponent' - An ImageComponent object of the
            %                           collapsed images per CubeComponent element.
            %                   'CubeComponent' - A (new) CubeComponent object
            %                           containing the collapse image.
            %                   Default is 'Cube'.
            %            'DataProp' - Data property in the 'AstroImage' or
            %                   'ImageComponent' in which to write the collpased
            %                   matrix. Default is 'Image'.
            % Output : - The collapsed result, of applying the unary
            %            function pixel-wise on the cube.
            %            The output is a 2D array.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im = AstroImage({ones(10,10), 2.*ones(10,10)}); 
            %          C=CubeComponent.images2cube(Im);             
            %          R=unaryFunCollapse(C, @sum)
            %          R=unaryFunCollapse(C, @sum, 'OutType','AstroImage')
            %          R=unaryFunCollapse(C, @sum, 'OutType','AstroImage', 'DataProp','Back')
            %          R=unaryFunCollapse(C, @sum, 'OutType','ImageComponent')
            %          R=unaryFunCollapse(C, @std, 'FunPreDimArgs',{[]})
            %          R=unaryFunCollapse(C, @sum, 'OutType','CubeComponent')
            
            arguments
                Obj
                Fun function_handle   % e.g., @sum
                Args.FunPreDimArgs   = {};  % for some functions (e.g., @std this need to be {[]})
                Args.FunPostDimArgs  = {'omitnan'};
                Args.Squeeze logical = false;
                Args.OutType         = 'cube';  % 'matrix'|'cube'
                Args.DataProp        = 'Image';
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
            
                FunArgs = [Args.FunPreDimArgs, Obj(Iobj).Dim, Args.FunPostDimArgs];
                Array  = Fun(Obj(Iobj).Data, FunArgs{:});

                if Args.Squeeze
                    Array = squeeze(Array);
                end
                
                switch lower(Args.OutType)
                    case 'matrix'
                        Result = Array;
                    case 'cube'
                        if Iobj==1
                            Result = zeros([size(Array), Nobj]);
                        end
                        Result(:,:,Iobj) = Array;
                    case 'astroimage'
                        if Iobj==1
                            Result = AstroImage([Nobj 1]);
                        end
                        Result(Iobj).(Args.DataProp) = Array;
                    case 'imagecomponent'
                        if Iobj==1
                            Result = ImageComponent([Nobj 1]);
                        end
                        Result(Iobj).(Args.DataProp) = Array;
                    case 'cubecomponent'
                        Result(Iobj) = CubeComponent;
                        Result(Iobj).Data = Array;
                    otherwise
                        error('Unknown OutType option');
                end
            end
        end
        
    end
    
    methods % overload operators
        function Result = sum(Obj, Args)
            % Calculate the collapsed (pixel-wise) sum of CubeComponent object
            %   Return a 2-D matrix
            % Input  : - A CubeComponent object
            %          * ...,key,val,...
            %            'OutType' - Output type:
            %                   'matrix' - A 2-D matrix containing the
            %                       collapsed image.
            %                       If CubeComponent has multi element then
            %                       this will contain only the last element
            %                       result.
            %                   'cube' - A cube of a 2-D matrices per
            %                           CubeComponent element.
            %                   'AstroImage' - An AstroImage object of the
            %                           collapsed images per CubeComponent element.
            %                   'ImageComponent' - An ImageComponent object of the
            %                           collapsed images per CubeComponent element.
            %                   'CubeComponent' - A (new) CubeComponent object
            %                           containing the collapse image.
            %                   Default is 'Cube'.
            % Output : - The collapsed result, of applying the unary
            %            function (@sum) pixel-wise on the cube.
            %            The output is a 2D array.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im = AstroImage({ones(100,100), 2.*ones(100,100)}); 
            %          C=CubeComponent.images2cube(Im);
            %          C.sum

            arguments
                Obj
                Args.OutType  = 'cube';
            end
            
            Result=unaryFunCollapse(Obj, @sum, 'FunPreDimArgs',{},...
                                               'FunPostDimArgs',{'omitnan'},...
                                               'OutType',Args.OutType);
                                           
        end
        
        function Result = mean(Obj, Args)
            % Calculate the collapsed (pixel-wise) mean of CubeComponent object
            %   Return a 2-D matrix
            % Input  : - A CubeComponent object
            %          * ...,key,val,...
            %            'OutType' - Output type:
            %                   'matrix' - A 2-D matrix containing the
            %                       collapsed image.
            %                       If CubeComponent has multi element then
            %                       this will contain only the last element
            %                       result.
            %                   'cube' - A cube of a 2-D matrices per
            %                           CubeComponent element.
            %                   'AstroImage' - An AstroImage object of the
            %                           collapsed images per CubeComponent element.
            %                   'ImageComponent' - An ImageComponent object of the
            %                           collapsed images per CubeComponent element.
            %                   'CubeComponent' - A (new) CubeComponent object
            %                           containing the collapse image.
            %                   Default is 'Cube'.
            % Output : - The collapsed result, of applying the unary
            %            function (@mean) pixel-wise on the cube.
            %            The output is a 2D array.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im = AstroImage({ones(100,100), 2.*ones(100,100)}); 
            %          C=CubeComponent.images2cube(Im);
            %          C.mean

            arguments
                Obj
                Args.OutType  = 'cube';
            end
            
            Result=unaryFunCollapse(Obj, @mean, 'FunPreDimArgs',{},...
                                               'FunPostDimArgs',{'omitnan'},...
                                               'OutType',Args.OutType);
                                           
        end
        
        function Result = median(Obj, Args)
            % Calculate the collapsed (pixel-wise) median of CubeComponent object
            %   Return a 2-D matrix
            % Input  : - A CubeComponent object
            %          * ...,key,val,...
            %            'OutType' - Output type:
            %                   'matrix' - A 2-D matrix containing the
            %                       collapsed image.
            %                       If CubeComponent has multi element then
            %                       this will contain only the last element
            %                       result.
            %                   'cube' - A cube of a 2-D matrices per
            %                           CubeComponent element.
            %                   'AstroImage' - An AstroImage object of the
            %                           collapsed images per CubeComponent element.
            %                   'ImageComponent' - An ImageComponent object of the
            %                           collapsed images per CubeComponent element.
            %                   'CubeComponent' - A (new) CubeComponent object
            %                           containing the collapse image.
            %                   Default is 'Cube'.
            % Output : - The collapsed result, of applying the unary
            %            function (@median) pixel-wise on the cube.
            %            The output is a 2D array.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im = AstroImage({ones(100,100), 2.*ones(100,100)}); 
            %          C=CubeComponent.images2cube(Im);
            %          C.median

            arguments
                Obj
                Args.OutType  = 'cube';
            end
            
            Result=unaryFunCollapse(Obj, @median, 'FunPreDimArgs',{},...
                                               'FunPostDimArgs',{'omitnan'},...
                                               'OutType',Args.OutType);
                                           
        end
        
        function Result = std(Obj, Args)
            % Calculate the collapsed (pixel-wise) std of CubeComponent object
            %   Return a 2-D matrix
            % Input  : - A CubeComponent object
            %          * ...,key,val,...
            %            'OutType' - Output type:
            %                   'matrix' - A 2-D matrix containing the
            %                       collapsed image.
            %                       If CubeComponent has multi element then
            %                       this will contain only the last element
            %                       result.
            %                   'cube' - A cube of a 2-D matrices per
            %                           CubeComponent element.
            %                   'AstroImage' - An AstroImage object of the
            %                           collapsed images per CubeComponent element.
            %                   'ImageComponent' - An ImageComponent object of the
            %                           collapsed images per CubeComponent element.
            %                   'CubeComponent' - A (new) CubeComponent object
            %                           containing the collapse image.
            %                   Default is 'Cube'.
            % Output : - The collapsed result, of applying the unary
            %            function (@std) pixel-wise on the cube.
            %            The output is a 2D array.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im = AstroImage({ones(100,100), 2.*ones(100,100)}); 
            %          C=CubeComponent.images2cube(Im);
            %          C.std

            arguments
                Obj
                Args.OutType  = 'cube';
            end
            
            Result=unaryFunCollapse(Obj, @std, 'FunPreDimArgs',{[]},...
                                               'FunPostDimArgs',{'omitnan'},...
                                               'OutType',Args.OutType);
                                           
        end
        
        function Result = rstd(Obj, Args)
            % Calculate the collapsed (pixel-wise) rstd of CubeComponent object
            %   Return a 2-D matrix
            % Input  : - A CubeComponent object
            %          * ...,key,val,...
            %            'OutType' - Output type:
            %                   'matrix' - A 2-D matrix containing the
            %                       collapsed image.
            %                       If CubeComponent has multi element then
            %                       this will contain only the last element
            %                       result.
            %                   'cube' - A cube of a 2-D matrices per
            %                           CubeComponent element.
            %                   'AstroImage' - An AstroImage object of the
            %                           collapsed images per CubeComponent element.
            %                   'ImageComponent' - An ImageComponent object of the
            %                           collapsed images per CubeComponent element.
            %                   'CubeComponent' - A (new) CubeComponent object
            %                           containing the collapse image.
            %                   Default is 'Cube'.
            % Output : - The collapsed result, of applying the unary
            %            function (@tools.math.stat.rstd) pixel-wise on the cube.
            %            The output is a 2D array.
            % Author : Eran Ofek (Jun 2023)
            % Example: Im = AstroImage({ones(100,100), 2.*ones(100,100)}); 
            %          C=CubeComponent.images2cube(Im);
            %          C.rstd

            arguments
                Obj
                Args.OutType  = 'cube';
            end
            
            Result=unaryFunCollapse(Obj, @tools.math.stat.rstd, 'FunPreDimArgs',{},...
                                               'FunPostDimArgs',{},...
                                               'OutType',Args.OutType);
                                           
        end
                    
    end
    
   
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
