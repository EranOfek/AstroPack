
classdef Coadd < Component
    properties
        ImObj AstroImage
        
    end
    
    methods % constructor
        function CObj = Coadd(Args)
            % Constructor for a Match object
            % Input  : * ...,key,val,...
            %            Can be any Match object property name followed by
            %            its new value.
            % Output : - A Match object
            % Author : Eran Ofek (Apr 2021)
            % Example: M = imProc.cat.Match('Radius',5,'RadiusUnits','arcsec');
           
            arguments
                Args.Cat              % catalaog
                Args.Ref              % ref catalog
                Args.Coo              % Search coordinates - used by coneSearch, match_catsHTM
                Args.CatName          % catsHTM name
                
                Args.CooUnits
                Args.Radius
                Args.RadiusUnits
                Args.Shape
                Args.AddDistCol
                Args.DistUnits
                Args.DistColName
                Args.DistColPos
                
                Args.OutIsSelfClass
                
                Args.CreateNewObj
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
    
    methods % basic normalization function
        
        function Result = applayUnaryFun(Obj, ImObj, Offset, Operator, Args)
            % Applay scalar an unary function (e.g., constant) on AstroImage
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          - An AstroImage object, or a cell array of matrices
            %            (images) or scalars, or a vector of scalars, or a
            %            function_handle.
            %            If an array, then will be converted to a cell of
            %            scalars using num2cell. Each cell element will be
            %            applied (using the operator) on the AstroImage. If one cell
            %            element then will be applied on all images.
            %            If function_handle (unary function) then it will
            %            be used to calculate the offset using this
            %            function.
            %          - Operator to use - e.g., subtracting the offset or
            %            dividing the factor. Default is @minus).
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of arguments to pass to
            %                   the Offset operator (if operator is provided).
            %            'PreDivide' - A logical indicating if to take
            %                   the inverse of the Offset prior to applying
            %                   the operator. This can be used to speed up
            %                   performences (in division). Default is false.
            %            'DataProp' - The data property in the AstroImage
            %                   (both input and offset) that contains the data.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - The data property in the
            %                   ImageComponent that contains the image
            %                   data. Default is 'Data'.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new object. If empty, then will
            %                   check the number of output argumnets. If
            %                   zero (e.g., C.subtractOffset(...)) then
            %                   will update the input image object. Else,
            %                   will create a new object.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            %          C  = imProc.image.Coadd;
            %          R  = C.applyUnaryFun(AI,1);
            %          R  = C.applayUnaryFun(AI,[1 2]);
            %          R  = C.applayUnaryFun(AI,{1 2}); % the same
            %          R  = C.applayUnaryFun(AI,AI); 
            %          R  = C.applayUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
            %          R  = C.applayUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});
            
            
            arguments
                Obj(1,1) 
                ImObj                         = [];
                Offset                        = [];   % AstroImage, cell of matrices, or array
                Operator function_handle      = @minus;
                Args.OpArgs cell              = {};
                Args.PreDivide(1,1) logical   = false;
                Args.DataProp char            = 'ImageData';
                Args.DataPropIn char          = 'Data';
                Args.CreateNewObj             = [];
            end
            
            if isempty(ImObj)
                if isempty(Obj.ImObj)
                    error('ImObj must be provided either as an argument or via the Coadd class properties');
                else
                    ImObj = Obj.ImObj;
                end
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
                    
            if isa(Offset,'function_handle')
                % Offset is an operator
                Offset = funUnaryScalar(ImObj, Offset, 'OpArgs',Args.OpArgs, 'DataProp',{Args.DataProp}, 'DataPropIn',{Args.DataPropIn});
            end
            if isnumeric(Offset)
                Offset = num2cell(Offset);
            end
            
            if Args.CreateNewObj
                Result = ImObj.copyObject;
            else
                Result = ImObj;
            end
            Nobj = numel(ImObj);
            Noff = numel(Offset);
            for Iobj=1:1:Nobj
                Ioff = min(Iobj, Noff);
                if iscell(Offset)
                    Tmp = Offset{Ioff};
                else
                    Tmp = Offset(Ioff).(Args.DataProp).(Args.DataPropIn);
                end
                if ~isempty(Args.PreDivide)
                    Tmp = 1./Tmp;
                end
                
                Result(Iobj).(Args.DataProp).(Args.DataPropIn) = Operator(ImObj(Iobj).(Args.DataProp).(Args.DataPropIn), Tmp);
            end
        end
        
        function Result = subtractOffset(Obj, ImObj, Offset, Args)
            % Remove offset (constant) from AstroImage
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          - An AstroImage object, or a cell array of matrices
            %            (images) or scalars, or a vector of scalars, or a
            %            function_handle.
            %            If an array, then will be converted to a cell of
            %            scalars using num2cell. Each cell element will be
            %            subtracted from the AstroImage. Of one cell
            %            element then will be subtracted from all images.
            %            If function_handle (unary function) then it will
            %            be used to calculate the offset using this
            %            function.
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of arguments to pass to
            %                   the Offset operator (if operator is provided).
            %            'DataProp' - The data property in the AstroImage
            %                   (both input and offset) that contains the data.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - The data property in the
            %                   ImageComponent that contains the image
            %                   data. Default is 'Data'.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new object. If empty, then will
            %                   check the number of output argumnets. If
            %                   zero (e.g., C.subtractOffset(...)) then
            %                   will update the input image object. Else,
            %                   will create a new object.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            %          C  = imProc.image.Coadd;
            %          R  = C.subtractOffset(AI,1);
            %          R  = C.subtractOffset(AI,[1 2]);
            %          R  = C.subtractOffset(AI,{1 2}); % the same
            %          R  = C.subtractOffset(AI,AI); 
            %          R  = C.subtractOffset(AI,@mean,'OpArgs',{'all'});
            
            arguments
                Obj(1,1) 
                ImObj                     = [];
                Offset                    = [];   % AstroImage, cell of matrices, or array
                Args.OpArgs cell          = {};
                Args.DataProp char        = 'ImageData';
                Args.DataPropIn char      = 'Data';
                Args.CreateNewObj         = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            
            Result  = Obj.applayUnaryFun(ImObj, Offset, @minus, 'OpArgs',{'all'}, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn, 'CreateNewObj',Args.CreateNewObj, 'PreDivide',false);
            
        end
        
        function Result = divideFactor(Obj, ImObj, Factor, Args)
            % Divide factor (constant) from AstroImage
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          - An AstroImage object, or a cell array of matrices
            %            (images) or scalars, or a vector of scalars, or a
            %            function_handle.
            %            If an array, then will be converted to a cell of
            %            scalars using num2cell. Each cell element will be
            %            divided from the AstroImage. Of one cell
            %            element then will be subtracted from all images.
            %            If function_handle (unary function) then it will
            %            be used to calculate the factor using this
            %            function.
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of arguments to pass to
            %                   the Offset operator (if operator is provided).
            %            'DataProp' - The data property in the AstroImage
            %                   (both input and offset) that contains the data.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - The data property in the
            %                   ImageComponent that contains the image
            %                   data. Default is 'Data'.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new object. If empty, then will
            %                   check the number of output argumnets. If
            %                   zero (e.g., C.subtractOffset(...)) then
            %                   will update the input image object. Else,
            %                   will create a new object.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            %          C  = imProc.image.Coadd;
            %          R  = C.divideFactor(AI,1);
            %          R  = C.divideFactor(AI,[1 2]);
            %          R  = C.divideFactor(AI,{1 2}); % the same
            %          R  = C.divideFactor(AI,AI); 
            %          R  = C.divideFactor(AI,@mean,'OpArgs',{'all'});
            
            arguments
                Obj(1,1) 
                ImObj                     = [];
                Factor                    = [];   % AstroImage, cell of matrices, or array
                Args.OpArgs cell          = {};
                Args.DataProp char        = 'ImageData';
                Args.DataPropIn char      = 'Data';
                Args.CreateNewObj         = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            
            Result  = Obj.applayUnaryFun(ImObj, Factor, @times, 'OpArgs',{'all'}, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn, 'CreateNewObj',Args.CreateNewObj, 'PreDivide',true);
            
        end
        
    end
    
    methods % coaddition functions
        function varargout = funCube(Obj, ImObj, Args)
            % Applay function/s on a single cube
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'CCDSEC' - [Xmin Xmax Ymin Ymax] to stack.
            %                       If empty, use entire image. Default is
            %                       [].
            %            'FunCube' - A function handle, or a cell array of
            %                   function handles to applay on cube.
            %                   Default is {@mean, @var}.
            %                   All the functions are applayed on the same
            %                   cube. Note that the number of functions
            %                   actually applied is min(nargout,numel(FunCube)).
            %                   Default is {@mean, @var}.
            %            'FunArgs' - If FunCube is a function handle then
            %                   this is a cell array of additional
            %                   arguments to pass to the FunCube function.
            %                   If FunCube is a cell array of function
            %                   handles, then this is a cell array of cell
            %                   arrays of additional arguments for each
            %                   function.
            %                   Default is {{3,'omitnan'}, {[],3,'omitnan'}}.
            %            'DataProp' - Data property in the AStroImage from
            %                   which to extract a cube.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - Data property in the ImageComponent
            %                   from which to extract the image data.
            %                   Default is 'Data'.
            %            'SaveInProp' - A cell array of AstroImage
            %                   properties (one to one correspondence to
            %                   FunCube) in which to save the output
            %                   results.
            %                   If empty, then the output is matrices.
            %                   If provided, then the first output argument
            %                   is an AstroImage with the specific field
            %                   populated with the results of the
            %                   corresponding FunCube results.
            %                   Default is {'ImageData','VarData'}.
            %            'DimIndex' - Dimension along which the cube will
            %                   be constructed. Default is 3.
            % Output : * Arbitrary number of arguments.
            %            Each output contains a matrix of a coadd image that
            %            corresponds to one 'FunCube' function.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});
            %          C = imProc.image.Coadd;
            %          [Cube1, Cube2] = funCube(AI);
            %          [CAI] = C.funCube(AI,'SaveInProp',{'ImageData','VarData'});
            
            arguments
                Obj(1,1)
                ImObj                              = [];
                Args.CCDSEC                        = [];
                Args.FunCube                       = {@mean, @var};
                Args.FunArgs cell                  = {{3,'omitnan'}, {[],3,'omitnan'}};
                Args.DataProp char                 = 'ImageData'; %,'BackData', 'VarData', 'MaskData'};
                Args.DataPropIn char               = 'Data';
                Args.SaveInProp                    = {'ImageData','VarData'};
                Args.DimIndex                      = 3;
            end
           
            if isempty(ImObj)
                if isempty(Obj.ImObj)
                    error('ImObj must be provided either as an argument or via the Coadd class properties');
                else
                    ImObj = Obj.ImObj;
                end
            end
            
            % convert AstroImage to cubes
            [Cube] = images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DataProp',{Args.DataProp}, 'DimIndex',Args.DimIndex);
            
            if ~iscell(Args.FunCube)
                Args.FunCube = {Args.FunCube};
                Args.FunArgs = {Args.FunArgs};
            end
            Nfun = numel(Args.FunCube);
            if isempty(Args.SaveInProp)
                Nfun = min(Nfun, nargout);
            else
                Nfun = min(Nfun, numel(Args.SaveInProp));
            end
            varargout = cell(1,Nfun);
            for Ifun=1:1:Nfun
                varargout{Ifun} = Args.FunCube{Ifun}(Cube, Args.FunArgs{Ifun}{:});
            end
            
            if ~isempty(Args.SaveInProp)
                Result = AstroImage;
                Nprop = numel(Args.SaveInProp);
                for Iprop=1:1:Nprop
                    Result.(Args.SaveInProp{Iprop}).(Args.DataPropIn) = varargout{Iprop};
                end
                varargout{1} = Result;
            end
        end
        
        function [Result, CoaddN] = coadd(Obj, ImObj, Args)
            % Coadd images in AstroImage object including pre/post normalization
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'DataPropIn' - The data property that contains the
            %                   the data in the ImageComponent.
            %                   Default is 'Data'.
            %            'Offset' - Either a function handle, a vector, or
            %                   empty. If function handle, then will applay
            %                   it to the ImageData to calculate an offset
            %                   per image. This offset will be subtracted
            %                   from each image. If vector, then this is a
            %                   offset value per image. If empty, do not
            %                   apply offset. Default is [].
            %            'OffsetArgs' - A cell array of additional
            %                   arguments to pass to the offset function.
            %                   Default is {}.
            %            'PreNorm' - Like offset, but for the
            %                   pre-normalization for the images. The
            %                   pre-normalization is done after the offset.
            %                   Default is [].
            %            'PreNormArgs' - A cell array of additional
            %                   arguments to pass to the pre-normalization function.
            %                   Default is {}.
            %            'UseWeights' - A logical indicating if to applay
            %                   weights. Default is true.
            %            'Weights' - A vector of variances (one per image).
            %                   If empty, then will attempt to use the
            %                   VarImage.Image in the AstroImage.
            %                   Default is [].
            %            'StackMethod' - - Stacking method. Options are:
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
            %              'MaskStackMethod' - Like 'StackMethod', but for the
            %                   coaddition of the Mask. Default is 'bitor'.
            %              'MaskStackArgs' - A cell array of arguments to pass to the
            %                   mask method function. Default is {}.
            %              'CombineBack' - A logical indicating if to
            %                   combine the background image (using the
            %                   StackMethod). Default is true.
            %              'CombineMask' - A logical indicating if to
            %                   combine the mask image. Default is true.
            %              '
            %              'EmpiricalVarFun' - Default is @var.
            %              'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.
            %              'MedianVarCorrForEmpirical' - A logical indicating if to
            %                   correct the variance calculation by the ratio between
            %                   the variance of the median and variance of the mean.
            %                   Default is false.
            %              'DivideEmpiricalByN' - A logical indicating if to divide
            %                   CoaddVarEmpirical by N. Default is false.
            %              'PostNorm' - Like offset, but for the
            %                   post-normalization for the images (a scalar). The
            %                   post-normalization is done after the stacking.
            %                   Default is [].
            %              'PostNormArgs' - A cell array of additional
            %                   arguments to pass to the post-normalization function.
            %                   Default is {}.
            % Output : - An AstroImage with the coadded image, includinf
            %            the coadded background and mask. The VarData is always
            %            including the empirical variance.
            %          - A matrix in which each pixel give the number of
            %            images on which the coaddition was based.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
            %          C = imProc.image.Coadd;
            %          [Result, CoaddN] = C.coadd(AI);
           
            arguments
                Obj(1,1)
                ImObj                                       = [];
                
                Args.CCDSEC                                 = [];
                Args.DataPropIn char                        = 'Data';
                
                Args.Offset                                 = [];  % function_handle, vector, or []
                Args.OffsetArgs cell                        = {};
                
                Args.PreNorm                                = [];
                Args.PreNormArgs cell                       = {};
                
                Args.UseWeights(1,1) logical                = true;
                Args.Weights                                = [];  % if empty use inverse variance
                
                Args.StackMethod                            = 'mean';
                Args.StackArgs cell                         = {};
                
                Args.MaskStackMethod                        = 'bitor';
                Args.MaskStackArgs cell                     = {};
                
                Args.CombineBack(1,1) logical               = true;
                Args.CombineMask(1,1) logical               = true;
                
                Args.EmpiricalVarFun function_handle        = @var;
                Args.EmpiricalVarFunArgs                    = {[],3,'omitnan'};
                Args.MedianVarCorrForEmpirical(1,1) logical = false;
                Args.DivideEmpiricalByN(1,1) logical        = false;
                
                Args.PostNorm                               = [];   % function_handle or []
                Args.PostNormArgs cell                      = {};
                
            end
            DataProp                      = {'ImageData','BackData', 'VarData', 'MaskData'};
            DimIndex                      = 3;
            
            if isempty(ImObj)
                if isempty(Obj.ImObj)
                    error('ImObj must be provided either as an argument or via the Coadd class properties');
                else
                    ImObj = Obj.ImObj;
                end
            end
            
            % allocate output
            Result = AstroImage;
            
            Nim = numel(ImObj);
            
            
            % create a cube for each dataset
            [ImageCube, BackCube, VarCube, MaskCube] = images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex, 'DataProp',DataProp, 'DataPropIn',Args.DataPropIn);
            
            % subtract offset (only from image)
            if ~isempty(Args.Offset)
                if isa(Args.Offset,'function_handle')
                    Args.Offset = Args.Offset(ImageCube, Args.OffsetArgs{:});
                end
                Noff = numel(Args.Offset);
                if Noff~=1 && Noff~=Nim
                    error('Number of offsets mnust be 1 or equal to number of images');
                end
                ImageCube = ImageCube - reshape(Args.Offset,[1 1 Noff]);
            end
            
            % pre normalization (only from image and variance)
            if ~isempty(Args.PreNorm)
                if isa(Args.PreNorm,'function_handle')
                    Args.PreNorm = Args.PreNorm(ImageCube, Args.PreNormArgs{:});
               end
                Nnorm = numel(Args.PreNorm);
                if Nnorm~=1 && Nnorm~=Nim
                    error('Number of pre normalizations mnust be 1 or equal to number of images');
                end
                PreNorm = reshape(1./Args.PreNorm,[1 1 Nnorm]);
                ImageCube = ImageCube .* PreNorm;
                VarCube   = VarCube   .* PreNorm.^2;
            end
            
            % stack the images
            if Args.UseWeights
                %
                if isempty(Args.Weights)
                    % use inverse variance as weights
                    Args.Weights = VarCube;
                else
                    Nw = nuem(Args.Weights);
                    Args.Weights = reshape(Args.Weights, [1 1 Nw]);
                end
            else
                Args.Weights = [];
            end
            [Coadd, CoaddVarEmpirical, ~, CoaddN] = imUtil.image.stackCube(ImageCube, 'StackMethod',Args.StackMethod,...
                                                                                     'StackArgs',Args.StackArgs,...
                                                                                     'EmpiricalVarFun',Args.EmpiricalVarFun,...
                                                                                     'EmpiricalVarFunArgs',Args.EmpiricalVarFunArgs,...
                                                                                     'VarCube',Args.Weights,...
                                                                                     'MedianVarCorrForEmpirical',Args.MedianVarCorrForEmpirical,...
                                                                                     'DivideEmpiricalByN',Args.DivideEmpiricalByN,...
                                                                                     'DivideVarByN',false,...
                                                                                     'CalcCoaddVarEmpirical',true,...
                                                                                     'CalcCoaddVar',false,...
                                                                                     'CalcCoaddN',true);
                
                
            
            if Args.CombineBack && ~isempty(BackCube)
                [BackCoadd] = imUtil.image.stackCube(BackCube, 'StackMethod',Args.StackMethod,...
                                                                                     'StackArgs',Args.StackArgs,...
                                                                                     'VarCube',[],...
                                                                                     'CalcCoaddVarEmpirical',false,...
                                                                                     'CalcCoaddVar',false,...
                                                                                     'CalcCoaddN',false);
                 Result.BackData.(Args.DataPropIn)  = BackCoadd;                                                                
            end
            if Args.CombineMask && ~isempty(MaskCube)
                [MaskCoadd] = imUtil.image.stackCube(MaskCube, 'StackMethod',Args.MaskStackMethod,...
                                                                                     'StackArgs',Args.MaskStackArgs,...
                                                                                     'VarCube',[],...
                                                                                     'CalcCoaddVarEmpirical',false,...
                                                                                     'CalcCoaddVar',false,...
                                                                                     'CalcCoaddN',false);
                Result.MaskData.(Args.DataPropIn)  = MaskCoadd;                                                                
            end
                                                                                 
            % post normalization (image and variance)
            if ~isempty(Args.PostNorm)
                if isa(Args.PostNorm,'function_handle')
                    Args.PostNorm = Args.PostNorm(Coadd, Args.PostNormArgs{:});
               end
                Nnorm = numel(Args.PostNorm);
                if Nnorm~=1
                    error('Number of post normalizations mnust be 1');
                end
                PostNorm          = 1./Args.PostNorm;
                Coadd             = Coadd             .* PostNorm;
                CoaddVarEmpirical = CoaddVarEmpirical .* PostNorm.^2;
            end
            
            % store in AstroImage object
            Result.ImageData.(Args.DataPropIn) = Coadd;
            Result.VarData.(Args.DataPropIn)   = CoaddVarEmpirical;
            
        end
        
        
    end
    
end