% obsolete:

% imProc.image.Stack class
%   This class provides functionality for stack/coadd images.
% Functionality:
%   applyUnaryFun  - Apply scalar-unary function (e.g., function that returns a scalar) on AstroImage
%   subtractOffset - Remove offset (constant) from AstroImage
%   divideFactor   - Divide factor (constant) from AstroImage
%   funCube        - Apply function/s on a single cube
%   coadd          - Coadd images in AstroImage object including pre/post normalization
%   functionalResponse - Fit the pixel response to light as a function of intensity in a cube of images
%


classdef Stack < Component
    properties
        ImObj AstroImage
        Cube                                           % will be used only if SaveCube=true
        SaveCube(1,1) logical              = false;   
    end
    
    methods % constructor
        function CObj = Stack(Args)
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
        
        
        function Result = applyUnaryFun(Obj, ImObj, Offset, Operator, Args)
            % Applay scalar-unary function (e.g., function that returns a scalar) on AstroImage
            % Input  : - A Stack object.
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
            %          C  = imProc.image.Stack;
            %          R  = C.applyUnaryFun(AI,1);
            %          R  = C.applyUnaryFun(AI,[1 2]);
            %          R  = C.applyUnaryFun(AI,{1 2}); % the same
            %          R  = C.applyUnaryFun(AI,AI); 
            %          R  = C.applyUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
            %          R  = C.applyUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});
            
            
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
                    error('ImObj must be provided either as an argument or via the Stack class properties');
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
                Result = ImObj.copy();
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
            % Input  : - A Stack object.
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
            %          C  = imProc.image.Stack;
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
            
            Result  = Obj.applyUnaryFun(ImObj, Offset, @minus, 'OpArgs',{'all'}, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn, 'CreateNewObj',Args.CreateNewObj, 'PreDivide',false);
            
        end
        
        function Result = divideFactor(Obj, ImObj, Factor, Args)
            % Divide factor (constant) from AstroImage
            % Input  : - A Stack object.
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
            %          C  = imProc.image.Stack;
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
            
            Result  = Obj.applyUnaryFun(ImObj, Factor, @times, 'OpArgs',{'all'}, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn, 'CreateNewObj',Args.CreateNewObj, 'PreDivide',true);
            
        end
        
    end
    
    methods % coaddition functions
        function varargout = funCube(Obj, ImObj, Args)
            % Apply function/s on a single cube
            % Input  : - A Stack object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'CCDSEC' - [Xmin Xmax Ymin Ymax] to stack.
            %                       If empty, use entire image. Default is
            %                       [].
            %            'FunCube' - A function handle, or a cell array of
            %                   function handles to apply on cube.
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
            %          C = imProc.image.Stack;
            %          [Cube1, Cube2] = C.funCube(AI);
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
                    error('ImObj must be provided either as an argument or via the Stack class properties');
                else
                    ImObj = Obj.ImObj;
                end
            end
            
            % convert AstroImage to cubes
            if Obj.SaveCube && ~isempty(Obj.Cube)
                % Cube exist - use it
                Cube = Obj.Cube;
            else
                % Cube doesn't exist
                [Cube] = images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DataProp',{Args.DataProp}, 'DimIndex',Args.DimIndex);
                if Obj.SaveCube
                    Obj.Cube = Cube;
                end
            end
                
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
        
        function [Result, CoaddN, ImageCube] = coadd(Obj, ImObj, Args)
            % Coadd images in AstroImage object including pre/post normalization
            % Input  : - A Stack object.
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
            %                   empty. If function handle, then will apply
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
            %            'UseWeights' - A logical indicating if to apply
            %                   weights. Default is true.
            %            'Weights' - A vector of variances (one per image).
            %                   If empty, then will attempt to use the
            %                   VarImage image in the AstroImage.
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
            %                   'bitor' - bit-wise or operation. Return only Stack.
            %                   'bitand' - bit-wise and operation. Return only Stack.
            %                   'bitnot' - bit-wise not operation. Return only Stack.
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
            %              'HeaderCopy1' - A logical indicating if to copy
            %                   the header from the 1st coadd image.
            %                   Default is true.
            %              'NewHeader' - An header to add to the coadd
            %                   image header. This can be a 3 column cell
            %                   array, an AstroHeader or AstroImage. If
            %                   empty do nothing. Default is [].
            %              'UpdateTimes' - A logical indicatin if to add
            %                   keywords regarding the number of coadded
            %                   images and update the EXPTIME and MIDJD.
            %                   Default is true.
            %              'SumExpTime' - A logical indicating if to sum
            %                   the EXPTIME in the new header, or to use
            %                   the mean (false). Default is true.
            % Output : - An AstroImage with the coadded image, includinf
            %            the coadded background and mask. The VarData is always
            %            including the empirical variance.
            %          - A matrix in which each pixel give the number of
            %            images on which the coaddition was based.
            %          - The cube of images
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
            %          C = imProc.image.Stack;
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
                
                Args.HeaderCopy1(1,1) logical               = true;
                Args.NewHeader                              = [];
                Args.UpdateTimes(1,1) logical               = true;
                Args.SumExpTime(1,1) logical                = true;
                
            end
            DataProp                      = {'ImageData','BackData', 'VarData', 'MaskData'};
            DimIndex                      = 3;
            
            if isempty(ImObj)
                if isempty(Obj.ImObj)
                    error('ImObj must be provided either as an argument or via the Stack class properties');
                else
                    ImObj = Obj.ImObj;
                end
            end
            
            % allocate output
            Result = AstroImage;
            
            Nim = numel(ImObj);
            
            % create a cube for each dataset
            if Obj.SaveCube && ~isempty(Obj.Cube)
                % Cube exist - use it
                ImageCube = Obj.Cube;
            else
                % Cube doesn't exist
                [ImageCube, BackCube, VarCube, MaskCube] = images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex, 'DataProp',DataProp, 'DataPropIn',Args.DataPropIn);
                if Obj.SaveCube
                    Obj.Cube = ImageCube;
                end
            end
            
            
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
            
            % FFU: update header
            if Args.HeaderCopy1
                % copy image header from first image
                Result.HeaderData.Data = ImObj(1).HeaderData.Data;
            end
            if ~isempty(Args.NewHeader)
                if isa(Args.NewHeader,'AstroHeader')
                    Result.HeaderData = Args.NewHeader;
                elseif iscell(Args.NewHeader)
                    Result.HeaderData.Data = Args.NewHeader;
                elseif isa(Args.NewHeader,'AstroImage')
                    Result.HeaderData = Args.NewHeader.HeaderData;
                else
                    error('Unknown NewHeader option');
                end
            end
            if Args.UpdateTimes
                % update ExpTime, and MIDJD + add info re coaddition
                VecExpTime = funHeader(ImObj, @getVal,'EXPTIME');
                MidJD      = funHeader(ImObj, @julday);
                InfoCell = {'NCOADD',Nim,'Number of coadded images';...
                            'COADDOP',Args.StackMethod,'Coaddition method';...
                            'AVNCOADD',mean(CoaddN,'all'),'Mean number of coadded images per pixel';...
                            'MINCOADD',min(CoaddN,[],'all'),'Minimum number of coadded images per pixel';...
                            'MINJD',min(MidJD),'MIDJD of first coadded observation';...
                            'MAXJD',max(MidJD),'MIDJD of last coadded observation'};
                Result.HeaderData = insertKey(Result.HeaderData, InfoCell, 'end');
                
                if Args.SumExpTime
                    Result.HeaderData = replaceVal(Result.HeaderData, 'EXPTIME', {sum(VecExpTime)});
                else
                    Result.HeaderData = replaceVal(Result.HeaderData, 'EXPTIME', {mean(VecExpTime)});
                end
                Result.HeaderData = replaceVal(Result.HeaderData, 'MIDJD', {median(MidJD)});
                
            end
            
        end
        
        function Result = functionalResponse(Obj, ImObj, Args)
            % Fit the pixel response to light as a function of intensity in a cube of images
            % Description: Given an AstroImage of flat field or dark images which have different mean
            %              intensity, fit some linear models to each model intensity
            %              vs. a user specified expected intensity or the mean value of
            %              the image.
            %              This function can be used to fit the flat/dark image in each
            %              pixel, to fit non-linaeity and to look for pixels with
            %              anomalous response.
            %              The function by default fit a null hypothesis model of the
            %              form intensity=alpha*MeanIntensity (where alpha is a fitted
            %              free parameter).
            %              In addition the function fit additional user specified
            %              models (e.g., offset + linear term, or quadratic model).
            %              The Delta Chi^2 between these models and the null hypothesis
            %              is calculated.
            % Input  : - A Coadd object.
            %          - An AStroImage object.
            %          * Pairs of ...,key,val,... arguments. Options are:
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'DataPropIn' - The data property that contains the
            %                   the data in the ImageComponent.
            %                   Default is 'Data'.
            %            'DataProp' - Data propery to fit. Default is
            %                   'ImageData'.
            %            'Gain' - Gain. The cube will be multiplied by these factor.
            %                   This is needed beacuse the function assume the images
            %                   noise is Poisson. Default is 1.
            %            'ReadNoise' - Readnoise in electrons, used for the \chi^2
            %                   calculation. Default is 5.
            %            'MeanFun' - If 'Intensity' is not porovided, this is a function
            %                   handle that will operate on each image in the cube in
            %                   order to calculate its mean value.
            %                   Default is @nanmedian.
            %            'MeanFunPar' - A cella array of additional parameters to pass
            %                   to the 'MeanFun' function handle.
            %            'Intensity' - A vector if intensities for each image in the
            %                   cube. This can be the mean intensity of each image
            %                   (after gain correction), or exposure time (if flat is
            %                   based on constant illumination surface).
            %                   If empty, then will estimate the intensity using the
            %                   'MeanFun' option.
            %                   Default is empty.
            %            'Model' - A cell array of additional models to fit.
            %                   Each element in the cell array is a string that
            %                   corresponds to one of the following models:
            %                   'c+x' : - bias + alpha*Intensity model
            %                   'c+x+X^2' - bias + alpha*I + beta.*I.^2 model
            %                   'x+x^2' - alpha*I + beta.*I.^2 model
            %                   Default is {'c+x','c+x+x^2','x+x^2'}.
            % Output : - A structure of the fit results. The following fields are
            %            available:
            %            .H0 (data for the null hypothesis fit.
            %               A structure with the following fields:
            %               .Model - Model name - i.e., 'x'
            %               .Par.Par - A matrix of the fitted parameters (response
            %                       image  - i.e., flat field image).
            %               .Chi2 - A matrix of \chi^2 per pixel.
            %               .Npar - The number of free parameters.
            %               .Ndof - The number of degrees of freedom (Nobs-Npar).
            %               .ProbChi2 - 1 - The cumulative probability of the chi2,dof.
            %            .H1 (data for the alternative hypothsis fits).
            %               This is a structure array with element per alternative
            %               model:
            %               .Model - Model name - e.g., 'c+x'
            %               .Par(i).Par - A matrix of the fitted parameters (response
            %                       image  - i.e., flat field image).
            %                       where i is the free parameter index.
            %                       For example, in the 'c+x' model i=1 is for the bias
            %                       level and i=2 is for the slope (response).
            %               .Chi2 - A matrix of \chi^2 per pixel.
            %               .Npar - The number of free parameters.
            %               .Ndof - The number of degrees of freedom (Nobs-Npar).
            %               .ProbDeltaChi2 - 1 - The cumulative probability of the
            %                       Delta \chi^2 between H1 and H0 where Npar-1 is the
            %                       number of degrees of freedom.
            %                       small or 0 where the model is prefered over H0.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 2.*ones(3,3), 10.*ones(3,3), 11.*ones(3,3), 13.*ones(3,3)});
            %          C  = imProc.image.Stack;
            %          Result = C.functionalResponse(AI);
            %          Result = C.functionalResponse(AI, 'Intensity',[1 2 10 11 13])
            
            
            arguments
                Obj(1,1)
                ImObj                              = [];
                Args.CCDSEC                        = [];
                Args.DataProp char                 = 'ImageData';
                Args.DataPropIn char               = 'Data';
                Args.Gain                          = 1;   % if char array then this is a header keyword name
                Args.ReadNoise                     = 5;   % if char array then this is a header keyword name
                Args.MeanFun function_handle       = @nanmedian
                Args.MeanFunPar cell               = {[1 2]};
                Args.Intensity                     = [];  % if char array then this is a header keyword name (e.g., 'EXPTIME')
                Args.Model cell                    = {'c+x','c+x+x^2','x+x^2'};
            end
            DimIndex = 3;
            
            if isempty(ImObj)
                if isempty(Obj.ImObj)
                    error('ImObj must be provided either as an argument or via the Stack class properties');
                else
                    ImObj = Obj.ImObj;
                end
            end
            
            % allocate output
            Result = AstroImage;
            
            Nim = numel(ImObj);
            
            % create a cube for each dataset
            if Obj.SaveCube && ~isempty(Obj.Cube)
                % Cube exist - use it
                Cube = Obj.Cube;
            else
                % Cube doesn't exist
                [Cube] = images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn);
                if Obj.SaveCube
                    Obj.Cube = Cube;
                end
            end
            
            
            % obtain Gain from header
            if ischar(Args.Gain) || iscellstr(Args.Gain)
                Args.Gain = funHeader(ImObj, @getVal, Args.Gain);
            end
            
            % obtain readnoise from header
            if ischar(Args.ReadNoise) || iscellstr(Args.ReadNoise)
                Args.ReadNoise = funHeader(ImObj, @getVal, Args.ReadNoise);
            end
            
            % obtain Intensity from header (e.g., EXPTIME)
            if ischar(Args.Intensity) || iscellstr(Args.Intensity)
                Args.Intensity = funHeader(ImObj, @getVal, Args.Intensity);
            end
            
            % fit response to each pixel
            Result = imUtil.calib.pixel_flat_response(Cube, 'Gain',Args.Gain,...
                                                            'ReadNoise',Args.ReadNoise,...
                                                            'MeanFun',Args.MeanFun,...
                                                            'MeanFunPar',Args.MeanFunPar,...
                                                            'Intensity',Args.Intensity,...
                                                            'Model',Args.Model);
            


        end
        
    end
    
    methods (Static)  % unitTest
        function Result = unitTest()
            % unitTest for the Stack class
            % Example: Result = imProc.image.Stack.unitTest
            
            % applyUnaryFun
            AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            C  = imProc.image.Stack;
            R  = C.applyUnaryFun(AI,1);
            R  = C.applyUnaryFun(AI,[1 2]);
            R  = C.applyUnaryFun(AI,{1 2}); % the same
            R  = C.applyUnaryFun(AI,AI); 
            R  = C.applyUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
            R  = C.applyUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});
            
            % subtractOffset
            AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            C  = imProc.image.Stack;
            R  = C.subtractOffset(AI,1);
            R  = C.subtractOffset(AI,[1 2]);
            R  = C.subtractOffset(AI,{1 2}); % the same
            R  = C.subtractOffset(AI,AI); 
            R  = C.subtractOffset(AI,@mean,'OpArgs',{'all'});

            % divideFactor
            AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            C  = imProc.image.Stack;
            R  = C.divideFactor(AI,1);
            R  = C.divideFactor(AI,[1 2]);
            R  = C.divideFactor(AI,{1 2}); % the same
            R  = C.divideFactor(AI,AI); 
            R  = C.divideFactor(AI,@mean,'OpArgs',{'all'});

            % funCube
            AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});
            C = imProc.image.Stack;
            [Cube1, Cube2] = C.funCube(AI);
            [CAI] = C.funCube(AI,'SaveInProp',{'ImageData','VarData'});

            % coadd
            AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
            C = imProc.image.Stack;
            [Result, CoaddN] = C.coadd(AI);
            
            % functionalResponse
            AI = AstroImage({ones(3,3), 2.*ones(3,3), 10.*ones(3,3), 11.*ones(3,3), 13.*ones(3,3)});
            C  = imProc.image.Stack;
            Result = C.functionalResponse(AI);
            Result = C.functionalResponse(AI, 'Intensity',[1 2 10 11 13]);
            
            
            Result = true;
                     
            
        end
    end
    
end