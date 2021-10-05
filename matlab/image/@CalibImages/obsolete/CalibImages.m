% CalibImages class - A class for storing calibration image, and performs
%       the basic calibration steps on images.
% Description: Each element of this class may contain all the calibration
%   images for a single detector or a section of a detector.
%   This allows performing calibration for multiple detectors
%   simultaneously.

% use case:
%   1. create dark/bias/flat/fringe
%       bias
%       dark
%       flat
%       fringe
%   2. save to disk/db
%      writeImages
%      write2DB
%   3. upload from disk/db
%       readImages - read calibration images from disk
%       searchDB - search recent calibration files
%   4. apply calib on image
%           Each calib element corresond to one sub/image
%       debias
%       dedark
%       deflat
%       defringe
%       maskSaturated
%       calibrate (do all)


classdef CalibImages < Component
    properties
        Bias AstroImage   
        Dark AstroImage   
        Flat AstroImage
        Fringe AstroImage 
        
        SubtractOverScan(1,1) logical   = true;
    end
    
    properties 
        FlatFilter         = NaN;
        DarkExpTime        = NaN;
        DarkTemp           = NaN;
        FringeFilter       = NaN;
        FringeExpTime      = NaN;
    end
   
    methods  % constructor
        function Obj = CalibImages(Args)
            %
            
            arguments
                Args.Bias           = [];
                Args.Dark           = [];
                Args.Flat           = [];
                Args.Fringe         = [];
                Args.FlatFilter     = [];
                Args.DarkExpTime    = [];
                Args.DarkTemp       = [];
                Args.FringeFilter   = [];
                Args.FringeExpTime  = [];
            end
            
            if ischar(Args.Bias)
                % read image from file
                Obj.Bias = AstroImage(Args.Bias);
            else
                Obj.Bias = Args.Bias;
            end
            if ischar(Args.Dark)
                % read image from file
                Obj.Dark = AstroImage(Args.Dark);
            else
                Obj.Dark = Args.Dark;
            end
            if ischar(Args.Flat)
                % read image from file
                Obj.Flat = AstroImage(Args.Flat);
            else
                Obj.Flat = Args.Flat;
            end
            if ischar(Args.Fringe)
                % read image from file
                Obj.Fringe = AstroImage(Args.Fringe);
              else
                Obj.Fringe = Args.Fringe;
            end
            
            
            
        end
    end
    
    methods % getters/setters
        function set.Flat(Obj, FlatAI)
            % setter for Flat property - set FlatFilter from Header
            
            if ischar(FlatAI)
                Obj.Flat   = AstroImage(FlatAI);
            else
                Obj.Flat   = FlatAI;
            end
            % read filter from Flat image header
            St = getStructKey(Obj.Flat, 'EXPTIME', 'UseDict',true);
            Obj.FlatFilter = [St.EXPTIME];
        end
        
        function set.Dark(Obj, DarkAI)
            % setter for Dark property - set DarkExpTime+darkTemp from
            % Header
            
            if ischar(DarkAI)
                Obj.Dark   = AstroImage(DarkAI);
            else
                Obj.Dark   = DarkAI;
            end
            % read ExpTime from Dark image header
            St = getStructKey(Obj.Dark, 'EXPTIME', 'UseDict',true);
            Obj.DarkExpTime = [St.EXPTIME];
            % read Temp
            St = getStructKey(Obj.Dark, 'CAMTEMP', 'UseDict',true);
            Obj.DarkTemp = [St.CAMTEMP];
            
        end
        
        function set.Fringe(Obj, FringeAI)
            % setter for Fringe property - set FringeExpTime+FringeFilter from
            % Header
            
            if ischar(FringeAI)
                Obj.Fringe   = AstroImage(FringeAI);
            else
                Obj.Fringe   = FringeAI;
            end
            % read filter from Dark image header
            St = getStructKey(Obj.Fringe, 'FILTER', 'UseDict',true);
            Obj.FringeFilter = [St.FILTER];
            % read ExpTime
            St = getStructKey(Obj.Fringe, 'EXPTIME', 'UseDict',true);
            Obj.FringeExpTime = [St.EXPTIME];
            
        end
        
    end
    
    methods (Access=private)
        function [Nobj, Nim] = checkObjImageSize(Obj, Image)
            % Check the validity of the size (number of elements) of CalibImages object and input image
            %   This function will return an error if not one of the
            %   following: size(Image)==size(Obj) or (numel(Obj)==1 and numel(Image)>1)');
            % Input  : - A CalibImages object.
            %          - An AstroImage object
            % Output : - Number of elements in the CalibImages object.
            %          - Number of elements in the AstroImage object.
            % Author : Eran Ofek (Jul 2021)
            
            arguments
                Obj
                Image AstroImage
            end
            
            Nobj = numel(Obj);
            Nim  = numel(Image);
            % check size of Obj and Image
            if all(size(Obj)==size(Image))
                % Image and Obj has the same size - apply one to one calib
            else
                if Nobj==1 && Nim>1
                    % apply calibration to each input image
                else
                    error('Not valid Obj/Image size: Options are: size(Image)==size(Obj) or (numel(Obj)==1 and numel(Image)>1)');
                end
            end
        end
    end
    
    methods % calibration functions
        function Obj = createBias(Obj, ImObj, Args)
            % Create master bias using imProc.dark.bias and store in CalibImages object.
            %   Optionaly, can break the bias image to sub images and store
            %   them in different elements of the CalibImages object.
            % Input  : - An CalibImages object
            %          - A list of AstroImages, or a cell array of file
            %            names, or a a template of file names (interpreted
            %            using regular expressions).
            %          * ...,key,val,...
            %            'BiasArgs' - A cell array of additional arguments
            %                   to pass to the imProc.dark.bias function.
            %                   Default is {}.
            %            'BlockSize' - Sub images block size to generate.
            %                   If empty, use the orginal image size and
            %                   return a single element CalibImages object.
            %                   Otherwise return multi-element CalibImages
            %                   object with a sub (bias) image per element.
            %            'image2subimagesArgs' - A cell array of additional
            %                   arguments to pass to imProc.image.image2subimages
            %                   function. Default is {}.
            % Output : - A CalibImages object with the Bias field
            %            populated. This is an handle to the original input
            %            object.
            % Author : Eran Ofek (Jul 2021)
            % Example: 
            
            arguments
                Obj
                ImObj
                Args.BiasArgs cell            = {};
                Args.BlockSize                = [];  % empty - do not generate sub images
                Args.image2subimagesArgs cell = {};
            end
            
            if isa(ImObj,'AstroImage')
                % do nothing
            else
                List  = io.files.filelist(ImObj);
                ImObj = AstroImage(List);
            end
            
            % seperate to sub image
            % work around for reading sub images from disk...
            
            % for each sub image
            
            [BiasImage, IsBias, CoaddN] = imProc.dark.bias(ImObj, Args.BiasArgs{:});
            
            if isempty(Args.BlockSize)
                Obj.Bias = BiasImage;
            else
                BiasSub = imProc.image.image2subimages(BiasImage, BlockSize, Args.image2subimagesArgs{:});
                Nres     = numel(BiasSub);
                [Obj(1:Nres).Bias] = deal(BiasSub);
            end
        end
        
        function Result = createFlat(Obj)
        end
        
        function Result = readCalibImages(Obj)
        end
        
        function Result = debias(Obj, Image, Args)
            % Subtract bias image from an image and update mask.
            % Input  : - A CalibImages object.
            %            If this is a single-element object, then the bias
            %            image will subtrcated from all input images.
            %            If this is a multi-element object, then the input
            %            must have the same number of elements, ane the
            %            bias subtraction will be done element-by-element.
            %          - An AstroImage object containing the input image
            %            from which to subtract the bias image.
            %          * ...,key,val,...
            %            'CreateNewObj' - [], false, true. 
            %                   See Base.createNewObj for details.
            %                   This referes to creation of a new copy of
            %                   the input AstroImage (not the CalibImages
            %                   object). Default is [].
            % Output : - The AstroImage object, from which the bias was
            %            subtrcated.
            % See also: imProc.dark.debias
            % Author : Eran Ofek (Jul 2021)
            % Example: debias(Obj, Image)
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj     = [];   % refers to the Image and not the Obj!!!
            end
            
            % create new copy of Image object
            [Result] = createNewObj(Image, Args.CreateNewObj, nargout);
           
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
                        
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                % Note taht CreateNewObj was already done (if needed)
                Result(Iim) = imProc.dark.debias(Result(Iim), CalibImages(Iobj).Bias, 'CreateNewObj',false);
            end
        end
        
        function Result = overscan(Obj, Image, Args)
            % Subtract and remove overscan bias from image.
            % Input  : - A CalibImages object.
            %            Not used.
            %          - An AstroImage object containing the input image
            %            from which to subtract the bias image.
            %          * ...,key,val,...
            %            'CreateNewObj' - [], false, true. 
            %                   See Base.createNewObj for details.
            %                   This referes to creation of a new copy of
            %                   the input AstroImage (not the CalibImages
            %                   object). Default is [].
            %            'OverScan' - Either an header keyword containing
            %                   the overscan region, or an [Xmin Xmax Ymin Ymax]
            %                   vector for the overscan.
            %                   Default is 'OVERSCAN'.
            %            'Subtract' - A logical indicating if to subtract
            %                   the overscan from the image. Default is true.
            %            'OverScanDir' - Indicating the direction of the overscan:
            %                   'x'|'y'|1|2| [].
            %                   See imProc.dark.overscan for details.
            %                   Default is [].
            %            'Method' - Method by which to calculate the overscan:
            %                   See imProc.dark.overscan for details.
            %                   Default is 'globalmedian'.
            %            'MethodArgs' - A cell array of additional
            %                   arguments to pass to the method.
            %                   (Defaults are defined for each medthod).
            % Output : - The AstroImage object, from which the overscan was
            %            subtrcated and removed.
            % See also: imProc.dark.overscan
            % Author : Eran Ofek (Jul 2021)
            % Example: 
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj     = [];   % refers to the Image and not the Obj!!!
            end
            
            % create new copy of Image object
            [Result] = createNewObj(Image, Args.CreateNewObj, nargout);
           
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
                        
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                % Note taht CreateNewObj was already done (if needed)
                Result(Iim) = imProc.dark.overscan(Result(Iim), 'CreateNewObj',false,...
                                                                'Subtract',true,...
                                                                'RemoveOthers',true,...
                                                                'OverScan',Args.OverScan,...
                                                                'OverScanDir',Args.OverScanDir,...
                                                                'Method',Args.Method,...
                                                                'MethodArgs',Args.MethodArgs);
            end
        end
        
        function Result = calibrate(Obj, Image, Args)
            % Perform basic calibration (bias, flat, etc) to input images
            %       Perform the following steps on an image:
            %   Create a mask image
            %   Flag staturated pixels in mask
            %   Subtract bias image
            %   Subtract and remove overscan from image
            %   Divide image by flat
            %   Correct for fringing
            %   Multiple image by gain
            % Input : - A CalibImages object.
            %           Each element in the CalibImages must correspond to
            %           an image in the AstroImage input object.
            %         - An AstroImage object.
            %           Number of elements must be equal to the number of
            %           elements in the CalibImages object, or if the
            %           CalibImages object is of size=1, then this may have
            %           any size.
            %         * ...,key,val,...
            %           'CreateNewObj' - [], false, true. Default is [].
            %           'BitDictinaryName' - Bit dictionary name.
            %                   Default is 'BitMask.Image.Default'.
            %           'MaskSaturated' - A logical indicating if to flag
            %                   saturated pixels, in the Mask image.
            %                   Default is true.
            %           'SubtractOverscan' - A logical indicating if to
            %                   subtract overscan. Default is true.
            %           'CorrectFringing' - A logical indicating if to
            %                   correct for fringing. Default is false.
            %           'MultiplyByGain' - A logical indicating if to set
            %                   gain to 1. Default is true.
            %           'InterpolateOverNan' - A logical indicating if to
            %                   interpolate over NaN pixels.
            %                   Default is true.
            %           'InterpolateOberSaturated' - A logical indicating if to
            %                   interpolate over saturated pixels.
            %                   Default is true.
            %           'ArgsSaturation' - A cell array of additional
            %                   arguments to pass to imProc.mask.maskSaturated.
            %                   Default is true.
            %           'ArgsDebias' - A cell array of additional
            %                   arguments to pass to imProc.dark.debias.
            %                   Default is true.
            %           'ArgsOverScan' - A cell array of additional
            %                   arguments to pass to imProc.dark.overscan.
            %                   Default is true.
            %           'ArgsDeflat' - A cell array of additional
            %                   arguments to pass to imProc.flat.deflat.
            %                   Default is true.
            %           'ArgsInterpOverNan' - A cell array of additional
            %                   arguments to pass to imProc.image.interpOverNan.
            %                   Default is true.
            %           'Bitname_Saturated' - The bit name for saturated
            %                   pixels in the mask image dictionary.
            %                   Default is 'Saturated'.
            % Output : - The output AstroImage after calibration.
            % Author : Eran Ofek (Jul 2021)
            % Example:
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj                   = [];   % refers to the Image and not the Obj!!!
                
                % bit dictionary
                Args.BitDictinaryName               = 'BitMask.Image.Default';
                
                Args.MaskSaturated(1,1) logical     = true;
                Args.SubtractOverscan(1,1) logical  = true;
                Args.CorrectFringing(1,1) logical   = false;
                Args.MultiplyByGain(1,1) logical    = true;
                Args.InterpolateOverNan(1,1) logical       = true;
                Args.InterpolateOberSaturated(1,1) logical = true;
                
                Args.ArgsSaturation cell            = {};
                Args.ArgsDebias cell                = {};
                Args.ArgsOverScan cell              = {};
                Args.ArgsDeflat cell                = {};
                Args.ArgsInterpOverNan cell         = {};
                
                Args.Bitname_Saturated              = 'Saturated';
                
            end
            
            % create new copy of Image object
            [Result, CreateNewObj] = createNewObj(Image, Args.CreateNewObj, nargout);
           
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
                  
            % populate calibration images in a different function
            
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                
                % mark satuarted pixels
                if Args.MaskSaturated
                    Result(Iim) = imProc.mask.maskSaturated(Result(Iim), Args.ArgSaturation{:},...
                                                                     'CreateNewObj',false,...
                                                                     'DefBitDict', BitDictionary(Args.BitDictinaryName) );
                end
                
                % subtract bias
                % Note taht CreateNewObj was already done (if needed)
                Result(Iim) = imProc.dark.debias(Result(Iim), CalibImages(Iobj).Bias, Args.ArgsDebias{:},...
                                                                                      'CreateNewObj',false,...
                                                                                      'BitDictinaryName',Args.BitDictinaryName);
                % FFU: dark
                
                % subtract overscan
                if Args.SubtractOverscan
                    Result(Iim) = imProc.dark.overscan(Result(Iim), 'Subtract',true,...
                                                                'RemoveOthers',true,...
                                                                Args.ArgsOverscan{:},...
                                                                'CreateNewObj',false);
                end                                            
                
                % divide by flat
                Result(Iim) = imProc.flat.deflat(Result(Iim), CalibImages(Iobj).Flat, Args.ArgsDeflat{:},...
                                                                                      'CreateNewObj',false,...
                                                                                      'BitDictinaryName',Args.BitDictinaryName);
                
                % Fring correction
                if Args.CorrectFringing
                    error('Correct fringing is not available yet');
                    % Use fring image Args.Fringe(Iobj)
                end
                
                % multipply image by gain
                if Args.MultiplyByGain
                    imProc.calib.gainCorrect(Result(Iim));            
                end
                
                % interpolate over satuiared pixels
                if Args.InterpolateOverNan && any(isnan(Result(Iim).Image),'all')
                    Result(Iim) = imProc.image.interpOverNan(Result(Iim), Args.ArgsInterpOverNan{:},...
                                            'CreateNewObj',false);
                end
                
                % interpolate over saturated pixels
                if Args.InterpolateOberSaturated
                    % find saturated pixels
                    [~, ~, Ind] = findBit(Result(Iobj), Args.Bitname_Saturated);
                    % set saturated pixels to NaN
                    Result(Iobj).Image(Ind) = NaN;
                    % interpolate over staurated pixels
                    Result(Iim) = imProc.image.interpOverNan(Result(Iim), Args.ArgsInterpOverNan{:},...
                                            'CreateNewObj',false);
                end
            end
        end
    end
    
        
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for CalibImages class

    end
    
end

