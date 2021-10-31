% CalibImages class - A class for storing calibration image, and performs
%       the basic calibration steps on images.
% Description: Each element of this class may contain all the calibration
%   images for a single detector or a section of a detector.
%   This allows performing calibration for multiple detectors
%   simultaneously.

% #functions (autogen)
% CalibImages - Constructor for the CalibImages class
% checkObjImageSize - Check the validity of the size (number of elements) of CalibImages object and input image This function will return an error if not one of the following: size(Image)size(Obj) or (numel(Obj)1 and numel(Image)>1)');
% createBias - Create master bias using imProc.dark.bias and store in CalibImages object. Optionaly, can break the bias image to sub images and store them in different elements of the CalibImages object.
% createFlat - Create a Flat image for all filters and populate in CalibImages object. Given a list of dark-subtracted images, identify flat images taken at a each filter, and generate a flat images. The flat image is added into the array of Flat images in the CalibImages object.
% createFlatFilter - Create a Flat image for specific filter and populate in CalibImages object. Given a list of dark-subtracted images, identify flat images taken at a specific filter, and generate a flat image. The flat image is added into the array of Flat images in the CalibImages object.
% debias - Subtract bias image from an image and update mask.
% deflat - Divide from image from an image and update mask (multiple filters).
% overscan - Subtract and remove overscan bias from image.
% processImages - Perform basic calibration (bias, flat, etc) to input images Perform the following steps on an image: Create a mask image Flag staturated pixels in mask Subtract bias image
% set.Dark - setter for Dark property - set DarkExpTime+darkTemp from Header
% set.Flat - setter for Flat property - set FlatFilter from Header
% set.Fringe - setter for Fringe property - set FringeExpTime+FringeFilter from Header
% #/functions (autogen)
%

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
    
    properties (SetAccess = private)
        % These are private properties as they are updated automatically
        % from the image header
        FlatFilter         = {};
        DarkExpTime        = [];
        DarkTemp           = [];
        FringeFilter       = {};
        FringeExpTime      = [];
    end
   
    methods  % constructor
        function Obj = CalibImages(Args)
            % Constructor for the CalibImages class
            % Input  : * ...,key,val,...
            %            'Bias' - A Bias AstroImage
            %            'Dark' - A Dark AstroImage (per ExpTime)
            %            'Flat' - A Flat AstroImage (per filter)
            %            'Fringe' - A fringe AstroImage (per filter)
            %            'FlatFilter' -
            %            'DarkExpTime'
            %            'DarkTemp'
            %            'FringeFilter'
            %            'FringeExpTime'
            %
            % Output : - A CalibImages object
            % Author : Eran Ofek (Aug 2021)
            % Example:
            
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
            
            % only if there is non-emoty first image
            [SizeI, SizeJ] = sizeImage(Obj.Flat(1));
            if all(SizeI>0) && all(SizeJ>0)
                % read filter from Flat image header
                St = getStructKey(Obj.Flat, 'FILTER', 'UseDict',true);
                Obj.FlatFilter = {St.FILTER};
            end
        end
        
        function set.Dark(Obj, DarkAI)
            % setter for Dark property - set DarkExpTime+darkTemp from
            % Header
            
            if ischar(DarkAI)
                Obj.Dark   = AstroImage(DarkAI);
            else
                Obj.Dark   = DarkAI;
            end
            
            % only if there is non-emoty first image
            [SizeI, SizeJ] = sizeImage(Obj.Dark(1));
            if all(SizeI>0) && all(SizeJ>0)
                % read ExpTime from Dark image header
                St = getStructKey(Obj.Dark, 'EXPTIME', 'UseDict',true);
                Obj.DarkExpTime = [St.EXPTIME];
                % read Temp
                St = getStructKey(Obj.Dark, 'CAMTEMP', 'UseDict',true);
                Obj.DarkTemp = [St.CAMTEMP];
            end
        end
        
        function set.Fringe(Obj, FringeAI)
            % setter for Fringe property - set FringeExpTime+FringeFilter from
            % Header
            
            if ischar(FringeAI)
                Obj.Fringe   = AstroImage(FringeAI);
            else
                Obj.Fringe   = FringeAI;
            end
            
            % only if there is non-emoty first image
            [SizeI, SizeJ] = sizeImage(Obj.Fringe(1));
            if all(SizeI>0) && all(SizeJ>0)
                % read filter from Dark image header
                St = getStructKey(Obj.Fringe, 'FILTER', 'UseDict',true);
                Obj.FringeFilter = {St.FILTER};
                % read ExpTime
                St = getStructKey(Obj.Fringe, 'EXPTIME', 'UseDict',true);
                Obj.FringeExpTime = [St.EXPTIME];
            end
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
            % Example: cd /data/euler/archive/AstroPack/data/LAST/TestImages
            %          CI = CalibImages;
            %          CI.createBias('LAST*_dark.fits');
            
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
                if isempty(List)
                    error('No bias images were supplied');
                end
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
        
        function Obj = createFlatFilter(Obj, ImObj, FilterName, Args)
            % Create a Flat image for specific filter and populate in CalibImages object.
            %       Given a list of dark-subtracted images, identify flat images taken at a
            %       specific filter, and generate a flat image. The flat
            %       image is added into the array of Flat images in the
            %       CalibImages object.
            % Input  : - An CalibImages object (dark-subtracted).
            %          - An AstroImage array containing images from which
            %            to construct flat images.
            %            The flat images in the specific filter are
            %            detected by the code.
            %          - Filter name for which to create the flat.
            %          * ...,key,val,...
            %            'FilterKey' - The header keyword name containing the
            %                   filter name. By defdault the search is done using
            %                   the synonyms dictionary, so if needed add
            %                   the keyword name to the Header.Synonyms.KeyNames.yml
            %                   Default is 'FILTER'.
            %            'IsFilterFlat' - A vector of logicals or empty.
            %                   If empty, then the flat images at the
            %                   specific filter will be selected
            %                   automatically. Alternatively, this can be a
            %                   vector of logicals indicating which images
            %                   to use. If a scalar true, then use all the
            %                   images. Default is [].
            %            'FilterList' - An optional cell array of filter
            %                   per image. If empty, will be generated.
            %                   Default is [].
            %            'getStructKeyArgs' - A cell array of additional
            %                   arguments to pass to getStructKey.
            %                   Default is {}.
            %            'isFlatArgs' - A cell array of additional
            %                   arguments to pass to imProc.flat.isFlat.
            %                   Default is {}.
            %            'flatArgs' - A cell array of additional
            %                   arguments to pass to imProc.flat.flat.
            %                   Default is {}.
            % Output : - A CalibImages object in which the new flat is
            %            populated.
            % Author : Eran Ofek (Oct 2021)
            % Example:
            
            arguments
                Obj
                ImObj           % Images from which to create Flat
                FilterName      % Filter for which to create Flat
                Args.FilterKey                    = 'FILTER';
                Args.FilterList cell              = {};
                Args.IsFilterFlat                 = [];
                Args.getStructKeyArgs cell        = {};
                Args.isFlatArgs cell              = {};
                Args.flatArgs cell                = {};
            end
                        
            % search for filter name
            if isempty(Args.FilterList)
                ImFilt      = getStructKey(ImObj, Args.FilterKey, Args.getStructKeyArgs{:});
                FilterList = {ImFilt.(FilterKey)};
            else
                FilterList = Args.FilterList;
            end
            Flag.Filter = strcmp(FilterList, FilterName);
            ImObj       = ImObj(Flag.Filter);
            
            % search for flat images
            if isempty(Args.IsFilterFlat)
                [Flag.IsFlat, Flag.AllIsFlat] = imProc.flat.isFlat(ImObj, Args.isFlatArgs{:});
            else
                if numel(Args.IsFilterFlat)==1 && Args.IsFilterFlat
                    Flag.IsFlat = true(size(ImObj));
                else
                    Flag.IsFlat = Args.IsFilterFlat;
                end
            end
             
            % create Flat
            [FlatImage] = imProc.flat.flat(ImObj, 'IsFlat',Flag.IsFlat, Args.flatArgs{:});
            
            % store the flat
            % the filter name is store automatically from the header
            Ind = numel(Obj.Flat) + 1;
            if Ind==2
                % check that the image is not empty
                IsEmpty = isemptyImage(Obj.Flat);
                if IsEmpty
                    Ind = 1;
                end
            end
            Obj.Flat(Ind) = FlatImage;
            
        end
        
        function Obj = createFlat(Obj, ImObj, Args)
            % Create a Flat image for all filters and populate in CalibImages object.
            %       Given a list of dark-subtracted images, identify flat images taken at a
            %       each filter, and generate a flat images. The flat
            %       image is added into the array of Flat images in the
            %       CalibImages object.
            % Input  : - An CalibImages object (dark subtracted).
            %          - An AstroImage array containing images from which
            %            to construct flat images.
            %            The flat images in the specific filter are
            %            detected by the code.
            %          * ...,key,val,...
            %            'FilterKey' - The header keyword name containing the
            %                   filter name. By defdault the search is done using
            %                   the synonyms dictionary, so if needed add
            %                   the keyword name to the Header.Synonyms.KeyNames.yml
            %                   Default is 'FILTER'.
            %            'UseFilters' - A cell array of filters for which
            %                   to generate flats. If empty, then search
            %                   for all possible filters and constrcu flat
            %                   for each one. Default is {}.
            %            'IgnoreFilters' - If 'UseFilters' is empty, then
            %                   you can provide here a cell array of filter
            %                   names for which not to generate flat image.
            %                   Default is {}.
            %            'getStructKeyArgs' - A cell array of additional
            %                   arguments to pass to getStructKey.
            %                   Default is {}.
            %            'isFlatArgs' - A cell array of additional
            %                   arguments to pass to imProc.flat.isFlat.
            %                   Default is {}.
            %            'flatArgs' - A cell array of additional
            %                   arguments to pass to imProc.flat.flat.
            %                   Default is {}.
            % Output : - A CalibImages object in which the new flat is
            %            populated.
            % Author : Eran Ofek (Oct 2021)
            % Example:
            
            arguments
                Obj
                ImObj           % Images from which to create Flat
                Args.FilterKey                    = 'FILTER';
                Args.UseFilters cell              = {}; % override IgnoreFilters
                Args.IgnoreFilters cell           = {};
                Args.getStructKeyArgs cell        = {};
                Args.isFlatArgs cell              = {};
                Args.flatArgs cell                = {};
            end
            
            % identify all possible filters
            ImFilt        = getStructKey(ImObj, Args.FilterKey, Args.getStructKeyArgs{:});
            FilterList    = {ImFilt.FILTER};
            UniqueFilters = unique(FilterList);
            
            % prepare filter list
            if ~isempty(Args.UseFilters)
                Flag = ismember(UniqueFilters, Args.UseFilters);
                UniqueFilters = UniqueFilters(Flag);
            else
                Flag = ~ismember(UniqueFilters, Args.IgnoreFilters);
                UniqueFilters = UniqueFilters(Flag);
            end
            
            % for each filter
            Nfilt = numel(UniqueFilters);
            for Ifilt=1:1:Nfilt
                Obj = createFlatFilter(Obj, ImObj, UniqueFilters{Ifilt}, 'FilterKey',Args.FilterKey,...
                                                                         'IsFilterFlat',[],...
                                                                         'FilterList',FilterList,...
                                                                         'getStructKeyArgs',Args.getStructKeyArgs,...
                                                                         'isFlatArgs',Args.isFlatArgs,...
                                                                         'flatArgs',Args.flatArgs);
                
            end
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
            %            'CreateNewObj' - [false] | true.
            %                   If true, then create a new copy of the
            %                   images input. Default is false.
            %                   Note that the CalibImage itself is not
            %                   copied.
            %            'debiasArgs' - A cell array of additional
            %                   arguments to pass to the imProc.dark.debias
            %                   function. Default is {}.
            % Output : - The AstroImage object, from which the bias was
            %            subtrcated.
            % See also: imProc.dark.debias
            % Author : Eran Ofek (Jul 2021)
            % Example: debias(Obj, Image)
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj logical     = false;   % refers to the Image and not the Obj!!!
                Args.debiasArgs cell          = {};
            end
            
            % create new copy of Image object
            %[Result] = createNewObj(Image, Args.CreateNewObj, nargout);
            if Args.CreateNewObj
                Result = Image.copy;
            else
                Result = Image;
            end
            
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
                        
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                % Note taht CreateNewObj was already done (if needed)
                Result(Iim) = imProc.dark.debias(Result(Iim), Obj(Iobj).Bias, 'CreateNewObj',false, Args.debiasArgs{:});
            end
        end
        
        function Result = overscan(Obj, Image, Args)
            % Subtract and remove overscan bias from image.
            % Input  : - A CalibImages object.
            %            Not used.
            %          - An AstroImage object containing the input image
            %            from which to subtract the bias image.
            %          * ...,key,val,...
            %            'CreateNewObj' - [false] | true.
            %                   Indicating of to create a new copy of the
            %                   input image. Default is false.
            %            'OverScan' - Either an header keyword containing
            %                   the overscan region, or an [Xmin Xmax Ymin Ymax]
            %                   vector for the overscan.
            %                   Default is 'OVERSCAN'.
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
                Args.CreateNewObj logical    = false;   % refers to the Image and not the Obj!!!
                Args.OverScan                = 'OVERSCAN';
                Args.OverScanDir             = [];
                Args.Method                  = 'globalmedian';
                Args.MethodArgs              = {};
            end
            
            % create new copy of Image object
            if Args.CreateNewObj
                Result = Image.copy;
            else
                Result = Image;
            end
           
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
                        
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                % Note taht CreateNewObj was already done (if needed)
                Result(Iim) = imProc.dark.overscan(Result(Iim), 'CreateNewObj',false,...
                                                                'Subtract',true,...
                                                                'RemoveOverScan',true,...
                                                                'RemoveOthers',true,...
                                                                'OverScan',Args.OverScan,...
                                                                'OverScanDir',Args.OverScanDir,...
                                                                'Method',Args.Method,...
                                                                'MethodArgs',Args.MethodArgs);
            end
        end
        
        function Result = deflat(Obj, Image, Args)
            % Divide from image from an image and update mask (multiple filters).
            % Input  : - A CalibImages object.
            %            If this is a single-element object, then the flat
            %            image will divide from all input images.
            %            If this is a multi-element object, then the input
            %            must have the same number of elements, ane the
            %            flat division will be done element-by-element.
            %          - An AstroImage object containing the input image
            %            from which to divide the flat image.
            %            The image should be bias/dark subtracted.
            %          * ...,key,val,...
            %            'CreateNewObj' - [false] | true.
            %                   If true, then create a new copy of the
            %                   images input. Default is false.
            %                   Note that the CalibImage itself is not
            %                   copied.
            %            'FilterKey' - The header keyword name containing the
            %                   filter name. By defdault the search is done using
            %                   the synonyms dictionary, so if needed add
            %                   the keyword name to the Header.Synonyms.KeyNames.yml
            %                   Default is 'FILTER'.
            %            'FilterList' - An optional cell array of filter
            %                   per image. If empty, will be generated.
            %                   Default is [].
            %            'getStructKeyArgs' - A cell array of additional
            %                   arguments to pass to getStructKey.
            %                   Default is {}.
            %            'deflatArgs' - A cell array of additional
            %                   arguments to pass to the imProc.flat.deflat
            %                   function. Default is {}.
            % Output : - The AstroImage object, from which the bias was
            %            subtrcated.
            % See also: imProc.dark.debias
            % Author : Eran Ofek (Jul 2021)
            % Example: debias(Obj, Image)
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj logical     = false;   % refers to the Image and not the Obj!!!
                Args.FilterKey                = 'FILTER';
                Args.FilterList cell          = {};
                Args.getStructKeyArgs cell    = {};
                Args.deflatArgs cell          = {};
            end
            
            % create new copy of Image object
            %[Result] = createNewObj(Image, Args.CreateNewObj, nargout);
            if Args.CreateNewObj
                Result = Image.copy;
            else
                Result = Image;
            end
            
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
                        
            % search for filter name
            if isempty(Args.FilterList)
                ImFilt      = getStructKey(Image, Args.FilterKey, Args.getStructKeyArgs{:});
                FilterList = {ImFilt.(Args.FilterKey)};
            else
                FilterList = Args.FilterList;
            end
                        
            UniqueFilter = unique(FilterList);
            Nfilt        = numel(UniqueFilter);
            for Ifilt=1:1:Nfilt
                % Index of flat image in CalibImages.Flat
                FlagFlat   = strcmp(Obj.FlatFilter, UniqueFilter{Ifilt});
                % indices of the Images taken with the specific filter
                FlagImages = strcmp(FilterList, UniqueFilter{Ifilt});
                
                if any(FlagImages)
                    if ~any(FlagFlat)
                        error('No Flat image for filter %s',UniqueFilter{Ifilt});
                    end
                    Result(FlagImages) = imProc.flat.deflat(Result(FlagImages), Obj.Flat(FlagFlat), 'CreateNewObj',false, Args.deflatArgs{:});
                end
            end
        end
        
        function Result = processImages(Obj, Image, Args)
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
            %           'maskSaturatedArgs' - A cell array of additional
            %                   arguments to pass to imProc.mask.maskSaturated
            %                   Default is {}.
            %           'debiasArgs' - A cell array of additional
            %                   arguments to pass to imProc.dark.debias.
            %                   Default is {}.
            %           'SubtractOverscan' - A logical indicating if to
            %                   subtract overscan. Default is true.
            %           'OverScan' - Either an header keyword containing
            %                   the overscan region, or an [Xmin Xmax Ymin Ymax]
            %                   vector for the overscan.
            %                   Default is 'OVERSCAN'.
            %           'MethodOverScan' - see imProc.dark.overscan.
            %                   Default is 'globalmedian'.
            %           'deflatArgs' - A cell array of additional
            %                   arguments to pass to imProc.flat.deflat.
            %                   Default is {}.
            %           'CorrectFringing' - A logical indicating if to
            %                   correct fringing. Default is false.
            %           'MultiplyByGain' - A logical indicating if to
            %                   multiply image values by gain.
            %                   Default is true.
            %           'InterpolateOberSaturated' - A logical indicating
            %                   if to interpolate over saturated pixels and NaNs.
            %                   Default is true.
            %           'Bitname_Saturated' - Satuarted pixels bitmask
            %                   name. Default is 'Saturated'.
            %           'interpOverNanArgs' - A cell array of additional
            %                   arguments to pass to imProc.image.interpOverNan.
            %                   Default is {}.
            % Output : - The output AstroImage after calibration.
            % Author : Eran Ofek (Oct 2021)
            % Example:
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj                   = false;   % refers to the Image and not the Obj!!!
                
                % bit dictionary
                Args.BitDictinaryName               = 'BitMask.Image.Default';
                
                Args.MaskSaturated logical          = true;
                Args.maskSaturatedArgs cell         = {};
                Args.debiasArgs cell                = {};
                Args.SubtractOverscan logical       = true;
                Args.OverScan                       = 'OVERSCAN';
                Args.MethodOverScan                 = 'globalmedian';
                Args.deflatArgs cell                = {};
                Args.CorrectFringing logical        = false;
                Args.MultiplyByGain logical         = true;
                Args.InterpolateOverSaturated logical = true;
                Args.Bitname_Saturated              = 'Saturated';
                Args.interpOverNanArgs cell         = {};
                
            end
            
            % create new copy of Image object
            if Args.CreateNewObj
                Result = Image.copy;
            else
                Result = Image;
            end
                
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
                  
            % populate calibration images in a different function
                        
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                
                % mark satuarted pixels
                if Args.MaskSaturated
                    Result(Iim) = imProc.mask.maskSaturated(Result(Iim), Args.maskSaturatedArgs{:},...
                                                                     'CreateNewObj',false,...
                                                                     'DefBitDict', BitDictionary(Args.BitDictinaryName) );
                end
            end
            
            % subtract bias
            Result = Obj.debias(Result, 'debiasArgs',Args.debiasArgs, 'CreateNewObj',false');
            
            % FFU: dark
            
                
            % subtract overscan
            if Args.SubtractOverscan
                Result = Obj.overscan(Result, 'OverScan',Args.OverScan, 'Method',Args.MethodOverScan, 'CreateNewObj',false');
            end
                
            % The overscan may change the size of the image.
            % Depands on how the flat was created, the new image size may
            % not be compatible with the flat size
            
            % divide by flat
            Result = Obj.deflat(Result, 'deflatArgs',Args.deflatArgs, 'CreateNewObj',false');
            
            % Fring correction
            if Args.CorrectFringing
                error('Correct fringing is not available yet');
                % Use fring image Args.Fringe(Iobj)
            end
                
            % multipply image by gain
            if Args.MultiplyByGain
                imProc.calib.gainCorrect(Result, 'CreateNewObj',false);
            end
                
            
            % interpolate over saturated pixels
            if Args.InterpolateOverSaturated
                % find saturated pixels
                Nim = numel(Result);
                for Iim=1:1:Nim
                    [~, ~, Ind] = findBit(Result(Iim).MaskData, Args.Bitname_Saturated);
                    % set saturated pixels to NaN
                    Result(Iim).Image(Ind) = NaN;
                    % interpolate over staurated pixels
                    %Result(Iim).cast('double');  % <<< FFU: REMOVE THIS AFTER BUG FIXED
                    
                    Result(Iim) = imProc.image.interpOverNan(Result(Iim), Args.interpOverNanArgs{:},...
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
