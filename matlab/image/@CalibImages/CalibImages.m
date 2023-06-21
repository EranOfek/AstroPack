% CalibImages class - A class for storing calibration image, and performs
%       the basic calibration steps on images.
% Description: Each element of this class may contain all the calibration
%   images for a single detector or a section of a detector.
%   This allows performing calibration for multiple detectors
%   simultaneously.
%
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
        Linearity

        % created by createBias
        DarkGroupsKey   % cell array of keys
        DarkGroupsVal   % Cell (size Nimages) of cells (size Nkeys)
        
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
    
    methods (Static)  % read
        function Obj = read(Args)
            % Read FITS files into a CalibImages object (Static)
            % Input  : * ...,key,val,...
            %            'Bias', 'BiasVar', 'BiasMask', 'Dark', 'DarkVar',
            %            'DarkMask', 'Flat', 'FlatVar', 'FlatMask' -
            %                   These are the file names containing these
            %                   specific components.
            % Output : - A CalibImages object with the calibration images.
            % Author : Eran Ofek (Jan 2022)
            % Example:
            % CI = CalibImages.read('Bias','LAST.2.1.2_20200821.060000_clear___1_0_dark_proc_Image_1.fits','BiasMask','LAST.2.1.2_20200821.060000_clear___1_0_dark_proc_Mask_1.fits','Flat','LAST.2.1.2_20200821.060000_clear___1_0_flat_proc_Image_1.fits','FlatMask','LAST.2.1.2_20200821.060000_clear___1_0_flat_proc_Mask_1.fits');
            
            arguments
                Args.Bias      = [];
                Args.BiasVar   = [];
                Args.BiasMask  = [];
                Args.Dark      = [];
                Args.DarkVar   = [];
                Args.DarkMask  = [];
                Args.Flat      = [];
                Args.FlatVar   = [];
                Args.FlatMask  = [];
            end
            
            Obj      = CalibImages;
            Obj.Bias = AstroImage(Args.Bias, 'Mask',Args.BiasMask, 'Var',Args.BiasVar);
            Obj.Dark = AstroImage(Args.Dark, 'Mask',Args.DarkMask, 'Var',Args.DarkVar);
            Obj.Flat = AstroImage(Args.Flat, 'Mask',Args.FlatMask, 'Var',Args.FlatVar);
            
        end
        
        function Obj = loadFromDir(DirName, Args)
            % load a single filter calibration files from directory
            %   assuming image names has some pattern with ImType and product
            %   name in the file name.
            %   E.g., file name is of the format: *ImType*Product*FileType
            % Input  : - Dir name in which to look for the files.
            %          * ...,key,val,...
            %            'BiasImType' - ImType that should appear in the
            %                   bias images names. Default is 'dark'.
            %            'FlatImType' - ImType that should appear in the
            %                   flat images names.
            %                   If empty then do not load the flat.
            %                   Default is 'flat'.
            %            'BiasProduct' - A cell array of product names to
            %                   upload, in addition to the 'Image', for
            %                   the bias image. Default is {'Mask'}.
            %            'FlatProduct' - A cell array of product names to
            %                   upload, in addition to the 'Image', for
            %                   the flat image. Default is {'Mask'}.
            %            'ImageProduct' - Image product to search and
            %                   upload. Default is 'Image'.
            %            'FileType' - File type to read. Default is 'fits'.
            %            'LoadLatest' - A logical indicating if to read the
            %                   latest files (by date), or the first.
            %                   Default is true.
            %            'GroupKeys' - A cell array of header keywords from
            %                   which to construct all unique groups.
            %                   Every dark image with these with these
            %                   unique key values will be loaded.
            %                   E.g., {'EXPTIME','CAMOFFS','CAMGAIN'}
            %                   If empty, then ignore and upload only one.
            %                   Default is empty.
            %            'LinearityName' - Linearity file name or template
            %                   name to load. If empty, do not load.
            %                   Default is [].
            %            'LinearityPathBase' - Linearity path.
            %                   See CalibImages/populateLinearity.
            %                   Default is [].
            % Output : - A CalibImages object in which the Bias and Flat
            %            fields are populated, and optionally also the
            %            Linearity property is loaded.
            % Author : Eran Ofek (Apr 2022)
            % Example: CI = CalibImages.loadFromDir
           
            arguments
                DirName                  = [];
                Args.BiasImType          = 'dark';
                Args.FlatImType          = 'flat';
                Args.BiasProduct         = {'Mask'};  % 'Image' always loaded
                Args.FlatProduct         = {'Mask'};
                Args.ImageProduct        = 'Image';
                Args.FileType            = 'fits';
                Args.LoadLatest logical  = true;
                Args.GroupKeys           = []; %{'EXPTIME','CAMOFFS','CAMGAIN'};
                Args.ExpTime             = [];

                Args.LinearityName       = [];
                Args.LinearityPathBase   = [];
            end
            
            if isempty(DirName)
                DirName = '';
            end
            if ~isempty(DirName)
                DirName = sprintf('%s%s',DirName, filesep);
            end
            
            Obj = CalibImages;
            
            % Bias
            Field  = 'BiasProduct';
            ImType = 'BiasImType'; 
            Nprod = numel(Args.(Field));
            Pat = sprintf('%s*%s*%s*%s',DirName, Args.(ImType), Args.ImageProduct, Args.FileType);
            PWD = pwd;
            cd(DirName);
            Files = io.files.dirSortedByDate(Pat);
            if isempty(Files)
                warning('Bias images were not found in %s',DirName);
            else
                if ~isempty(Args.GroupKeys)
                    % read headers
                    AH     = AstroHeader({Files.name});
                    Groups = AH.groupByKeyVal(Args.GroupKeys);
                    Ngroup = numel(Groups);
                    Obj.DarkGroupsKey = Args.GroupKeys;
                    Obj.DarkGroupsVal = {Groups.Content};
                else
                    if ~isempty(Args.ExpTime)
                        AH     = AstroHeader({Files.name});
                        ET     = AH.getStructKey('EXPTIME');
                        Groups(1).ptr = find([ET.EXPTIME]==Args.ExpTime,1,'last');
                        Ngroup = 1;
                    else
                    
                        Ngroup = 1;
                        Groups(1).ptr = (1:1:numel(Files));
                    end
                end
                
%                 if ~isempty(Args.ExpTime)
%                     AH     = AstroHeader({Files.name});
%                     ET = AH.getStructKey('EXPTIME');
%                     FlagET = [ET.EXPTIME] == Args.ExpTime;
%                     Files  = Files(FlagET);
%                 end
                
                for Igroup=1:1:Ngroup
                    FilesG = Files(Groups(Igroup).ptr);
                    
                    
                    if Args.LoadLatest
                        FilesG = FilesG(end);
                    else
                        FilesG = FilesG(1);
                    end
                    ArgsAI    = cell(1, 1+Nprod.*2);
                    I         = 1;
                    ArgsAI{I} = fullfile(FilesG.folder,FilesG.name);

                    for Iprod=1:1:Nprod
                        I = I + 1;
                        ArgsAI{I} = Args.(Field){Iprod};
                        I = I + 1;
                        ArgsAI{I} = strrep(fullfile(FilesG.folder,FilesG.name), Args.ImageProduct, Args.(Field){Iprod});
                    end
                    Obj.Bias(Igroup) = AstroImage(ArgsAI{:});
                end

                % populate Linearity
                Obj = Obj.populateLinearity(Args.LinearityName, 'PathBase',Args.LinearityPathBase);

            end
            
            % Flat
            if ~isempty(Args.FlatImType)
            
                Field  = 'FlatProduct';
                ImType = 'FlatImType'; 
                Nprod = numel(Args.(Field));
                Pat = sprintf('%s*%s*%s*%s',DirName, Args.(ImType), Args.ImageProduct, Args.FileType);
                Files = io.files.dirSortedByDate(Pat);
                if isempty(Files)
                    warning('Bias images were not found in %s',DirName);
                else
                    if Args.LoadLatest
                        Files = Files(end);
                    else
                        Files = Files(1);
                    end
                    ArgsAI    = cell(1, 1+Nprod.*2);
                    I         = 1;
                    ArgsAI{I} = fullfile(Files.folder,Files.name);

                    for Iprod=1:1:Nprod
                        I = I + 1;
                        ArgsAI{I} = Args.(Field){Iprod};
                        I = I + 1;
                        ArgsAI{I} = strrep(fullfile(Files.folder,Files.name), Args.ImageProduct, Args.(Field){Iprod});
                    end
                    Obj.Flat = AstroImage(ArgsAI{:});
                end
            end
            cd(PWD);
        end
        
    end
    
    methods
%         function write1(Obj, IP, Type, Products, Args)
%             % Write CalibImages products to disk
%             %   Works on a CalibImages object that contains a single calib
%             %   image of each type.
%             % Input  : - A CalibImages object.
%             %
%            
%             arguments
%                 Obj
%                 IP  % IP or cell of file names
%                 Type          = {'Bias','Flat'};
%                 Products      = {'Image', 'Var', 'Mask'};
%                 Args.Dir      = [];  % if empty use genPath
%             end
%             
%             Ntype = numel(Type);
%             Nprod = numel(Products);
%             if isa((IP,'ImagePath')
%                 % assume IP is an ImagePath object
%                 
%                 IP.Level   = 'proc';
%                 for Itype=1:1:Ntype
%                     IP.Type = Type{Itype};
%                    
%                     for Iprod=1:1:Nprod
%                         IP.Product = Products{Iprod};
%                         if isempty(Args.Dir)
%                             MasterName = IP.genFull;
%                         else
%                             MasterName = sprintf('%s%s%s',Args.Dir,filesep,IP.genFile);
%                         end
%                         write1(Obj.(Type{Itype}), MasterName, Products{Iprod});
%                         
%                     end
%                 end
%             end
%          
%         end
    end
    
    methods % utility functions
        function varargout = isemptyProp(Obj, Prop, ImageProp)
            % Check if some properties (e.g., 'Bias') images are empty
            %   For each requested CalibImages property, check if any of
            %   the images fields are empty.
            % Input  : - A single element CalibImages object.
            %          - A cell array of properties to check in the
            %            CalibImages (e.g., {'Bias','Flat'}).
            %          - Property in the AstroImage to check if empty.
            %            Default is 'Image'.
            % Output : * The number of output arguments equal to the number
            %            of requested properties (second input argument).
            %            IN each output, return an array of logicals
            %            indicating if any of the calib images are empty.
            % Author : Eran Ofek (Jan 2022)
            % Example: [R1, R2] = isemptyProp(Obj, {'Bias','Flat'}, 'Image')
            
            arguments
                Obj(1,1)
                Prop
                ImageProp    = 'Image';
            end
            
            if ischar(Prop)
                Prop = {Prop};
            end
            Nprop = numel(Prop);
            varargout = cell(1:1:Nprop);
            for Iprop=1:1:Nprop
                varargout{Iprop} = any(isemptyImage(Obj.(Prop{Iprop}), ImageProp));
            end
            
        end
        
        function Result = crop(Obj, CCDSEC, Args)
            % Crop all images in a CalibImages object
            % Input  : - A CalibImages object.
            %          - A CCDSEC [xmin, xmax, ymin, ymax]
            %            or [Xcenter, Ycenter, Xhalfsize, Yhalfsize]
            %            (select option using 'Type').
            %          * ...,key,val,...
            %            'Type' - ['ccdsec'] | 'center'
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object. Default is false.
            %            'DataProp' - A cell array of properties in the
            %                   CalibImages object to crop.
            %                   Default is {'Bias','Flat'}
            % Output : - A calibImages object with cropped images.
            % Author : Eran Ofek (Sep 2022)
            % Example: CI.crop([1 100 1 100])
           
            arguments
                Obj
                CCDSEC
                Args.Type                   = 'ccdsec';  % 'center'
                Args.CreateNewObj logical   = false;
                Args.DataProp               = {'Bias','Flat'};
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj  = numel(Obj);
            Nprop = numel(Args.DataProp);
            
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    Result(Iobj).(Args.DataProp{Iprop}).crop(CCDSEC, 'Type',Args.Type, 'CreateNewObj',false);
                end
            end
        end
        
        function Result=populateLinearity(Obj, Name, Args)
            % read Linaerity file from config file and populate the CalibImages
            % Input  : - A single element CalibImages object.
            %          - Configuration file name or file template name.
            %          * ...,key,val,...
            %            'PathBase' - Path in which the directory tree
            %                   containing the linearity configuration file
            %                   reside.
            %                   If empty,... [option not yet available]
            %                   Default is [].
            %            'FileLoad' - How to load the file:
            %                   'load' - will use the load function to read
            %                       a ascii/mat file containing two columns
            %                       [Counts(ADU), Linearity]
            % Output : - The linarity two column table:
            %            [Counts(ADU), Linearity]
            %            Also populating the .Linearity property in the
            %            CalibImages object.
            % Author : Eran Ofek (Jan 2023)

            arguments
                Obj(1,1)
                Name            = [];
                Args.PathBase   = [];
                Args.FileLoad   = 'load';
            end
            
            
            if ~isempty(Name)
                if isempty(Args.PathBase)
                    % search for config file in some directiry tree
                    PWD = pwd;
                    if ~isempty(Args.PathBase)
                        cd(Args.PathBase);
                    end
                    
                    DirF = io.files.rdir(Name);
                    Nf = numel(DirF);
                    switch Nf
                        case 1
                            File = fullname(DirF.folder,DirF.name);
                        case 0
                            error('File Name template %s was not found in path %s',Name, Args.PathBase);
                        otherwise
                            error('More than one file Name template %s was not found in path %s',Name, Args.PathBase);
                    end
    
                    switch Args.FileLoad
                        case 'load'
                            Result = io.files.load2(File);   % [ADU, corr-factor] 
                        otherwise
                            error('FileLoad option %s is not supported',Args.FileLoad);
                    end
                    Obj.Linearity = Result;  % [ADU, corr-factor] 
    
                    cd(PWD);
                else
                    error('Empty PathBase option is not yet available')
                end
            else
                % do nothing - do not populate Linearity (Name is empty)
                Result = Obj;
            end

%             if ischar(Name)
%                 Obj(1).Config.Data.CameraConfig.Linearity.(Name)
%             end
                
        end
        
        function [Result,Flag]=exist(Obj, Type, Prop)
            % Check if Bias/Dark/Flat/... image is populated in a CalibImages object
            % Input  : - A CalibImages object.
            %          - Type: 'Bias', 'Dark', 'Flat'. Default is 'Bias'.
            %          - Properties to check. Default is {'Image','Mask'}.
            % Output : - A logical indicating if images of all properties
            %            are non empty.
            %          - A vector of logical indicating if each one of the
            %            properties is non empty.
            % Author : Eran Ofek (Apr 2023)
            % Example: CI.exist('Dark')

            arguments
                Obj
                Type  = 'Bias';
                Prop  = {'Image','Mask'};
            end


            if ischar(Prop)
                Prop = {Prop};
            end
            Nprop = numel(Prop);

            Flag = false(Nprop,1);
            if isempty(Obj.(Type))
                %Result = false;
            else
                for Iprop=1:1:Nprop
                    [Ny, Nx] = sizeImage(Obj.(Type), Prop{Iprop});
                    if Ny>0 && Nx>0
                        Flag(Iprop) = true;
                    end
                end
            end

            Result = all(Flag);

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
            %            'Convert2single' - A logical indicating if to
            %                   convert the image to single precsion before
            %                   processing (needed if images are stored in
            %                   e.g., uint16). Default is false.
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
                Args.Convert2single logical   = false;
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
            
            if Args.Convert2single
                ImObj.cast('single');
            end
            [BiasImage, ~, ~] = imProc.dark.bias(ImObj, Args.BiasArgs{:});
            
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
            %            'Convert2single' - A logical indicating if to
            %                   convert the image to single precsion before
            %                   processing (needed if images are stored in
            %                   e.g., uint16). Default is false.
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
                Args.Convert2single logical       = false;
            end
            
            if isa(ImObj,'AstroImage')
                % do nothing
            else
                List  = io.files.filelist(ImObj);
                if isempty(List)
                    error('No flat images were supplied');
                end
                ImObj = AstroImage(List);
            end
            

            if Args.Convert2single
                ImObj.cast('single');
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
            % Input  : - A CalibImages object, a cell array of images, or 
            %            a string of image names template.
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
                Image
                Args.CreateNewObj logical     = false;   % refers to the Image and not the Obj!!!
                Args.debiasArgs cell          = {};
            end
            
            if isa(Image, 'AstroImage')
                % create new copy of Image object
                %[Result] = createNewObj(Image, Args.CreateNewObj, nargout);
                if Args.CreateNewObj
                    Result = Image.copy;
                else
                    Result = Image;
                end
            else
                if iscell(Image)
                    % Input is cell
                    Image = AstroImage(Image);
                elseif ischar(Image)
                    % Input is char array
                    List  = io.files.filelist(Image);
                    Image = AstroImage(List);
                else
                    error('Second input must be an AstroImage, a cell array of images, or a string of image names template');
                end
                Result = Image;
            end
            
            [Nobj, ~] = Obj.checkObjImageSize(Image);
                        
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                % Note taht CreateNewObj was already done (if needed)
                if isemptyImage(Obj(Iobj).Bias)
                    error('Bias image is empty');
                end                
                % subtract bias/dark by groups
                if isempty(Obj(Iobj).DarkGroupsKey)
                    IndBias = 1;
                else
                    
                    DarkKeyVal  = Obj(Iobj).DarkGroupsVal;
                    DarkKeyVal  = cellfun(@cell2mat, DarkKeyVal, 'UniformOutput',false);
                    DarkKeyVal  = cell2mat(DarkKeyVal.');
                    
                    ImageKeyVal = Result(Iim).HeaderData.getCellKey(Obj(Iobj).DarkGroupsKey);
                    ImageKeyVal = cell2mat(ImageKeyVal);
                    % index of bias image to use
                    IndBias     = find(all(ImageKeyVal == DarkKeyVal, 2), 1);
                end
                Result(Iim) = imProc.dark.debias(Result(Iim), Obj(Iobj).Bias(IndBias), 'CreateNewObj',false, Args.debiasArgs{:});
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
            %            'TrimOverScan' - A logical indicating if to crop
            %                   out the overscan region. Default is false.
            %            'FinalCrop' - Either a header keyword, or a [Xmin Xmax Ymin Ymax]
            %                   containing the final image to keep.
            %                   If empty, then will attempt to estimate using:
            %                   imUtil.ccdsec.remove_edge_section.
            %                   Default is [].
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
                Args.TrimOverScan logical    = false;
                Args.FinalCrop               = [];
                Args.OverScanDir             = [];
                Args.Method                  = 'globalmedian';
                Args.MethodArgs              = {};
                Args.TrimDataProp            = {'Image','Mask'};
            end
            
            % create new copy of Image object
            if Args.CreateNewObj
                Result = Image.copy;
            else
                Result = Image;
            end
           
            [Nobj, ~] = Obj.checkObjImageSize(Image);
                        
            for Iim=1:1:Nobj
                % FFU: Iobj = min(Iim, Nobj);
                % Note taht CreateNewObj was already done (if needed)
                Result(Iim) = imProc.dark.overscan(Result(Iim), 'CreateNewObj',false,...
                                                                'Subtract',true,...
                                                                'TrimOverScan',false,...
                                                                'OverScan',Args.OverScan,...
                                                                'FinalCrop',Args.FinalCrop,...
                                                                'OverScanDir',Args.OverScanDir,...
                                                                'TrimDataProp',Args.TrimDataProp,...
                                                                'Method',Args.Method,...
                                                                'MethodArgs',Args.MethodArgs);
            end
        end
        
        function Result = deflatOneFilt(Obj, Image, Args)
            % Divide multiple flats from multiple images assuming a single filter
            %   The division is one to one or one to many
            % Input  : - A CalibImages object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'deflatArgs' - A cell array of additional
            %                   arguments to pass to imProc.dark.deflat
            %            'CreateNewObj' - Create new Image object.
            %                   Default is false.
            % Output : - Flat divided AstroImage.
            % Author : Eran Ofek (Dec 2021)
            % Example: 
            
            arguments
                Obj
                Image AstroImage
                Args.deflatArgs cell            = {};
                Args.CreateNewObj logical       = false;
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
                if isemptyImage(Obj(Iobj).Flat)
                    error('Flat image is empty');
                end
                Result(Iim) = imProc.flat.deflat(Result(Iim), Obj(Iobj).Flat, 'CreateNewObj',false, Args.deflatArgs{:});
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
            
            %[Nobj, Nim] = Obj.checkObjImageSize(Image);
                        
            % search for filter name [in first image only]
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
            %   Nonlinearity correction
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
            %           'CreateNewObj' - false, true. Default is false.
            %           'BitDictinaryName' - Bit dictionary name.
            %                   Default is 'BitMask.Image.Default'.
            %           'SingleFilter' - A logical indicating if the
            %                   provided images were taken using a single filter.
            %                   If true then will use deflatOneFilt,
            %                   otherwaise will use deflat.
            %                   Default is false.
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
            %           'FinalCrop' - Either a header keyword, or a [Xmin Xmax Ymin Ymax]
            %                   containing the final image to keep.
            %                   If empty, then will attempt to estimate using:
            %                   imUtil.ccdsec.remove_edge_section.
            %                   Default is [].
            %           'MethodOverScan' - see imProc.dark.overscan.
            %                   Default is 'globalmedian'.
            %           'overscanArgs' - A cell array of additional
            %                   arguments to pass to imProc.dark.overscan
            %                   Default is {}.
            %           'trimOverscanArgs' - A cell array of additional
            %                   arguments to pass to imProc.dark.trimOverscan
            %                   Default is {}.
            %           'NonLinCorr' - A correction table [Flux, CorrectionFactor]
            %                   or a structure with .Flux and .Corr fields.
            %                   The correction factor is either multiplicative or by
            %                   division (see 'Operator' argument in imProc.calib.nonlinearCorrection).
            %                   Default is @rdivide.
            %                   If empty, will attempt to read the table
            %                   from the Linearity property of the
            %                   CalibImages object.
            %                   If not exist, the will skip this step.
            %                   Default is [].
            %           'NonLinCorrArgs' - A cell array of additional
            %                   arguments to pass to
            %                   imProc.calib.nonlinearCorrection.
            %                   Default is {}.
            %           'deflatArgs' - A cell array of additional
            %                   arguments to pass to imProc.flat.deflat.
            %                   Default is {}.
            %           'CorrectFringing' - A logical indicating if to
            %                   correct fringing. Default is false.
            %           'MultiplyByGain' - A logical indicating if to
            %                   multiply image values by gain.
            %                   Default is true.
            %           'BitNameNaN' - The bit name for NaN pixels.
            %                   If empty, then will not set the bit mask.
            %                   Default is 'NaN'.
            %           'BitNameNegative' - Name of Negative bit-mask.
            %                   Default is 'Negative'.
            %           'SetNegativeTo0' - A logical indicating if to set
            %                   negative pixels to zeros. Default is true.
            %           'InterpolateOberBadPix' - A logical indicating
            %                   if to interpolate over bad pixels.
            %                   Default is true.
            %           'BitNameBadPix' - A cell array of bad pixels over
            %                   which to interpolate.
            %                   Default is {'Saturated','NaN'}.
            %           'BitName_Interpolated' - Bit name for interpolated
            %                   pixels. Default is 'Interpolated'.
            %           'interpOverNanArgs' - A cell array of additional
            %                   arguments to pass to imProc.image.interpOverNan.
            %                   Default is {}.
            % Output : - The output AstroImage after calibration.
            % Author : Eran Ofek (Oct 2021)
            % Example:
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj logical           = false;   % refers to the Image and not the Obj!!!
                
                % bit dictionary
                Args.BitDictinaryName               = 'BitMask.Image.Default';
                
                Args.SingleFilter logical           = false;
                Args.MaskSaturated logical          = true;
                Args.maskSaturatedArgs cell         = {};
                Args.debiasArgs cell                = {};
                Args.SubtractOverscan logical       = true;
                Args.OverScan                       = 'OVERSCAN';
                Args.FinalCrop                      = [];
                Args.MethodOverScan                 = 'globalmedian';
                Args.overscanArgs cell              = {};
                Args.trimOverscanArgs cell          = {};
                Args.NonLinCorr                     = [];   % nonlinear correction table [flux, factor]
                Args.NonLinCorrArgs cell            = {};   % args for imProc.calib.nonlinearCorrection
                Args.deflatArgs cell                = {};
                Args.CorrectFringing logical        = false;
                Args.MultiplyByGain logical         = true;
                Args.BitNameNaN                     = 'NaN';
                Args.BitNameNegative                = 'Negative';
                Args.SetNegativeTo0 logical         = true;
                Args.InterpolateOverBadPix logical  = true;
                Args.BitNameBadPix                  = {'Saturated','NaN', 'Negative'};
                Args.BitNameInterpolated            = 'Interpolated';
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
                  
            Result.createMask;
            
            for Iim=1:1:Nobj
                % FFU: Iobj = min(Iim, Nobj);
                
                % mark satuarted pixels
                if Args.MaskSaturated
                    Result(Iim) = imProc.mask.maskSaturated(Result(Iim), Args.maskSaturatedArgs{:},...
                                                                     'CreateNewObj',false,...
                                                                     'DefBitDict', BitDictionary(Args.BitDictinaryName) );
                end
            end
                        
            % subtract bias by groups
            Result = Obj.debias(Result, 'debiasArgs',Args.debiasArgs, 'CreateNewObj',false');
            
            % FFU: dark
                
            % measure and subtract overscan
            if Args.SubtractOverscan
                %Result = Obj.overscan(Result, 'OverScan',Args.OverScan, 'Method',Args.MethodOverScan, 'FinalCrop',Args.FinalCrop, 'CreateNewObj',false');
                Result = imProc.dark.overscan(Result, 'OverScan',Args.OverScan, 'Method',Args.MethodOverScan, 'FinalCrop',Args.FinalCrop, 'TrimOverScan',false, 'CreateNewObj',false', Args.overscanArgs{:});
            end
                
            if isempty(Args.NonLinCorr)
                Args.NonLinCorr = Obj.Linearity;
            end
            % apply non-linear correction
            Result = imProc.calib.nonlinearityCorrection(Result, Args.NonLinCorr, Args.NonLinCorrArgs{:}, 'CreateNewObj',false);
                        
            % The overscan may change the size of the image.
            % Depands on how the flat was created, the new image size may
            % not be compatible with the flat size
                        
            % divide by flat
            if Args.SingleFilter
                Result = Obj.deflatOneFilt(Result, 'deflatArgs',Args.deflatArgs, 'CreateNewObj',false');
            else
                Result = Obj.deflat(Result, 'deflatArgs',Args.deflatArgs, 'CreateNewObj',false');
            end
            
            % Fring correction
            if Args.CorrectFringing
                error('Correct fringing is not available yet');
                % Use fring image Args.Fringe(Iobj)
            end
                
            % multipply image by gain
            if Args.MultiplyByGain
                imProc.calib.gainCorrect(Result, 'CreateNewObj',false);
            end
            
            
            % Set the MaskImage for NaN pixels
            for Iim=1:1:Nim
                FlagNaN              = isnan(Result(Iim).Image);
                Result(Iim).MaskData = maskSet(Result(Iim).MaskData, FlagNaN, Args.BitNameNaN, 1);
                
                FlagNeg              = Result(Iim).Image<0;
                Result(Iim).MaskData = maskSet(Result(Iim).MaskData, FlagNeg, Args.BitNameNegative, 1);
                
                if Args.SetNegativeTo0
                    Result(Iim).Image(FlagNeg) = 0;
                end
            end

            % trim overscan region
            Result = imProc.dark.trimOverscan(Result, 'OverScan',Args.OverScan,...
                                                      'FinalCrop',Args.FinalCrop,...
                                                      Args.trimOverscanArgs{:},...
                                                      'CreateNewObj',false);
          


            % interpolate over saturated pixels
            if Args.InterpolateOverBadPix
                % find saturated pixels and interpolate over
                
                Result = imProc.mask.interpOverMaskedPix(Result, 'BitNamesToInterp',Args.BitNameBadPix,...
                                                                 'interpOverNanArgs', Args.interpOverNanArgs,...
                                                                 'BitNameInterpolated',Args.BitNameInterpolated,...
                                                                 'CreateNewObj',false);
            end
        end
    end
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for CalibImages class

    end

end
