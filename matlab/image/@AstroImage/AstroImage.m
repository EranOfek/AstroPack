% Astronomical images container class
%       This class provides a container for images and images meta data, as
%       well as basic functionality for image manipulation.
% Properties (Dependent):
%       Image
%       Back
%       Var
%       Mask
%       Header
%       Key
%       Cat
%       PSFf
% Properties:
%       ImageData
%       BackData
%       VarData
%       MaskData
%       HeaderData
%       CatData
%       PSFData
%       WCS
%       PropagateErr
% Functionality:
%       AstroImage - Constructor and image reader for AstroImage class
%       isemptyImage - Check if data images in AstroImage object are empty
%       sizeImage - Return the size of images in AstroImage object.
%       astroImage2ImageComponent - Convert an AstroImage data into SciImage, BackImage, etc. objects.
%       astroImage2AstroCatalog - Convert the CataData in AstroImage object into an AstroCatalog object array.
%       cast - Cast the image/back/var data in AstroImage (transform to a new type)
%       funCat - Apply function of Cat properties in AstroImage array.
%       funHeader - Apply function of HeaderData properties in AstroImage array.
%       funHeaderScalar - Apply function that return a scalar on HeaderData properties in AstroImage array
%       getStructKey - Get multiple keys from headers in multiple AstroImage and store in a structure array
%       funWCS - Apply function of WCS properties in AstroImage array
%       funPSF - Apply function of PSF properties in AstroImage array
%       maskSet - Set the value of a bit in a bit mask (Maskdata) in AstroImage
%       isImType - Check if header IMTYPE keyword value equal some type
%       julday - Return the Julian day for AstroImage object
%       funUnary - Apply an unary function on AstroImage object.
%       funUnaryScalar - Apply a unary operator that return scalar on AstroImage and return an numeric array
%       funBinaryProp - Apply binary function on a single property of AstroImage
%       funBinaryImVar - Apply a binary operator with error propagation to the
%       funBinary - Apply a binary operator to AstroImage
%       crop - crop an AstroImage images and catalogs and update WCS
%       object2array - Convert an AstroImage object that contains scalars into an array
%       plus - Apply the plus operator between AstroImage objects.
%       minus - Apply the minus operator between AstroImage objects.
%       times - Apply the times operator between AstroImage objects.
%       rdivide - Apply the rdivide operator between AstroImage objects.
%       conv - Convolve images with their PSF, or another PSF
%       filter - Filter images with their PSF, or another PSF
%       fft - fft2 on all images
%
% Functionality (Static):
%       imageIO2AstroImage - Convert an ImageIO object into an AstroImage object
%       readImages2AstroImage - Create AstroImage object and read images into a specific property.
%       help - show mlx manual
%       unitTest - unitTest for AstroImage
%
%
% #functions (autogen)
% AstroImage - Constructor and image reader for AstroImage class
% astroImage2AstroCatalog - Convert the CataData in AstroImage object into an AstroCatalog object array.
% astroImage2ImageComponent - Convert an AstroImage data into SciImage, BackImage, etc. objects.
% cast - Cast the image/back/var data in AstroImage (transform to a new type)
% conv - Convolve images with their PSF, or another PSF
% copyElement - Custom copy of object properties Called from copy() of matlab.mixin.Copyable decendents
% crop - crop an AstroImage images and catalogs and update WCS
% filter - Filter images with their PSF, or another PSF
% funBinary - Apply a binary operator to AstroImage
% funBinaryImVar - Apply a binary operator with error propagation to the ImageData and VarData in an AstroImage object.
% funBinaryProp - Apply binary function on a single property of AstroImage without error propagation.
% funCat - Apply function of Cat properties in AstroImage array This function doesn't create a new object
% funHeader - Apply function of HeaderData properties in AstroImage array This function doesn't create a new object
% funHeaderScalar - Apply function that return a scalae on HeaderData properties in AstroImage array
% funPSF - Apply function of PSF properties in AstroImage array
% funUnary - Apply an unary function on AstroImage object. This include applying the function  on specific data fields, and or image sections (CCDSEC), and error propagation. Note that error propgation is activated using the
% funUnaryScalar - Apply a unary operator that return scalar on AstroImage and return an numeric array
% funWCS - Apply function of WCS properties in AstroImage array
% get.Back - getter for BackImage
% get.Header - getter for Header
% get.Image - getter for Image - get image from ImageData property
% get.Key - getter for Header keys
% get.Mask - getter for MaskImage
% get.Var - getter for VarImage
% get.WCS - getter for WCS if empty, attempt to create from header
% getStructKey - Get multiple  keys from headers in multiple AstroImage and store in a structure array The keyword search can be exact (UseDict=false), or using a keywords dictionary (UseDict=true).
% imageIO2AstroImage - Convert an ImageIO object into an AstroImage object
% isImType - Check if header IMTYPE keyword value equal some type
% isemptyImage - Check if data images in AstroImage object are empty
% julday - Return the Julian day for AstroImage object
% maskSet - Set the value of a bit in a bit mask (Maskdata) in AstroImage
% minus - Apply the minus operator between AstroImage objects. This function utilize the funBinary method. See funBinary for details and additional arguments. Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)}); R = AI - AI
% object2array - Convert an AstroImage object that contains scalars into an array
% plus - Apply the plus operator between AstroImage objects. This function utilize the funBinary method. See funBinary for details and additional arguments. Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)}); R = AI + AI
% prepOperand2 - Prepare the 2nd operand for binary operation
% propagateWCS - Given An AstroImage with WCS property, propagate it to the header and catalog
% rdivide - Apply the rdivide operator between AstroImage objects. This function utilize the funBinary method. See funBinary for details and additional arguments. Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)}); R = AI ./ AI
% readImages2AstroImage - Create AstroImage object and read images into a specific property.
% set.Back - setter for BackImage
% set.Image - setter for Image - store image in ImageData property Obj.(Relations.Image).Image = Data;   can use this instead
% set.Mask - setter for MaskImage
% set.Var - setter for VarImage
% setKeyVal - Replace/insert keyword/value to HeaderData in AstroImage
% sizeImage - Return the size of images in AstroImage object
% times - Apply the times operator between AstroImage objects. This function utilize the funBinary method. See funBinary for details and additional arguments. Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)}); R = AI .* AI
% #/functions (autogen)
%


classdef AstroImage < Component
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent) % Access image data directly
        Image
        Back
        Var
        Mask
        Exp
        Header  % e.g., Header, Header('EXPTIME'), Header({'EXPTIME','IMTYPE'}), Header('IMTYPE',{additional args to keyVal})
        Key
        PSF
        %WCS
    end
    
    properties (SetAccess = public)
        % Data
        %ImageData(1,1) NoisyImage
        
        Table     = [];
        
        ImageData(1,1) SciImage              %= SciImage;
        BackData(1,1) BackImage              %= BackImage;
        VarData(1,1) VarImage                %= VarImage;
        MaskData(1,1) MaskImage              %= MaskImage;
        ExpData(1,1) ExpImage
        
        HeaderData(1,1) AstroHeader          %= AstroHeader;
        CatData(1,1) AstroCatalog            %= AstroCatalog;
        PSFData(1,1) AstroPSF                %= AstroPSF;
        WCS(1,1) AstroWCS
        
    end
    
    properties (Hidden)
        PropagateErr(1,1) logical          = false;
        BackSub logical                    = false;
        
    end
    
    properties (Hidden, Constant)
        % set the relation between the Dependent prop and the data prop
        Relations   = struct('Image','ImageData',...
                             'Back','BackData',...
                             'Var','VarData',...
                             'Mask','MaskData',...
                             'Exp','ExpData');
        
        
    end
    
    methods % Constructor
       
        function Obj = AstroImage(FileNames, Args)
            % Constructor and image reader for AstroImage class
            % Input  : - A file name (with optional wild cards),
            %            or cell of matrices.
            %            Examples: '*.fits' - all fits file in current dir.
            %               '/a/b/c.fits' - NOT SUPPORTED.
            %               {'/a/b/c.fits','a/b/d.fits'} - list of files.
            %          * ...,key,val,...
            %            'HDU' - HDU number. Default is 1.
            %            'Scale' - Image scale. Default is [].
            %            'ReadHeader' - A logical indicating if to read
            %                   header. Default is true.
            %            'CCDSEC' - CCDSEC for image to read [Xmin Xmax
            %                   Ymin Ymax]. If empty, read the entire image.
            %                   Default is [].
            %            'Back' - The same as the file name argument, but
            %                   for a background image. Default is [].
            %            'BackHDU' - The same as HDU, but for the
            %                   background image. Default is [].
            %            'BackScale' - The same as scale, but for the
            %                   background image. Default is [].
            %            'Var' - The same as the file name argument, but
            %                   for a variance image. Default is [].
            %            'VarHDU' - The same as HDU, but for the
            %                   variance image. Default is [].
            %            'VarScale' - The same as scale, but for the
            %                   variance image. Default is [].
            %            'Mask' - The same as the file name argument, but
            %                   for a mask image. Default is [].
            %            'MaskHDU' - The same as HDU, but for the
            %                   mask image. Default is [].
            %            'MaskScale' - The same as scale, but for the
            %                   mask image. Default is [].
            %            'MaskDict' - Mask bit dictionary.
            %                   Default is 'BitMask.Image.Default'.
            %            'Exp' - The same as the file name argument, but
            %                   for a Exp image. Default is [].
            %            'ExpHDU' - The same as HDU, but for the
            %                   Exp image. Default is [].
            %            'ExpScale' - The same as scale, but for the
            %                   Exp image. Default is [].
            %            'FileType' - If empty, use auto detection.
            %                   Default is [].
            %            'UseRegExp' - Ues regexp for file name
            %                   interpretation. Default is false.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Jun 2021)
            % Example:
            %          AI = AstroImage([2 2]);
            %          AI = AstroImage(FileNames,'HDU',1);
            %          AI = AstroImage(FileNames,'HDU',1,'Back',FileNames,'BackHDU',2);
            %          AI = AstroImage(FileNames,'HDU',1,'Back',FileNamesBack,'BackHDU',1);
            %          AI = AstroImage({rand(10,10)},'Back',{rand(5,5)},'BackScale',2,'var',{rand(5,5)},'VarScale',2);
            
            arguments
                FileNames                     = [];
                Args.HDU                      = 1;
                Args.Scale                    = [];
                Args.ReadHeader(1,1) logical  = true;
                Args.CCDSEC                   = [];
                
                Args.Back                     = []; % if empty and BackHDU is not empty, them read from the primary FileNames
                Args.BackHDU                  = [];
                Args.BackScale                = [];
                
                Args.Var                      = [];
                Args.VarHDU                   = [];
                Args.VarScale                 = [];
                
                Args.Mask                     = [];
                Args.MaskHDU                  = [];
                Args.MaskScale                = [];
                Args.MaskDict                 = 'BitMask.Image.Default';
                
                Args.Exp                      = [];
                Args.ExpHDU                   = [];
                Args.ExpScale                 = [];
                
                Args.PSF                      = [];
                Args.PSFHDU                   = [];
                Args.PSFScale                 = [];

                Args.Cat                      = [];
                Args.CatHDU                   = [];
                Args.CatScale                 = [];  % dummy
                
                Args.FileType                 = [];
                Args.UseRegExp(1,1) logical   = false;
                
            end
            
            if isempty(FileNames)
                % create a single elemeny empty object
                % must initilaize all the internal objects
                Obj.ImageData   = SciImage;
                Obj.BackData    = BackImage;
                Obj.VarData     = VarImage;
                Obj.MaskData    = MaskImage;
                Obj.HeaderData  = AstroHeader;
                Obj.CatData     = AstroCatalog;
                Obj.WCS         = AstroWCS;
                Obj.PSFData     = AstroPSF;
                %Obj.WCS                             % FFU: update when WCS class is ready
                
            else
                if isnumeric(FileNames)
                    Nobj = prod(FileNames);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = AstroImage([]);
                    end
                    if prod(FileNames)>0
                        Obj = reshape(Obj, FileNames);
                    else
                        Obj = AstroImage.empty;
                    end
                    
                else
                    if isa(FileNames,'AstroImage')
                        Obj = FileNames;
                    elseif isa(FileNames,'SIM')
                        % convert SIM to AstroImage
                        
                    elseif isa(FileNames,'imCl')
                        % convert imCl to AstroImage
                        
                    else
                        % ImageData
                        if ischar(FileNames)
                            %FN = {FileNames};
                            FN = io.files.filelist(FileNames);
                        else
                            FN = FileNames;
                        end
                        Obj = AstroImage.readImages2AstroImage(FileNames,'HDU',Args.HDU,...
                                                                        'Obj',[],...
                                                                        'CCDSEC',Args.CCDSEC,...
                                                                        'FileType',Args.FileType,...
                                                                        'UseRegExp',Args.UseRegExp,...
                                                                        'Scale',Args.Scale,...
                                                                        'ReadHeader',Args.ReadHeader,...
                                                                        'DataProp','ImageData',...
                                                                        'FileNames',FN);
                                                                        
                        % Other data properties
                        ListProp  = {'Back','Var','Mask', 'Exp', 'PSF','Cat'};
                        ListData  = {'BackData','VarData','MaskData', 'ExpData', 'PSFData','CatData'};
                        ListHDU   = {'BackHDU','VarHDU','MaskHDU', 'ExpHDU', 'PSFHDU','CatHDU'};
                        ListScale = {'BackScale','VarScale','MaskScale', 'ExpScale', 'PSFScale','CatScale'};
                        
                        Nlist = numel(ListProp);
                        for Ilist=1:1:Nlist
                            if ~isempty(Args.(ListHDU{Ilist})) && isempty(Args.(ListProp{Ilist}))
                                % read the Back/Var/... images from the science images
                                % (FileNames), but from a different HDU.
                                Args.(ListProp{Ilist}) = FileNames;
                            end
                            if ~isempty(Args.(ListProp{Ilist}))
                                % do not read header
                                try
                                    Obj = AstroImage.readImages2AstroImage(Args.(ListProp{Ilist}),'HDU',Args.(ListHDU{Ilist}),...
                                                                            'Obj',Obj,...
                                                                            'FileType',Args.FileType,...
                                                                            'UseRegExp',Args.UseRegExp,...
                                                                            'Scale',Args.(ListScale{Ilist}),...
                                                                            'ReadHeader',false,...
                                                                            'DataProp',ListData{Ilist});
                                catch
                                    warning('Fail reading data product %s - likely does not exist in directory', ListProp{Ilist});
                                end
                                
                                % treat integers in case of Mask
                                switch ListProp{Ilist}
                                    case 'Mask'
                                        Nobj = numel(Obj);
                                        for Iobj=1:1:Nobj
                                            Obj(Iobj).MaskData.Dict = BitDictionary(Args.MaskDict);
                                            switch class(Obj(Iobj).(ListProp{Ilist}))
                                                case 'int8'
                                                    Obj(Iobj).(ListProp{Ilist}) = cast(Obj(Iobj).(ListProp{Ilist}), 'uint8');
                                                case 'int16'
                                                    Obj(Iobj).(ListProp{Ilist}) = cast(Obj(Iobj).(ListProp{Ilist}), 'uint16');
                                                case 'int32'
                                                    Obj(Iobj).(ListProp{Ilist}) = cast(Obj(Iobj).(ListProp{Ilist}), 'uint32');
                                                case 'int64'
                                                    Obj(Iobj).(ListProp{Ilist}) = cast(Obj(Iobj).(ListProp{Ilist}), 'uint64');
                                            end
                                        end
                                end
                                        
                            end
                        end
                            
                    end
                    
                end
            end
                            
%             % this is here for testing only
%             Obj.ImageData  = SciImage({300 + 10.*randn(100,100)});
%             Obj.VarData    = VarImage({10.*ones(100,100)});
%             Obj.BackData   = BackImage({300});
%             Obj.HeaderData.Data = {'EXPTIME',1,'';'FILTER','R',''};
%
%             arguments
%                 AnotherObj            = [1 1];
%                 Args.
%             end
            
        end

    end

    methods (Static) % utilities
        
        function Obj = imageIO2AstroImage(ImIO, DataProp, Scale, FileNames, CopyHeader, Obj)
            % Convert an ImageIO object into an AstroImage object
            % Input  : - An ImageIO object.
            %          - data property in which to store the image.
            %          - Scale of the image.
            %          - A cell array of file names to write to the
            %            ImageComponent.FileName property.
            %            Default is {}.
            %          - A logical indicating if to copy the header.
            %            Default is true.
            %          - An AstroImage object in which to put the data.
            %            If empty, create a new object. Default is empty.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage.imageIO2AstroImage(ImIO, 'ImageData', [], true)
       
            arguments
                ImIO
                DataProp
                Scale
                FileNames            = {};
                CopyHeader logical   = true;
                Obj                  = [];
            end
            
            if isempty(Obj)
                Obj  = AstroImage(size(ImIO));
            else
                % use supplied object
                if ~isa(Obj,'AstroImage')
                    error('Obj (argument at position 5) must be an AstroImage object');
                end
            end
            
            Nobj = numel(Obj);
            if Nobj~=numel(ImIO)
                error('Supplied AstroImage object and ImageIO object must have the same size');
            end
            
            for Iobj=1:1:Nobj
                if strcmp(DataProp, 'CatData')
                    Obj(Iobj).(DataProp).Catalog  = ImIO(Iobj).Data;
                    Obj(Iobj).(DataProp).table2array;
                else
                    Obj(Iobj).(DataProp).Data  = ImIO(Iobj).Data;
                    Obj(Iobj).(DataProp).Scale = Scale;
                end
                if ~isempty(FileNames)
                    if iscellstr(FileNames)
                        Obj(Iobj).(DataProp).FileName = FileNames{Iobj};
                    end
                end
                
                if CopyHeader
                    Obj(Iobj).HeaderData.Data = ImIO(Iobj).Header;
                end
            end
        end
        
        function Obj = readImages2AstroImage(FileName, Args)
            % Create AstroImage object and read images into a specific property.
            % Input  : - Either:
            %            [] - will return a single element empty object.
            %            [N, M,...] - will return an empty objects
            %                   which size is [N, M,...].
            %            A table to put in the Data property.
            %            A cell array of matrices to put in the Data
            %                   property.
            %            FileName with or without wild cards or regexp,
            %                   or a cell of file names to read.
            %          * ...,key,val,...
            %            'Obj' - An AstroImage object in which to put the
            %                   data in. If empty create a new object.
            %                   Default is empty.
            %            'HDU' - HDU number or Dataset name from which to
            %                   read the images.
            %            'CCDSEC' - CCDSEC for image to read [Xmin Xmax
            %                   Ymin Ymax]. If empty, read the entire image.
            %                   Default is [].
            %            'FileType' - See ImageIO. Default is [].
            %            'UseRegExp' - See ImageIO. Default is false.
            %            'Scale' - The scale of the image (see definition
            %                   in ImageComponent). Default is [].
            %            'DataProp' - AstroImage data property in wjich to
            %                   store the data. Default is 'ImageData'.
            %            'ReadHeader' - Default is true.
            %            'FileNames' - A cell array of file names to write
            %                   in the ImageComponent.FileName property.
            %                   Default is {}.
            % Outout : - An AstroImage object with the images stored in the
            %            requested field.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage.readImages2AstroImage('*.fits', 'DataProp', 'ImageData');
            %          AI=AstroImage.readImages2AstroImage([]);
            %          AI=AstroImage.readImages2AstroImage([1 2]);
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)});
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)},'DataProp','VarData');
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','ImageData');
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','BackData','Obj',AI);
            %          AI=AstroImage.readImages2AstroImage({rand(5,5), rand(10,10)},'DataProp','VarData','Obj',AI,'Scale',2);
            
            
            arguments
                FileName
                Args.Obj                    = [];
                Args.HDU                    = 1;
                Args.CCDSEC                 = [];
                Args.FileType               = [];
                Args.UseRegExp(1,1) logical = false;
                Args.Scale                  = [];
                Args.DataProp               = 'ImageData';
                Args.ReadHeader             = true;
                Args.FileNames cell         = {};
            end
            
            try
                switch lower(Args.DataProp)
                    case {'imagedata','backdata','vardata','maskdata','psfdata','expdata'}
                        ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                                 'FileType',Args.FileType,...
                                                 'CCDSEC',Args.CCDSEC,...
                                                 'IsTable',false,...
                                                 'ReadHeader',Args.ReadHeader,...
                                                 'UseRegExp',Args.UseRegExp);
                        
                    case {'cat','catdata'}
                        ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                                 'FileType',Args.FileType,...
                                                 'IsTable',true,...
                                                 'ReadHeader',Args.ReadHeader,...
                                                 'UseRegExp',Args.UseRegExp);

                        % if isempty(Args.Obj)
                        %     Obj = AstroImage;
                        % else
                        %     Obj = Args.Obj;
                        % end
                        % Obj.CatData = FITS.readTable1(FileName, 'HDUnum',Args.HDU, 'OutTable','astrocatalog', 'TableType','bintable');
                    otherwise
                        error('DataProp %s is not supported',Args.DataProp);
                end
                Obj = AstroImage.imageIO2AstroImage(ImIO, Args.DataProp, Args.Scale, Args.FileNames, Args.ReadHeader, Args.Obj);
            catch
                if iscell(FileName)
                    Tmp = FileName{1};
                else
                    Tmp = FileName;
                end
                warning('Image %s not found - skip upload',Tmp);
                ImIO = ImageIO; % empty ImageIO
                Obj = AstroImage.imageIO2AstroImage(ImIO, Args.DataProp, Args.Scale, Args.FileNames, Args.ReadHeader, Args.Obj);
                            
            end
            
        end
         
        function Obj = readByCCDSEC(FileName, CCDSEC, Args)
            % Read sections of a single image into multiple AstroImage elements.
            % Input  : - A single file name.
            %          - A 4 column matrix of CCDSEC [Xmin Xmax Ymin Ymax]
            %          * ...,key,val,...
            %            'ReadHeader' - A logical indicating if to read the
            %                   header. Default is 1.
            %            'HDU' - HDU number to read. Default is 1.
            % Output : - An AstroImage array of sub images.
            % AUthor : Eran Ofek (Dec 2021)
            % Example: Obj = AstroImage.readByCCDSEC(FileName, [1 100 1 100;101 200 101 200])
            
            arguments
                FileName char
                CCDSEC
                Args.ReadHeader logical    = true;
                Args.HDU                   = 1;
            end
            
            Nsec = size(CCDSEC,1);
            
            if Args.ReadHeader
                HeadCell = FITS.readHeader1(FileName, Args.HDU);
            else
                HeadCell = cell(0,3);
            end
            
            Obj = AstroImage([Nsec 1]);
            for Isec=1:1:Nsec
                Obj(Isec).Image = FITS.read1(FileName,Args.HDU, 'CCDSEC', CCDSEC(Isec,:));
                Obj(Isec).HeaderData.Data = HeadCell;
            end
            
        end
        
        function LongProp = PropDataTranslation(ShortProp)
            % Translate short propery (e.g., 'Image') to long ('ImageData')
            % Input  : - Short prop.
            % Output : - Long prop.
            % Author : Eran Ofek (May 2022)
            % Example: AstroImage.PropDataTranslation('Back')
            
            switch ShortProp
                case 'Image'
                    LongProp = 'ImageData';
                case 'Back'
                    LongProp = 'BackData';
                case 'Var'
                    LongProp = 'VarData';
                case 'Mask'
                    LongProp = 'MaskData';
                case 'Cat'
                    LongProp = 'CatData';
                otherwise
                    error('Unknown property %s',ShortProp);
            end
        end
        
        function Result = readIP(FileBase, Args)
            % Read image which name obeys the ImagePath standard, along
            % with all its metadata images (e.g., Cat, PSF,...)
            % Input  : - File name base that contains a wild card.
            %            E.g., 'LAST.01.02.03_20220615.205431.830_clear_205-10_020_001_024_sci_proc_*'
            %            All files with this template name will be
            %            searched, and classified according to the product.
            %          * ...,key,val,...
            %            'Path' - Path in which the files reside.
            %                   Default is ''.
            %            'ReadProd' - Products to read:
            %                   Default is
            %                   {'Image','Var','Back','Mask','Exp','PSF','Cat'}.
            %            'HDU' - HDU number of HDF5 dataset name.
            %                   Default is 1.
            %            'FileType' - [] will attempt to identify
            %                   automatically. Otherwise, 'fits' | 'hdf5'.
            %                   Default is [].
            %            'CCDSEC' - CCDSEC for image to read [Xmin Xmax
            %                   Ymin Ymax]. If empty, read the entire image.
            %                   Default is [].
            % Output : - An AstroImage object populated with the loaded
            %            images and metadata.
            % Author : Eran Ofek (Jul 2022)
            % Example: AI = AstroImage.readIP('LAST.01.02.03_20220615.205431.830_clear_205-10_020_001_024_sci_proc_*');
            
            arguments
                FileBase
                Args.Path        = '';
                Args.ReadProd    = {'Image','Var','Back','Mask','Exp','PSF','Cat'};
                Args.HDU         = 1;
                Args.FileType    = [];
                Args.CCDSEC      = [];
            end
            
            Nprod = numel(Args.ReadProd);
            
            FullFileBase = fullfile(Args.Path,FileBase);
            Files        = dir(FullFileBase);
            ListFiles    = {Files.name};
            Nfiles       = numel(Files);
            if Nfiles==0
                Result = [];
                warning('No files found');
            else
                Result = AstroImage;
                
                for Iprod=1:1:Nprod
                    Flag = contains(ListFiles, Args.ReadProd{Iprod});
                    Nfound = sum(Flag);
                    if Nfound>1
                        error('More than one file of product type %s was found', Args.ReadProd{Iprod});
                    else
                        if Nfound>0
                            % file found
                            FileName = ListFiles{Flag};
                            switch Args.ReadProd{Iprod}
                                case 'Cat'
                                    T = ImageIO.read1(FileName, 'HDU',Args.HDU, 'FileType',Args.FileType, 'CCDSEC',Args.CCDSEC, 'IsTable',true);
                                    Result.CatData = AstroCatalog(T);
                                case 'Image'
                                    [Im,Header] = ImageIO.read1(FileName, 'HDU',Args.HDU, 'FileType',Args.FileType, 'CCDSEC',Args.CCDSEC, 'IsTable',false);
                                    Result.(Args.ReadProd{Iprod}) = Im;
                                    Result.HeaderData.Data        = Header;
                                case 'PSF'
                                    [Im,Header] = ImageIO.read1(FileName, 'HDU',Args.HDU, 'FileType',Args.FileType, 'IsTable',false);
                                    Result.PSFData.DataPSF = Im;
                                otherwise
                                    Im = ImageIO.read1(FileName, 'HDU',Args.HDU, 'FileType',Args.FileType, 'CCDSEC',Args.CCDSEC, 'IsTable',false);
                                    Result.(Args.ReadProd{Iprod}) = Im;
                            end
                            
                        end
                    end
                end
            end
        end
        
        function Result = readFileNamesObj(ObjFN, Args)
            % Read the images and products associated with an image contained in a FileNames object into an AstroImage object.
            %   Optionally read not only the image but also additional
            %   products (e.g., 'Cat','PSF').
            % Input  : - A single element FileNames object (that may contain
            %            multiple file names) from which file names can be
            %            generated, or a file name with optional wild cards,
            %            or a cell array of file names.
            %            If the 'AddProduct' is empty, then just read the
            %            specified files into an AstroImage.
            %            However, if the 'AddProduct' is not empty, then in
            %            addition to the specifoed files, will locate the
            %            other data products associated with the image and
            %            upload them into the AstroImage.
            %          * ...,key,val,...
            %            'Path' - A path for the files. If given then will
            %                   override the genPath method.
            %                   Default is [].
            %            'MainProduct' - The main product type to be read
            %                   into the AstroImage Image property.
            %                   Default is 'Image'.
            %            'AddProduct' - A cell array of additional products
            %                   to read. Default is {'Mask','Cat'}.
            %            'PopulateWCS' - Populate the WCS object in the
            %                   AstroImage. Default is true.
            % Output : - An AstroImage object with the images and other
            %            data products.
            % Author : Eran Ofek (Jan 2023)
            % Example: % load the image specified in the file name along
            %          % with the requested data products to an AstroImage
            %          AI=AstroImage.readFileNamesObj('LAST.00.01.01_20220303.224914.224_clear__001_001_001_sci_proc_Image_1.fits');
            %          % the same for a FileNames object
            %          AI=AstroImage.readFileNamesObj(FN); 
            %          % or read only the specified file
            %          AI=AstroImage.readFileNamesObj(FN, 'AddProduct',{}); 
            arguments
                ObjFN
                Args.Path                     = [];
                Args.MainProduct char         = 'Image';
                Args.AddProduct               = {'Mask','Cat','PSF'};        
                Args.PopulateWCS logical      = true;
            end
            
            if ischar(Args.AddProduct)
                Args.AddProduct = {Args.AddProduct};
            end
            
            if isa(ObjFN, 'FileNames')
                % already a FileNames object
            else
                ObjFN = FileNames.generateFromFileName(ObjFN);
            end


            FilesList = ObjFN.genFull('Product',Args.MainProduct, 'FullPath',Args.Path);
        
            Nprod  = numel(Args.AddProduct);
            if Nprod==0
                AI_Args = {};
            end
            for Iprod=1:1:Nprod
                AI_Args{Iprod.*2-1} = Args.AddProduct{Iprod};
                AI_Args{Iprod.*2}   = ObjFN.genFull('Product',Args.AddProduct{Iprod}, 'FullPath',Args.Path);
            end
            
            Result = AstroImage(FilesList, AI_Args{:});
            
            if Args.PopulateWCS
                Result = populateWCS(Result);
            end
        end
    end

 
    methods % Setters/Getters
        function Obj = set.Image(Obj, Data)
            % setter for Image - store image in ImageData property
            %Obj.(Relations.Image).Image = Data;  % can use this instead
            Obj.ImageData.Image = Data;
        end
        
        function Data = get.Table(Obj)
            % Get Catdata.Catalog in table format
            % To update set it to []
           
            if isempty(Obj.Table)
                Data = array2table(Obj.CatData.Catalog);
                Data.Properties.VariableNames = Obj.CatData.ColNames;
                Obj.Table = Data;
            else
                Data = Obj.Table;
            end
        end
        
        function Data = get.Image(Obj)
            % getter for Image - get image from ImageData property
            Data = Obj.ImageData.Image;
        end
        
        function Obj = set.Back(Obj, Data)
            % setter for BackImage
            Obj.BackData.Image = Data;
        end
        
        function Data = get.Back(Obj)
            % getter for BackImage
            Data = Obj.BackData.Image;
        end
        
        function Obj = set.Var(Obj, Data)
            % setter for VarImage
            Obj.VarData.Image = Data;
        end
        
        function Data = get.Var(Obj)
            % getter for VarImage
            Data = Obj.VarData.Image;
        end
        
        function Obj = set.Mask(Obj, Data)
            % setter for MaskImage
            Obj.MaskData.Image = Data;
        end
        
        function Data = get.Mask(Obj)
            % getter for MaskImage
            Data = Obj.MaskData.Image;
        end
        
        function Obj = set.Exp(Obj, Data)
            % setter for ExpImage
            Obj.ExpData.Image = Data;
        end
        
        function Data = get.Exp(Obj)
            % getter for ExpImage
            Data = Obj.ExpData.Image;
        end
        
        function Obj = set.PSF(Obj, Data)
            % setter for PSFData
            Obj.PSFData.Data = Data;
        end

        function Data = get.PSF(Obj)
            % getter for PSFData
            Data = Obj.PSFData.Data;
        end
        
        function Data = get.Header(Obj)
            % getter for Header
            Data = Obj.HeaderData.Data;
        end
        
        function Data = get.Key(Obj)
            % getter for Header keys
            Data = Obj.HeaderData.Key;
        end
        
        function Data = get.WCS(Obj)
            % getter for WCS
            % if empty, attempt to create from header
            
            if strcmp(Obj.WCS.ProjType,'none') && ~isempty(Obj.HeaderData)
                Obj.WCS = AstroWCS.header2wcs(Obj.HeaderData);
            end
            Data = Obj.WCS;
                
        end
       
    end
    
    methods (Static)  % static methods
        
    end
    
    
%     methods % translate Data property names
%         function DataName = translateDataPropName(Obj, DataProp)
%             % translate the Data propert names (e.g., 'Image' -> 'ImageData')
%             % Output : - A cell array of Data property names.;
%             % Example: AI.translateDataPropName('Var')
%             %          AI.translateDataPropName({'Back','Var'})
%
%             if ischar(DataProp)
%                 DataProp = {DataProp};
%             end
%
%             Nprop    = numel(DataProp);
%             DataName = cell(1,Nprop);
%             for Iprop=1:1:Nprop
%                 if isfield(Obj(1).Relations,DataProp{Iprop})
%                     DataName{Iprop} = Obj(1).Relations.(DataProp{Iprop});
%                 else
%                     % do not translate - return as is
%                     DataName{Iprop} = DataProp{Iprop};
%                     %error('Requested DataProp: %s - can not be translated into valid data property name',DataProp{Iprop});
%                 end
%             end
%
%         end
%     end
    
    methods % empty and size
        function varargout = isemptyImage(Obj, Prop)
            % Check if data images in AstroImage object are empty
            % Input  : - An AstroImage object (multi elements supported).
            %          - A cell array of data properties for which to check
            %            if empty.
            %            Default is {'Image','Back','Var','Mask','Exp'}.
            % Output : * One output per requested data property. For each
            %            data property, this is an array of logical
            %            indicating if the data isempty.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage;
            %          [a,b]=AI.isemptyImage({'Image','Back'})
            
            arguments
                Obj
                Prop        = {'Image','Back','Var','Mask','Exp'};
            end
            
            if ischar(Prop)
                Prop = {Prop};
            end
            
            Nprop = numel(Prop);
            Nobj = numel(Obj);
            varargout = cell(1,nargout);
            if nargout>Nprop
                error('Number of requested output (%d) must be equal or smaller then number of reqested data properties (%d)',nargout,Nprop);
            else
                Prop  = Prop(1:nargout);
                Nprop = nargout;
            end
            for Iprop=1:1:Nprop
                varargout{Iprop} = false(size(Obj));
                for Iobj=1:1:Nobj
                    [varargout{Iprop}(Iobj)] = isempty(Obj(Iobj).(Prop{Iprop}));
                end
            end
            
        end
        
        function [Ny, Nx] = sizeImage(Obj, Prop)
            % Return the size of images in AstroImage object
            % Input  : - An AstroImage object (multi elements supported).
            %          - A single propery name (char array)
            % Output : - Number of rows in each AstroImage element.
            %          - Number of columns in each AstroImage element.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage;
            %          [Ny, Nx] = AI.sizeImage
            %          [Ny, Nx] = AI.sizeImage('Back')
            
            arguments
                Obj
                Prop char       = 'Image';
            end
            
            Nobj = numel(Obj);
            Nx   = zeros(size(Obj));
            Ny   = zeros(size(Obj));
            for Iobj=1:1:Nobj
                [Ny(Iobj), Nx(Iobj)] = size(Obj(Iobj).(Prop));
            end
            
        end
        
        function [Nrow,Ncol] = sizeCatalog(Obj)
            % Return the number of sources in the CatData.Catalog property.
            % Input  : - An AstroImage object.
            % Output : - The number of sources in each catalog.
            %          - The number of columns in each catalog.
            % Author : Eran Ofek (May 2022)
            % Example: AI = AstroImage; sizeCatalog(AI)
            
            Nobj = numel(Obj);
            Nrow = nan(size(Obj));
            Ncol = nan(size(Obj));
            for Iobj=1:1:Nobj
                [Nrow(Iobj), Ncol(Iobj)] = size(Obj(Iobj).CatData.Catalog);
            end
                
        end
        
        function Result = isemptyPSF(Obj)
            % Check if PSF in AstroImage is empty
            % Input  : - An AstroImage object.
            % Output : - An array of logicals indicating if PSF is empty in
            %            each image element.
            % Author : Eran Ofek (May 2022)
            % Example: AI=AstroImage; AI.isemptyPSF
           
            Nobj = numel(Obj);
            Result = false(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = isempty(Obj(Iobj).PSFData.Data);
            end
            
        end
    
        function Obj = createMask(Obj, Type)
            % If not exist, create an MaskImage of zeros in an AstroImage object.
            % Input  : - An AstroImage object.
            % Output : - If MaskImage does not exist, then create one with
            %            zeros.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Type   = 'uint32';
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(Obj(Iobj).MaskData.Data)
                    Obj(Iobj).MaskData.Data = zeros(size(Obj(Iobj).Image), Type);
                end
            end

        end
    end
    
    methods (Access=private)  % private functions
        function [Obj2, Obj2IsCell] = prepOperand2(Obj1, Obj2)
            % Prepare the 2nd operand for binary operation
            
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
            % at this stage Obj2 must be a cell, AstroImage or an ImageComponent
            if iscell(Obj2)
                Obj2IsCell = true;
            else
                Obj2IsCell = false;
            end
            if isa(Obj2,'ImageComponent')
                Obj2IsCell = true;
                error('ImageComponent input is not yet supported');
                %Obj2       = convert ImageComponent to cell of images
            end
                
            if ~Obj2IsCell && ~isa(Obj2,'ImageComponent') && ~isa(Obj2,'AstroImage')
                error('Obj2 must be a cell, or AstroImage, or ImageComponent, or a numeric array');
            end
            
        end
    end
    
    
    methods % class conversion
        function varargout = astroImage2ImageComponent(Obj, Args)
            % Convert an AstroImage data into SciImage, BackImage, etc. objects.
            % Input  : - An AstroImage object (multiple elements supported)
            %          * ...,key,val,...
            %            'ReturnImageComponent' - A logical indicating if
            %                   to return an ImageComponent class, or the
            %                   native class of the data object (e.g.,
            %                   SciImage). Default is false.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object. Default is false.
            %                   Note this parameter must be a logical and
            %                   it is independent of nargout.
            %            'DataProp' - A list of Data properties to copy.
            %                   Default is {'ImageData','BackData',
            %                   'VarData', 'MaskData','ExpData'}.
            %                   The output are returned by this order.
            % Output : * An object per requested DataProp.
            %            By default, the first output arg is a SciImage
            %            object containing all the Images, etc.
            % Author : Eran Ofek (Apr 2021)
            % Example: [S,B] = astroImage2ImageComponent(AI)
            %          [S,B] = astroImage2ImageComponent(AstroImage([2 2]))
            %          [S,B] = astroImage2ImageComponent(AI,'CreateNewObj',true)
            %          [S,B] = astroImage2ImageComponent(AI,'CreateNewObj',true,'ReturnImageComponent',true)
            
            arguments
                Obj
                Args.ReturnImageComponent(1,1) logical  = false;
                Args.CreateNewObj(1,1) logical          = false;
                Args.DataProp                           = {'ImageData','BackData', 'VarData', 'MaskData','ExpData'};
            end
            
            
            if ischar(Args.DataProp)
                Args.DataProp = {Args.DataProp};
            end
            
            if nargout>numel(Args.DataProp)
                error('Number of requested ImageComponent is larger than the number of elements in DataProp list');
            end
            
            Nobj = numel(Obj);
            
            varargout = cell(1,nargout);
            for Iout=1:1:nargout
                if Args.ReturnImageComponent
                    OutClass = @ImageComponent;
                else
                    OutClass = str2func(class(Obj(1).(Args.DataProp{Iout})));
                end
                varargout{Iout} = OutClass(size(Obj));
                for Iobj=1:1:Nobj
                    if Args.CreateNewObj
                        varargout{Iout}(Iobj) = Obj(Iobj).(Args.DataProp{Iout}).copy();
                    else
                        varargout{Iout}(Iobj) = Obj(Iobj).(Args.DataProp{Iout});
                    end
                end
            end
             
            
        end
        
        function Result = astroImage2AstroCatalog(Obj, Args)
            % Convert the CatData in AstroImage object into an AstroCatalog object array.
            % Input  : - An AstroImage object (multi components supported).
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object or to provide and handle to
            %                   the existing object. Default is false.
            % Output : - An astroCatalog object.
            %            Note that if CreateNewObj=false then changes in
            %            this object will take place also in the original
            %            AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AC= astroImage2AstroCatalog(AI);
            %          AC= astroImage2AstroCatalog(AI,'CreateNewObj',true);
            
            arguments
                Obj
                Args.CreateNewObj(1,1) logical          = false;
            end
            
            Result = AstroCatalog(size(Obj));
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if Args.CreateNewObj
                    Result(Iobj) = Obj(Iobj).CatData.copy();
                else
                    Result(Iobj) = Obj(Iobj).CatData;
                end
            end
        end

        function Result = astroImage2AstroHeader(Obj, Args)
            % Convert the HeaderData in AstroImage object into an AstroHeader object array.
            % Input  : - An AstroImage object (multi components supported).
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object or to provide and handle to
            %                   the existing object. Default is false.
            % Output : - An AstroHeader object.
            %            Note that if CreateNewObj=false then changes in
            %            this object will take place also in the original
            %            AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AC= astroImage2AstroHeader(AI);
            %          AC= astroImage2AstroHeader(AI,'CreateNewObj',true);
            
            arguments
                Obj
                Args.CreateNewObj(1,1) logical          = false;
            end
            
            Result = AstroHeader(size(Obj));
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if Args.CreateNewObj
                    Result(Iobj) = Obj(Iobj).HeaderData.copy();
                else
                    Result(Iobj) = Obj(Iobj).HeaderData;
                end
            end
        end
    end


    
    
    methods % read / write
        function Status=write1(Obj, Name, DataProp, Args)
            % Write a single data property in a single element AstroImage to file
            % Input  : - A single element AstroImage object.
            %          - File name to write.
            %          - Data property to write. Default is 'Image'.
            %          * ...,key,val,...
            %            'FileType' - Default is 'fits'.
            %            'IsSimpleFITS' - If true, use FITS.writeSimpleFITS
            %                       Default is false.
            %            'WriteHeader' - Default is true.
            %            'Append' - Append in a new HDU.
            %                   Default is false.
            %            'OverWrite' - Default is false.
            %            'WriteTime' - Add the write time to header.
            %                   Default is false.
            %            'Mkdir' - A logical indicating if to create
            %                   directory if file name contains full path.
            %                   Default is false.
            %            'Status' - Status structure to which to append the
            %                   new status.
            % Output : - Status structure with entry per problem.
            % Author : Eran Ofek (Nov 2021)
            % Example: AllSI(1).write1('try1.fits')
            
            arguments
                Obj(1,1)
                Name
                DataProp                      = 'Image';
                Args.FileType                 = 'fits';
                Args.IsSimpleFITS logical     = false;
                Args.WriteHeader logical      = true;
                Args.Append logical           = false;
                Args.OverWrite logical        = false;
                Args.WriteTime logical        = false;
                Args.MkDir logical            = false;
                Args.Status                   = [];
                Args.SanifyPath               = true; 
                Args.FastHeader logical       = false;
                Args.WriteMethod              = 'Standard';
            end
            
            if Args.WriteHeader
                HeaderData = Obj.HeaderData.Data;
            else
                HeaderData = [];
            end
            
            if Args.MkDir
                Path = fileparts(Name);
                if ~isempty(Path)
                    mkdir(Path);
                end
            end


            Istat  = numel(Args.Status);
            Status = Args.Status;
            switch lower(Args.FileType)
                case 'fits'
                    switch DataProp
                        case {'Image','Back','Var','Mask','PSF','Exp'}
                            switch DataProp
                                case 'PSF'
                                    HeaderDataToWrite = [];
                                otherwise
                                    HeaderDataToWrite = HeaderData;
                            end

                            if isempty(Obj.(DataProp))
                                Istat = Istat + 1;
                                Status(Istat).Msg = sprintf('FileName=%s, DataProperty=%s, image is empty - not saved', Name, DataProp);
                            else
                                if Args.IsSimpleFITS                                    
                                    FITS.writeSimpleFITS(Obj.(DataProp), Name, 'Header',HeaderDataToWrite,...
                                                                     'SanifyPath',Args.SanifyPath,...
                                                                     'WriteMethod',Args.WriteMethod); %,...
                                                               %    'DataType',class(Obj.(DataProp)));
                                else                                    
                                    FITS.write(Obj.(DataProp), Name, 'Header',HeaderDataToWrite,...
                                                                   'DataType',class(Obj.(DataProp)),...
                                                                   'Append',Args.Append,...
                                                                   'OverWrite',Args.OverWrite,...
                                                                   'SanifyPath',Args.SanifyPath,...
                                                                   'WriteTime',Args.WriteTime);
                                end
                            end
                        case {'Cat','CatData'}
                            if isempty(Obj.CatData.ColNames)
                                stat = Istat + 1;
                                Status(Istat).Msg = sprintf('FileName=%s, DataProperty=%s, is empty - not saved', Name, 'CatData');
                            else
                                FITS.writeTable1(Obj.CatData, Name, 'Header',HeaderData,...
                                                                   'Append',Args.Append,...
                                                                   'OverWrite',Args.OverWrite,...
                                                                   'WriteTime',Args.WriteTime,...
                                                                   'FastHeader',Args.FastHeader);
                            end
                        otherwise
                            % FFU
                            error('DataProp %s is not yet supported',DataProp);
                    end
                otherwise
                    error('FileType %s is not yet supported',Args.FileType);
            end
            
        end
        
        function Status=write(Obj, Name, DataProp, Args)
            %
            % Example: AI.write(
            
            arguments
                Obj
                Name                 % cell array of images
                DataProp                      = 'Image';
                Args.FileType                 = 'fits';
                Args.WriteHeader logical      = true;
                Args.Append logical           = false;
                Args.OverWrite logical        = false;
                Args.WriteTime logical        = false;
                
               
            end
            
            Istat  = 0;
            Status = [];
            if ischar(Args.DataProp)
                Args.DataProp = {Args.DataProp};
            end
            
            if ischar(Name)
                Name = {Name};
            end
            
            Nobj  = numel(Obj);
            if iscell(Name)
                if isvector(Name)
                    Name = Name(:);
                end
            elseif isa(Name, 'ImagePath')
                % generate file names from ImagePath object
                for Iobj=1:1:Nobj
                    % try to get ImagePath data from Header
                    
                    % This should be in a sperate function
                    
                end
            else
                error('Unknown Name option');
            end
            
            
            Nprop = numel(Args.DataProp);
            [Nname,Nnp] = size(Name);
            if Nobj~=Nname
                error('Number of file names must be equal to number of AstroImage elements');
            end
            if Nnp~=Nprop
                error('Number of properties to write must be equal to the number of columns in the input Name cell');
            end
            
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    switch lower(Args.DataProp{Iprop})
                        case {'image','back','var','mask','exp'}
                            if Args.WriteHeader
                                Header = Obj(Iobj).HeaderData.Data;
                            else
                                Header = [];
                            end
                            
                            % Check that image exist
                            if isempty(Obj(Iobj).(Args.DataProp{Iprop}))
                                Istat = Istat + 1;
                                Status(Istat).Msg = sprintf('FileName=%s, DataProperty=%s, image is empty - not saved',Name{Iobj, Iprop}, Args.DataProp{Iprop});
                            else
                                FITS.write(Obj(Iobj).(Args.DataProp{Iprop}), Name{Iobj, Iprop}, 'Header',Header,...
                                                                                            'DataType',class(Obj(Iobj).(Args.DataProp{Iprop})),...
                                                                                            'Append',Args.Append,...
                                                                                            'OverWrite',Args.OverWrite,...
                                                                                            'WriteTime',Args.WriteTime);
                            end
                        otherwise
                            error('DataProp %s not supported yet',Args.DataProp{Iprop});
                    end
                end
            end
            
        end
        

        
        
    end
    
    methods % functions on specific data properties
        
        function Result = cast(Obj, NewClass, CreateNewObj, DataProp)
            % Cast the image/back/var data in AstroImage (transform to a new type)
            %  Input  : - An AstroImage object.
            %           - A char array containing the new type.
            %             Default is 'single'.
            %           - Indicating if the output
            %             is a new copy of the input (true), or an
            %             handle of the input (false).
            %             Default is false.
            %          - A cell array of data properties which to transform
            %            to the new class. Default is
            %            {'ImageData','BackData','VarData','ExpData'}.;
            % Output : - An ImageComponent object in which the image 'Data'
            %            is transformed into the new type.
            % Author : Eran Ofek (May 2021)
            % Example: AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
            %          Res = cast(AI,'single');
            
            arguments
                Obj
                NewClass               = 'single';
                CreateNewObj logical   = false;
                DataProp cell          = {'ImageData','BackData','VarData','ExpData'};
            end
            
            if CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Nobj  = numel(Obj);
            Nprop = numel(DataProp);
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    % call cast overload of ImageComponent
                    Result(Iobj).(DataProp{Iprop}) = Obj(Iobj).(DataProp{Iprop}).cast(NewClass, CreateNewObj);
                end
            end
                
        end
        
        function Result = funCat(Obj, Fun, varargin)
            % Apply function of Cat properties in AstroImage array
            % This function doesn't create a new object
            % Input  : - AstroImage object
            %          - An AstroCatalog function handle.
            %          * Additional arguments to pass to the function.
            % Output : * If no output argument is specified then this
            %            will modify the input object with the updated Cat.
            %            If output argument is specified then the output will be
            %            written to this output.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({rand(10,10), rand(10,10)});
            %          AI(1).CatData.Catalog=rand(10,2);
            %          AI(2).CatData.Catalog=rand(10,2);
            %          funCat(AI,@sortrows,1);
            
            Nobj = numel(Obj);
            if nargout==0
                Result = Obj;
            end
            for Iobj=1:1:Nobj
                if nargout>0
                    Result = Fun(Obj(Iobj).CatData, varargin{:});
                else
                    Fun(Result(Iobj).CatData, varargin{:});
                end
            end
            
        end
        
        function Result = funHeader(Obj, Fun, varargin)
            % Apply function of HeaderData properties in AstroImage array
            % This function doesn't create a new object
            % Input  : - AstroImage object
            %          - An AstroHeader function handle.
            %          * Additional arguments to pass to the function.
            % Output : * If no output argument is specified then this
            %            will modify the input object with the updated Header.
            %            If output argument is specified then the output will be
            %            written to this output.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({rand(10,10), rand(10,10)});
            %          funHeader(AI,@insertKey,{'GAIN',2,''});
            
            Nobj = numel(Obj);
            if nargout==0
                Result = Obj;
            end
            for Iobj=1:1:Nobj
                if nargout>0
                    Result(Iobj) = Fun(Obj(Iobj).HeaderData, varargin{:});
                else
                    Fun(Result(Iobj).HeaderData, varargin{:});
                end
            end
            
        end
        
        function Result = funHeaderScalar(Obj, Fun, varargin)
            % Apply function that return a scalae on HeaderData properties in AstroImage array
            % Input  : - AstroImage object
            %          - An AstroHeader function handle.
            %          * Additional arguments to pass to the function.
            % Output : - An array in which each element corresponds to the
            %            result of applying the function on a single element
            %            of the AStroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({rand(10,10), rand(10,10)});
            %          funHeaderScalar(AI,@julday)
           
            Nobj = numel(Obj);
            Result = nan(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = Fun(Obj(Iobj).HeaderData, varargin{:});
            end
            
        end
        
        function Result = getStructKey(Obj, Key, varargin)
            % Get multiple  keys from headers in multiple AstroImage and store in a structure array
            %       The keyword search can be exact (UseDict=false), or
            %       using a keywords dictionary (UseDict=true).
            % Input  : - An AstroImage object (multiple elements supported)
            %          - A cell array of keyword names. These are exact
            %            keyword names without a dictionary interpretation.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Output : - A structure array, where number of elements equal
            %            to the number of elements in the AstroImage.
            %            For each requested keyword name there is a
            %            corresponding field name, which value is the
            %            keyword value.
            % Author: Eran Ofek  (Jun 2021)
            % Example: Im = ones(500,500);
            %          AI = AstroImage({poissrnd(Im.*1e3), poissrnd(Im*3e3), poissrnd(Im.*1e4), poissrnd(Im.*1e4), poissrnd(Im.*2e4)});
            %          AI(1).HeaderData.insertKey({'EXPTIME',1}); AI(2).HeaderData.insertKey({'EXPTIME',3}); AI(3).HeaderData.insertKey({'EXPTIME',10});
            %          AI(4).HeaderData.insertKey({'EXPTIME',10}); AI(5).HeaderData.insertKey({'EXPTIME',20});
            %          [Result] = getStructKey(AI, {'EXPTIME'})
            
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj) = getStructKey(Obj(Iobj).HeaderData, Key, varargin{:});
            end
        end
        
        function Obj = setKeyVal(Obj, Key, Val, varargin)
            % Replace/insert keyword/value to HeaderData in AstroImage
            % Input  : - An AstroImage object
            %          - A key name or a cell array of key names.
            %          - A cell array of values, corresponding to the key
            %            names. Alternatively, a vector of numbers corresponding to the
            %            key names.
            %          * ...,key,val,...
            %            see AstroHeader/replaceVal for options.
            % Output : - The input AstroImage with the updated header (this
            %            is a pointer to the original input).
            % Author : Eran Ofek (Jul 2021)
            % Example: AI = AstroImage({rand(10,10)});
            %          AI.setKeyVal('TYPE','science');
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).HeaderData = replaceVal(Obj(Iobj).HeaderData, Key, Val, varargin{:});
            end
            
        end
        
        function Result = isKeyVal(Obj, Key, Val, Args)
            % Check if a multiple keyword value equal to some value.
            % Input  : - An AstroImage object.
            %          - A cell array of keywords to test.
            %          - A cell array of keyword values to test.
            %          * ...,key,val,..
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Output : - A matrix of logicals. Line per AstroImage object,
            %            row per keyword name. The logicals indicating if
            %            the keyword equal the value.
            % Author : Eran Ofek (Sep 2022)
            % Example: Result = isKeyVal(AI,{'EXPTIME','CAMOFFS'},{20,4});
            
            arguments
                Obj
                Key cell
                Val cell
                
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char                                            = 'strcmp';
                Args.Fill                                                       = NaN;
                Args.Val2Num(1,1) logical                                       = true;
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
            end
            
            
            Nkey   = numel(Key);
            Nobj   = numel(Obj);
            Result = nan(Nobj, Nkey);
            for Iobj=1:1:Nobj
                St(Iobj) = Obj(Iobj).HeaderData.getStructKey(Key, 'UseDict',Args.UseDict,...
                                                                  'CaseSens',Args.CaseSens,...
                                                                  'SearchAlgo',Args.SearchAlgo,...
                                                                  'Fill',Args.Fill,...
                                                                  'Val2Num',Args.Val2Num,...
                                                                  'IsInputAlt',Args.IsInputAlt,...
                                                                  'KeyDict',Args.KeyDict);
                
                for Ikey=1:1:Nkey
                    KeyVal = St(Iobj).(Val{Ikey});
                    if ischar(KeyVal)
                        if Args.CaseSens
                            Result(Iobj, Ikey) = strcmp(KeyVal,Val{Ikey});
                        else
                            Result(Iobj, Ikey) = strcmpi(KeyVal,Val{Ikey});
                        end
                    else
                        Result(Iobj, Ikey) = KeyVal == Val{Ikey};
                    end
                end
            end
            
        end
        
        
        function Obj = propagateWCS(Obj, Args)
            % Given An AstroImage with WCS property, propagate it to the header and catalog
            % Input  : - An AstroImage object with populated WCS.
            %          * ...,key,val,...
            %            'OutCatCooUnits' - Units of RA/Dec added to catalog.
            %                   Default is 'deg'.
            %            'OutCatColRA' - RA Column name added to catalog.
            %                   Default is 'RA'.
            %            'OutCatColDec' - Dec Column name added to catalog.
            %                   Default is 'Dec'.
            %            'OutCatColPos' - Position of RA/Dec columns added to catalog.
            %                   Default is Inf.
            %            'OnlyIfSuccess' - Propagate WCS to header and
            %                   catalog only if WCS.Success==true.
            %                   Default is true.
            %            'UpdateCat' - Update catalog. Default is true.
            %            'UpdateHead' - Update header. Default is true.
            % Output - An AstroImage object with the Catalog and Header
            %          updated with the WCS information.
            % Author : Eran Ofek (Sep 2021)
            
            arguments
                Obj
                
                Args.OutCatCooUnits             = 'deg';
                Args.OutCatColRA                = 'RA';
                Args.OutCatColDec               = 'Dec';
                Args.OutCatColPos               = Inf;
                Args.OnlyIfSuccess logical      = true;  % propogate only if Success=true
                Args.UpdateCat logical          = true;
                Args.UpdateHead logical         = true;
            end
            
            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if Args.OnlyIfSuccess
                    Propagate = Obj(Iobj).WCS.Success;
                else
                    Propagate = true;
                end
                        
                if Propagate
                    % update the Obj with the new CatData:
                    if Args.UpdateCat
                        XY = getXY(Obj(Iobj).CatData);
                        if ~isempty(XY)
                            [ObjSrcRA, ObjSrcDec] = xy2sky(Obj(Iobj).WCS, XY(:,1), XY(:,2),...
                                                                          'OutUnits',Args.OutCatCooUnits);
                            Obj(Iobj).CatData = insertCol(Obj(Iobj).CatData, [ObjSrcRA, ObjSrcDec],...
                                                          Args.OutCatColPos, {Args.OutCatColRA, Args.OutCatColDec}, {Args.OutCatCooUnits, Args.OutCatCooUnits});
                        end
                    end

                    if Args.UpdateHead
                        % add WCS kesy to Header
                        Obj(Iobj).HeaderData = wcs2header(Obj(Iobj).WCS, Obj(Iobj).HeaderData);
                    end
                end
            end
           
            
        end
        
        function Obj = populateWCS(Obj, Args)
            % Populate the WCS of an AstroImage based on the header.
            % Input  : - An AstroImage object.
            %          * ...,key,val,...
            %            'header2wcsArgs' - A cell array of additional
            %                   arguments to pass to AstroWCS.header2wcs
            %                   Default is {}.
            % Output : - An AstroImage object in which the WCS property is
            %            populated with a AstroWCS object.
            %            The AstroWCS object is built using the AstroWCS.header2wcs
            %            static function.
            % Author : Eran Ofek (May 2022)
            % Example: AI.populateWCS
            
            arguments
                Obj
                Args.header2wcsArgs cell      = {};
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).WCS = AstroWCS.header2wcs(Obj(Iobj).HeaderData, Args.header2wcsArgs{:});
            end
            
        end
        
        function Result = funWCS(Obj, Fun, ArgsToFun)
            % Apply function of WCS properties in AstroImage array
        end

        function Result = cooImage(Obj, CCDSEC, Args)
            % Return the image center and corners coordinates (from WCS)
            % Input  : - An AstroImage object in which the WCS property is
            %            populated.
            %          - CCDSEC [Xmin, Xmax, Ymin, Ymax]
            %            If empty, will use the image size.
            %            If char, this is an header keyword name from which
            %            to read the CCDSEC.
            %            Default is [].
            %          * ...,key,val,...
            %            'OutUnits' - Output units. Default is 'deg'.
            % Output : - A structure containing:
            %            .Center - [RA, Dec] of center (of CCDSEC).
            %            .Corners - [RA, Dec] of 4 image corners.
            %            .FOV_Radius - max Radius to corners.
            % Author : Eran Ofek (Jan 2023)
            % Example: RR=AI.cooImage([1 1000 1 1000])
            %          RR=AI.cooImage([])
            %          RR=AI.cooImage('CCDSEC')

            arguments
                Obj
                CCDSEC         = [];
                Args.OutUnits  = 'deg';
            end
            Factor = convert.angular(Args.OutUnits, 'rad');

            Nobj = numel(Obj);
            Result = struct('Center',cell(Nobj,1), 'Corners',cell(Nobj,1));
            for Iobj=1:1:Nobj
                if isempty(CCDSEC)
                    % get CCDSEC from image size
                    [Ny, Nx] = Obj(Iobj).sizeImage;
                    CCDSECxy = [1 Nx 1 Ny];
                else
                    if ischar(CCDSEC)
                        % get CCDSEC from header keyword
                        %CCDSEC = eval(Obj(Iobj).HeaderData.getVal(CCDSEC));
                        CCDSECxy = sscanf(Obj(Iobj).HeaderData.getVal('CCDSEC'),'[ %d %d %d %d]');
                    else
                        CCDSECxy = CCDSEC;
                    end
                end
                
                Result(Iobj) = Obj(Iobj).WCS.cooImage(CCDSECxy, 'OutUnits',Args.OutUnits);
                
                % add Radius:
                Result(Iobj).FOV_Radius = max(celestial.coo.sphere_dist_fast(Result(Iobj).Center(1).*Factor, Result(Iobj).Center(2).*Factor, Result(Iobj).Corners(:,1).*Factor, Result(Iobj).Corners(:,2).*Factor))./Factor;
                
            end
            
            
            

        end
        
        function [Result] = isSkyCooInImage(Obj, Alpha, Delta, CCDSEC, Units)
            % Check if RA/Dec are within AstroImage image footprint (CCDSEC)
            % Input  : - A single element AstroWCS object.
            %          - J2000.0 R.A.
            %          - J2000.0 Dec.
            %          - CCDSEC. Either [xmin xmax ymin ymax] or
            %            [xmax, ymax], or a character array containing
            %            header keyword name from which to obtain the
            %            CCDSEC (e.g., 'CCDSEC' | 'ORIGSEC' | 'ORIGUSEC' | 'UNIQSEC'
            %            If empty, use image size. Default is [].
            %          - Input RA/Dec units. Default is 'deg'.
            % Output : - A structure array with results. Element per image.
            %            Each containing the following fields:
            %            .InImage - A vector of logicals indicating, for each
            %                   coordinate, if it is inside CCDSEC footprint.
            %            .MinDist - Vector of minimum distance of each position from
            %                   image boundries. If the image WCS has
            %                   Sucess=false, then this will be NaN.
            % Author : Eran Ofek (Jan 2023)

            arguments
                Obj
                Alpha
                Delta
                CCDSEC   = [];
                Units    = 'deg';
            end
            
            Nobj   = numel(Obj);
            Result = struct('InImage',cell(Nobj,1), 'MinDist',cell(Nobj,1));
            for Iobj=1:1:Nobj
                if Obj(Iobj).WCS.Success
                    if isempty(CCDSEC)
                        % get CCDSEC from image size
                        [Ny, Nx] = Obj(Iobj).sizeImage;
                        CCDSECxy = [1 Nx 1 Ny];
                    end
                    if ischar(CCDSEC)
                        % get CCDSEC from header keyword
                        %CCDSEC = eval(Obj(Iobj).HeaderData.getVal(CCDSEC));
                        CCDSECxy = sscanf(Obj(Iobj).HeaderData.getVal('CCDSEC'),'[ %d %d %d %d]');
                    end
                    if isnumeric(CCDSEC) && ~isempty(CCDSEC)
                        if numel(CCDSEC)<4
                            CCDSECxy=[1, CCDSEC(1), 1 ,CCDSEC(2)];
                        else
                            CCDSECxy=CCDSEC(1:4);
                        end
                    end
                    
                    [Result(Iobj).InImage, Result(Iobj).MinDist] = isSkyCooInImage(Obj(Iobj).WCS, Alpha, Delta, CCDSECxy, Units);
                else
                    Result(Iobj).InImage = false;
                    Result(Iobj).MinDist = NaN;
                end
            end
        end
        
        function Result = funPSF(Obj, Fun, ArgsToFun)
            % Apply function of PSF properties in AstroImage array
        end
        
        function Result = maskSet(Obj, Flag, BitName, SetVal, Args)
            % Set the value of a bit in a bit mask (Maskdata) in AstroImage
            % Input  : - An AsstroImage Object.
            %          - A matrix of logicals, with the same size as the
            %            Image in the MaskData.Image, in which values which are
            %            true will be set.
            %            Alternatively, this can be a vector of indices.
            %          - Bit name, or bit index (start from 0), to set.
            %          - Value to set (0 | 1). Default is 1.
            %          * ...,key,val,...
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   Default is false.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (May 2021)
            % Example: AI = AstroImage({rand(3,3)},'Mask',{uint32(zeros(3,3))})
            %       AI.MaskData.Dict=BitDictionary('BitMask.Image.Default')
            %       Flag = false(3,3); Flag(1,2)=true;
            %       Result = AI.maskSet(Flag,'Saturated')
            %       Result = AI.maskSet(Flag,'Streak')
            
            arguments
                Obj
                Flag                         % matrix of logicals
                BitName                      % name or bit index (start with zero)
                SetVal                 = 1;
                Args.CreateNewObj logical = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
                    
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result.MaskData = maskSet(Result(Iobj).MaskData, Flag, BitName, SetVal, 'CreateNewObj', Args.CreateNewObj);
            end
         
        end
        
        function Result = subtractBackground(Obj, Args)
            % Subtract background from image and set the IsBackSubtracted to true.
            % Input  : - An AstroImage object.
            %          * ...,key,val,...
            %            'ReCalcBack' - Logical. Indicating if to re
            %                   calculate background. If Back property is
            %                   empty, the background will be calculated.
            %                   Default is false.
            %            'backgroundArgs' - A cell array of arguments to
            %                   pass to the imProc.background.background 
            %                   function.
            %                   default is {}.
            %            'SubIfIsBackSubtracted' - Logical. If true, will
            %                   subtract the background even if the
            %                   'IsBackSubtracted' property in the
            %                   ImageComponent (store in 'ImageProp') is
            %                   set to true.
            %                   If false, and IsBackSubtracted=true, then
            %                   the background will not be subtracted.
            %                   Default is true.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is false.
            %            'ImageProp' - Property name of the image from
            %                   which to subtract the background.
            %                   Default is 'Image'.
            %            'BackProp' - Property name of the background image
            %                   to subtract from the image.
            %                   Default is 'Back'.
            % Output : - An AstroImage object, in which the Image data is
            %            background subtracted, and the IsBackSubtracted
            %            property in the ImageComponent is set to true.
            %            Note that if the background was calcaulated, and
            %            createNewObj=true, it will be updated only in the
            %            output object.
            % Author : Eran Ofek (Jun 2023)
            % Example: AI=AI.subtractBackground;
            %          AI=AI.subtractBackground('backgroundArgs',{'SubSizeXY',[]});
            
           
            arguments
                Obj
                Args.ReCalcBack logical             = false;
                Args.backgroundArgs cell            = {};
                Args.SubIfIsBackSubtracted logical  = true;
                Args.CreateNewObj logical           = false;
                Args.ImageProp                      = 'Image';
                Args.BackProp                       = 'Back';
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if Args.SubIfIsBackSubtracted && Obj(Iobj).ImageData.IsBackSubtracted
                    warning('Note that the IsBackSubtracted property in the ImageComponent of AstroImage %d is set to true - subtracting anyhow',Iobj);
                    Sub = true;
                else
                    if Args.Obj(Iobj).ImageData.IsBackSubtracted
                        Sub = false;
                    else
                        Sub = true;
                    end
                end
                
                if Args.ReCalcBack || isempty(Obj(Iobj).(Args.BackProp))
                    % recalc background
                    Result(Iobj) = imProc.background.background(Obj(Iobj), Args.backgroundArgs{:});
                end
                
                if Sub
                    Result(Iobj).Image = Result(Iobj).(Args.ImageProp) - Result(Iobj).(Args.BackProp);
                    Result(Iobj).ImageData.IsBackSubtracted = true;
                end
            
            end
            
        end
        
    end
    
    methods % specific header functions
        function Result = isImType(Obj, ImTypeVal, Args)
            % Check if header IMTYPE keyword value equal some type
            % Input  : - An AstroImage object.
            %          - IMTYPE type to check (e.g., 'bias').
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Output : - An array of logicals (one per AstroImage element)
            %            indicating if the IMTYPE value equal the requested
            %            value.
            % Author : Eran Ofek (Apr 2021)
            % Example: H=AstroImage('*.fits');
            %          Ans = isImType(H, 'bias')
            
            arguments
                Obj
                ImTypeVal
                Args.ImTypeKeyName                                   = 'IMTYPE';
                Args.UseDict(1,1) logical                            = true;
                Args.CaseSens(1,1) logical                           = true;
                Args.SearchAlgo                                      = 'strcmp';
                Args.IsInputAlt(1,1) logical                         = true;
                Args.KeyDict                                         = [];
            end
            
            Nobj   = numel(Obj);
            Result = false(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = isImType(Obj(Iobj).HeaderData, ImTypeVal, 'ImTypeKeyName',Args.ImTypeKeyName,...
                                                                         'UseDict',Args.UseDict,...
                                                                         'CaseSens',Args.CaseSens,...
                                                                         'SearchAlgo',Args.SearchAlgo,...
                                                                         'IsInputAlt',Args.IsInputAlt,...
                                                                         'KeyDict',Args.KeyDict);
            end
            
        end
        
        function [JD, ExpTime] = julday(Obj, varargin)
            % Return the Julian day for AstroImage object
            % Input  : - An AstroImage object
            %          * Arbitrary number of arguments to pass to the
            %          AstroHeader/juday function.
            % Output : - An array of JD (one per AstroImage element).
            %          - An array of ExpTime.
            % Author : Eran Ofek
            % Example: AI=AstroImage('*.fits');
            %          [JD,ET] = julday(AI(2:3));
            
            Nobj = numel(Obj);
            JD      = nan(size(Obj));
            ExpTime = nan(size(Obj));
            for Iobj=1:1:Nobj
                [JD(Iobj), ExpTime(Iobj)] = julday(Obj(Iobj).HeaderData, varargin{:});
            end
        end
    end
    
    methods % basic functionality: funUnary, funUnaryScalar, funBinary, funStack, funTransform
        
        function varargout = getImageVal(Obj, X, Y, Args)
            % Get AstroImage image value at specific positions.
            % Input  : - A single element AstroImage object.
            %          - X coordinates, or indices/flags of image positions
            %            to return.
            %          - Y coordinates. If empty, then assume 'X' is
            %            indices oe flags. Default is [].
            %          * ...,key,val,...
            %            'DataProp' - Cell of image data properties for which to
            %                   return values. Default is 
            %                   {'Image','Back','Var','Mask','Exp'}
            % Output : * Vector of values at requested image positions.
            %            Output argument per each 'DataProp' element.
            %            If pixel position is out of image bounds than
            %            return [].
            % Author : Eran Ofek (May 2021)
            % Example: AI = AstroImage({rand(100,80)});
            %          [V1] = getImageVal(AI,2,2)
           
            arguments
                Obj(1,1)
                X
                Y                = [];
                Args.DataProp    = {'Image','Back','Var','Mask','Exp'};
            end
            
            if ischar(Args.DataProp)
                Args.DataProp = {Args.DataProp};
            end
            
            Nprop   = numel(Args.DataProp);
            if nargout>Nprop
                error('Nuber of requested output arguments is larger than number of DataProp');
            end
                
            [SizeY, SizeX] = Obj.sizeImage;
            
            if isempty(Y)
                Ind = X;
            else
                FlagOut = X<1 | Y<1 | X>SizeX | Y>SizeY;
                XNN = X(~FlagOut);
                YNN = Y(~FlagOut);
                Ind = imUtil.image.sub2ind_fast([SizeY, SizeX], round(YNN), round(XNN));
%                 
%                 if X<1 || Y<1 || X>SizeX || Y>SizeY
%                     Ind = NaN;
%                 else
%                     Ind = imUtil.image.sub2ind_fast([SizeY, SizeX], round(Y), round(X));
%                 end
            end
            varargout = cell(1, nargout);
            for Iarg=1:1:nargout    
                if isempty(Obj.(Args.DataProp{Iarg}))
                    varargout{Iarg} = [];
                elseif numel(Obj.(Args.DataProp{Iarg}))==1
                    varargout{Iarg} = Obj.(Args.DataProp{Iarg})(1);
                else
                    if isnan(Ind)
                        varargout{Iarg} = [];
                    else
                        varargout{Iarg} = Obj.(Args.DataProp{Iarg})(Ind);
                    end
                end
            end
            
        end
        
        function Result = funUnary(Obj, Operator, Args)
            % Apply an unary function on AstroImage object.
            %       This include applying the function  on specific data
            %       fields, and or image sections (CCDSEC), and error
            %       propagation.
            %       Note that error propgation is activated using the
            %       AstroImage.PropagateErr property (default is false).
            % Input  : - An AstroImage object (multi elements supported)
            %          - Operator (function_handle) (e.g., @sin)
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'PropagateErr' - If empty, will use the object
            %                   PropagateErr property. Otherwise will set
            %                   it using the new value. Default is [].
            %            'CreateNewObj' - Logical indicating if the output
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
            %                   Default is true.
            %            'CalcImage' - A logical indicating if to apply the
            %                   operator to the Image field.
            %                   Default is true.
            %            'CalcVar' - A logical indicating if to apply the
            %                   operator to the Var field.
            %                   Default is true.
            %            'CalcBack' - A logical indicating if to apply the
            %                   operator to the Back field.
            %                   Default is true.
            %            'ReturnBack' - A logical indicating if to return
            %                   the background to the image before applying
            %                   the operator. Default is true.
            %            'ReRemoveBack' - A logical indicating if to remove
            %                   the background from the image after
            %                   applying the operator (relevant only if the
            %                   SciImage.IsBackSubtracted=true).
            %                   Default is true.
            %            'UpdateHeader' - A logical indicating if to update
            %                   the header of the output image.
            %                   Default is true.
            %            'AddHistory' - A logical indicating if to add an
            %                   HISTORY line in the header specifying the
            %                   operation. Default is true.
            %            'NewUnits' - If empty do nothing. If provided will
            %                   put this value in the header 'UNITS' key.
            %                   Default is [].
            %            'InsertKeys' - A cell array of 3 columns cell
            %                   array {key,val,comment}, to insert to the
            %                   header. Default is {}.
            %            'ReplaceKeys' - A cell array of keywords to
            %                   replace. Default is {}.
            %            'ReplaceVals' - A cell array of keywords values to
            %                   replace (corresponding top ReplaceKeys).
            %                   Default is {}.
            %            'replaceValArgs' - A cell array of additional
            %                   arguments to pass to the replaceVal function.
            %                   Default is {}.
            %            'insertKeyArgs' - A cell array of additional
            %                   arguments to pass to the insertKey function.
            %                   Default is {}.
            %            'DeleteCat' - A logical indicating if to delete
            %                   the catalog data. Default is false.
            %            'ImCompDataPropIn' - Data property in the
            %                   ImageComponent on which the operator
            %                   will be operated. Default is 'Data'.
            %            'ImCompDataPropOut' - Data property in the
            %                   ImageComponent in which the output
            %                   will be stored. Default is 'Data'.
            % Output : - An AstroImage object with the operator applied on
            %            the data.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({10.*ones(10,10)},'Back',{ones(5,5)},'BackScale',2,'var',{ones(5,5)},'VarScale',2);
            %          B=AI.funUnary(@sin,'CreateNewObj',true)
            %          B=AI.funUnary(@mean,'OpArgs',{'all'})
            %          AI.PropagateErr=true; B=AI.funUnary(@mean,'OpArgs',{'all'})
            %          B=AI.funUnary(@median,'OpArgs',{'all'}); % must result in error
            %          B=AI.funUnary(@median,'OpArgs',{'all'},'PropagateErr',false,'OperateOnVar',true)
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.PropagateErr               = []; % empty, false or true - if not empty, set PropagateErr property.
                Args.OperateOnVar(1,1) logical  = true;  % only if PropagateErr=false
                
                Args.CreateNewObj(1,1) logical  = false;
                Args.CCDSEC                     = [];
                Args.OutOnlyCCDSEC(1,1) logical = true;
                
                Args.CalcImage(1,1) logical     = true;
                Args.CalcVar(1,1) logical       = true;
                Args.CalcBack(1,1) logical      = true;
                Args.ReturnBack(1,1) logical    = true;
                Args.ReRemoveBack(1,1) logical  = true;
                
                Args.UpdateHeader(1,1) logical  = true;
                Args.AddHistory(1,1) logical    = true;
                Args.NewUnits                   = []; % if empty don't change
                Args.InsertKeys                 = {};
                Args.ReplaceKeys                = {};
                Args.ReplaceVals                = {};
                Args.replaceValArgs             = {};
                Args.insertKeyArgs              = {};
               
                Args.DeleteCat(1,1) logical     = false;
                
%                 Args.DataPropIn                 = {'Image','Back','Var'};  % should not operate on Mask
%                 Args.DataPropOut                = {};
                Args.ImCompDataPropIn           = 'Image';   % don't change unless you understand
                Args.ImCompDataPropOut          = 'Image';   % don't change unless you understand
            end
        
            
            if ~isempty(Args.PropagateErr)
                [Obj(1:1:numel(Obj)).PropagateErr] = deal(Args.PropagateErr);
            end
            
%             if isempty(Args.DataPropOut)
%                 Args.DataPropOut = Args.DataPropIn;
%             end
%             % translate all the requested property names to "Data"
%             % properties
%             Args.DataPropIn  = translateDataPropName(Obj(1), Args.DataPropIn);
%             Args.DataPropOut = translateDataPropName(Obj(1), Args.DataPropOut);
            
            
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Nobj  = numel(Obj);
            for Iobj=1:1:Nobj
                
                VarMat = Obj(Iobj).VarData.(Args.ImCompDataPropIn);
                
                % return background to image if needed
                if Args.ReturnBack && Obj(Iobj).ImageData.IsBackSubtracted && ~isempty(Obj(Iobj).BackData.(Args.ImCompDataPropIn))
                    ImageMat = Obj(Iobj).ImageData.(Args.ImCompDataPropIn) + Obj(Iobj).BackData.(Args.ImCompDataPropIn);
                    RetBack  = true;
                else
                    ImageMat = Obj(Iobj).ImageData.(Args.ImCompDataPropIn);
                    RetBack  = false;
                end
                    
               [ResultFun,ResultVar,Flag,FunH] = imUtil.image.fun_unary_withVariance(Operator, ImageMat,...
                                                                                       VarMat,...
                                                                                       'OpArgs',Args.OpArgs,...
                                                                                       'CCDSEC',Args.CCDSEC,...
                                                                                       'OutOnlyCCDSEC',Args.OutOnlyCCDSEC,...
                                                                                       'OperateOnVar',Args.OperateOnVar,...
                                                                                       'PropagateErr',Obj(Iobj).PropagateErr && Args.CalcVar);
                
                if Args.CalcImage
                    if RetBack && Args.ReRemoveBack
                        % remove background from image
                        ResultFun = ResultFun - Obj(Iobj).BackData.(Args.ImCompDataPropIn);
                        Obj(Iobj).ImageData.IsBackSubtracted = true;
                    else
                        Obj(Iobj).ImageData.IsBackSubtracted = false;
                    end
                    Result(Iobj).ImageData.(Args.ImCompDataPropOut) = ResultFun;
                end
                
                if Args.CalcVar
                    Result(Iobj).VarData.(Args.ImCompDataPropOut) = ResultVar;
                end
                if Args.CalcBack && ~isempty(Result(Iobj).Back)
                    % use funUnary of ImageComponent
                    Result(Iobj).BackData = funUnary(Result(Iobj).BackData, Operator, 'OpArgs',Args.OpArgs,...
                                                                                      'CreateNewObj',Args.CreateNewObj,...
                                                                                      'CCDSEC',Args.CCDSEC,...
                                                                                      'OutOnlyCCDSEC',Args.OutOnlyCCDSEC,...
                                                                                      'DataPropIn',Args.ImCompDataPropIn,...
                                                                                      'DataPropOut',Args.ImCompDataPropOut);
                end
                
                % update Header
                Result(Iobj).HeaderData = funUnary(Result(Iobj).HeaderData, Operator, 'OpArgs',Args.OpArgs,...
                                                                                      'UpdateHeader',Args.UpdateHeader,...
                                                                                      'AddHistory',Args.AddHistory,...
                                                                                      'NewUnits',Args.NewUnits,...
                                                                                      'InsertKeys',Args.InsertKeys,...
                                                                                      'ReplaceKeys',Args.ReplaceKeys,...
                                                                                      'ReplaceVals',Args.ReplaceVals,...
                                                                                      'CreateNewObj',false,...
                                                                                      'replaceValArgs',Args.replaceValArgs,...
                                                                                      'insertKeyArgs',Args.insertKeyArgs);
                                                                                  
                
                
                if Args.DeleteCat
                    Obj(Iobj).CatData.deleteCatalog;
                end
                
                % Do not modify: PSF, WCS
                
                
            end
            
        
        end
            
        function varargout = funUnaryScalar(Obj, Operator, Args)
            % Apply a unary operator that return scalar on AstroImage and return an numeric array
            % Input  : - An AstroImage object (multi elements supported)
            %          - Operator (a function handle, e.g., @mean).
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'DataProp' - A cell array of AstroImage data
            %                   properties on which the operator will operated.
            %                   Default is
            %                   {'ImageData','BackData','VarData','MaskData','ExpData'}.
            %            'DataPropIn' - Data property in the ImageComponent
            %                   on which the operator
            %                   will be operated. Default is 'Data'.
            % Output : - An array in which each element corresponds to the operator applied
            %            to an DataProp in the AstroImage object.
            %            If operator returns empty, then this function will
            %            return NaN.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({randn(100,100), randn(100,100)},'Back',{randn(100,100), randn(100,100)});
            %          [A,B] = funUnaryScalar(AI, @mean, 'OpArgs',{'all'})
            %          [A,B] = funUnaryScalar(AI, @std, 'OpArgs',{[],'all'})
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.CCDSEC                     = [];
                Args.DataProp                   = {'ImageData','BackData','VarData','MaskData','ExpData'};
                Args.DataPropIn                 = 'Data';
            end
            
            % Convert AstroImage to (up to 4) ImageComponent objects
            % CellIC is a cell array of ImageComponent objects
            [CellIC{1:1:nargout}] = astroImage2ImageComponent(Obj, 'CreateNewObj',false, 'ReturnImageComponent',false, 'DataProp',Args.DataProp);
            
            Nic = numel(CellIC); % Number of output arguments
            varargout = cell(1,Nic);
            for Iic=1:1:Nic
                Nim = numel(CellIC{Iic}); % number of images in each output arg
                varargout{Iic} = CellIC{Iic}.funUnaryScalar(Operator, 'OpArgs',Args.OpArgs, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn);
            end
        end

        function varargout = funUnaryScalarWithMask(Obj, Operator, Args)
            % Apply a scalar-output function on pixels in AstrooImage selected by their Mask values.
            % % Input  : - An AstroImage object (multi elements supported)
            %          - Operator (a function handle, e.g., @mean).
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'BitNames' - A cell array of bit mask names. The
            %                   operator will be applied only on pixels
            %                   that their bit mask is on (or off).
            %                   For example {'Saturated'} will select
            %                   saturated pixels.
            %                   Default is {}.
            %            'UseNot' - If true, then select pixels specified
            %                   by BitNames that are on. If false, then
            %                   select off pixels. Default is false.
            %            'Method' - 'all'|'any'. When selecting pixels
            %                   using multiple bit names, use all pixels or
            %                   any pixels. Default is 'all'.
            %
            %            'DataProp' - A cell array of AstroImage data
            %                   properties on which the operator will operated.
            %                   Default is
            %                   {'ImageData','BackData','VarData','MaskData','ExpData'}.
            %            'DataPropIn' - Data property in the ImageComponent
            %                   on which the operator
            %                   will be operated. Default is 'Data'.
            % Output : - An array in which each element corresponds to the operator applied
            %            to an DataProp in the AstroImage object.
            %            If operator returns empty, then this function will
            %            return NaN.
            % Author : Eran Ofek (Apr 2021)
            % Example: % mean of saturated pixels
            %          AI.funUnaryScalarWithMask(@mean,{'Saturated'}) 
            %          % mean of non-saturated pixels
            %          AI.funUnaryScalarWithMask(@mean,{'Saturated'},'UseNot',true)
            %          % mean of pixels that are non saturated and non HighRN
            %          AI.funUnaryScalarWithMask(@mean,{'Saturated', 'HighRN'},'UseNot',true)

            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.CCDSEC                     = [];
                Args.BitNames cell              = {};
                Args.UseNot logical             = false;
                Args.Method                     = 'all';
                Args.DataProp                   = {'ImageData','BackData','VarData','MaskData','ExpData'};
                Args.DataPropIn                 = 'Data';
            end

            Ndp  = numel(Args.DataProp);
            if nargout>Ndp
                error('Requested %d output for %d DataProp',Nout,Ndp);
            end
            

            Nobj = numel(Obj);
            for Iout=1:1:nargout
                varargout{Iout} = nan(size(Obj));
            end
            for Iobj=1:1:Nobj
                if isempty(Args.BitNames)
                    for Idp=1:1:nargout
                        varargout{Idp}(Iobj) = Operator(Obj(Iobj).(Args.DataProp{Idp}).(Args.DataPropIn), Args.OpArgs{:});
                    end
                else
                    Flag = findBit(Obj(Iobj).MaskData, Args.BitNames, 'Method',Args.Method, 'OutType','mat');
                    for Idp=1:1:nargout
                        if isempty(Obj(Iobj).(Args.DataProp{Idp}).(Args.DataPropIn))
                            varargout{Idp}(Iobj) = Operator(Obj(Iobj).(Args.DataProp{Idp}).(Args.DataPropIn), Args.OpArgs{:});
                        else
                            if Args.UseNot
                                varargout{Idp}(Iobj) = Operator(Obj(Iobj).(Args.DataProp{Idp}).(Args.DataPropIn)(~Flag), Args.OpArgs{:});
                            else
                                varargout{Idp}(Iobj) = Operator(Obj(Iobj).(Args.DataProp{Idp}).(Args.DataPropIn)(Flag), Args.OpArgs{:});
                            end
                        end
                    end



                end


            end
        end
                
        function Result = funBinaryProp(Obj1, Obj2, Operator, Args)
            % Apply binary function on a single property of AstroImage
            %       without error propagation.
            %       Note that Mask image will be propagated only if the
            %       Mask of the 1st operand is not empty.
            % Input  : - 1st operand - An AstroImage object.
            %          - 2nd operand - An AstroImage object or a
            %            cell array of matrices, or an array of numbers.
            %            If a cell array each element of the cell array
            %            will be treated as the 2nd operand image.
            %            If a vector than this will be treated as a single
            %            image.
            %          - Operator (a function handle). E.g., @plus.
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
            %            'CCDSEC' - The CCDSEC in the output image. Must be
            %                   of the same size as CCDSEC1 and CCDSEC2.
            %            'DataProp' - Data property on which to operate.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - Data property of the ImageComponent
            %                   on which to operate. Default is 'Data'.
            %            'UseOrForMask' - A logical indicating if to use
            %                   the @bitor operator instead of the input operator
            %                   if the requested data property is
            %                   'MaskImage'. Default is true.
            %            'Result' - An AstroImage object in which the
            %                   results will be written. If empty, then use
            %                   the CreateNewObj scheme.
            %                   Default is [].
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage({ones(3,3)});
            %          Res = funBinaryProp(AI,3,@plus);
            %          Res = funBinaryProp(AI,[3 2 1 2],@plus);
            %          Res = funBinaryProp(AI,{2 1},@plus);
            %          Res = funBinaryProp(AI,AI,@minus)
           
            arguments
                Obj1
                Obj2
                Operator function_handle
                Args.OpArgs                    = {};
                Args.DataProp                  = 'ImageData';
                Args.DataPropIn                = 'Data';
                Args.CCDSEC                    = [];
                Args.CCDSEC1                   = [];
                Args.CCDSEC2                   = [];
                Args.UseOrForMask(1,1) logical = true;
                Args.CreateNewObj              = [];
                Args.Result                    = [];
            end
            
            if isempty(Args.Result)
                if isempty(Args.CreateNewObj)
                    if nargout>0
                        Args.CreateNewObj = true;
                    else
                        Args.CreateNewObj = false;
                    end
                end
            
                if Args.CreateNewObj
                    Result = Obj1.copy();
                else
                    Result = Obj1;
                end
            else
                Result = Args.Result;
            end
            
            % convert Obj2 to a cell array of images or keep as AstroImage
            [Obj2, Obj2IsCell] = prepOperand2(Obj1, Obj2);
            
            Nobj1 = numel(Obj1);
            Nobj2 = numel(Obj2);
            Nres = max(Nobj1, Nobj2);
            for Ires=1:1:Nres
                Iobj1 = min(Ires, Nobj1);
                Iobj2 = min(Ires, Nobj2);
                
                if isempty(Args.CCDSEC1)
                    Tmp1 = Obj1(Iobj1).(Args.DataProp).(Args.DataPropIn);
                else
                    Tmp1 = Obj1(Iobj1).(Args.DataProp).(Args.DataPropIn)(Args.CCDSEC1(3):Args.CCDSEC1(4), Args.CCDSEC1(1):Args.CCDSEC1(2));
                end
                
                if isempty(Args.CCDSEC2)
                    if Obj2IsCell
                        Tmp2 = Obj2{Iobj2};
                    else
                        Tmp2 = Obj2(Iobj2).(Args.DataProp).(Args.DataPropIn);
                    end
                else
                    if Obj2IsCell
                        Tmp2 = Obj2{Iobj2}(Args.CCDSEC2(3):Args.CCDSEC2(4), Args.CCDSEC2(1):Args.CCDSEC2(2));
                    else
                        Tmp2 = Obj2(Iobj2).(Args.DataProp).(Args.DataPropIn)(Args.CCDSEC2(3):Args.CCDSEC2(4), Args.CCDSEC2(1):Args.CCDSEC2(2));
                    end
                end
                
                % make sure Tmp1 and Tmp2 are not empty
                if isempty(Tmp1) && ~isempty(Tmp2)
                    Tmp1 = 0;
                end
                if isempty(Tmp2) && ~isempty(Tmp1)
                    Tmp2 = 0;
                end
            
                if ~isempty(Obj1(Iobj1).(Args.DataProp).(Args.DataPropIn))
                    if isa(Obj1(Iobj1).(Args.DataProp),'MaskImage') && Args.UseOrForMask
                        if isempty(Args.CCDSEC)
                            Result(Ires).(Args.DataProp).(Args.DataPropIn) = bitor(Tmp1, Tmp2);
                        else
                            Result(Ires).(Args.DataProp).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)) = bitor(Tmp1, Tmp2);
                        end
                    else
                        if isempty(Args.CCDSEC)
                            Result(Ires).(Args.DataProp).(Args.DataPropIn) = Operator(Tmp1, Tmp2, Args.OpArgs{:});
                        else
                            Result(Ires).(Args.DataProp).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)) = Operator(Tmp1, Tmp2, Args.OpArgs{:});
                        end
                    end
                end
            
            end
            
        end
        
        function Result = funBinaryImVar(Obj1, Obj2, Operator, Args)
            % Apply a binary operator with error propagation to the
            % ImageData and VarData in an AstroImage object.
            % Input  : - 1st operand - An AstroImage object.
            %          - 2nd operand - An AstroImage object or a
            %            cell array of matrices, or an array of numbers.
            %            If a cell array each element of the cell array
            %            will be treated as the 2nd operand image.
            %            If a vector than this will be treated as a single
            %            image.
            %          - Operator (a function handle). E.g., @plus.
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
            %            'CCDSEC' - The CCDSEC in the output image. Must be
            %                   of the same size as CCDSEC1 and CCDSEC2.
            %            'DataPropIn' - Data property of the ImageComponent
            %                   on which to operate. Default is 'Data'.
            %            'Result' - An AstroImage object in which the
            %                   results will be written. If empty, then use
            %                   the CreateNewObj scheme.
            %                   Default is [].
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3)},'Var',{ones(3,3)})
            %          Res = funBinaryImVar(AI,AI,@plus);
            %          Res = funBinaryImVar(AI,AI,@minus);
            %          Res = funBinaryImVar(AI,3,@times);
            
            arguments
                Obj1
                Obj2
                Operator function_handle
                Args.OpArgs                    = {};
                Args.DataPropIn                = 'Data';
                Args.CCDSEC                    = [];
                Args.CCDSEC1                   = [];
                Args.CCDSEC2                   = [];
                Args.CreateNewObj              = [];
                Args.Result                    = [];
            end
            
            DataPropImage  = 'ImageData';
            DataPropVar    = 'VarData';
            
            if isempty(Args.Result)
                if isempty(Args.CreateNewObj)
                    if nargout>0
                        Args.CreateNewObj = true;
                    else
                        Args.CreateNewObj = false;
                    end
                end
            
                if Args.CreateNewObj
                    Result = Obj1.copy();
                else
                    Result = Obj1;
                end
            else
                Result = Args.Result;
            end
            
            % convert Obj2 to a cell array of images or keep as AstroImage
            [Obj2, Obj2IsCell] = prepOperand2(Obj1, Obj2);
            
            Nobj1 = numel(Obj1);
            Nobj2 = numel(Obj2);
            Nres = max(Nobj1, Nobj2);
            for Ires=1:1:Nres
                Iobj1 = min(Ires, Nobj1);
                Iobj2 = min(Ires, Nobj2);
                
                if isempty(Args.CCDSEC1)
                    TmpImage1 = Obj1(Iobj1).(DataPropImage).(Args.DataPropIn);
                    TmpVar1   = Obj1(Iobj1).(DataPropVar).(Args.DataPropIn);
                else
                    TmpImage1 = Obj1(Iobj1).(DataPropImage).(Args.DataPropIn)(Args.CCDSEC1(3):Args.CCDSEC1(4), Args.CCDSEC1(1):Args.CCDSEC1(2));
                    TmpVar1   = Obj1(Iobj1).(DataPropVar).(Args.DataPropIn)(Args.CCDSEC1(3):Args.CCDSEC1(4), Args.CCDSEC1(1):Args.CCDSEC1(2));
                end
                
                if isempty(Args.CCDSEC2)
                    if Obj2IsCell
                        TmpImage2 = Obj2{Iobj2};
                        TmpVar2   = [];
                    else
                        TmpImage2 = Obj2(Iobj2).(DataPropImage).(Args.DataPropIn);
                        TmpVar2   = Obj2(Iobj2).(DataPropVar).(Args.DataPropIn);
                    end
                else
                    if Obj2IsCell
                        TmpImage2 = Obj2{Iobj2}(Args.CCDSEC2(3):Args.CCDSEC2(4), Args.CCDSEC2(1):Args.CCDSEC2(2));
                        TmpVar2   = [];
                    else
                        TmpImage2 = Obj2(Iobj2).(DataPropImage).(Args.DataPropIn)(Args.CCDSEC2(3):Args.CCDSEC2(4), Args.CCDSEC2(1):Args.CCDSEC2(2));
                        TmpVar2   = Obj2(Iobj2).(DataPropVar).(Args.DataPropIn)(Args.CCDSEC2(3):Args.CCDSEC2(4), Args.CCDSEC2(1):Args.CCDSEC2(2));
                    end
                end
                
                if isempty(Args.CCDSEC)
                    [Result(Ires).(DataPropImage).(Args.DataPropIn)  ,Result(Ires).(DataPropVar).(Args.DataPropIn)] = ...
                                imUtil.image.fun_binary_withVariance(Operator, TmpImage1, TmpImage2, TmpVar1, TmpVar2, 0, Args.OpArgs);
                else
                    [Result(Ires).(DataPropImage).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2))  ,...
                     Result(Ires).(DataPropVar).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2))] = ...
                            imUtil.image.fun_binary_withVariance(Operator, TmpImage1, TmpImage2, TmpVar1, TmpVar2, 0, Args.OpArgs);
                end

                
            end
            
        end
                
        function Result = funBinary(Obj1, Obj2, Operator, Args)
            % Apply a binary operator to AstroImage
            % Input  : - 1st operand - An AstroImage object.
            %          - 2nd operand - An AstroImage object or a
            %            cell array of matrices, or an array of numbers.
            %            If a cell array each element of the cell array
            %            will be treated as the 2nd operand image.
            %            If a vector than this will be treated as a single
            %            image.
            %          - Operator (a function handle). E.g., @plus.
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CalcImage' - A logical that state if to apply the
            %                   operator to the ImageData property.
            %                   Default is true.
            %            'CalcBack' - A logical that state if to apply the
            %                   operator to the BackData property.
            %                   Default is true.
            %            'CalcVar' - A logical that state if to apply the
            %                   operator to the VarData property.
            %                   Default is true.
            %            'CalcMask' - A logical that state if to apply the
            %                   operator to the MaskData property.
            %                   Default is true.
            %            'CalcExp' - A logical that state if to apply the
            %                   operator to the ExpData property.
            %                   Default is true.
            %            'CalcPSF' - A logical that state if to apply the
            %                   operator to the PSF property.
            %                   Default is true.
            %            'PropagateErr' - A logical stating if to apply
            %                   error propagation. If empty, then use the
            %                   object PropagateErr property. Default is
            %                   [].
            %                   NOTE THAT the first element state dicatates
            %                   all the rest.
            %            'DeleteCat' - A logical indicating if to delete
            %                   the catalog data. Default is false.
            %            'UpdateHeader' - a logical indicating if to update
            %                   the header. Default is true.
            %            'DataPropIn' - Data property of the ImageComponent
            %                   on which to operate. Default is 'Data'.
            %            'CCDSEC1' - [Xmin Xmax Ymin Ymax] CCDSEC for the
            %                   1st oprand. The Operator will be applied
            %                   only on this section.
            %                   If empty, use all image. Default is [].
            %            'CCDSEC2' - The same as CCDSEC1, but for the 2nd
            %                   operand. Default is [].
            %            'CCDSEC' - The CCDSEC in the output image. Must be
            %                   of the same size as CCDSEC1 and CCDSEC2.
            %            'UseOrForMask' - A logical indicating if to use
            %                   the @bitor operator instead of the input operator
            %                   if the requested data property is
            %                   'MaskImage'. Default is true.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            %            'Result' - An AstroImage object in which the
            %                   results will be written. If empty, then use
            %                   the CreateNewObj scheme.
            %                   Default is [].
            % Output : An AstroImage object
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3)});
            %          Result = funBinary(AI,3,@plus)
            %          AI = AstroImage({3.*ones(3,3)}, 'Back',{ones(3,3)}, 'Var',{3.*ones(3,3)});
            %          Result = funBinary(AI,3,@plus)
            %          Result = funBinary(AI,3,@plus,'PropagateErr',true)
            
            arguments
                Obj1
                Obj2
                Operator function_handle
                Args.OpArgs                    = {};
                Args.CalcImage(1,1) logical    = true;
                Args.CalcBack(1,1) logical     = true;
                Args.CalcVar(1,1) logical      = true;
                Args.CalcMask(1,1) logical     = true;
                Args.CalcExp(1,1) logical      = true;
                Args.CalcPSF(1,1) logical      = false;
                Args.PropagateErr              = [];
                Args.DeleteCat(1,1) logical    = false;
                Args.UpdateHeader(1,1) logical = true;
                Args.DataPropIn                = 'Data';
                Args.CCDSEC                    = [];
                Args.CCDSEC1                   = [];
                Args.CCDSEC2                   = [];
                Args.UseOrForMask(1,1) logical = true;
                Args.CreateNewObj              = [];
                Args.Result                    = [];
            end
            
            if isempty(Args.Result)
                if isempty(Args.CreateNewObj)
                    if nargout>0
                        Args.CreateNewObj = true;
                    else
                        Args.CreateNewObj = false;
                    end
                end
            
                if Args.CreateNewObj
                    Result = Obj1.copy();
                else
                    Result = Obj1;
                end
            else
                Result = Args.Result;
            end
            
            if ~isempty(Args.PropagateErr)
                [Obj1(1:1:numel(Obj1)).PropagateErr] = deal(Args.PropagateErr);
            end
            
            % Use PropagateErr from Obj1(1) only
            if Obj1(1).PropagateErr
                if Args.CalcImage || Args.CalcVar
                    % propagate errors Image/Var
                    Result = funBinaryImVar(Obj1, Obj2, Operator, 'OpArgs',Args.OpArgs,...
                                                                  'DataPropIn',Args.DataPropIn,...
                                                                  'CCDSEC',Args.CCDSEC,...
                                                                  'CCDSEC1',Args.CCDSEC1,...
                                                                  'CCDSEC2',Args.CCDSEC2,...
                                                                  'CreateNewObj',Args.CreateNewObj,...
                                                                  'Result',Result);
                end
            else
                if Args.CalcImage
                    Result = funBinaryProp(Obj1, Obj2, Operator, 'OpArgs',Args.OpArgs,...
                                                                 'DataProp','ImageData',...
                                                                 'DataPropIn',Args.DataPropIn,...
                                                                 'CCDSEC',Args.CCDSEC,...
                                                                 'CCDSEC1',Args.CCDSEC1,...
                                                                 'CCDSEC2',Args.CCDSEC2,...
                                                                 'CreateNewObj',Args.CreateNewObj,...
                                                                 'UseOrForMask',Args.UseOrForMask,...
                                                                 'Result',Result);
                end
                if Args.CalcVar
                    Result = funBinaryProp(Obj1, Obj2, Operator, 'OpArgs',Args.OpArgs,...
                                                                 'DataProp','VarData',...
                                                                 'DataPropIn',Args.DataPropIn,...
                                                                 'CCDSEC',Args.CCDSEC,...
                                                                 'CCDSEC1',Args.CCDSEC1,...
                                                                 'CCDSEC2',Args.CCDSEC2,...
                                                                 'CreateNewObj',Args.CreateNewObj,...
                                                                 'UseOrForMask',Args.UseOrForMask,...
                                                                 'Result',Result);
                end
            end
               
            if Args.CalcBack
                Result = funBinaryProp(Obj1, Obj2, Operator, 'OpArgs',Args.OpArgs,...
                                                                 'DataProp','BackData',...
                                                                 'DataPropIn',Args.DataPropIn,...
                                                                 'CCDSEC',Args.CCDSEC,...
                                                                 'CCDSEC1',Args.CCDSEC1,...
                                                                 'CCDSEC2',Args.CCDSEC2,...
                                                                 'CreateNewObj',Args.CreateNewObj,...
                                                                 'UseOrForMask',Args.UseOrForMask,...
                                                                 'Result',Result);
            end
            if Args.CalcExp
                Result = funBinaryProp(Obj1, Obj2, Operator, 'OpArgs',Args.OpArgs,...
                                                                 'DataProp','ExpData',...
                                                                 'DataPropIn',Args.DataPropIn,...
                                                                 'CCDSEC',Args.CCDSEC,...
                                                                 'CCDSEC1',Args.CCDSEC1,...
                                                                 'CCDSEC2',Args.CCDSEC2,...
                                                                 'CreateNewObj',Args.CreateNewObj,...
                                                                 'UseOrForMask',Args.UseOrForMask,...
                                                                 'Result',Result);
            end
            if Args.CalcMask
                Result = funBinaryProp(Obj1, Obj2, Operator, 'OpArgs',Args.OpArgs,...
                                                                 'DataProp','MaskData',...
                                                                 'DataPropIn',Args.DataPropIn,...
                                                                 'CCDSEC',Args.CCDSEC,...
                                                                 'CCDSEC1',Args.CCDSEC1,...
                                                                 'CCDSEC2',Args.CCDSEC2,...
                                                                 'CreateNewObj',Args.CreateNewObj,...
                                                                 'UseOrForMask',Args.UseOrForMask,...
                                                                 'Result',Result);
            end
            
            
            
            if Args.CalcPSF
                error('PSF funBinary is not implemented yet');
            end
            
            if Args.DeleteCat
                Result.deleteCatalog;
            end
            
            % header
            if Args.UpdateHeader
                Nres = numel(Result);
                for Ires=1:1:Nres
                    funUnary(Result(Ires).HeaderData, Operator, 'OpArgs',Args.OpArgs, 'UpdateHeader',Args.UpdateHeader);
                end
            end
            
            
        end
        
        function Result = crop(Obj, CCDSEC, Args)
            % crop an AstroImage images and catalogs and update WCS
            % Input  : - An AstroImage object.
            %          - A CCDSEC [xmin, xmax, ymin, ymax]
            %            or 'center' [Xcenter, Ycenter, Xhalfsize, Yhalfsize].
            %            If multiple lines then each line corresponding to
            %            an AstroImage element.
            %            The Xmax and Ymax can be replaced with Inf. In
            %            this case , the max number of pix along this
            %            dimension will be used.
            %            If empty, then do not crop.
            %          * ...,key,val,...
            %            'Type' - ['ccdsec'] | 'center'
            %            'DataProp' - A cell array of image data properties
            %                   to crop. Default is {'ImageData','BackData','VarData','MaskData','ExpData'}
            %            'DataPropIn' - Data property on which to operate.
            %                   Default is 'Image'.
            %            'DeleteProp' - A cell array of properties to
            %                   delete. Default is {}.
            %            'UpdateCat' - A logical indicating if to crop
            %                   catalog by XY (using cropXY). Default is true.
            %            'cropXYargs' - A cell array of arguments to pass
            %                   to AstroCatalog/cropXY. Default is {}.
            %            'UpdateHeader' - A logical indicating if to update
            %                   the {'NAXIS1','NAXIS2','CCDSEC','ORIGSEC'}
            %                   header keywords. Default is true.
            %            'UpdateWCS' - A logical indicating if to update
            %                   the WCS. Default is true.
            %            'FillVal' - In case that the trim section is near the edge,
            %                   this is the fill value to insert into the edge, such that the
            %                   trim section will have the requires size. If empty, then
            %                   return only the overlap region.
            %                   Default is [].
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   Default is false.
            % Output : - A cropped AstroImage object.
            % Author : Eran Ofek (Jul 2021)
            % Example: AI = AstroImage({rand(100,100),rand(100,200)},'Back',{rand(100,100),rand(100,200)});
            %          Res = crop(AI,[11 20 11 30])

            arguments
                Obj
                CCDSEC
                Args.Type                      = 'ccdsec'; % 'center' mat results in errors!
                Args.DataProp cell             = {'ImageData','BackData','VarData','MaskData','ExpData'};
                Args.DataPropIn                = 'Image';
                Args.DeleteProp cell           = {};
                Args.UpdateCat(1,1) logical    = true;
                Args.cropXYargs cell           = {};
                Args.UpdateHeader(1,1) logical = true;
                Args.UpdateWCS(1,1) logical    = true;
                Args.FillVal                   = [];
                Args.CreateNewObj logical      = false;
            end

            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            Result.deleteProp(Args.DeleteProp);

            if isempty(CCDSEC)
                % do nothing - no crop
            else
                % crop
                KeyNames = {'NAXIS1','NAXIS2','CCDSEC','ORIGSEC'}; %,'ORIGUSEC','UNIQSEC'};
                KeyVals  = cell(size(KeyNames));

                Nobj  = numel(Obj);
                Nprop = numel(Args.DataProp);
                Nsec  = size(CCDSEC,1);
                for Iobj=1:1:Nobj
                    Isec = min(Iobj, Nsec);
                    % replace Inf with actual size
                    if isinf(CCDSEC(Isec,2))
                        [SizeX, SizeY] = sizeImage(Result(Iobj));
                        CCDSEC(Isec,2) = SizeX;
                    end
                    if isinf(CCDSEC(Isec,4))
                        [SizeX, SizeY] = sizeImage(Result(Iobj).(Args.DataProp{1}));
                        CCDSEC(Isec,4) = SizeY;
                    end
                    
                    for Iprop=1:1:Nprop
                        Result(Iobj).(Args.DataProp{Iprop}) = crop(Result(Iobj).(Args.DataProp{Iprop}), CCDSEC(Isec,:),...
                                                        'Type',Args.Type,...
                                                        'DataPropIn',Args.DataPropIn,...
                                                        'FillVal',Args.FillVal,...
                                                        'CreateNewObj',false);
                    end
                    % make sure CCDSEC is in 'ccdsec' format and not 'center'
                    NewCCDSEC = Result(Iobj).(Args.DataProp{1}).CCDSEC;

                    if Args.UpdateCat
                        Result(Iobj).CatData = cropXY(Result(Iobj).CatData, NewCCDSEC,...
                                                      'CreateNewObj',Args.CreateNewObj,...
                                                      Args.cropXYargs{:});
                    end
                    if Args.UpdateWCS
                        %warning('UpdateWCS in AstroImage/crop is not implemented');
                        if isempty(Args.FillVal)
                            CCDSEC_Min = max(CCDSEC(Isec,[1 3]),1);
                            Result(Iobj).WCS.CRPIX = Result(Iobj).WCS.CRPIX - CCDSEC_Min + [1 1];
                        else
                            Result(Iobj).WCS.CRPIX = Result(Iobj).WCS.CRPIX - CCDSEC(Isec,[1 3]) + [1 1];
                        end
                        Result(Iobj).propagateWCS('UpdateCat',false);
                    end
                    

                    if Args.UpdateHeader
                        SizeIm = size(Result(Iobj).Image);
                        KeyVals{1} = SizeIm(2);  % NAXIS1
                        KeyVals{2} = SizeIm(1);  % NAXIS2
                        KeyVals{3} = imUtil.ccdsec.ccdsec2str([1 SizeIm(2) 1 SizeIm(1)]);  % CCDSEC
                        KeyVals{4} = imUtil.ccdsec.ccdsec2str(NewCCDSEC);   % ORIGSEC
                        Result(Iobj).HeaderData = replaceVal(Result(Iobj).HeaderData, KeyNames, KeyVals);
                    end
                end
            end
        end

        function [Result, Info] = cropLonLat(Obj, RA, Dec, Args)
            % crop an AstroImage images and catalogs by RA/Dec and update WCS
            %   The crop return a stamp which axes are along the X, Y axes.
            % Input  : - An AstroImage object.
            %          - J2000.0 RA for crop cnter. A single coordinate for
            %            all images.
            %          - J2000.0 Dec for crop center.
            %          * ...,key,val,...
            %            'HalfSizeXY' - Half size of stamp in pix, in the X
            %                   and Y axes.
            %                   Default is [50 50].
            %            'CooUnits' - RA/Dec units. Default is 'deg'.
            %            'DataProp' - A cell array of image data properties
            %                   to crop. Default is {'ImageData','BackData','VarData','MaskData','ExpData'}
            %            'DataPropIn' - Data property on which to operate.
            %                   Default is 'Image'.
            %            'DeleteProp' - A cell array of properties to
            %                   delete. Default is {}.
            %            'UpdateCat' - A logical indicating if to crop
            %                   catalog by XY (using cropXY). Default is true.
            %            'cropXYargs' - A cell array of arguments to pass
            %                   to AstroCatalog/cropXY. Default is {}.
            %            'UpdateHeader' - A logical indicating if to update
            %                   the {'NAXIS1','NAXIS2','CCDSEC','ORIGSEC'}
            %                   header keywords. Default is true.
            %            'UpdateWCS' - A logical indicating if to update
            %                   the WCS. Default is true.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   Default is true.
            % Output : - A cropped AstroImage object.
            %          - A structure containing the
            %            .X - X coordinate corresponding to the [RA, Dec].
            %            .Y - Y coordinate corresponding to the [RA, Dec].
            %            .CCDSEC - Selected CCDSEC.
            % Author : Eran Ofek (Jul 2021)
            % Example: 
            %          

            
            arguments
                Obj
                RA                                 % scalar
                Dec                                % scalar
                Args.HalfSizeXY                = [50 50];  % [x y] pix
                Args.CooUnits                  = 'deg';
                Args.DataProp cell             = {'ImageData','BackData','VarData','MaskData','ExpData'};
                Args.DataPropIn                = 'Image';
                Args.DeleteProp                = {};
                Args.UpdateCat(1,1) logical    = true;
                Args.cropXYargs cell           = {};
                Args.UpdateHeader(1,1) logical = true;
                Args.UpdateWCS(1,1) logical    = true;
                
                Args.CreateNewObj logical      = true;
            end
                        
            Nobj   = numel(Obj);
            Result = AstroImage(size(Obj));
            Info.X      = zeros(Nobj,1);
            Info.Y      = zeros(Nobj,1);
            Info.CCDSEC = zeros(Nobj,4);
            for Iobj=1:1:Nobj
                [X, Y] = sky2xy(Obj(Iobj).WCS, RA, Dec, 'InUnits', Args.CooUnits);
                
                CCDSEC = [X - Args.HalfSizeXY(1), X + Args.HalfSizeXY(1), Y - Args.HalfSizeXY(2), Y + Args.HalfSizeXY(2)];
                CCDSEC = ceil(CCDSEC);
                Info.X(Iobj)        = X;
                Info.Y(Iobj)        = Y;
                Info.CCDSEC(Iobj,:) = CCDSEC;
                
                % crop position from AstroImage
                Result(Iobj) = crop(Obj(Iobj), CCDSEC, 'Type','ccdsec',...
                                                       'DataProp',Args.DataProp,...
                                                       'DataPropIn',Args.DataPropIn,...
                                                       'DeleteProp',Args.DeleteProp,...
                                                       'UpdateCat',Args.UpdateCat,...
                                                       'cropXYargs',Args.cropXYargs,...
                                                       'UpdateHeader',Args.UpdateHeader,...
                                                       'UpdateWCS',Args.UpdateWCS,...
                                                       'FillVal',[],...
                                                       'CreateNewObj',Args.CreateNewObj);
            end
        
        end
               
        function varargout = object2array(Obj,DataProp)
            % Convert an AstroImage object that contains scalars into an array
            % Input  : - An AstroImage object.
            %          - A cell array of data properties to collect. Each
            %            property must contains a scalar.
            %            Default is {'Image'}.
            % Output : * Number of output arguments equal to the number of
            %            data properties.
            %            Each output argument is an array which size is
            %            equal to size(Obj), and contains all the scalars
            %            in the corresponding data property.
            % Author : - Eran Ofek (Apr 2021)
            % Example:
            
            arguments
                Obj
                DataProp                    = {'Image','Back','Var'};
            end
            
            if ischar(DataProp)
                DataProp = {DataProp};
            end
            
            % select only the requested data properties
            DataProp = DataProp(1:1:nargout);
            
            Nprop = numel(DataProp);
            Nobj  = numel(Obj);
            
            varargout = cell(1,Nprop);
            for Iprop=1:1:Nprop
                varargout{Iprop} = nan(size(Obj));
                for Iobj=1:1:Nobj
                    varargout{Iprop}(Iobj) = Obj.(DataProp{Iprop});
                end
            end
            
        end
                
        function Obj = deleteProp(Obj, PropList)
            % Clean the content of specific properties of AstroImage
            % Input  : - An AstroImage object.
            %          - A cell array of properties to delete.
            %            Default is {}.
            % Output : - The original AstroImage with the deleted
            %            properties.
            % Author : Eran Ofek (Nov 2021)
            % Example: AI = AstroImage; AI.VarData.Image=1;
            %          AI.deleteProp('VarData');
           
            arguments
                Obj
                PropList     = {};
            end
            
            if ischar(PropList)
                PropList = {PropList};
            end
            Nobj  = numel(Obj);
            Nprop = numel(PropList);
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                   Obj(Iobj).(PropList{Iprop}) = [];
                end
            end
        end
    end
    
    methods % specific functionality and overloads
        function varargout = plus(Obj1, Obj2, varargin)
            % Apply the plus operator between AstroImage objects.
            %       This function utilize the funBinary method.
            %       See funBinary for details and additional arguments.
            % Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)});
            %          R = AI + AI
            %          R = AI + 1
            %          AI = AstroImage({ones(10,10), 2.*ones(10,10)});
            %          R = AI + AI(1)
            
            [varargout{1:nargout}] = funBinary(Obj1, Obj2, @plus, varargin{:});
            
        end
        
        function varargout = minus(Obj1, Obj2, varargin)
            % Apply the minus operator between AstroImage objects.
            %       This function utilize the funBinary method.
            %       See funBinary for details and additional arguments.
            % Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)});
            %          R = AI - AI
            %          R = AI - 1
            %          AI = AstroImage({ones(10,10), 2.*ones(10,10)});
            %          R = AI - AI(1)
            
            [varargout{1:nargout}] = funBinary(Obj1, Obj2, @minus, varargin{:});
            
        end
        
        function varargout = times(Obj1, Obj2, varargin)
            % Apply the times operator between AstroImage objects.
            %       This function utilize the funBinary method.
            %       See funBinary for details and additional arguments.
            % Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)});
            %          R = AI .* AI
            %          R = AI .* 1
            %          AI = AstroImage({ones(10,10), 2.*ones(10,10)});
            %          R = AI .* AI(1)
            
            [varargout{1:nargout}] = funBinary(Obj1, Obj2, @times, varargin{:});
            
        end
        
        function varargout = rdivide(Obj1, Obj2, varargin)
            % Apply the rdivide operator between AstroImage objects.
            %       This function utilize the funBinary method.
            %       See funBinary for details and additional arguments.
            % Example: AI = AstroImage({ones(10,10), 2.*ones(20,20)});
            %          R = AI ./ AI
            %          R = AI ./ 2
            %          AI = AstroImage({ones(10,10), 2.*ones(10,10)});
            %          R = AI ./ AI(1)
            
            [varargout{1:nargout}] = funBinary(Obj1, Obj2, @rdivide, varargin{:});
            
        end
                
        function Result = conv(Obj, PSF, Args)
            % Convolve images with their PSF, or another PSF
            % Input  : - An AstroImage object.
            %          - PSF/kernel with which to convolve the input image.
            %            This can be one of the following:
            %            1. A matrix.
            %            2. A function handle: Fun(Args.ArgsPSF{:})
            %            3. AstroPSF object (multiple elements supported)
            %            4. ImageComponent (multiple elements supported)
            %            5. AstroImage object (multiple) use the .ImageData.Image field.
            %            6. empty.
            %            If empty, then will attempt to use the AstroPSF
            %            object in the AstroImage object.
            %            Default is [].
            %          * ...,key,val,...
            %            'ArgsPSF' - A cell array of arguments to pass to
            %                   the getPSF function of AstroPSF or to the
            %                   function_handle (if the PSF is a function).
            %                   Default is {}.
            %            'UseFFT' - A logical indicating if to use fft or direct
            %                   convolution. If empty, use faster method
            %                   for the specific image and kernel sizes.
            %                   Default is [].
            %            'PadMethod' - Padding before operation (padded region
            %                   will be removed from output).
            %                   '' - do nothing (Default).
            %                   'circular' - circular boundry conditions.
            %                   'replicate' - relpicate nearest edge value.
            %                   'symmetric' - mirror reflection boundry conditions.
            %            'DataProp' - A cell array of data properties on
            %                   which to aplly the operator.
            %                   Default is {'ImageData'}.
            %            'DataPropIn' - Data property in the ImageComponent
            %                   on which to apply the operator.
            %                   Default is 'Image'.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - An AstroImage object in which the operator applied
            %            to the images.
            % Author : Eran Ofek (May 2021)
            % Example: AI = AstroImage({rand(100,100), rand(200,200)});
            %          AI.conv(imUtil.kernel2.annulus);
            %          Mat = zeros(30,30); Mat(15,15)=1;
            %          AI = AstroImage({Mat});
            %          Res = conv(AI, @imUtil.kernel2.gauss)
            
            arguments
                Obj
                PSF                              = [];
                Args.ArgsPSF cell                = {};
                Args.UseFFT                      = [];
                Args.PadMethod                   = '';
                Args.DataProp cell               = {'ImageData'}
                Args.DataPropIn                  = 'Image';
                Args.CreateNewObj(1,1) logical   = true;
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Nobj  = numel(Obj);
            Nprop = numel(Args.DataProp);
            if isa(PSF,'AstroPSF') || isa(PSF,'AstroImage') || isa(PSF,'ImageComponent')
                Nkernel = numel(PSF);
            else
                Nkernel = 1;
            end
            for Iobj=1:1:Nobj
                Ikernel = min(Iobj, Nkernel);
                if isempty(PSF)
                    % take PSF from PSFData
                    PSF = Obj(Iobj).getPSF(Args.ArgsPSF);
                end
                if isa(PSF,'AstroPSF')
                    Kernel = PSF(Ikernel).getPSF(Args.ArgsPSF{:});
                elseif isnumeric(PSF)
                    Kernel = PSF;
                elseif isa(PSF,'ImageComponent')
                    Kernel = PSF(Ikernel).(Args.DataPropIn);
                elseif isa(PSF,'AstroImage')
                    Kernel = PSF(Ikernel).ImageData.Image;
                elseif isa(PSF,'function_handle')
                    Kernel = PSF(Args.ArgsPSF{:});
                else
                    error('Unknown PSF option');
                end
                
                for Iprop=1:1:Nprop
                    Result(Iobj).(Args.DataProp{Iprop}).(Args.DataPropIn) = imUtil.filter.conv2_fast(Obj(Iobj).(Args.DataProp{Iprop}).(Args.DataPropIn), Kernel, Args.UseFFT, Args.PadMethod);
                end
                
            end
            
        end
        
        function Result = filter(Obj, PSF, Args)
            % Filter images with their PSF, or another PSF
            % Input  : - An AstroImage object.
            %          - PSF/kernel with which to filter (cross-corelate) the input image.
            %            This can be one of the following:
            %            1. A matrix.
            %            2. A function handle: Fun(Args.ArgsPSF{:})
            %            3. AstroPSF object (multiple elements supported)
            %            4. ImageComponent (multiple elements supported)
            %            5. AstroImage object (multiple) use the .ImageData.Image field.
            %            6. empty.
            %            If empty, then will attempt to use the AstroPSF
            %            object in the AstroImage object.
            %            Default is [].
            %          * ...,key,val,...
            %            'ArgsPSF' - A cell array of arguments to pass to
            %                   the getPSF function of AstroPSF or to the
            %                   function_handle (if the PSF is a function).
            %                   Default is {}.
            %            'UseFFT' - A logical indicating if to use fft or direct
            %                   convolution. If empty, use faster method
            %                   for the specific image and kernel sizes.
            %                   Default is [].
            %            'PadMethod' - Padding before operation (padded region
            %                   will be removed from output).
            %                   '' - do nothing (Default).
            %                   'circular' - circular boundry conditions.
            %                   'replicate' - relpicate nearest edge value.
            %                   'symmetric' - mirror reflection boundry conditions.
            %            'DataProp' - A cell array of data properties on
            %                   which to aplly the operator.
            %                   Default is {'ImageData'}.
            %            'DataPropIn' - Data property in the ImageComponent
            %                   on which to apply the operator.
            %                   Default is 'Image'.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   Default is true.
            % Output : - An AstroImage object in which the operator applied
            %            to the images.
            % Author : Eran Ofek (May 2021)
            % Example: AI = AstroImage({rand(100,100), rand(200,200)});
            %          AI.filter(imUtil.kernel2.annulus);
            %          Mat = zeros(30,30); Mat(15,15)=1;
            %          AI = AstroImage({Mat});
            %          Res = filter(AI, @imUtil.kernel2.gauss)
            
            arguments
                Obj
                PSF                              = [];
                Args.ArgsPSF cell                = {};
                Args.UseFFT                      = [];
                Args.PadMethod                   = '';
                Args.DataProp cell               = {'ImageData'}
                Args.DataPropIn                  = 'Image';
                Args.CreateNewObj logical        = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
             Nobj  = numel(Obj);
            Nprop = numel(Args.DataProp);
            if isa(PSF,'AstroPSF') || isa(PSF,'AstroImage') || isa(PSF,'ImageComponent')
                Nkernel = numel(PSF);
            else
                Nkernel = 1;
            end
            for Iobj=1:1:Nobj
                Ikernel = min(Iobj, Nkernel);
                if isempty(PSF)
                    % take PSF from PSFData
                    PSF = Obj(Iobj).PSFData.getPSF(Args.ArgsPSF);
                end
                if isa(PSF,'AstroPSF')
                    Kernel = PSF(Ikernel).getPSF(Args.ArgsPSF{:});
                elseif isnumeric(PSF)
                    Kernel = PSF;
                elseif isa(PSF,'ImageComponent')
                    Kernel = PSF(Ikernel).(Args.DataPropIn);
                elseif isa(PSF,'AstroImage')
                    Kernel = PSF(Ikernel).ImageData.Image;
                elseif isa(PSF,'function_handle')
                    Kernel = PSF(Args.ArgsPSF{:});
                else
                    error('Unknown PSF option');
                end
                 
                for Iprop=1:1:Nprop
                    Result(Iobj).(Args.DataProp{Iprop}).(Args.DataPropIn) = imUtil.filter.filter2_fast(Obj(Iobj).(Args.DataProp{Iprop}).(Args.DataPropIn), Kernel, Args.UseFFT, Args.PadMethod);
                end
                
            end
            
        end
        
        function Result = fft(Obj, Args)
            % Return the fft2 of images data in an AstroImage object.
            % Input  : - An AstroImage object.
            %          * ...,key,val,...
            %            'DataProp' - A cell array of data properties on
            %                   which to calculate fft2.
            %                   Default is {'Image'}.
            %            'NrowsNcols' - A cell array of {NROWS, NCOLS}
            %                   padding argument to pass to fft2.
            %                   Default is {}.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object. Default is false.
            % Output : - An AstroImage object in which the images data
            %            contains the fft.
            % Example: AI = AstroImage({rand(100,100)});
            %          R  = fft(AI)
            
            arguments
                Obj
                Args.DataProp               = {'Image'}; %,'Back','Var'};
                Args.NrowsNcols             = {};
                Args.CreateNewObj logical   = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            if ischar(Args.DataProp)
                Args.DataProp = {Args.DataProp};
            end
            
            Nprop = numel(Args.DataProp);
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    Result(Iobj).(Args.DataProp{Iprop}) = fft2(Obj(Iobj).(Args.DataProp{Iprop}), Args.NrowsNcols{:});
                end
            end
        end
    end
       
    methods % utilities
        function DataProp = depandentProp2DataProp(Obj, Prop)
            % Depandent property to data property containing the ImageComponent object.
            %   Given a dependent data property (e.g., 'Image') convert to
            %   property name containing the ImageComponent object (e.g., 'ImageData').
            % Input  : - An AstroImage object.
            %          - Dependent property (e.g., 'Image','Back','Exp')
            % Output : - The property name containing the data corresponding 
            %            to the dependent property
            %            (e.g., 'ImageData','BackData','ExpData').
            % Author : Eran Ofek (May 2023)
            % Example: AI.depandentProp2DataProp('Exp')
            
            arguments
                Obj(1,1)
                Prop char
            end
            
            FN = fieldnames(Obj.Relations);            
            Flag = strcmp(FN, Prop);
            DataProp = Obj.Relations.(FN{Flag});
            
        end
        
        function Obj=setAutoScale(Obj, Args)
            % Set the Scale of the Back/Var/Mask/Exp images such that the image size will be equal to the Image data.
            % Input  : - An AstroImage object.
            %          * ...,key,val,...
            % Output : - Update the AstroImage object such that the Scale
            %            property in the ImageComponent objects is set such
            %            that the returned image sizes will be equal to the
            %            Image size.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Args.ImageProp = {'Back','Var','Mask','Exp'};
            end

            Nprop = numel(Args.ImageProp);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                SizeImage = Obj(Iobj).sizeImage('Image');
                for Iprop=1:1:Nprop
                    SizeProp = Obj(Iobj).sizeImage(Args.ImageProp{Iprop});
                    Scale    = SizeImage./SizeProp;
                    
                    DataProp = Obj(Iobj).depandentProp2DataProp(Args.ImageProp{Iprop});
                    %FN = fieldnames(Obj.Relations);
                    %Ind = strcmp(FN, Args.ImageProp{Iprop});
                    %DataProp = Obj.Relations.(FN{Ind});

                    Obj(Iobj).(DataProp).Scale = Scale;
                end
            end

        end
    end

    %----------------------------------------------------------------------
    methods (Access = protected)
        function NewObj = copyElement(Obj)
            % Custom copy of object properties
            % Called from copy() of matlab.mixin.Copyable decendents
            
            % Make shallow copy of all properties
            NewObj = copyElement@Component(Obj);

            % Deep copy class properties
            NewObj.ImageData    = Obj.ImageData.copy();
            NewObj.BackData     = Obj.BackData.copy();
            NewObj.VarData      = Obj.VarData.copy();
            NewObj.MaskData     = Obj.MaskData.copy();
            NewObj.ExpData      = Obj.ExpData.copy();
            NewObj.HeaderData   = Obj.HeaderData.copy();
            NewObj.CatData      = Obj.CatData.copy();
            NewObj.PSFData      = Obj.PSFData.copy();
            NewObj.WCS          = Obj.WCS.copy();
        end
    end
    
    %----------------------------------------------------------------------
        
    methods (Static)
        function help
            % show mlx manual for AstroImage
            open manuals.classes.AstroImage
        end
        
        Result = unitTest()
            % Unit-Test
            
        Result = perfTest()
            % perfTest for the AstroImage class
        
        Result = perfTestMat()
            % Low level perfTest for the Matlab matrix
            
    end
    
end
