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
%       PSF
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
%       funHeaderScalar - Apply function that return a scalae on HeaderData properties in AstroImage array
%       getStructKey - Get multiple  keys from headers in multiple AstroImage and store in a structure array
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
%       
% Functionality (Static):
%       imageIO2AstroImage - Convert an ImageIO object into an AstroImage object
%       readImages2AstroImage - Create AstroImage object and read images into a specific property.
%       unitTest - unitTest for AstroImage


classdef AstroImage < Component
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent) % Access image data directly        
        Image 
        Back 
        Var
        Mask 
        Header  % e.g., Header, Header('EXPTIME'), Header({'EXPTIME','IMTYPE'}), Header('IMTYPE',{additional args to keyVal})
        Key
        Cat     % e.g., Cat, Cat([1 3]), Cat('RA'), Cat({'RA','Dec'})
        PSF
        %WCS
    end
    
    properties (SetAccess = public)
        % Data
        %ImageData(1,1) NoisyImage
        
        ImageData(1,1) SciImage              %= SciImage;
        BackData(1,1) BackImage              %= BackImage;
        VarData(1,1) VarImage                %= VarImage;
        MaskData(1,1) MaskImage              %= MaskImage;
        
        HeaderData(1,1) AstroHeader          %= AstroHeader;
        CatData(1,1) AstroCatalog            %= AstroCatalog;
        PSFData(1,1) AstroPSF                %= AstroPSF;
        WCS   % not ready: AstroWCS
        
        PropagateErr(1,1) logical          = false;
        
    end
    
    properties (Hidden, Constant)
        % set the relation between the Dependent prop and the data prop
        Relations   = struct('Image','ImageData',...
                             'Back','BackData',...
                             'Var','VarData',...
                             'Mask','MaskData');
        
        
    end
    
    methods % Constructor
       
        function Obj = AstroImage(FileNames, Args)
            % Constructor and image reader for AstroImage class
            % Input  : - A file name, files, or cell of matrices.
            %          * ...,key,val,...
            %            'HDU' - HDU number. Default is 1.
            %            'Scale' - Image scale. Default is [].
            %            'ReadHeder' - A logical indicating if to read
            %                   header. Default is true.
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
                
                Args.Back                     = []; % if empty and BackHDU is not empty, them read from the primary FileNames
                Args.BackHDU                  = [];
                Args.BackScale                = [];
                
                Args.Var                      = [];
                Args.VarHDU                   = [];
                Args.VarScale                 = [];
                
                Args.Mask                     = [];
                Args.MaskHDU                  = [];
                Args.MaskScale                = [];
                
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
                Obj.CatData     = AstroCatalog;
                Obj.PSFData     = AstroPSF;
                Obj.WCS         = [];            % FFU: update when WCS class is ready
                
            else
                if isnumeric(FileNames)
                    Nobj = prod(FileNames);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = AstroImage([]);
                    end
                    Obj = reshape(Obj, FileNames);
                    
                else
                    if isa(FileNames,'AstroImage')
                        Obj = FileNames;
                    elseif isa(FileNames,'SIM')
                        % convert SIM to AstroImage
                        
                    elseif isa(FileNames,'imCl')
                        % convert imCl to AstroImage
                        
                    else
                        % ImageData
                        Obj = AstroImage.readImages2AstroImage(FileNames,'HDU',Args.HDU,...
                                                                        'Obj',[],...
                                                                        'FileType',Args.FileType,...
                                                                        'UseRegExp',Args.UseRegExp,...
                                                                        'Scale',Args.Scale,...
                                                                        'ReadHeader',Args.ReadHeader,...
                                                                        'DataProp','ImageData');
                                                                        
                        % Other data properties
                        ListProp  = {'Back','Var','Mask'};
                        ListData  = {'BackData','VarData','MaskData'};
                        ListHDU   = {'BackHDU','VarHDU','MaskHDU'};
                        ListScale = {'BackScale','VarScale','MaskScale'};
                        
                        Nlist = numel(ListProp);
                        for Ilist=1:1:Nlist
                            if ~isempty(Args.(ListHDU{Ilist})) && isempty(Args.(ListProp{Ilist}))
                                % read the Back/Var/... images from the science images
                                % (FileNames), but from a different HDU.
                                Args.(ListProp{Ilist}) = FileNames;
                            end
                            if ~isempty(Args.(ListProp{Ilist}))
                                % do not read header
                                Obj = AstroImage.readImages2AstroImage(Args.(ListProp{Ilist}),'HDU',Args.(ListHDU{Ilist}),...
                                                                            'Obj',Obj,...
                                                                            'FileType',Args.FileType,...
                                                                            'UseRegExp',Args.UseRegExp,...
                                                                            'Scale',Args.(ListScale{Ilist}),...
                                                                            'ReadHeader',false,...
                                                                            'DataProp',ListData{Ilist});
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
        function Obj = imageIO2AstroImage(ImIO, DataProp, Scale, CopyHeader, Obj)
            % Convert an ImageIO object into an AstroImage object
            % Input  : - An ImageIO object.
            %          - data property in which to store the image.
            %          - Scale of the image.
            %          - A logical indicating if to copy the header.
            %          - An AstroImage object in which to put the data.
            %            If empty, create a new object. Default is empty.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage.imageIO2AstroImage(ImIO, 'ImageData', [], true)
       
            if nargin<5
                % create new Obj
                Obj = [];
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
                Obj(Iobj).(DataProp).Data  = ImIO(Iobj).Data;
                Obj(Iobj).(DataProp).Scale = Scale;
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
            %            'FileType' - See ImageIO. Default is [].
            %            'UseRegExp' - See ImageIO. Default is false.
            %            'Scale' - The scale of the image (see definition
            %                   in ImageComponent). Default is [].
            %            'DataProp' - AstroImage data property in wjich to
            %                   store the data. Default is 'ImageData'.
            %            'ReadHeader' - Default is true.
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
                Args.FileType               = [];
                Args.UseRegExp(1,1) logical = false;
                Args.Scale                  = [];
                Args.DataProp               = 'ImageData';
                Args.ReadHeader             = true;
            end
            
                
            switch lower(Args.DataProp)
                case {'imagedata','backdata','vardata','maskdata'}
                    ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                             'FileType',Args.FileType,...
                                             'IsTable',false,...
                                             'ReadHeader',Args.ReadHeader,...
                                             'UseRegExp',Args.UseRegExp);
                case {'cat','catdata'}
                    ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                             'FileType',Args.FileType,...
                                             'IsTable',true,...
                                             'ReadHeader',Args.ReadHeader,...
                                             'UseRegExp',Args.UseRegExp);
                otherwise
                    error('DataProp %s is not supported',Args.DataProp);
            end
            Obj = AstroImage.imageIO2AstroImage(ImIO, Args.DataProp, Args.Scale, Args.ReadHeader, Args.Obj);
            
            
        end
                                         
    end

 
    methods % Setters/Getters
        function Obj = set.Image(Obj, Data)
            % setter for Image - store image in ImageData property
            %Obj.(Relations.Image).Image = Data;  % can use this instead
            Obj.ImageData.Image = Data;
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
        
        function Data = get.Header(Obj)
            % getter for Header
            Data = Obj.HeaderData.Data;
        end
        
        function Data = get.Key(Obj)
            % getter for Header keys
            Data = Obj.HeaderData.Key;
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
            %            Default is {'Image','Back','Var','Mask'}.
            % Output : * One output per requested data property. For each
            %            data property, this is an array of logical
            %            indicating if the data isempty.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage;
            %          [a,b]=AI.isemptyImage({'Image','Back'})
            
            arguments
                Obj
                Prop        = {'Image','Back','Var','Mask'};
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
        
        function [Nx, Ny] = sizeImage(Obj, Prop)
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
                [Ny, Nx] = size(Obj(Iobj).(Prop));
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
            %                   'VarData', 'MaskData'}.
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
                Args.DataProp                           = {'ImageData','BackData', 'VarData', 'MaskData'};
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
                        varargout{Iout}(Iobj) = Obj(Iobj).(Args.DataProp{Iout}).copyObject;
                    else
                        varargout{Iout}(Iobj) = Obj(Iobj).(Args.DataProp{Iout});
                    end
                end
            end
             
            
        end
        
        function Result = astroImage2AstroCatalog(Obj, Args)
            % Convert the CataData in AstroImage object into an AstroCatalog object array.
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
                    Result(Iobj) = Obj(Iobj).CatData.copyObject;
                else
                    Result(Iobj) = Obj(Iobj).CatData;
                end
            end
        end
    end
    
    methods % write
%         function write1(Obj, FileName)
%             % Write a single image in a single object
%         
%             arguments
%                 Obj
%                 FileName
%                 DataProp char              = 'ImageData';
%                 DataPropIn char            = 'Image';
%                 WriteHeader(1,1) logical   = true;
%             end
%             
%             
%             
%             
%         end
        
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
            %             If empty (default), then this argument will
            %             be set by the number of output args.
            %             If 0, then false, otherwise true.
            %             This means that IC.fun, will modify IC,
            %             while IB=IC.fun will generate a new copy in
            %             IB.
            %          - A cell array of data properties which to transform
            %            to the new class. Default is
            %            {'ImageData','BackData','VarData'}.;
            % Output : - An ImageComponent object in which the image 'Data'
            %            is transformed into the new type.
            % Author : Eran Ofek (May 2021)
            % Example: AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
            %          Res = cast(AI,'single');
            
            arguments
                Obj
                NewClass         = 'single';
                CreateNewObj     = [];
                DataProp cell    = {'ImageData','BackData','VarData'};
            end
            
            if isempty(CreateNewObj)
                if nargout==0
                    CreateNewObj = false;
                else
                    CreateNewObj = true;
                end
            end
            if CreateNewObj
                Result = Obj.copyObject;
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
        
        function Result = funWCS(Obj, Fun, ArgsToFun)
            % Apply function of WCS properties in AstroImage array
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
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
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
                Args.CreateNewObj      = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                    Result = Obj;
                else
                    % create new obj
                    Args.CreateNewObj = true;
                    Result = Obj.copyObject;
                end
            else
                if Args.CreateNewObj
                    Result = Obj.copyObject;
                else
                    Result = Obj;
                end
            end
                    
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result.MaskData = maskSet(Result(Iobj).MaskData, Flag, BitName, SetVal, 'CreateNewObj', Args.CreateNewObj);
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
                Result = Obj.copyObject;
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
            %                   {'ImageData','BackData','VarData','MaskData'}.
            %            'DataPropIn' - Data property in the ImageComponent 
            %                   on which the operator
            %                   will be operated. Default is 'Data'.
            % Output : - An array in which each element corresponds to the operator applied
            %            to an element in the ImageComponent object.
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
                Args.DataProp                   = {'ImageData','BackData','VarData','MaskData'};
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
                
        function Result = funBinaryProp(Obj1, Obj2, Operator, Args)
            % Apply binary function on a single property of AstroImage
            %       without error propagation.
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
                    Result = Obj1.copyObject;
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
                            Result(Ires).(Args.DataProp).(Args.DataPropIn)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)) = perator(Tmp1, Tmp2, Args.OpArgs{:});
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
                    Result = Obj1.copyObject;
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
                    Result = Obj1.copyObject;
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
            %            If empty, then do not crop.
            %          * ...,key,val,...
            %            'Type' - ['ccdsec'] | 'center'
            %            'DataProp' - A cell array of image data properties
            %                   to crop. Default is {'ImageData','BackData','VarData','MaskData'}
            %            'DataPropIn' - Data property on which to operate.
            %                   Default is 'Image'.
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
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - A cropped AstroImage object.
            % Author : Eran Ofek (Jul 2021)
            % Example: AI = AstroImage({rand(100,100),rand(100,200)},'Back',{rand(100,100),rand(100,200)});
            %          Res = crop(AI,[11 20 11 30])

            arguments
                Obj
                CCDSEC
                Args.Type                      = 'ccdsec'; % 'center' mat results in errors!
                Args.DataProp cell             = {'ImageData','BackData','VarData','MaskData'};
                Args.DataPropIn                = 'Image';
                Args.UpdateCat(1,1) logical    = true;
                Args.cropXYargs cell           = {};
                Args.UpdateHeader(1,1) logical = true;
                Args.UpdateWCS(1,1) logical    = true;
                Args.CreateNewObj              = [];
            end

            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            if Args.CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end

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
                    for Iprop=1:1:Nprop
                        Result(Iobj).(Args.DataProp{Iprop}) = crop(Result(Iobj).(Args.DataProp{Iprop}), CCDSEC(Isec,:),...
                                                        'Type',Args.Type,...
                                                        'DataPropIn',Args.DataPropIn,...
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
                        % FFU
                        warning('UpdateWCS in AstroImage/crop is not implemented');
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
                Result = Obj.copyObject;
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
                Result = Obj.copyObject;
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
                    Result(Iobj).(Args.DataProp{Iprop}).(Args.DataPropIn) = imUtil.filter.filter2_fast(Obj(Iobj).(Args.DataProp{Iprop}).(Args.DataPropIn), Kernel, Args.UseFFT, Args.PadMethod);
                end
                
            end
            
        end
        
        
    end
    
    methods (Static) % Unit-Test
        function Result = unitTest()
            % unitTest for AstroImage
            % Example: AstroImage.unitTest
            %
            % Issues:
            % -funHeaderScalar returns NaN
            %
            % To do:
            % -Add tests for:
            %     -object2array
            % -Add output checks for:
            %     -imageIO2AstroImage
            %     -readImages2AstroImage
            %     -isemptyImage
            %     -funCat
            %     -funHeader
            %     -funHeaderScalar
            %     -maskSet
            %     -isImType
            %     -funUnaary
            %     -funUnaryScalar
            %     -funBinaryProp
            %     -image2subimages
            %     -conv
            %     -filter
            
            io.msgStyle(LogLevel.Test, '@start', 'AstroImage test started')
            
            DataSampleDir = tools.os.getTestDataDir;
            PWD = pwd;
            cd(DataSampleDir);
            
            Astro = AstroImage;
            
            % cast
            io.msgLog(LogLevel.Test, 'testing AstroImage cast');
            AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
            Res = cast(AI,'single');
            if ~isa(Res.Back,'single')
                error('Type casting failed');
            end
            
            % funBinaryProp
            % funBinary for a single property / no error propagation
            AI=AstroImage({ones(3,3)});
            Res = funBinaryProp(AI,3,@plus);
            if ~all(Res.Image==4)
                error('funBinaryProp failed');
            end
            Res = funBinaryProp(AI,[3 2 1],@plus);
            if ~all(Res.Image==[4 3 2])
                error('funBinaryProp failed');
            end
            Res = funBinaryProp(AI,{2 1},@plus);
            if numel(Res)~=2
                error('funBinaryProp failed');
            end
            if ~all(Res(1).Image==3,'all') || ~all(Res(2).Image==2,'all')
                error('funBinaryProp failed');
            end
            Res = funBinaryProp(AI,AI,@minus);
            if ~all(Res.Image==0)
                error('funBinaryProp failed');
            end
            Res = funBinaryProp(AI,AI,@minus,'DataProp','VarData');
            if ~all(Res.Image==1)
                error('funBinaryProp failed');
            end
            
            io.msgLog(LogLevel.Test, 'testing AstroImage julday');
            AI = AstroImage('*.fits');
            [JD,ET] = julday(AI(2:3));
            [JD,ET] = julday(AI(1));
            
            % image2subimages
            io.msgLog(LogLevel.Test, 'testing AstroImage image2subimages');
            AI = AstroImage({rand(1024, 1024)},'Back',{rand(1024, 1024)});
            Result = imProc.image.image2subimages(AI,[256 256]);
            s=Result(1).MaskData.bitStat;
            s.BitSummary;

            
            AI = AstroImage({rand(100,100),rand(100,200)},'Back',{rand(100,100),rand(100,200)});
            Res = crop(AI,[11 20 11 30])

            
            % overload operators
            io.msgLog(LogLevel.Test, 'testing AstroImage operators')
            AI = AstroImage({ones(10,10), 2.*ones(20,20)});
            % perform: AI(1)+AI(1) and AI(2)+AI(2)
            R = AI + AI;
            % perform: AI(1)+2 to all elements
            R = AI + 2;
            if ~all(R(1).Image==3,'all')
                error('Problem with plus operator');
            end
            % add 2 to all elements in AI
            AI + 2;
            if ~all(AI(1).Image==3,'all')
                error('Problem with plus operator');
            end
            
            AI = AstroImage({ones(10,10), 2.*ones(10,10)});
            R = AI + AI;
            R = AI + AI(1);
            if ~all(R(2).Image==3)
                error('Problem with plus operator');
            end
            
            R = AI - AI;
            R = AI - 3;
            if ~all(R(2).Image==-1)
                error('Problem with minus operator');
            end
            
            R = AI.*AI;
            R = AI.*3;
            if ~all(R(2).Image==6)
                error('Problem with times operator');
            end
            
            R = AI./AI;
            R = AI./3;
            if ~all(R(2).Image==2./3)
                error('Problem with rdivide operator');
            end
            
            % conv
            io.msgLog(LogLevel.Test, 'testing AstroImage conv');
            AI = AstroImage({rand(100,100), rand(200,200)});
            AI.conv(imUtil.kernel2.annulus);
            Mat = zeros(30,30); Mat(15,15)=1;
            AI = AstroImage({Mat});
            Res = conv(AI, @imUtil.kernel2.gauss);

            % filter (cross-correlation)
            io.msgLog(LogLevel.Test, 'testing AstroImage filter')
            AI = AstroImage({rand(100,100), rand(200,200)});
            AI.filter(imUtil.kernel2.annulus);
            Mat = zeros(30,30); Mat(15,15)=1;
            AI = AstroImage({Mat});
            Res = filter(AI, @imUtil.kernel2.gauss);
           
            % object2array
            % ???
            % No idea how to use this
            
            % imageIO2AstroImage
            io.msgLog(LogLevel.Test, 'testing imageIO2AstroImage')
            I = ImageIO('WFPC2ASSNu5780205bx.fits','ReadHeader',1);
            AI = AstroImage.imageIO2AstroImage(I, 'ImageData', [], true);
            if (~prod(reshape((I.Data == AI.Image).',1,[])))
                error('problem with IO2AstroImage')
            end
            AI = AstroImage.imageIO2AstroImage(I, 'BackData', 2, true);
            
            % readImages2AstroImage
            io.msgLog(LogLevel.Test, 'testing readImages2AstroImage')
            AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'ImageData');
            AI=AstroImage.readImages2AstroImage([]);
            AI=AstroImage.readImages2AstroImage([1 2]);
            AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)});
            AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)},'DataProp','VarData');
            AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','ImageData');
            AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','BackData','Obj',AI);
            AI=AstroImage.readImages2AstroImage({rand(5,5), rand(10,10)},'DataProp','VarData','Obj',AI,'Scale',2);
            
            % isemptyImage
            io.msgLog(LogLevel.Test, 'testing AstroImage isemptyImage')
            AI=AstroImage.readImages2AstroImage([]);
            [a,b]=AI.isemptyImage({'Image','Back'});
            AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'ImageData');
            [a,b]=AI.isemptyImage({'Image','Back'});
            AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'BackData');
            [a,b]=AI.isemptyImage({'Image','Back'});
            
            % sizeImage
            io.msgLog(LogLevel.Test, 'testing AstroImage sizeImage')
            AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'ImageData');
            [Ny, Nx] = AI.sizeImage;
            AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'BackData');
            [Ny, Nx] = AI.sizeImage('Back');
            
            % funCat
            io.msgLog(LogLevel.Test, 'testing AstroImage funCat')
            AI = AstroImage({rand(10,10), rand(10,10)});
            AI(1).CatData.Catalog=rand(10,2);
            AI(2).CatData.Catalog=rand(10,2);
            funCat(AI,@sortrows,1);
            
            % funHeader
            io.msgLog(LogLevel.Test, 'testing AstroImage funHeader')
            AI = AstroImage({rand(10,10), rand(10,10)});
            funHeader(AI,@insertKey,{'GAIN',2,''});
            
            % funHeaderScalar
            io.msgLog(LogLevel.Test, 'testing AstroImage funHeaderScalar')
            AI = AstroImage({rand(10,10), rand(10,10)});
            funHeaderScalar(AI,@julday)
            
            % getStructKey
            Im = ones(500,500);
            AI = AstroImage({poissrnd(Im.*1e3), poissrnd(Im*3e3), poissrnd(Im.*1e4), poissrnd(Im.*1e4), poissrnd(Im.*2e4)});
            AI(1).HeaderData.insertKey({'EXPTIME',1}); AI(2).HeaderData.insertKey({'EXPTIME',3}); AI(3).HeaderData.insertKey({'EXPTIME',10});
            AI(4).HeaderData.insertKey({'EXPTIME',10}); AI(5).HeaderData.insertKey({'EXPTIME',20});
            [Result] = getStructKey(AI, {'EXPTIME'});

            % setKeyVal
            AI = AstroImage({rand(10,10)});
            AI.setKeyVal('TYPE','science');
            
            % maskSet
            io.msgLog(LogLevel.Test, 'testing AstroImage maskSet')
            AI = AstroImage({rand(3,3)},'Mask',{uint32(zeros(3,3))});
            AI.MaskData.Dict=BitDictionary('BitMask.Image.Default')
            Flag = false(3,3); Flag(1,2)=true;
            Res = AI.maskSet(Flag,'Saturated');
            Res = AI.maskSet(Flag,'Streak');
            
            % isImType
            io.msgLog(LogLevel.Test, 'testing AstroImage isImType')
            AI = AstroImage({rand(3,3)},'Mask',{uint32(zeros(3,3))})
            Res = isImType(AI, 'bias');
            
            % funUnary
            io.msgLog(LogLevel.Test, 'testing AstroImage funUnary')
            AI = AstroImage({10.*ones(10,10)},'Back',{ones(5,5)},'BackScale',2,'var',{ones(5,5)},'VarScale',2);
            B=AI.funUnary(@sin,'CreateNewObj',true);
            B=AI.funUnary(@mean,'OpArgs',{'all'}); 
            AI.PropagateErr=true; B=AI.funUnary(@mean,'OpArgs',{'all'});
            % B=AI.funUnary(@median,'OpArgs',{'all'}); % must result in error
            B=AI.funUnary(@median,'OpArgs',{'all'},'PropagateErr',false,'OperateOnVar',true);
            
            % funUnaryScalar
            io.msgLog(LogLevel.Test, 'testing AstroImage funUnaryScalar')
            AI = AstroImage({randn(100,100), randn(100,100)},'Back',{randn(100,100), randn(100,100)});
            [A,B] = funUnaryScalar(AI, @mean, 'OpArgs',{'all'});
            [A,B] = funUnaryScalar(AI, @std, 'OpArgs',{[],'all'});
            
            % funBinaryImVar
            io.msgLog(LogLevel.Test, 'testing AstroImage funBinaryImVar')
            AI = AstroImage({ones(3,3)},'Var',{ones(3,3)});
            Res = funBinaryImVar(AI,AI,@plus);
            Res = funBinaryImVar(AI,AI,@minus);
            Res = funBinaryImVar(AI,3,@times);
            
            cd(PWD);
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroImage test passed')
            Result = true;
        end
    end
    

end

            
