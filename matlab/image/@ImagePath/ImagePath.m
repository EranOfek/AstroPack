% ImagePath - A class for generating stand storing image/path names
%       for ULTRASAT and LAST.
%
% File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>


classdef ImagePath < Base %Component
    % Construct and parse (@Todo) image path used in storage, database, and headers.
    % For storage and database, we should implement ImagePathDb class
    % The file path is described in the LAST/ULTRASAT file naming convension document.
    %
    % Path format (@Todo add from gdoc):
    % /base/data/YYYY/MM/DD/raw/ - contains all the science and calibration raw data
    %
    % File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
    % Example: 'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits'
    
    properties       
        % These fields are the input parameters for getPath() and getFileName()
        
        ProjName        = '';           % Examples: 'ULTRASAT', 'LAST.1.12.4' (LAST node=1, mount=12, camera=4)
        Time            = [];           % Empty: current time, Numeric: time as JD, char: YYYY-MM-DDTHH:MM:SS.FFF, cell: {YYYY, MM, DD}
        TimeZone        = 2;            % Bias in hours, to generate folder name                
        Filter          = 'clear';      % Filter name
        FieldID         = '';           % Sky position identifier, like field ID, object name, CCD I’d, sub image ID etc. May include CropId. Examples: 314.1.1 is field I’d 314, CCD 1, sub image 1.
        Counter         = '';           % Counter
        CCDID           = '';           % CCD ID
        CropID          = '';           % Used with sub-images
        Type            = 'sci';        % [lower] sci, bias, dark, domeflat, twflat, skyflat, fringe, focus,...
        Level           = 'raw';        % [lower] log, raw, proc, stack, coadd, merged, ref.
        PathLevel       = [];           % Level for path: If empty use Level
        SubLevel        = '';           % Sublevel, see below:
            % SubLevel: n - normal, s - proper subtraction S, sp - proper subtraction S tag, d - proper subtraction D, t - Translient, r - proper coaddition R, m - matched filter with unspecified filter
            % SubLevel: Single capital letter prefix may be added to this name: F - Fourier Transform, R - Radon Transform, L - Laplacian, G - x gradient, H - y gradient. 
        Product         = 'Image';      % Product: Image, Back, Var, Exp, Nim, PSF, Cat, Spec, Mask, Evt, MergedMat
        Version         = '1';          % Version (for multiple processing)
        FileType        = 'fits';       % fits / hdf5 / fits.gz          
        Area            = '';           % Used by genPath()
        SubDir          = '';           % This is the area/location directory below the coadd/ref directory
    end
    
    properties (Hidden)
        % Fields formatting
        FormatFieldID   = '%06d';       % Used with FieldID
        FormatCounter   = '%03d';       % Used with Counter        
        FormatCCDID     = '%03d';       % Used with CCDID
        FormatCropID    = '%03d';       % Used with CropID
        FormatVersion   = '%d'; %'%03d';       % Used with Version
        
        % @Todo: Defaults should be loaded from configuration
        BasePath        = '/euler/archive'; % Loaded from Config
        DataDir         = 'LAST';           % Loaded from Config
    end

    properties (Hidden, SetAccess=protected, GetAccess=public)
        % Generated from Obj.Time
        JD              = [];           % UTC start of exposure
        TimeStr         = '';           % Time as appears in file name (YYYYMMDD.HHMMSS.FFF)       
        
        % Generated
        Path            = '';           % Path part         
        FileName        = '';           % Filename part
        FullName        = '';           % Full name Path/FileName
               
        %
        DictKeyNames Dictionary         % Dictionary, used to access Header keys
    end
    
    
    methods % Constructor
       
        function Obj = ImagePath(Pars)
            % Constructor for ImagePath
            % Input  : - Either a:
            %            1. Scalar indicting the number of elements in the
            %            empty ImagePath object that will be created.
            %            2. A cell array of file nsmaes that will be
            %            parsed.
            %            3. A structure from which all the relevant fields
            %            will be copied to the ImageObject properties.
            % Output : - An ImagePath object.
            % Author : Eran Ofek (Jan 2022)
            % Example: IP = ImagePath(2);
            %          IP = ImagePath({'LAST_20220118.193339.010_clear_____sci_raw_Image_1.fits'});
            
            arguments
                Pars = 1;
            end
            
            if iscell(Pars)
                % Pars is a list of image names
                Obj = ImagePath.parseFileName(Pars);
                
            elseif isnumeric(Pars)
                % length of ImagePath object
                N = Pars;
                for I=1:1:N
                    Obj(I).FileName = '';
                end
                
            elseif isstruct(Pars)
                % populate from a struct according to field names
                Obj = setProps(Obj, Pars);
                
            else
                error('Unknown option');
            end
            
            
            % Load defaults from configuration
            % i.e.
            %Obj.BasePath = Obj.Config.Data.System.ImagePath.BasePath;
            %Obj.DataDir  = Obj.Config.Data.System.ImagePath.DataDir;
            %Obj.TimeZone = Obj.Config.Data.System.Time.TimeZone;
            
            % Load header key names mapping from configuation
            %Obj.DictKeyNames = Dictionary.getDict('Header.ImagePath.KeyNames');
            
        end
             
        function [ResultPath, ResultFileName] = setTestData(Obj)
            % Set data for unit-test and debugging, return expected result
            
            Obj.ProjName        = 'USAT';            
            Obj.Time            = '2021-09-09T12:34:56.789';
            Obj.TimeZone        = 2;
            Obj.Filter          = 'clear';
            Obj.FieldID         = 'fld';
            Obj.Counter         = 'cnt';
            Obj.CCDID           = 'ccdid';            
            Obj.CropID          = 'crop';
            Obj.Type            = 'sci';
            Obj.Level           = 'raw';
            Obj.SubLevel        = 'sub';
            Obj.Product         = 'Image';
            Obj.Version         = 'ver1';
            Obj.FileType        = 'fits';
            Obj.SubDir          = 'subdir';
            
            % Debug? or have it?
            Obj.BasePath        = '/home/last';
            Obj.DataDir         = 'data';                   

            % Return expected results, used by unitTest()
            ResultPath = '/home/last/data/2021/09/09/raw/';  % '/home/last/data/.../subdir'; %'/home/last/data/2021/10/03/raw/'
            ResultFileName = 'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_Image_ver1.fits';
        end
    end
    
    methods % setter/getters
        function Result = get.Counter(Obj)
            % getter for Counter
            % convert to numeric
           
            if ischar(Obj.Counter)
                Result = str2double(Obj.Counter);
            else
                Result = Obj.Counter;
            end
        end
        
%         function Result = get.JD(Obj)
%             % Getter for JD, including populating JD from Time if needed
%             
%             if isempty(Obj.JD)
%                 Obj = setTime(Obj);
%             end
%             Result = Obj.JD;
%         end
        
    end
      
    methods % Generate Path & FileName
        
        function Result = genPath(Obj, Args)
            % Construct image/catalog file path based on the LAST/ULTRASAT standard
            % Options are:
            %
            %   Form 1:
            %       /data/YYYY/MM/DD/raw/     - contains all the science raw data
            %       /data/YYYY/MM/DD/log/     - contains all the log files
            %       /data/YYYY/MM/DD/proc/    - contains all the single processed images including: image, mask, back (if provided), var (if provided), PSF (if provided), and catalogs.
            %       /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction products 
            %            
            %   Form 2: 
            %       /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF
            %
            %   Form 3: 
            %       /data/coadd/area/          - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods)             
            %
            %   Form 4:
            %       /data/YYYY/MM/DD/calib/   - contains all the processed calibration images/variance/masks/catalogs            
            % Input  : - An ImagePath object.
            %          * ...,key,val,...
            %            can be used to set the following ImagePath
            %            properties:
            %            'BasePath', 'DataDir', 'SubDir', 'Time',
            %            'TimeZone', 'PathLevel', 'Area'.
            % Output : - The path is updated in the Path property.
            %            The path of the last element of ImagePath is
            %            returned.
            % Example: IP = ImagePath;
            %          Path = IP.genPath('PathLevel','ref','SubDir','x')
            
            arguments
                Obj
                Args.BasePath       % See constructor
                Args.DataDir        % See constructor
                Args.SubDir         %
                Args.Time           % Empty -> current computer time, UTC, Numeric -> time is in JD, char -> YYYY-MM-DDTHH:MM:SS.FFF
                                    %      Should match 'convert'             format.Date, TimeZone} or {YYYY, MM, DD}, or []}
                Args.TimeZone       % Hours
                Args.PathLevel      % Also in file name
                Args.Area
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % Set properties from arguments, only properties that exist in Args are set
                Obj(Iobj).setProps(Args);

                if isempty(Obj(Iobj).PathLevel)
                    PathLevel = Obj(Iobj).Level;
                else
                    PathLevel = Obj(Iobj).PathLevel;
                end

                % Convert Time to JD and TimeStr
                Obj(Iobj).setTime();

                % Fix field values
                Obj(Iobj).fixFields();

                % Generate YMD based on JD and TimeZone
                [Year, Month, Day] = imUtil.util.file.date_directory(Obj(Iobj).JD, Obj(Iobj).TimeZone);
                YMD = sprintf('%04d%s%02d%s%02d', Year, filesep, Month, filesep, Day);

                UseYMD   = false;
                PreDate  = '';
                PostDate = '';

                % Check Level
                switch PathLevel
                    % /base/data/ref/<area>/version<#>/ - All sky reference/coadd image - images/masks/catalogs/PSF
                    case { 'ref', 'coadd' }
                        PostDate = sprintf('%s%s%s%s', PathLevel, filesep, Obj(Iobj).Area, ...
                            filesep, Obj(Iobj).Version);
                    case 'merged'
                        UseYMD = true;                     
                        PostDate = 'proc'; %Obj.Level; 
                    case { 'raw', 'log', 'proc', 'stacked' }
                        UseYMD = true;                     
                        PostDate = PathLevel;                    
                    case { 'calib' }
                        PostDate = sprintf('%s%s%s', PathLevel, filesep, Obj(Iobj).SubLevel);
                    otherwise
                        error('Unknown path Level: %s', PathLevel);
                end

                %
                if UseYMD
                    FPath = sprintf('%s%s%s%s%s%s%s%s%s%s%s%s', Obj(Iobj).BasePath, filesep, ...
                       Obj(Iobj).DataDir, filesep, PreDate, filesep, YMD, filesep, PostDate, ...
                       filesep, Obj(Iobj).SubDir, filesep);
                else
                    FPath = sprintf('%s%s%s%s%s%s%s%s%s%s%s%s', Obj(Iobj).BasePath, filesep, ...
                       Obj(Iobj).DataDir, filesep, PostDate, filesep, filesep, Obj(Iobj).SubDir, filesep);
                end                      

                % Clean path from multiple /
                FPath = strrep(FPath, '\', '/');            
                FPath = regexprep(FPath, sprintf('%s{2,5}', '/'), '/');

                Result = FPath; 
                Obj(Iobj).Path = FPath;
            end
        end
        
        function Result = genFile(Obj, Args)
            % Construct image/catalog file name based on the LAST/ULTRASAT standard
            % Description: Return data product file name and path according to the
            %              LAST/ULTRASAT standard.
            %              <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>
            % Input  : - An ImagePath object.
            %          * Pairs of ..., key,val, ... Possible keywords include:
            %            'ProjName' - Default is 'LAST.0.1'.
            %            'Time' - If empty, then will use current computer time, and
            %                   will assume the time is in UTC.
            %                   If numeric then assume the time is in JD, if char then
            %                   assume the time is in the YYYY-MM-DDTHH:MM:SS.FFF
            %                   format.
            %                   Default is [].
            %            'TimeZone' - Time zone [hours]. Default is 2.
            %            'Filter' - Default is 'clear'.
            %            'FieldID' - Default is ''.
            %
            %            'Type' - either bias, dark, domeflat, twflat, skyflat, fringe,
            %                   sci, wave.
            %                   Default is 'sci'.
            %            'Level' - either log, raw, proc, stack, coadd, ref.
            %                   Default is 'raw'.
            %            'SubLevel' - Options are:
            %                   n - normal
            %                   s - proper subtraction S
            %                   sp - proper subtraction S tag.
            %                   d - proper subtraction D
            %                   t - Translient
            %                   r - proper coaddition R
            %                   m - matched filter with unspecified filter
            %                   Default is ''.
            %            'Product' - either: im, back, var, imflag, exp, nim, psf, cat, spec.
            %                   Default is 'im'.
            %            'Version' - Default is 1.
            %
            %                   Default is '%03d'.
            %            'FileType' - Default is 'fits'.
            %            'SubDir' - This is the area/location directory below the
            %                   coadd/ref directory. Default is ''.
            %            'DataDir' - Default is 'data'.
            %            'Base' - Default is '/home/last'.
            %
            %            'FormatFieldID' - Formatting of FieldID if is given as number. Default is '%06d'.            
            %            'FormatVersion' - Formatting of Version if is given as number.            
            % Output : File name or path and file name (if Args.FullPath is
            %          true). Only the last file name is returned, while
            %          the FileName property in the object is also updated.
            % Example: IP = ImagePath(2);
            %          FileName = IP.genFile;
            
            arguments
                Obj
                Args.ProjName           % project name and telescope ID. Examples: 'ULTRASAT', 'LAST.1.12.4'
                Args.Time               % %{TimeZone} or {YYYY, MM, DD} or [] (required for path) [Save in 2 properties: JD & TimeStr]
                Args.Filter             % filter name, e.g., “clear”.
                Args.FieldID            % (char or number) [Saved as char]
                Args.Counter            % (number or char) - if the user didn’t supply then apply auto increase (if AutoIncrease = true)
                Args.CCDID              %
                Args.CropID             %
                Args.Type               %
                Args.Level              %
                Args.SubLevel           %- default is ‘n’ [note that n will be replaced by “”, without “.” seperator)
                Args.Product            %
                Args.Version            % (char or number) [saved as char]
                Args.FileType           % - default is ‘fits’.                                    
                Args.TimeZone           % Hours
                Args.Area               % Used when FullPath is true
                Args.FullPath = false;  %               
                Args.FormatFieldID      %
                Args.FormatCounter      %                
                Args.FormatCCDID        %
                Args.FormatCropID       %
                Args.FormatVersion      %
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
            
                % Set properties from arguments, only properties that exist in Args are set
                Obj(Iobj).setProps(Args);            

                % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
                % USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits

                % Set JD and TimeStr
                Obj(Iobj).setTime();

                % Format numeric fields
                StrFieldID  = Obj(Iobj).formatNumeric(Obj(Iobj).FieldID, Obj(Iobj).FormatFieldID); 
                StrCounter  = Obj(Iobj).formatNumeric(Obj(Iobj).Counter, Obj(Iobj).FormatCounter);            
                StrCCDID    = Obj(Iobj).formatNumeric(Obj(Iobj).CCDID, Obj(Iobj).FormatCCDID);
                StrCropID   = Obj(Iobj).formatNumeric(Obj(Iobj).CropID, Obj(Iobj).FormatCropID);
                StrVersion  = Obj(Iobj).formatNumeric(Obj(Iobj).Version, Obj(Iobj).FormatVersion);

                % Fix and validate field values
                Obj(Iobj).fixFields();
                Obj(Iobj).valiadateFields();

                % Level / Level.SubLevel
                if isempty(Obj(Iobj).SubLevel)
                    MergedLevel = Obj(Iobj).Level;
                else
                    MergedLevel = sprintf('%s.%s', Obj(Iobj).Level, Obj(Iobj).SubLevel);    
                end

                % Prepare the final result
                % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
                Obj(Iobj).FileName = sprintf('%s_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.%s', Obj(Iobj).ProjName, ...
                               Obj(Iobj).TimeStr, Obj(Iobj).Filter, StrFieldID, StrCounter, StrCCDID, ...
                               StrCropID, Obj(Iobj).Type, MergedLevel, Obj(Iobj).Product, StrVersion, Obj(Iobj).FileType);


                % Get path
                Obj(Iobj).Path = ''; 
                if Args.FullPath
                    Obj(Iobj).Path     = Obj(Iobj).genPath('Time', Obj(Iobj).Time);
                    Obj(Iobj).FileName = fullfile(Obj(Iobj).Path, Obj(Iobj).FileName);
                end

                % Clean path from multiple /
                Obj(Iobj).FileName = strrep(Obj(Iobj).FileName, '\', '/');            
                Obj(Iobj).FileName = regexprep(Obj(Iobj).FileName, sprintf('%s{2,5}', '/'), '/');

                Result = Obj.FileName;
            end
            
        end
        
        function Result = genFull(Obj, Args)
            % Generate a full file name + path from a populated ImagePath
            % Input  : - A populated ImagePath object.
            %         * ...,key,val,...
            %           'PathLevel' - Level value for the path only.
            %                   If empty do nothing. Use this to modify the
            %                   path only. Default is [].
            % Output : - A full path + file name. Only the last full path
            %            is returned. All the rest are populated in the
            %            FullName property.
            % Author : Eran Ofek (Nov 2021)
            % Example: IP = ImagePath(2);
            %          IP.genFull
            
            arguments
                Obj
                Args.PathLevel  = [];  % [] - don't touch 
            end
                            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                File = Obj(Iobj).genFile;

                Level = Obj(Iobj).Level;
                if ~isempty(Args.PathLevel)
                    Obj(Iobj).Level = Args.PathLevel;
                end
                Path = Obj(Iobj).genPath;
                Obj(Iobj).Level = Level;  % return lebel to original value

                Result = sprintf('%s%s',Path,File);
                Obj(Iobj).FullName = Result;
            end
            
        end
        
        
        function Result = genFullCell(Obj, Args)
            % Generate a cell array of full file name + path from a populated ImagePath
            % Input  : - A populated ImagePath object.
            %         * ...,key,val,...
            %           'PathLevel' - Level value for the path only.
            %                   If empty do nothing. Use this to modify the
            %                   path only. Default is [].
            % Output : - A cell array of full path + file name. Only the last full path
            %            is returned. All the rest are populated in the
            %            FullName property.
            % Author : Eran Ofek (Nov 2021)
            % Example: IP = ImagePath(2);
            %          IP.genFull
            
            arguments
                Obj
                Args.PathLevel  = [];  % [] - don't touch 
            end
                            
            Nobj = numel(Obj);
            Result = cell(size(Obj));
            for Iobj=1:1:Nobj
                File = Obj(Iobj).genFile;

                Level = Obj(Iobj).Level;
                if ~isempty(Args.PathLevel)
                    Obj(Iobj).Level = Args.PathLevel;
                end
                Path = Obj(Iobj).genPath;
                Obj(Iobj).Level = Level;  % return lebel to original value

                Result{Iobj} = sprintf('%s%s',Path,File);
                Obj(Iobj).FullName = Result{Iobj};
            end
            
        end
        
    end
    
   
    methods % Read/Write from Header
        
        function Obj = readFromHeader(Obj, Input, DataProp)
            % Read ImagePath parameters from header.
            % Input  : - An ImagePath object.
            %          - A single element AstroImage or AstroHeader.
            %          - Either data property name or product name.
            % Output : - A populated ImagePath object.
            
            arguments
                Obj(1,1)
                Input(1,1)       % AstroHeader | AstroImage
                DataProp    = 'Image';    % DataProp in AstroImage or Product name
            end
            
            %Obj.msgLog(LogLevel.Debug, 'readFromHeader: ');
                                
            if isa(Input, 'AstroHeader')
                Header = Input;
                if isempty(DataProp)
                    Obj.Product         = Header.getVal('PRODUCT');
                else
                    Obj.Product         = DataProp;
                end
            elseif isa(Input, 'AstroImage')
                Header = Input.HeaderData;
            else
                error('INput must be an AstroHeader or AstroImage');
            end
              
            
            Obj.ProjName        = Header.getVal({'INSTRUME','PROJNAME'}); %Obj.DictKeyNames.PROJNAME);
            Obj.Time            = julday(Header);  %.getVal('JD');
            Obj.TimeZone        = Header.getVal('TIMEZONE');
            Obj.Filter          = Header.getVal('FILTER');
            Obj.FieldID         = Header.getVal('FIELDID');
            Obj.Counter         = Header.getVal('COUNTER');
            Obj.CCDID           = Header.getVal('CCDID');
            Obj.CropID          = Header.getVal('CROPID');
            Obj.Type            = Header.getVal('IMTYPE');
            Obj.Level           = Header.getVal('LEVEL');
            Obj.SubLevel        = Header.getVal('SUBLEVEL');
            if isnan(Obj.SubLevel)
                Obj.SubLevel = '';
            end
            Obj.Product         = DataProp;
            Obj.Version         = Header.getVal('VERSION');
            Obj.FileType        = 'fits';
            Obj.SubDir          = Header.getVal('SUBDIR');
            if isnan(Obj.SubDir)
                Obj.SubDir = '';
            end

        end
        
        function Result = writeToHeader(Obj, Header)
            % Write data to AstroHeader, DictKeyNames is used to get the
            % correct key names            
            arguments
                Obj
                Header AstroHeader
            end
            
            %Obj.msgLog(LogLevel.Debug, 'writeToHeader: ');
                 
            Header.setVal(Obj.DictKeyNames.JD,          Obj.JD);
            Header.setVal(Obj.DictKeyNames.TimeZone,    Obj.TimeZone);
            Header.setVal(Obj.DictKeyNames.Filter,      Obj.Filter);
            Header.setVal(Obj.DictKeyNames.FieldID,     Obj.FieldID);
            Header.setVal(Obj.DictKeyNames.Counter,     Obj.Counter);
            Header.setVal(Obj.DictKeyNames.CCDID,       Obj.CCDID);                        
            Header.setVal(Obj.DictKeyNames.CropID,      Obj.CropID);
            Header.setVal(Obj.DictKeyNames.Type,        Obj.Type);
            Header.setVal(Obj.DictKeyNames.Level,       Obj.Level);
            Header.setVal(Obj.DictKeyNames.SubLevel,    Obj.SubLevel);
            Header.setVal(Obj.DictKeyNames.Product,     Obj.Product);
            Header.setVal(Obj.DictKeyNames.Version,     Obj.Version);
            Header.setVal(Obj.DictKeyNames.FileType,    Obj.FileType);
            Header.setVal(Obj.DictKeyNames.SubDir,      Obj.SubDir);            
            
            Result = true;
        end        
        
        function Result = readFromDb(Obj, Query)
            % Read data from database table, current record of Query.ResultSet
            % Fields are defined in Google Sheet "common_image_path"
            arguments
                Obj
                Query io.db.DbQuery
            end            
            
            %Obj.msgLog(LogLevel.Debug, 'readFromDb: ');
            st = Query.getRecord();
            Result = Obj.readFromStruct(st);
        end
        
    end
    
    
    methods % search and utilities
        function I = findFirstLast(Obj, IsLast, ProductName)
            % find image, of some product type, with latest/earliest JD
            % Input  : - An ImagePath object.
            %          - Search for last (true), first (false) image name.
            %          - Select only products of this type.
            %            Default is 'Image'.
            % Output : - The index of the selected ImagePath element.
            %            Empty if no JD, or no images.
            % Author : Eran Ofek (Jan 2022)
            % Example: IP = ImagePath(2);
            %          I = findFirstLast(IP);
            
            arguments
                Obj
                IsLast logical    = true;
                ProductName       = 'Image';
            end
            
            Ind = find(strcmp({Obj.Product}, ProductName));
            
            if IsLast
                [~,I] = max([Obj(Ind).JD]);
            else
                [~,I] = min([Obj(Ind).JD]);
            end
            I = Ind(I);
        end
        
        function [Obj, SI] = sortByJD(Obj)
            % Sort ImagePath object by JD
            % Input  : - An ImagePath object.
            % Output : - An ImagePath object, where the elements are sorted
            %            by JD.
            %          - sorted indices.
            % Author : Eran Ofek (Jan 2022)
            
            [~,SI] = sort([Obj.JD]);
            Obj    = Obj(SI);
        end     
        
        function Ind = getAllProductsFromImageName(Obj, FileName)
            % Given an ImagePath and an image name (or index), return all the corresponding products 
            % Input  : - An ImagePath object.
            %          - A file name or an ImagePath element index.
            % Output : - The indices in the ImagePath object of elements for
            %            which the image name, with the exception of the
            %            Product name, is identical to the input FileName.
            % Author : Eran Ofek (Jan 2022)
            % Example: IP = ImagePath;
            %          FN  = IP.genFile;
            %          FN1 = strrep(FN, '_Image_', '_Mask_');
            %          IP.Filter = 'AA'; FN2 = IP.genFile;
            %          F   = {FN, FN1, FN2};
            %          IP  = ImagePath.parseFileName(F);
            %          Ind = getAllProductsFromImageName(IP, FN)
            
            if ischar(FileName)
                Ind = find(strcmp({Obj.FileName}, FileName));
            else
                % FileName is already an image index
                if isinf(FileName)
                    Ind = numel(Obj);
                else
                    Ind = FileName;
                end
            end
            
            Obj.genFile;
            AllNames    = {Obj.FileName};
            Splitted    = split(AllNames{Ind}, '_');
            % Replace the Product name with '\w*'
            Splitted{10} = '\w*';
            Template     = tools.string.unsplit(Splitted, '_');
            
            Match = regexp(AllNames, Template, 'match');
            Ind = find(~cellfun(@isempty, Match));
            
        end
        
        function [Ind, IndCounter] = getAllInCounterSeries(Obj, FileName, ModNumber)
            % Return indices of a series of images belonging to the same counter series (sorted by JD).
            % Given an ImagePath and a file name or an index of an element
            %   in ImagePath:
            %   sort by JD,
            %   search for the indices (in the sorted list) of all the
            %   images that belongs to the same counter series.
            %   For example, given a set of images with counter = [19    20     1     2     3     4] 
            %   and the last counter is an ancor, will return 3:6.
            % Input  : - An ImagePath object.
            %          - Either a file name, or an index of the image in
            %            the ImagePath object.
            %            If Inf then will set the index to the last element
            %            in ImagePath.
            %            If this is empty, then the output index will be
            %            empty.
            %          - A scalar. For the calculations the counter will be
            %            replace by the mod(Counter, This_Number).
            %            Default is Inf.
            % Output : - Indices of all the elements in the sorted
            %            ImagePath that belong to the series that contains
            %            the input image name. The input image name is
            %            always the last element in the series.
            %            NOTE that the ImagePath object will be sorted by
            %            JD.
            %          - Vector of image counters.
            % Author : Eran Ofek (Jan 2022)
            % Example: 
            
            arguments
                Obj
                FileName
                ModNumber = Inf;
            end
            
            Obj.genFile;
            
            [~,SI] = Obj.sortByJD;
            if ischar(FileName)
                Ind = find(strcmp({Obj.FileName}, FileName));
            else
                % FileName is already an image index
                if isinf(FileName)
                    Ind = numel(Obj);
                else
                    %Ind = FileName;
                    Ind = SI(FileName);
                end
            end
            
            if ~isempty(Ind)
                AllCounter = mod([Obj(1:Ind).Counter], ModNumber+1);

                AllCounter = [Inf, AllCounter, 0];
                I = find(diff(AllCounter)<0, 2, 'last');
                Ind = (I(1):I(2)-1);
                
                IndCounter = [Obj(Ind).Counter];
            end
        end
        
        function Obj = setAllVal(Obj, Prop, Val)
            % Set the value of one of the properties in all the elements.
            % Input  : - An ImagePath object.
            %          - Property name.
            %          - Value to set.
            % Output : - The updated ImagePath object.
            % Author : Eran Ofek (Jan 2022)
            % Example: Obj = setAllVal(Obj, 'FormatCounter', '%d');
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).(Prop) = Val;
            end
        end
        
        function [SunAlt] = sunAlt(Obj, Args)
            % Calculate Sun Altitude for images in ImagePath object
            % Input  : - An ImagePath object
            %          * ...,key,val,...
            %            'GeoPos' - Geodetic position [Lon, Lat] in deg.
            %                   Default is [35 30].
            % Output : - An array of Sun altitude (deg) for each image
            %            entry.
            % Author : Eran Ofek (May 2022)
            % Example: % delete all images taken when the Sun is up
            %          List=io.files.filelist('LAST*.fits');                         
            %          IP=ImagePath.parseFileName(List);
            %          SA=IP.sunAlt;
            %          I=find(SA>0);
            %          for ii=1:numel(I), delete(IP(I(ii)).genFile); end    
            
            arguments
                Obj
                Args.GeoPos    = [35 30];
            end
            
            RAD = 180./pi;
            
            VecJD    = [Obj.Time];
            VecJD    = VecJD(:);
            LST      = celestial.time.lst(VecJD, Args.GeoPos(1)./RAD);  % frac of day
            [RA,Dec] = celestial.SolarSys.suncoo(VecJD, 'j'); % [rad]
            HA       = LST.*2.*pi - RA;                       % [rad]
            [SunAz,SunAlt] = celestial.coo.hadec2azalt(HA, Dec, Args.GeoPos(2)./RAD); % [rad]
            SunAlt   = SunAlt.*RAD; 
            
        end
    end
    
    methods % raed/write from stuct
        function Result = readFromStruct(Obj, st)
            % Read data from struct or DbRecord (common_image_path table)
            % Struct field names should match database fields
            Obj.Telescope       = st.tel;
            Obj.Node            = st.node;
            Obj.Mount           = st.mount;
            Obj.Camera          = st.camera;
            
            Obj.JD              = st.jd;
            Obj.TimeZone        = st.timezone;
            Obj.Filter          = st.filter;
            Obj.FieldID         = st.field_id;
            Obj.CropID          = st.crop_id;
            Obj.ImageType       = st.imtype;
            Obj.ImageLevel      = st.imlevel;
            Obj.ImageSubLevel   = st.imslevel;
            Obj.ImageProduct    = st.improd;
            Obj.ImageVer        = st.imver;
            Obj.FileType        = st.filetype;
            Result = true;
        end
        
        function st = writeToStruct(Obj)
            % Write data fields to struct (common_image_path table)            
            st = struct;
            st.tel      = Obj.Telescope;
            st.node     = Obj.Node;
            st.mount    = Obj.Mount;
            st.camera   = Obj.Camera;
            st.jd       = Obj.JD;
            st.timezone = Obj.TimeZone;
            st.filter   = Obj.Filter;
            st.field_id = Obj.FieldID;
            st.crop_id  = Obj.CropID;
            st.imtype   = Obj.ImageType;
            st.imlevel  = Obj.ImageLevel;
            st.imslevel = Obj.ImageSubLevel;
            st.improd   = Obj.ImageProduct;
            st.imver    = Obj.ImageVer;
            st.filetype = Obj.FileType;
        end        
    end
    
        
    methods % Helpers (for internal use)
        
        function Result = setTime(Obj)
            % Set JD and TimeStr from Obj.Time
            % Input:  Obj.Time
            % Output: Obj.DT, Obj.TimeStr
            % @Todo: Convert to static? or move to convert.time?
            
            % Empty: use current time
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                
                if isempty(Obj(Iobj).Time)
                    Obj(Iobj).Time = celestial.time.julday;
                end

                % Convert number to string, set JD, TimeStr (assume JD)
                if isnumeric(Obj(Iobj).Time)                
                    StrDate = convert.time(Obj(Iobj).Time, 'JD', 'StrDate');
                    Obj(Iobj).TimeStr = StrDate{1};
                    Obj(Iobj).JD = Obj(Iobj).Time;
                elseif ischar(Obj(Iobj).Time)
                    Obj(Iobj).TimeStr = Obj(Iobj).Time;
                    Obj(Iobj).JD = convert.time(Obj(Iobj).Time, 'StrDate', 'JD');
                elseif iscellstr(Obj(Iobj).Time)
                    Obj(Iobj).TimeStr = Obj(Iobj).Time{1};
                    Obj(Iobj).JD = convert.time(Obj(Iobj).Time, 'StrDate', 'JD');
                end

                % Remove '-' and ':' from date (StrDate: 'YYYY-MM-DDTHH:MM:SS.FFF')
                Obj(Iobj).TimeStr = strrep(Obj(Iobj).TimeStr, '-', '');
                Obj(Iobj).TimeStr = strrep(Obj(Iobj).TimeStr, 'T', '.');
                Obj(Iobj).TimeStr = strrep(Obj(Iobj).TimeStr, ':', '');
            end
            
            Result = true;
        end
            
        function Result = fixFields(Obj)
            % Fix field values: type, level, sublevel, product
            Obj.Type  = lower(Obj.Type);
            Obj.Level = lower(Obj.Level);
            Result = true;
        end
        
        function Result = valiadateFields(Obj)
            % Validate fields: Type, Level, Product

            Result = true;
            
            % Verify Type
            %Obj.msgLog(LogLevel.Debug, 'valiadateFields: Type=%s', Obj.Type);
            switch Obj.Type
                case { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'focus', 'sci', 'science', 'wave', 'type' }
                    % Ok
                otherwise
                    error('Unknown Type option: %s', Obj.Type);
            end

            % Verify Level
            %Obj.msgLog(LogLevel.Debug, 'valiadateFields: Level=%s', Obj.Level);
            switch Obj.Level
                case {'log', 'raw', 'proc', 'stacked', 'ref', 'coadd', 'merged', 'calib', 'junk'}
                    % Ok
                otherwise
                    error('Unknown Level option: %s', Obj.Level);
            end

            % Verify Product
            %Obj.msgLog(LogLevel.Debug, 'valiadateFields: Product=%s', Obj.Product);
            switch Obj.Product
                case { 'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat', 'Asteroids'}
                    % Ok
                otherwise
                    error('Unknown Product option: %s', Obj.Product);
            end
            
        end
        
        function Result = formatNumeric(Obj, Value, Format)
            % connvert numeric to string
            
            if ischar(Value)
                if strcmpi(Value, 'nan')
                    Result = '';
                else
                    Result = Value;
                end
            else
                if isempty(Format)
                    Result = '';
                else
                    Result = sprintf(Format, Value);
                end
            end

        end
    end    
    
    methods (Static) % Parsers
              
        %function Result = parsePath(Obj, Path)
        %    % Convert a string containing a path to a structure with all available information (e.g., date, type, level, fieldID, ProjName)
        %end
        
        
        function Obj = parseFileName(FileName)
            % Populate ImagePath object from file name.
            % Input  : - A file name, or a cell array of file names.
            % Output : - A populated ImagePath object based on the file
            %            name only.
            % Author : Eran Ofek (Jan 2022)
            % Example:
            % IP=ImagePath.parseFileName('LAST_20220118.193339.010_clear_____sci_raw_Image_1.fits')
            
            if ischar(FileName)
                FileName = {FileName};
            end
            Nf = numel(FileName);
            
            Obj = ImagePath(Nf);
            for If=1:1:Nf
                [Path, File, Ext]  = fileparts(FileName{If});
                Obj(If).FileName = File;
                Obj(If).FullName = FileName{If};
                Obj(If).Path     = Path;
                
                Obj(If).FileType = Ext(2:end);
                Parts = split(File,'_');
                % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
                Obj(If).ProjName = Parts{1};
                if numel(Parts)>1
                    VecTime = datevec(Parts{2}, 'yyyymmdd.HHMMSS.FFF');
                    Obj(If).Time     = celestial.time.julday(VecTime([3 2 1 4 5 6]));
                else
                    Obj(If).Time     = [];
                end
                if isempty(Parts{3})
                    Obj(If).Filter = '';
                else
                    Obj(If).Filter   = Parts{3};
                end
                if isempty(Parts{4})
                    Obj(If).FieldID = '';
                else
                    Obj(If).FieldID  = Parts{4};
                end
                Obj(If).Counter  = str2double(Parts{5});
                Obj(If).CCDID    = Parts{6};
                Obj(If).CropID   = Parts{7};
                Obj(If).Type     = Parts{8};
                PartsLevel   = split(Parts{9}, '.');
                Obj(If).Level    = PartsLevel{1};
                if numel(PartsLevel)>1
                    Obj(If).SubLevel = PartsLevel{2};
                else
                    Obj(If).SubLevel = '';
                end
                Obj(If).Product  = Parts{10};
                Obj(If).Version  = Parts{11};
                
            end
        end
        
        function [List, Flag, IP] = selectByDate(Files, StartDate, EndDate)
            % Select files in some range of dates and times.
            % Input  : - A file name with possible wild cards, or a cell
            %            array of file names.
            %            Default is '*.fits'.
            %          - Start date (JD, [Y M D H M S], or date string).
            %            Default is -Inf.
            %            If empty, then select all.
            %          - End date (JD, [Y M D H M S], or date string).
            %            Default is Inf.
            % Output : - A list of selected files which dates is between
            %            the start and end dates.
            %          - A logical vector of selected files.
            %          - FileNames object
            % Author : Eran Ofek (Jun 2022)
            % Example: ImagePath.selectByDate('PTF*.fits');
            
            arguments
                Files       = '*.fits';
                StartDate   = -Inf;
                EndDate     = Inf;
            end
           
            if ischar(Files)
                List = io.files.filelist(Files);
            else
                List = Files;
            end
           
            if isempty(StartDate)
                % do nothing
                Flag = [];
                if nargout>2
                    IP   = FileNames.generateFromFileName(Files);
                end
            else
                if numel(StartDate)==1
                    StartJD = StartDate;
                else
                    StartJD = celestial.time.julday(StartDate);
                end
                if numel(EndDate)==1
                    EndJD = EndDate;
                else
                    EndJD = celestial.time.julday(EndDate);
                end

                IP = ImagePath.parseFileName(List);

                Flag = [IP.Time]>StartJD & [IP.Time]<EndJD;
                List = List(Flag);
            end
        end
       
        function [IP, List, Flag] = selectByProp(Files, PropVal, Prop, IsVal)
            % Select files with ImagePath format by some property (e.g., Type).
            % Input  : - If this is a char array than use io.files.filelist
            %            to generate a cell array of file names.
            %            Alternatively, this is a cell array of file names.
            %          - Image property value to select (or ignore).
            %            Default is {'sci','science'}.
            %          - Property name to select by (e.g., 'Type','Level').
            %            Default is 'Type'.
            %          - A logical flag indicating if to use files of
            %            the specified property value. If false, then
            %            ignore this value and select all the rest.
            %            Default is true.
            % Output : - An ImagePath pbject with the selected image.
            %          - A cell array of selected files.
            %          - A vector of logical flags indicating the selected
            %            files.
            % Author : Eran Ofek (Aug 2022)
            % Example: [IP,List] = ImagePath.selectByProp('LAST*.fits', {'focus'}, 'Type')
            % Example: [IP,List] = ImagePath.selectByProp('LAST*.fits', {'focus'}, 'Type',false)
           
            arguments
                Files
                PropVal              = {'sci','science'};
                Prop                 = 'Type';
                IsVal logical        = true;
            end
            
            if ischar(Files)
                List = io.files.filelist(Files);
            else
                List = Files;
            end
            
            IP   = ImagePath.parseFileName(List);
            Flag = ismember({IP.(Prop)}, PropVal);
            if ~IsVal
                Flag = ~Flag;
            end
            
            IP   = IP(Flag);
            List = List(Flag);
            
        end
        
        function [Gr, List] = groupByCounter(Files, MinInGroup, MaxInGroup)
            % Group ImagePath file names by counter groups
            % Input  : - If this is a char array than use io.files.filelist
            %            to generate a cell array of file names.
            %            Alternatively, this is a cell array of file names,
            %            or a populated ImagePath object.
            %          - Maximum number of files in group. Default is 20.
            % Output : - A structure array with element per group and the
            %            following fields:
            %            .I1 - Index in List (second output) of first
            %                   element in group.
            %            .I2 - Index of last element in group.
            %          - The sorted (by date) list of files.
            % Author : Eran Ofek (Aug 2022)
            % Example: [St, List] = ImagePath.groupByCounter('LAST*.fits');
            
            arguments
                Files
                MinInGroup = 10;
                MaxInGroup = 20;
            end
            
            if isa(Files, 'ImagePath')
                IP = Files;
                List = IP.genFullCell;
            else
                if ischar(Files)
                    List = io.files.filelist(Files);
                else
                    List = Files;
                end

                IP     = ImagePath.parseFileName(List);
            end
            % sort by time
            [~,SI] = sort([IP.Time]);
            List   = List(SI);
            IP     = IP(SI);
            Counter = [IP.Counter];
            
            Gr = tools.find.groupCounter(Counter, 'MinInGroup',MinInGroup, 'MaxInGroup',MaxInGroup);
            
        end
        
    end


    methods % write product
        function ObjIP = writeProduct(ObjIP, ObjProduct, Args)
            % Write an array of data prodicts to disk.
            % Input  : - An ImagePath object array.
            %          - A data product (e.g., an AstroImage). If this is
            %            an AstroImage then the number of elements must be
            %            equal to the number of elements in the first input
            %            argument.
            %          * ...,key,val,...
            %            'Save' - A logical indicating if to save the
            %                   products. Default is true.
            %            'SaveFields' - Fields to save.
            %                   Default is {'Image','Mask','Cat'}.
            %            'WriteFun' - Default is @write1.
            %                   Fun(Object, Name, Field, WriteFunArgs{:})
            %            'WriteFunArgs' - Cell array of arguments to pass
            %                   to the WriteFun.
            %                   Default is {'IsSimpleFITS',true,
            %                   'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false}.
            %            'Product' - For a non AstroImage object. This is
            %                   the data type that will be written.
            %                   Default is 'Asteroids'.
            %            'Level' - Overide level. If empty, do not
            %                   override. Default is [].
            % Output : - The ImagePath object (without changes).
            % Author : Eran Ofek (Feb 2022)
            %
            
            arguments
                ObjIP
                ObjProduct
                Args.Save
                Args.SaveFields                = {'Image','Mask','Cat'};
                Args.WriteFun function_handle  = @write1;
                Args.WriteFunArgs cell         = {'IsSimpleFITS',true, 'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
                Args.Product                   = 'Asteroids';   %'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat', 'Asteroids'
                Args.Level                     = []; % override Level
                Args.FileType                  = []; % override FileType
            end
            
            if Args.Save
                % verify that directory exist
                mkdir(ObjIP(1).genPath);

                Nfield  = numel(Args.SaveFields);
                Nprod   = numel(ObjProduct);
                if isa(ObjProduct, 'AstroImage')
                    for Iprod=1:1:Nprod
                        if ~isempty(Args.Level)
                            ObjIP(Iprod).Level = Args.Level;
                        end
                        if ~isempty(Args.FileType)
                            ObjIP(Iprod).FileType = Args.FileType;
                        end
                        for Ifield=1:1:Nfield
                            ObjIP(Iprod).Product = Args.SaveFields{Ifield};
                            Args.WriteFun(ObjProduct(Iprod), ObjIP(Iprod).genFull, Args.SaveFields{Ifield}, Args.WriteFunArgs{:});
                        end
                    end
                elseif isa(ObjProduct, 'AstroCatalog')
                    % save AstroCatalog
                    switch Args.SaveFields{1}
                        case 'Cat'
                            for Iprod=1:1:Nprod
                                if ~isempty(Args.Level)
                                    ObjIP(Iprod).Level = Args.Level;
                                end
                                if ~isempty(Args.FileType)
                                    ObjIP(Iprod).FileType = Args.FileType;
                                end
                                ObjIP(Iprod).Product = 'Cat';
                                Args.WriteFun(ObjProduct(Iprod), ObjIP(Iprod).genFull, Args.WriteFunArgs{:});
                            end
                    end
                elseif isa(ObjProduct, 'MatchedSources')
                    % save MatchedSources
                    switch Args.SaveFields{1}
                        case 'Cat'
                            for Iprod=1:1:Nprod
                                if ~isempty(Args.Level)
                                    ObjIP(Iprod).Level = Args.Level;
                                end
                                if ~isempty(Args.FileType)
                                    ObjIP(Iprod).FileType = Args.FileType;
                                end
                                ObjIP(Iprod).Product = 'MergedMat';
                                Args.WriteFun(ObjProduct(Iprod), ObjIP(Iprod).genFull, Args.WriteFunArgs{:});
                            end
                    end
                else
                    % save as MAT file
                    IP1 = ObjIP(1).copy;
                    IP1.Counter   = 0;
                    IP1.CropID    = 0;
                    IP1.Product   = Args.Product;
                    IP1.FileType  = 'mat';

                    save('-v7.3',IP1.genFull, 'ObjProduct');
                end
            end
        end  
            
        function [Future, ObjIP] = saveProduct(ObjIP, ObjProduct, Args) 
            % Save product to disk
            % Input  : - An ImagePath object
            %          - An object to save (e.g., AstroImage)
            %          * ...,key,val,...
            %            'Save' - A logical indicating if to save the
            %                   pdoducts. Default is true.
            %            'ParEval' - Use parfeval. Default is false.
            %            'SaveFun' - A function handle which is a method of
            %                   the the object product used to save products.
            %                   Default is @write1.
            %            'SaveFunArgs' - A cell array of additional
            %                   arguments to pass to the 'SaveFun'.
            %                   Default is {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false}
            %            'PropFromHeader' - Attempt to populate the
            %                   ImagePath properties from the Header.
            %                   Default is true.
            %            'CropID_FromInd' - If true, then CropID is taken
            %                   from object element index. Default is false.
            %            'SetProp' - Pairs of additional ImagePath properties to
            %                   populated (key,val). This is overriding the
            %                   Header properties.
            %                   Default is {'Product','Image'}.
            %            'DataDirFromProjName' - Set DataDir value from
            %                   ProjName. Default is false.
            % Output : - A future object (for parfeval).
            %          - The updated ImagePath object.
            % Author : Eran Ofek (Jan 2022)
            
            
            arguments
                ObjIP
                ObjProduct
                Args.Save logical              = true;
                Args.ParEval logical           = false;
                Args.SaveFun function_handle   = @write1;
                Args.SaveFunArgs cell          = {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
                Args.PropFromHeader logical    = true;
                Args.CropID_FromInd logical    = false;
                Args.SetProp cell              = {'Product','Image'};   % overide header
                Args.DataDirFromProjName logical = false;
            end
            Future = [];
            if Args.Save

                % FFU:
                % create dir
                % use parfeval

                if Args.ParEval
                    Future = parfeval(@ImagePath.saveProductBlocking, 0, ObjIP, ObjProduct, 'SaveFun',Args.SaveFun, 'SaveFunArgs',Args.SaveFunArgs, 'PropFromHeader',Args.PropFromHeader, 'SetProp',Args.SetProp);
                else
                    ObjIP = ImagePath.saveProductBlocking(ObjIP, ObjProduct,...
                                                         'SaveFun',Args.SaveFun,...
                                                         'SaveFunArgs',Args.SaveFunArgs,...
                                                         'PropFromHeader',Args.PropFromHeader,...
                                                         'CropID_FromInd',Args.CropID_FromInd,...
                                                         'SetProp',Args.SetProp,...
                                                         'DataDirFromProjName',Args.DataDirFromProjName);
                end

            end
        end
    end
            
    methods (Static) % utilities
        function ObjIP = saveProductBlocking(ObjIP, ObjProduct, Args)
            % Save product to disk - utility blocking function
            %   This function is for the internal use by
            %   ImagePath/saveProduct
            % Input  : - A single-element ImagePath object.
            %          - Object product to save (e.g., an AstroImage)
            %          * ...,key,val,...
            %            'SaveFun' - Function handle for saving object
            %                   Default is @write1
            %            'SaveFunArgs' - A cell array of additional arguments to pass
            %                   to the SaveFun function.
            %                   Default is {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
            %            'PropFromHeader' - A logical indicating if to
            %                   popuAlate ImagePath properties from image header.
            %                   Default is true.
            %            'CropID_FromInd' - If true, then CropID is taken
            %                   from object element index. Default is false.
            %            'SetProp' - A cell array of pairs of additional
            %                   ImagePath properties to set (override
            %                   header). These are ...Prop,val,...
            %                   Default is  {'Product','Image'}
            %            'DataDirFromProjName' - Set DataDir value from
            %                   ProjName. Default is false.
            % Output : The updated ImagePath object.
            % Author : Eran Ofek (Jan 2022)
            % Example: 

            arguments
                ObjIP(1,1) ImagePath
                ObjProduct
                Args.SaveFun function_handle   = @write1;
                Args.SaveFunArgs cell          = {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
                Args.PropFromHeader logical    = true;
                Args.CropID_FromInd logical    = false;
                Args.SetProp cell              = {'Product','Image'};   % overide header
                Args.DataDirFromProjName logical = false;
            end

            NsetProp = numel(Args.SetProp);
            if (0.5.*NsetProp)~=floor(0.5.*NsetProp)
                % odd number of SetProp
                error('Number of elements in SetProp argument must be even (key,val)');
            end

            Nprod = numel(ObjProduct);
            for Iprod=1:1:Nprod
                if Args.PropFromHeader
                    ObjIP.readFromHeader(ObjProduct(Iprod));  
                end
                for Iset=1:2:NsetProp
                    ObjIP.(Args.SetProp{Iset}) = Args.SetProp{Iset+1};
                end
                if Args.CropID_FromInd
                    ObjIP.CropID = Iprod;
                end
                if Args.DataDirFromProjName
                    ObjIP.DataDir = ObjIP.ProjName;
                end

                FullName = ObjIP.genFull;
                if Iprod==1
                    % create Dir
                    Path = fileparts(FullName);
                    if ~isfolder(Path)
                        %exist(Path,'dir')==0
                        % create dir
                        mkdir(Path);
                    end
                end
                    
                Args.SaveFun(ObjProduct(Iprod), FullName, Args.SaveFunArgs{:});
            end
            
        end
        
        
        
        function ObjIP = generateImagePathFromProduct(ObjProduct, Args)
            % Generate an ImagePath object from product (e.g., AstroImage)
            % Input  : - A product object (e.g., AstroImage).
            %          * ...,key,val,...
            %            'PropFromHeader' - A logical indicating if to
            %                   popuAlate ImagePath properties from image header.
            %                   Default is true.
            %            'CropID_FromInd' - If true, then CropID is taken
            %                   from object element index. Default is false.
            %            'SetProp' - A cell array of pairs of additional
            %                   ImagePath properties to set (override
            %                   header). These are ...Prop,val,...
            %                   Default is  {'Product','Image'}
            %            'DataDirFromProjName' - Set DataDir value from
            %                   ProjName. Default is false.
            % Output : - An ImagePath object populated based on product
            %            data.
            % Author : Eran Ofek (Feb 2022)
            % Example: IP = ImagePath.generateImagePathFromProduct(AllSI(1));
            
            arguments
                ObjProduct
                Args.PropFromHeader logical      = true;
                Args.CropID_FromInd logical      = false;
                Args.SetProp cell                = {'Product','Image'};   % overide header
                Args.DataDirFromProjName logical = false;
                Args.AutoSubDir logical          = true;
            end
           
            NsetProp = numel(Args.SetProp);
            if (0.5.*NsetProp)~=floor(0.5.*NsetProp)
                % odd number of SetProp
                error('Number of elements in SetProp argument must be even (key,val)');
            end            
                        
            Nprod = numel(ObjProduct);
            ObjIP = ImagePath(Nprod);
            CopySubDirFromPrevious = false;
            for Iprod=1:1:Nprod
                if Args.PropFromHeader
                    ObjIP(Iprod).readFromHeader(ObjProduct(Iprod));  
                end
                for Iset=1:2:NsetProp
                    ObjIP(Iprod).(Args.SetProp{Iset}) = Args.SetProp{Iset+1};
                end
                if Args.CropID_FromInd
                    ObjIP(Iprod).CropID = Iprod;
                end
                if Args.DataDirFromProjName
                    ObjIP(Iprod).DataDir = ObjIP.ProjName;
                end
                
                if Args.AutoSubDir && ~CopySubDirFromPrevious
                    % automatically set the SubDir directory : +1 to
                    % largest existing dir
                    FullPath = ObjIP(Iprod).genPath;
                    FullPath = strrep(FullPath,sprintf('%sNaN%s',filesep,filesep),'');
                    
                    if isfolder(FullPath)
                        FL       = dir(FullPath);
                        Flag     = [FL.isdir]' & ~strcmp({FL.name}.','.') & ~strcmp({FL.name}.','..');
                        FL       = FL(Flag);
                        if isempty(FL)
                            % no dirs
                            ObjIP(Iprod).SubDir = '1';
                        else
                            Max = max(str2double({FL.name}));
                            if isnan(Max)
                                ObjIP(Iprod).SubDir = '1';
                            else
                                ObjIP(Iprod).SubDir = sprintf('%d',Max + 1);
                            end
                        end
                        
                    else
                        ObjIP(Iprod).SubDir = '1';
                    end
                else
                    if CopySubDirFromPrevious
                        ObjIP(Iprod).SubDir = ObjIP(Iprod-1).SubDir;
                    end
                end
                    

%                FullName = ObjIP.genFull;
%                 if Iprod==1
%                     % create Dir
%                     Path = fileparts(FullName);
%                     %if exist(Path,'dir')==0
%                     %    % create dir
%                     %    mkdir(Path);
%                     %end
%                 end
            end
        end
        
    end
            
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
