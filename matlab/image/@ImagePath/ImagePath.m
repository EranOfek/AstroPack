
classdef ImagePath < Component
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
        
        ProjName        = '';           % Examples: “ULTRASAT”, “LAST.1.12.4” (LAST node=1, mount=12, camera=4)
        Time            = [];           % Empty: current time, Numeric: time as JD, char: YYYY-MM-DDTHH:MM:SS.FFF, cell: {YYYY, MM, DD}
        TimeZone        = 2;            % Bias in hours, to generate folder name                
        Filter          = 'clear';      % Filter name
        FieldID         = '';           % Sky position identifier, like field ID, object name, CCD I’d, sub image ID etc. May include CropId. Examples: 314.1.1 is field I’d 314, CCD 1, sub image 1.
        Counter         = '';           % Counter
        CCDID           = '';           % CCD ID
        CropID          = '';           % Used with sub-images
        Type            = 'sci';        % [lower] sci, bias, dark, domeflat, twflat, skyflat, fringe
        Level           = 'raw';        % [lower] log, raw, proc, stack, coadd, merged, ref.
        SubLevel        = '';           % Sublevel, see below:
            % SubLevel: n - normal, s - proper subtraction S, sp - proper subtraction S tag, d - proper subtraction D, t - Translient, r - proper coaddition R, m - matched filter with unspecified filter
            % SubLevel: Single capital letter prefix may be added to this name: F - Fourier Transform, R - Radon Transform, L - Laplacian, G - x gradient, H - y gradient. 
        Product         = 'Image';      % Product: Image, Back, Var, Exp, Nim, PSF, Cat, Spec, Mask, Evt, MergedMat
        Version         = '1';          % Version (for multiple processing)
        FileType        = 'fits';       % fits / hdf5 / fits.gz          
        Area            = '';           % Used by genPath()
        SubDir          = '';           % This is the area/location directory below the coadd/ref directory
        
        % Fields formatting
        FormatFieldID   = '%06d';       % Used with FieldID
        FormatCounter   = '%03d';       % Used with Counter        
        FormatCCDID     = '%03d';       % Used with CCDID
        FormatCropID    = '%03d';       % Used with CropID
        FormatVersion   = '%03d';       % Used with Version
        
        % @Todo: Defaults should be loaded from configuration
        BasePath        = '/euler/archive'; % Loaded from Config
        DataDir         = 'LAST';           % Loaded from Config
    end

    properties(Hidden, SetAccess=protected, GetAccess=public)
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
       
        function Obj = ImagePath(varargin)
            % Constructor
            
            % Load defaults from configuration
            % i.e.
            Obj.BasePath = Obj.Config.Data.System.ImagePath.BasePath;
            Obj.DataDir  = Obj.Config.Data.System.ImagePath.DataDir;
            Obj.TimeZone = Obj.Config.Data.System.Time.TimeZone;
            
            % Load header key names mapping from configuation
            Obj.DictKeyNames = Dictionary.getDict('Header.ImagePath.KeyNames');
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
    
      
    methods % Generate Path & FileName
        
        function Result = genPath(Obj, Args)
            % Construct image/catalog file path based on the LAST/ULTRASAT standard
            % Options are:
            %
            % Form 1:
            % /data/YYYY/MM/DD/raw/     - contains all the science raw data
            % /data/YYYY/MM/DD/log/     - contains all the log files
            % /data/YYYY/MM/DD/proc/    - contains all the single processed images including: image, mask, back (if provided), var (if provided), PSF (if provided), and catalogs.
            % /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction products 
            %            
            % Form 2: 
            % /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF
            %
            % Form 3: 
            % /data/coadd/area/          - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods)             
            %
            % Form 4:
            % /data/YYYY/MM/DD/calib/   - contains all the processed calibration images/variance/masks/catalogs            
            %
            % Example: Path=imUtil.util.file.genPath('Level','ref','SubDir','x')
            %
            arguments
                Obj
                Args.BasePath       % See constructor
                Args.DataDir        % See constructor
                Args.SubDir         %
                Args.Time           % Empty -> current computer time, UTC, Numeric -> time is in JD, char -> YYYY-MM-DDTHH:MM:SS.FFF
%      Should match 'convert'             format.Date, TimeZone} or {YYYY, MM, DD}, or []}
                Args.TimeZone       % Hours
                Args.Level          % Also in file name
                Args.Area
            end
            
            % Set properties from arguments, only properties that exist in Args are set
            Obj.setProps(Args);
                        
            % Convert Time to JD and TimeStr
            Obj.setTime();
            
            % Fix field values
            Obj.fixFields();
            
            % Generate YMD based on JD and TimeZone
            [Year, Month, Day] = imUtil.util.file.date_directory(Obj.JD, Obj.TimeZone);
            YMD = sprintf('%04d%s%02d%s%02d', Year, filesep, Month, filesep, Day);
                       
            UseYMD = false;
            PreDate = '';
            PostDate = '';

            % Check Level
            switch Obj.Level
                % /base/data/ref/<area>/version<#>/ - All sky reference/coadd image - images/masks/catalogs/PSF
                case { 'ref', 'coadd' }
                    PostDate = sprintf('%s%s%s%s', Obj.Level, filesep, Obj.Area, ...
                        filesep, Obj.Version);
                        
                case { 'raw', 'log', 'proc', 'stacked' }
                    UseYMD = true;                     
                    PostDate = Obj.Level;                    
                
                case { 'calib' }
                    PostDate = sprintf('%s%s%s', Obj.Level, filesep, Obj.SubLevel);
            
                otherwise
                    error('Unknown path Level: %s', Obj.Level);
                    
            end

            %
            if UseYMD
                FPath = sprintf('%s%s%s%s%s%s%s%s%s%s%s%s', Obj.BasePath, filesep, ...
                   Obj.DataDir, filesep, PreDate, filesep, YMD, filesep, PostDate, ...
                   filesep, Obj.SubDir, filesep);
            else
                FPath = sprintf('%s%s%s%s%s%s%s%s%s%s%s%s', Obj.BasePath, filesep, ...
                   Obj.DataDir, filesep, PostDate, filesep, filesep, Obj.SubDir, filesep);
                
            end                      
   
            % Clean path from multiple /
            FPath = strrep(FPath, '\', '/');            
            FPath = regexprep(FPath, sprintf('%s{2,5}', '/'), '/');
            
            %Obj.msgLog(LogLevel.Debug, 'Path: %s', FPath);
            Result = FPath;            
        end
        
        
        function Result = genFile(Obj, Args)
            % Construct image/catalog file name based on the LAST/ULTRASAT standard
            % Description: Return data product file name and path according to the
            %              LAST/ULTRASAT standard.
            %              <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>
            % Input  : * Pairs of ..., key,val, ... Possible keywords include:
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
            % Output : File name or path and file name (if Args.FullPath is true)
            % Example: FileName=imUtil.util.file.construct_filename
            %          [FileName,Path]=imUtil.util.file.construct_filename('FieldID',100)            
            % Returns: string containing image name 
            % Returns: string containing image path (if nargout>1, call constructPath)
            
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
            
            % Set properties from arguments, only properties that exist in Args are set
            Obj.setProps(Args);            
            
            % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
            % USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits
            
            % Set JD and TimeStr
            Obj.setTime();
            
            % Format numeric fields
            Obj.FieldID  = Obj.formatNumeric(Obj.FieldID, Obj.FormatFieldID);                   
            Obj.Counter  = Obj.formatNumeric(Obj.Counter, Obj.FormatCounter);            
            Obj.CCDID    = Obj.formatNumeric(Obj.CCDID, Obj.FormatCCDID);
            Obj.CropID   = Obj.formatNumeric(Obj.CropID, Obj.FormatCropID);
            Obj.Version  = Obj.formatNumeric(Obj.Version, Obj.FormatVersion);
            
            % Fix and validate field values
            Obj.fixFields();
            Obj.valiadateFields();
                       
            % Level / Level.SubLevel
            if isempty(Obj.SubLevel)
                MergedLevel = Obj.Level;
            else
                MergedLevel = sprintf('%s.%s', Obj.Level, Obj.SubLevel);    
            end

            % Prepare the final result
            % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
            Obj.FileName = sprintf('%s_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.%s', Obj.ProjName, ...
                           Obj.TimeStr, Obj.Filter, Obj.FieldID, Obj.Counter, Obj.CCDID, ...
                           Obj.CropID, Obj.Type, MergedLevel, Obj.Product, Obj.Version, Obj.FileType);

       
            % Get path
            Obj.Path = ''; 
            if Args.FullPath
                Obj.Path = Obj.genPath('Time', Obj.Time);
                Obj.FileName = fullfile(Obj.Path, Obj.FileName);
            end

            % Clean path from multiple /
            Obj.FileName = strrep(Obj.FileName, '\', '/');            
            Obj.FileName = regexprep(Obj.FileName, sprintf('%s{2,5}', '/'), '/');
            
            % Done
            %Obj.msgLog(LogLevel.Debug, 'FileName: %s', Obj.FileName);
            Result = Obj.FileName;

        end
        
        function Result = genFull(Obj, Args)
            % Generate a full file name + path from a populated ImagePath
            % Input  : - A populated ImagePath object.
            %         * ...,key,val,...
            %           'PathLevel' - Level value for the path only.
            %                   If empty do nothing. Use this to modify the
            %                   path only. Default is [].
            % Output : - A full path + file name.
            % Author : Eran Ofek (Nov 2021)
            % Example: IP.genFull
            
            arguments
                Obj
                Args.PathLevel  = [];  % [] - don't touch 
            end
        
            File = Obj.genFile;
            
            Level = Obj.Level;
            if ~isempty(Args.PathLevel)
                Obj.Level = Args.PathLevel;
            end
            Path = Obj.genPath;
            Obj.Level = Level;  % return lebel to original value
            
            Result = sprintf('%s%s',Path,File);
            
        end
        
        
        function Result = genFromDbQuery(Obj, Query)
            % Generate ImagePath current record of Query.ResultSet
            % @Todo
                
        end
        
    end
    
   
    methods % Read/Write from Header and Struct
        
        function Obj = readFromHeader(Obj, Input, DataProp)
            % Read ImagePath parameters from header.
            % Input  : - An ImagePath object.
            %          - A single element AstroImage or AstroHeader.
            %          - Either data property name or product name.
            % Output : - A populated ImagePath object.
            
            arguments
                Obj(1,1)
                Input(1,1)       % AstroHeader | AstroImage
                DataProp    = 'image';    % DataProp in AstroImage or Product name
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
              
            
            Obj.ProjName        = Header.getVal('INSTRUME'); %Obj.DictKeyNames.PROJNAME);
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
    
    
    methods % Additional
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
            if isempty(Obj.Time)
                Obj.Time = celestial.time.julday;
            end
            
            % Convert number to string, set JD, TimeStr (assume JD)
            if isnumeric(Obj.Time)                
                StrDate = convert.time(Obj.Time, 'JD', 'StrDate');
                Obj.TimeStr = StrDate{1};
                Obj.JD = Obj.Time;
            elseif ischar(Obj.Time)
                Obj.TimeStr = Obj.Time;
                Obj.JD = convert.time(Obj.Time, 'StrDate', 'JD');
            elseif iscellstr(Obj.Time)
                Obj.TimeStr = Obj.Time{1};
                Obj.JD = convert.time(Obj.Time, 'StrDate', 'JD');
            end

            % Remove '-' and ':' from date (StrDate: 'YYYY-MM-DDTHH:MM:SS.FFF')
            Obj.TimeStr = strrep(Obj.TimeStr, '-', '');
            Obj.TimeStr = strrep(Obj.TimeStr, 'T', '.');
            Obj.TimeStr = strrep(Obj.TimeStr, ':', '');
            
            Result = true;
        end
            
        function Result = fixFields(Obj)
            % Fix field values: type, level, sublevel, product
            Obj.Type = lower(Obj.Type);
            Obj.Level = lower(Obj.Level);
            Result = true;
        end
        
        function Result = valiadateFields(Obj)
            % Validate fields: Type, Level, Product

            Result = true;
            
            % Verify Type
            %Obj.msgLog(LogLevel.Debug, 'valiadateFields: Type=%s', Obj.Type);
            switch Obj.Type
                case { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'sci', 'science', 'wave' }
                    % Ok
                otherwise
                    error('Unknown Type option: %s', Obj.Type);
            end

            % Verify Level
            %Obj.msgLog(LogLevel.Debug, 'valiadateFields: Level=%s', Obj.Level);
            switch Obj.Level
                case {'log', 'raw', 'proc', 'stacked', 'ref', 'coadd', 'merged', 'calib'}
                    % Ok
                otherwise
                    error('Unknown Level option: %s', Obj.Level);
            end

            % Verify Product
            %Obj.msgLog(LogLevel.Debug, 'valiadateFields: Product=%s', Obj.Product);
            switch Obj.Product
                case { 'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat'}
                    % Ok
                otherwise
                    error('Unknown Product option: %s', Obj.Product);
            end
            
        end
        
        
        function Result = formatNumeric(Obj, Value, Format)
            if isnumeric(Value) && ~isempty(Format)
                Result = sprintf(Format, Value);
            else
                Result = Value;
            end

        end
    end
    
    
    methods % Parsers
              
        function Result = parsePath(path)
            % Convert a string containing a path to a structure with all available information (e.g., date, type, level, fieldID, ProjName)
        end
        
        
        function Result = parseFileName(fname)
            % Convert a file name string to a structure with all available information
        end
        
    end
            
            
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
