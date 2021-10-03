
classdef ImagePath < Component
    % Construct and parse (@Todo) image path used in storage, database, and headers.
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
        Type            = 'sci';        % sci, bias, dark, domeflat, twflat, skyflat, fringe
        Level           = 'raw';        % log, raw, proc, stack, coadd, ref.
        SubLevel        = '';           % Subleve, see below:
            % SubLevel: n - normal, s - proper subtraction S, sp - proper subtraction S tag, d - proper subtraction D, t - Translient, r - proper coaddition R, m - matched filter with unspecified filter
            % SubLevel: Single capital letter prefix may be added to this name: F - Fourier Transform, R - Radon Transform, L - Laplacian, G - x gradient, H - y gradient. 
        Product         = 'im';         % Product: im, back, var, imflag, exp, Nim, psf, cat, spec.
        Version         = '1';          % Version (for multiple processing)
        FileType        = 'fits';       % fits / hdf5 / fits.gz          
        Area            = '';           % Used by genPath()
        SubDir          = '';           % This is the area/location directory below the coadd/ref directory
        
        % Fields formatting
        FormatFieldID   = '%06d';       %
        FormatCCDID     = '03d';        % 
        FormatCropID    = '03d';        %
        FormatVersion   = '%03d';       %
        
        % Defaults should be loaded from configuration
        BasePath        = '/home/last'; % Base storage path
        DataPath        = 'data';       % Parent folder under BasePath       
    end
        
    properties(Hidden)
        % Generated from Obj.Time
        JD              = [];           % UTC start of exposure @Eran - UTC or JD???
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
            Obj.Product         = 'im';
            Obj.Version         = 'ver1';
            Obj.FileType        = 'fits';
            Obj.SubDir          = 'subdir';
            
            % Debug? or have it?
            Obj.BasePath        = '/home/last';
            Obj.DataPath        = 'data';                   

            ResultPath = '/home/last/data/2021/09/09/raw/';  % '/home/last/data/.../subdir'; %'/home/last/data/2021/10/03/raw/'
            ResultFileName = 'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits';
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
            % /data/YYYY/MM/DD/calib/   - contains all the processed calibration images/variance/masks/catalogs
            % /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction products 
            %
            % Form 2: 
            % /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF
            %
            % Form 3: 
            % /data/coadd/area/          - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods)             
            %
            % Example: Path=imUtil.util.file.genPath('Level','ref','SubDir','x')
            %
            arguments
                Obj
                Args.Base       = '/home/last';     %
                Args.DataDir    = 'data';           %
                Args.SubDir     = '';               %
                Args.Time       = [];               % Empty -> current computer time, UTC, Numeric -> time is in JD, char -> YYYY-MM-DDTHH:MM:SS.FFF
%      Should match 'convert'             format.Date, TimeZone} or {YYYY, MM, DD}, or []}
                Args.TimeZone   = 2;                % Hours
                Args.Level      = 'raw';            % Also in file name
                Args.Area       = '';               %
            end
            
            % Set properties from arguments, only properties that exist in Args are set
            Obj.setProps(Args);
                        
            % Convert Time to JD and TimeStr
            Obj.setTime();
            
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
                        
                case { 'raw', 'log', 'proc', 'stacked'}
                    UseYMD = true;                     
                    PostDate = Obj.Level;                    
                
                case { 'calib' }
                    PostDate = sprintf('%s%s%s', Obj.Level, filesep, Obj.SubLevel);
            
                otherwise
                    error('Unknown path Level: %s', Obj.Level);
                    
            end

            %
            if UseYMD
                FPath = sprintf('%s%s%s%s%s%s%s%s%s%s%s', Obj.BasePath, filesep, ...
                   Obj.DataPath, filesep, PreDate, filesep, YMD, filesep, PostDate, ...
                   filesep, Obj.SubDir);
            else
                FPath = sprintf('%s%s%s%s%s%s%s%s%s%s%s', Obj.BasePath, filesep, ...
                   Obj.DataPath, filesep, PostDate, filesep, filesep, Obj.SubDir);
                
            end                      
   
            % Clean path from multiple /
            FPath = strrep(FPath, '\', '/');            
            FPath = regexprep(FPath, sprintf('%s{2,5}', '/'), '/');
            
            Obj.msgLog(LogLevel.Debug, 'Path: %s', FPath);
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
            %            'Product' - either: im, back, var, imflag, exp, Nim, psf, cat, spec.
            %                   Default is 'im'.
            %            'Version' - Default is 1.
            
            %                   Default is '%03d'.
            %            'FileType' - Default is 'fits'.
            %            'SubDir' - This is the area/location directory below the
            %                   coadd/ref directory. Default is ''.
            %            'DataDir' - Default is 'data'.
            %            'Base' - Default is '/home/last'.
            
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
                Args.Counter            % (number) - if the user didn’t supply then apply auto increase (if AutoIncrease = true)
                Args.CCDID              %
                Args.CropID             %
                Args.Type               %
                Args.Level              %
                Args.SubLevel           %- default is ‘n’ [note that n will be replaced by “”, without “.” seperator)
                Args.Product            %
                Args.Version            % (char or number) [saved as char]
                Args.FileType           % - default is ‘fits’.                                    
                Args.TimeZone = 2;      %
                Args.Area               % Used when FullPath is true
                Args.FullPath = false;  %               
                Args.FormatFieldID      %
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
            Obj.CCDID    = Obj.formatNumeric(Obj.CCDID, Obj.FormatCCDID);
            Obj.CropID   = Obj.formatNumeric(Obj.CropID, Obj.FormatCropID);
            Obj.Version  = Obj.formatNumeric(Obj.Version, Obj.FormatVersion);
            
            % Validate field values
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
            Obj.msgLog(LogLevel.Debug, 'FileName: %s', Obj.FileName);
            Result = Obj.FileName;

        end
        
        
        function Result = genFromDbQuery(Obj, Query)
            % Generate ImagePath current record of Query.ResultSet
            % @Todo
                
        end
        
    end
    
   
    methods % Read/Write from Header and Struct
        
        function Result = readFromHeader(Obj, Header)
            % Read data from AstroHeader, DictKeyNames is used to get the
            % correct key names            
            % @TODO: @Eran - Validate field names in FITS header
            arguments
                Obj
                Header AstroHeader
            end
            
            Obj.msgLog(LogLevel.Debug, 'readFromHeader: ');
                                
            Obj.ProjName        = Header.getVal(Obj.DictKeyNames.ProjName);
            Obj.JD              = Header.getVal(Obj.DictKeyNames.JD);
            Obj.TimeZone        = Header.getVal(Obj.DictKeyNames.TimeZone);
            Obj.Filter          = Header.getVal(Obj.DictKeyNames.Filter);
            Obj.FieldID         = Header.getVal(Obj.DictKeyNames.FieldID);
            Obj.Counter         = Header.getVal(Obj.DictKeyNames.Counter);
            Obj.CCDID           = Header.getVal(Obj.DictKeyNames.CCDID);
            Obj.CropID          = Header.getVal(Obj.DictKeyNames.CropID);
            Obj.Type            = Header.getVal(Obj.DictKeyNames.Type);
            Obj.Level           = Header.getVal(Obj.DictKeyNames.Level);
            Obj.SubLevel        = Header.getVal(Obj.DictKeyNames.SubLevel);
            Obj.Product         = Header.getVal(Obj.DictKeyNames.Product);
            Obj.Version         = Header.getVal(Obj.DictKeyNames.Version);
            Obj.FileType        = Header.getVal(Obj.DictKeyNames.FileType);
            Obj.SubDir          = Header.getVal(Obj.DictKeyNames.SubDir);

            Result = true;
        end
        
        
        function Result = writeToHeader(Obj, Header)
            % Write data to AstroHeader, DictKeyNames is used to get the
            % correct key names            
            arguments
                Obj
                Header AstroHeader
            end
            
            Obj.msgLog(LogLevel.Debug, 'writeToHeader: ');
                 
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
            
            Obj.msgLog(LogLevel.Debug, 'readFromDb: ');
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
            
            
        function Result = valiadateFields(Obj)
            % Validate fields: Type, Level, Product

            Result = true;
            
            % Verify Type
            Obj.msgLog(LogLevel.Debug, 'valiadateFields: Type=%s', Obj.Type);
            switch Obj.Type
                case { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'sci', 'wave'}
                    % Ok
                otherwise
                    error('Unknown Type option: %s', Obj.Type);
            end

            % Verify Level
            Obj.msgLog(LogLevel.Debug, 'valiadateFields: Level=%s', Obj.Level);
            switch Obj.Level
                case {'log', 'raw', 'proc', 'stack', 'ref', 'coadd'}
                    % Ok
                otherwise
                    error('Unknown Level option: %s', Obj.Level);
            end

            % Verify Product
            Obj.msgLog(LogLevel.Debug, 'valiadateFields: Product=%s', Obj.Product);
            switch Obj.Product
                case { 'im', 'back', 'var', 'exp', 'nim', 'psf', 'cat', 'spec', 'pixflag', 'imflag' }
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
