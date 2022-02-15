
% #functions (autogen)
% ImagePath - Constructor
% constructFile - Construct image/catalog file name based on the LAST/ULTRASAT standard Description: Return data product file name and path according to the              LAST/ULTRASAT standard.              <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>
% constructPath - Construct image/catalog file path based on the LAST/ULTRASAT standard Options are: Form 1: /data/YYYY/MM/DD/raw/ - contains all the science raw data
% createFromDbQuery - Create ImagePath @Todo
% parseFileName - Convert a file name string to a structure with all available information
% parsePath - Convert a string containing a path to a structure with all available information (e.g., date, type, level, fieldID, ProjName)
% readFromDb - Read data from database table, current record of Query.ResultSet Fields are defined in Google Sheet "common_image_path"
% readFromHeader - Read data from AstroHeader, DictKeyNames is used to get the correct key names @TODO: @Eran - Validate field names in FITS header
% readFromStruct - Read data from struct or DbRecord (common_image_path table) Struct field names should match database fields
% setTestData - Set data for unit-test and debugging The result should be: USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits
% setTime -
% valiadateFields -
% writeToDb - Insert/update data to database table using the specified Query Fields are defined in Google Sheet "common_image_path"
% writeToHeader - Write data to AstroHeader, DictKeyNames is used to get the correct key names
% writeToStruct - Write data fields to struct (common_image_path table)
% #/functions (autogen)
%

classdef ImagePathDb < Component
    % Construct and parse image path used in storage, database, and headers.
    % The file path is described in the LAST/ULTRASAT file naming convension document.
    % File name format:
    % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
    properties  % Separate to Hidden props
               
        % Each property is mapped to field in common_image_path table
        PkFieldName     = '';           % Primary key field name, currently we use ... or Query it from the DbTable
        TableName       = '';           %
        
        % Fields from file name
        ProjName        = '';           % Examples: "ULTRASAT", "LAST.1.12.4" (LAST node=1, mount=12, camera=4)
        Time            = [];           %
        JD              = [];           % UTC start of exposure @Eran - UTC or JD???
        TimeStr         = '';           % Time as appears in file name (YYYYMMDD.HHMMSS.FFF)
        Filter          = 'clear';      % Filter name
        FieldId         = '';           % Sky position identifier, like field ID, object name, CCD Id, sub image ID etc. May include CropId. Examples: 314.1.1 is field Id 314, CCD 1, sub image 1.
        Counter         = '';           %
        CCDID           = '';           %
        CropId          = '';           % Part of FieldId
        Type            = 'sci';        % sci, bias, dark, domeflat, twflat, skyflat, fringe
        Level           = 'raw';        % log, raw, proc, stack, coadd, ref.
        SubLevel        = '';           % n - normal, s - proper subtraction S, sp - proper subtraction S tag, d - proper subtraction D, t - Translient, r - proper coaddition R, m - matched filter with unspecified filter
        
        %                               % Single capital letter prefix may be added to this name: F - Fourier Transform, R - Radon Transform, L - Laplacian, G - x gradient, H - y gradient.
        Product         = 'im';         % Product: im, back, var, imflag, exp, Nim, psf, cat, spec.
        Version         = '1';          % Version (for multiple processing)
        FileType        = 'fits';       % fits / hdf5 / fits.gz

        % Additional fields of database table that are not part of the file
        % name. The use may fill them by setting these properties
        
        TimeZone        = 2;            % Bias in hours, to generate folder name
        Title           = '';           % Text description
        Metadata        = '';           % Additional metadata, such as key=value\n...
        Telescope       = 'USAT';       % From ProjName - source instrument (last_xx/ultrasat) - LAST.<#>.<#>.<#> or ???
        Node            = '';           % From ProjName - Node ID
        Mount           = '';           % From ProjName - Monut ID
        Camera          = '';           % From ProjName - Camera ID

        % Path
        % /data/ref/<area>/version<#>/ - All sky reference/coadd image -
        % images/masks/catalogs/PSF - The area/region directory will be located below the version number.
        % /data/coadd/<area>/<time++>/ - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods) - TBD
        % /data/YYYY/MM/DD/raw/ - contains all the science and calibration raw data
        % /data/YYYY/MM/DD/log/  - contains all the log files
        % /data/YYYY/MM/DD/proc/<visit>/ - contains all the single processed images including: image, mask, back (if provided), var (if provided), PSF (if provided), and catalogs
        % /data/YYYY/MM/DD/stacked/<visit> - contains all the processed coadd images (coaddition of images of the same field taken continuously only; e.g., 20x15s coadds) - images/masks/catalogs/PSF/subtraction products
        % /data/calib/bias/ - all master bias images, same for dark, flat, fringe
        RefVersion      = 1;            %
        Area            = '';           %
        Visit           = '';           %
        
        % Fields formatting
        FormatFieldID   = '%06d';       % @Eran?
        FormatVersion   = '%03d';       %
                
        %
        BasePath        = '/home/last'; % Base storage path, should be updated from AstroStorage
        DataPath        = 'data';       %
        SubDir          = '';           % This is the area/location directory below the coadd/ref directory
        FileName        = '';           % Filename part
        Path            = '';           % Path part
        FullName        = '';           % Full name Path/FileName
        
        % Optional
        SrcFileName     = '';           % Source file name (Debug?)
        
        %
        DictKeyNames Dictionary         % Dictionary, used to access Header keys
        Store = []                      % Optional link to AstroStore
    end
    
    
    methods % Constructor
       
        function Obj = ImagePath(varargin)
            % Constructor
            
            % Link to Store and get base path
            Obj.Store = db.AstroStore.getSingleton();
            Obj.BasePath = Obj.Store.getBasePath();
            
            % Load header key names mapping from configuation
            Obj.DictKeyNames = Dictionary.getDict('Header.ImagePath.KeyNames');
        end
        
        
        function Result = setTestData(Obj)
            % Set data for unit-test and debugging
            % The result should be: USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits
            
            Obj.JD = [];  %juliandate(DateUTC);
            Obj.ProjName        = 'USAT';
            Obj.Telescope       = 'USAT';
            Obj.Node            = '';
            Obj.Time            = '2021-09-09T12:34:56.789';
            Obj.JD              = [];
            Obj.TimeZone        = 2;
            Obj.Mount           = 'mnt01';
            Obj.Camera          = 'cam01';

            % Filter and Field
            Obj.Filter          = 'clear';
            Obj.FieldId         = 'fld';
            Obj.Counter         = 'cnt';
            Obj.CCDID           = 'ccdid';
            
            Obj.CropId          = 'crop';
            
            % Image - Mandatoty fields
            Obj.Type            = 'sci';
            Obj.Level           = 'raw';
            Obj.SubLevel        = 'sub';
            Obj.Product         = 'im';
            Obj.Version         = 'ver1';
            Obj.FileType        = 'fits';

            % Debug? or have it?
            %Obj.BasePath        = '/data/store';
            Obj.FileName        = '';
            Obj.Path            = '';
            Obj.FullName        = '';

            % Optional (@Todo discuss with @Eran)
            Obj.SrcFileName     = '';
            Obj.Title           = '';
            Obj.Metadata        = '';
            Obj.TableName       = 'processed_cropped_images';

            Result = 'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits';
        end
    end
    

    methods % Read/Write
        
        function Result = readFromHeader(Obj, Header)
            % Read data from AstroHeader, DictKeyNames is used to get the
            % correct key names
            % @TODO: @Eran - Validate field names in FITS header
            arguments
                Obj
                Header AstroHeader
            end
            
            Obj.msgLog(LogLevel.Debug, 'readFromHeader: ');
            
            Obj.Telescope       = Header.getVal(Obj.DictKeyNames.Telescope);
            Obj.Node            = Header.getVal(Obj.DictKeyNames.Node);
            Obj.Mount           = Header.getVal(Obj.DictKeyNames.Mount);
            Obj.Camera          = Header.getVal(Obj.DictKeyNames.Camera);
            Obj.JD              = Header.getVal(Obj.DictKeyNames.JD);
            Obj.TimeZone        = Header.getVal(Obj.DictKeyNames.TimeZone);
            Obj.Filter          = Header.getVal(Obj.DictKeyNames.Filter);
            Obj.FieldId         = Header.getVal(Obj.DictKeyNames.FieldId);
            Obj.CropId          = Header.getVal(Obj.DictKeyNames.CropId);
            Obj.ImageType       = Header.getVal(Obj.DictKeyNames.ImageType);
            Obj.ImageLevel      = Header.getVal(Obj.DictKeyNames.ImageLevel);
            Obj.ImageSubLevel   = Header.getVal(Obj.DictKeyNames.ImageSubLevel);
            Obj.ImageProduct    = Header.getVal(Obj.DictKeyNames.ImageProduct);
            Obj.ImageVer        = Header.getVal(Obj.DictKeyNames.ImageVer);
            Obj.FileType        = Header.getVal(Obj.DictKeyNames.FileType);

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
            
            Header.setVal(Obj.DictKeyNames.Telescope,   Obj.Telescope);
            Header.setVal(Obj.DictKeyNames.Node,        Obj.Node);
            Header.setVal(Obj.DictKeyNames.Mount,       Obj.Mount);
            Header.setVal(Obj.DictKeyNames.Camera,      Obj.Camera);
            Header.setVal(Obj.DictKeyNames.JD,          Obj.JD);
            Header.setVal(Obj.DictKeyNames.TimeZone,    Obj.TimeZone);
            Header.setVal(Obj.DictKeyNames.Filter,      Obj.Filter);
            Header.setVal(Obj.DictKeyNames.FieldId,     Obj.FieldId);
            Header.setVal(Obj.DictKeyNames.CropId,      Obj.CropId);
            Header.setVal(Obj.DictKeyNames.ImageType,   Obj.ImageType);
            Header.setVal(Obj.DictKeyNames.ImageLevel,  Obj.ImageLevel);
            Header.setVal(Obj.DictKeyNames.ImageSubLevel, Obj.ImageSubLevel);
            Header.setVal(Obj.DictKeyNames.ImageProduct,  Obj.ImageProduct);
            Header.setVal(Obj.DictKeyNames.ImageVer,    Obj.ImageVer);
            Header.setVal(Obj.DictKeyNames.FileType,    Obj.FileType);
            
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
        
        
        function Result = writeToDb(Obj, Query, Args)
            % Insert/update data to database table using the specified Query
            % Fields are defined in Google Sheet "common_image_path"
            arguments
                Obj
                Query io.db.DbQuery     % @Todo? Get default Query from Store/Db? if empty?
                Args.Insert = True;     % frue=Insert, false=Update
            end
            
            st = Obj.writeToStruct();
            TableName = Obj.TableName;
            if Args.Insert
                Obj.msgLog(LogLevel.Debug, 'writeToDb: insert: ');
                Query.insertRecord(TableName, st);
            else
                % @Todo
                % Obj.msgLog(LogLevel.Debug, 'writeToDb: update: ');
                %Query.updateRecord(TableName, st);
            end
            Result = true;
        end
                
        
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
            Obj.FieldId         = st.field_id;
            Obj.CropId          = st.crop_id;
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
            st.field_id = Obj.FieldId;
            st.crop_id  = Obj.CropId;
            st.imtype   = Obj.ImageType;
            st.imlevel  = Obj.ImageLevel;
            st.imslevel = Obj.ImageSubLevel;
            st.improd   = Obj.ImageProduct;
            st.imver    = Obj.ImageVer;
            st.filetype = Obj.FileType;
        end
    end
    
    
    methods % Construct
        
        function Result = constructPath(Obj, Args)
            % Construct image/catalog file path based on the LAST/ULTRASAT standard
            % Options are:
            %
            % Form 1:
            % /data/YYYY/MM/DD/raw/ - contains all the science raw data
            % /data/YYYY/MM/DD/log/  - contains all the log files
            % /data/YYYY/MM/DD/proc/ - contains all the single processed images including: image, mask, back (if provided), var (if provided), PSF (if provided), and catalogs.
            % /data/YYYY/MM/DD/calib/ - contains all the processed calibration images/variance/masks/catalogs
            % /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction products
            %
            % Form 2:
            % /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF
            %
            % Form 3:
            % /data/coadd/area/ - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods)
            
            % Example: Path = imUtil.util.file.construct_path
            %          Path=imUtil.util.file.construct_path('Level','ref','SubDir','x')
            %          Path=imUtil.util.file.construct_path('Level','proc','Type','bias')
            arguments
                Obj
                Args.Base       = '/home/last';     %
                Args.DataDir    = 'data';           %
                Args.SubDir     = '';               %
                Args.Time       = [];               % Empty -> current computer time, UTC, Numeric -> time is in JD, char -> YYYY-MM-DDTHH:MM:SS.FFF
%                   format.Date, TimeZone} or {YYYY, MM, DD}, or []}
                Args.TimeZone   = 2;                % Hours
                Args.Level      = 'raw';            %
                Args.SubLevel   = '';
                Args.Area       = '';               %
                Args.Version    = '1';              % Reference image version
            end
            
            % Set properties from arguments, only properties that exist in
            % Args are set
            Obj.setProps(Args);
            
            % From this point we should work with Obj properties and not with Args
            
            % Convert date to JD
            Obj.setTime();
            
            % Check TimeZone!
            % Convert to JD
            % JD = convert.time(InPar.Date, 'StrDate', 'JD');
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
            FPath = regexprep(FPath, sprintf('%s{2,5}', filesep), '/');
            
            Obj.msgLog(LogLevel.Debug, 'Path: %s', FPath);
            Result = FPath;
        end
        

        
        function Result = constructFile(Obj, Args)
            % Construct image/catalog file name based on the LAST/ULTRASAT standard
            % Description: Return data product file name and path according to the
            %              LAST/ULTRASAT standard.
            %              <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>
            % Input  : * Pairs of ...,key,val,... Possible keywords include:
            %            'ProjName' - Default is 'LAST.0.1'.
            %            'Date' - If empty, then will use current computer time, and
            %                   will assume the time is in UTC.
            %                   If numeric then assume the time is in JD, if char then
            %                   assume the time is in the YYYY-MM-DDTHH:MM:SS.FFF
            %                   format.
            %                   Default is [].
            %            'TimeZone' - Time zone [hours]. Default is 2.
            %            'Filter' - Default is 'clear'.
            %            'FieldID' - Default is ''.
            %            'FormatFieldID' - Formatting of FieldID if is given as number.
            %                   Default is '%06d'.
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
            %            'FormatVersion' - Formatting of Version if is given as number.
            %                   Default is '%03d'.
            %            'FileType' - Default is 'fits'.
            %            'RefVersion' - Reference image version. Default is 1.
            %            'FormatRefVersion' - Format for numeric reference version.
            %                   Default is '%03d'.
            %            'SubDir' - This is the area/location directory below the
            %                   coadd/ref directory. Default is ''.
            %            'DataDir' - Default is 'data'.
            %            'Base' - Default is '/home/last'.
            % Output : -File name.
            %          - Path string.
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
                Args.FormatFieldID      % format - default is %05d [Not in DB]
                Args.Counter            % (number) - if the user didn’t supply then apply auto increase (if AutoIncrease = true)
                Args.CCDID              %
                Args.CropId             %
                Args.Type               %
                Args.Level              %
                Args.SubLevel           %- default is ‘n’ [note that n will be replaced by “”, without “.” seperator)
                Args.Product            %
                Args.Version            % (char or number) [saved as char]
                Args.FormatVersion      % format - default is %03d. [Not in DB]
                Args.FileType           % - default is ‘fits’.
            end
            
            % Set properties from arguments, only properties that exist in Args are set
            Obj.setProps(Args);
            
            % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
            
            %
            Obj.setTime();
            
            % FieldID, use specified formatting if numeric
            if isnumeric(Obj.FieldId) && ~isempty(Obj.FormatFieldID)
                Obj.FieldID = sprintf(Obj.FormatFieldID, Obj.FieldID);
            end

            % Validate field values
            Obj.valiadateFields();
            
            % Get path
            Path = ''; %Obj.constructPath();

            % Level / Level.SubLevel
            if isempty(Obj.SubLevel)
                MergedLevel = Obj.Level;
            else
                MergedLevel = sprintf('%s.%s', Obj.Level, Obj.SubLevel);
            end

            % Format version if numeric
            if isnumeric(Obj.Version) && ~isempty(Obj.FormatVersion)
                Obj.Version = sprintf(Obj.FormatVersion, Obj.Version);
            end

            % Prepare the final result
            % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
            Obj.FileName = sprintf('%s_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.%s', Obj.ProjName, ...
                           Obj.TimeStr, Obj.Filter, Obj.FieldId, Obj.Counter, Obj.CCDID, ...
                           Obj.CropId, Obj.Type, MergedLevel, Obj.Product, Obj.Version, Obj.FileType);

       
            % Done
            Obj.msgLog(LogLevel.Debug, 'FileName: %s', Obj.FileName);
            Result = Obj.FileName;

        end
        
        
        function Result = createFromDbQuery(Obj, Query)
            % Create ImagePath
            % @Todo
                
        end

        
    end
    
    
    methods % Helpers (for internal use)
        
        function Result = setTime(Obj)
            
            % Use current time
            if isempty(Obj.Time)
                Obj.Time = celestial.time.julday;
            end
            
            % Convert number to string (assume JD)
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

