
classdef ImagePath < Base
    % Construct and parse image path used in storage, database, and headers
    % The file path is described in the LAST/ULTRASAT file naming convension document.
    properties
        
        Store = [] % db.AstroStore = []     % 
        
        % Each property is mapped to field in common_image_path table
        Uuid            = [];       %
        PkFieldName     = '';       % Primary key field name
        TableName       = '';       %
        
        % Fields from file name
        ProjName        = '';       % 
        JD              = 0;        % UTC start of exposure @Eran - UTC or JD???
        TimeStr         = '';       % Time as appears in file name

        % Filter and Field
        Filter          = 'clear';  % 
        FieldId         = '';       % 
        CropId          = '';       % 
        ImageType       = 'sci';    % sci, bias, dark, domeflat, twflat, skyflat, fringe
        ImageLevel      = 'raw';    % log, raw, proc, stack, coadd, ref.
        ImageSubLevel   = '';       % Sub level
        ImageProduct    = 'im';     % Product: im, back, var, imflag, exp, Nim, psf, cat, spec.
        ImageVer        = '1';      % Version (for multiple processing)
        FileType        = 'fits';   % fits / hdf5 / fits.gz

        % Additional fields from database table
        Timezone        = 0;        % Bias in hours, to generate folder name        
        Title           = '';       % Text description
        Metadata        = '';       % Other textual data
        Telescope       = 'USAT';   % (ProjName) - source instrument (last_xx/ultrasat) - LAST.<#>.<#>.<#> or ???
        Node            = '';       % (ProjName) - 
        Mount           = '';       % (ProjName) - 
        Camera          = '';       % (ProjName) - 

        % Debug? or have it?
        BasePath        = '/data/store';    % 
        FileName                    %
        Path                        %
        FullName        = '';       %
        
        % Optional (@Todo discuss with @Eran)
        SrcFileName     = '';

        
        %
        DictKeyNames Dictionary             %

%         ProjName char       = 'none';
%         Date                = NaN;
%         Filter char         = 'clear';
%         FieldID             = '';
%         FormatFieldID char  = '%06d';
%         Type char           = 'sci';
%         Level char          = 'raw';
%         SubLevel char       = '';
%         Product char        = 'im';
%         Version             = 1;
%         FormatVersion       = '%03d';
%         FileType            = 'fits';
%         TimeZone            = 2;
%         RefVersion          = 1;
%         FormatRefVersion    = '%03d';
%         SubDir              = '';
%         DataDir             = 'data';
%         Base                = '/home/last';
    end
    
    
    
    
    methods % Constructor
       
        function Obj = ImagePath(varargin)
            % Setup 
            %Obj.Store = db.AstroStore.get();
            
            Obj.DictKeyNames = Dictionary.getDict('Header.ImagePath.KeyNames');
            
            Obj.setDefault();
        end
        
        
        function Result = setDefault(Obj)
            % Set default values
            
            DateStr = '2021-01-02 13:14:15.678';
            DateUTC = datetime(DateStr, 'InputFormat', 'yyyy-MM-dd HH:mm:ss.SSS'); 
            Obj.JD = juliandate(DateUTC);

            % For unit test
            Obj.Mount = 'mnt01';
            Obj.Camera = 'cam01';
            Obj.FieldId = 'fld1';
            Obj.CropId = 'crop001-001';
            
            
            Obj.ProjName = 'LAST.1.1.1';        
            Obj.Filter char         = 'clear';
            Obj.FieldID             = '';
            FormatFieldID char  = '%06d';
            Obj.Type char           = 'sci';
            Obj.Level char          = 'raw';
            Obj.SubLevel char       = '';
            Obj.Product char        = 'im';
            Obj.Version             = 1;
            Obj.FormatVersion       = '%03d';
            Obj.FileType            = 'fits';
            Obj.TimeZone            = 2;
            Obj.RefVersion          = 1;
            Obj.FormatRefVersion    = '%03d';
            Obj.SubDir              = '';
            Obj.DataDir             = 'data';
            Obj.Base                = '/home/last';            
        end

        
        function Result = setTestData(Obj)
            % Set 
  
            Obj.Telescope       = 'USAT';            
            Obj.Node            = '';
            Obj.JD              = 0;
            Obj.Timezone        = 0;
            Obj.Mount           = '';
            Obj.Camera          = '';

            % Filter and Field
            Obj.Filter          = 'clear';
            Obj.FieldId         = 'FLD1';
            Obj.CropId          = 'CROP123';

            % Image - Mandatoty fields
            Obj.ImageType       = 'sci';
            Obj.ImageLevel      = 'raw';
            Obj.ImageSubLevel   = 'sub';
            Obj.ImageProduct    = 'im';
            Obj.ImageVer        = '1';
            Obj.FileType        = 'fits';

            % Debug? or have it?
            Obj.BasePath        = '/data/store';
            Obj.FileName        = '';
            Obj.Path            = '';       
            Obj.FullName        = '';       

            % Optional (@Todo discuss with @Eran)
            Obj.SrcFileName     = '';
            Obj.Title           = '';       
            Obj.Metadata        = '';       
            Obj.TableName       = 'processed_cropped_images';       

            Result = true;
        end
    end
    
    
    methods
        
    end


    methods % read/write
        function Result = readFromHeader(Obj, Header)
            % Read data from AstroHeader
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
            Obj.Timezone        = Header.getVal(Obj.DictKeyNames.Timezone);
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
            % Write data to AstroHeader            
            arguments
                Obj
                Header AstroHeader
            end
            
            Obj.msgLog(LogLevel.Debug, 'writeToHeader: ');
            
            Header.setVal(Obj.DictKeyNames.Telescope, Obj.Telescope);
            Header.setVal(Obj.DictKeyNames.Node, Obj.Node);
            Header.setVal(Obj.DictKeyNames.Mount, Obj.Mount);
            Header.setVal(Obj.DictKeyNames.Camera, Obj.Camera);
            Header.setVal(Obj.DictKeyNames.JD, Obj.JD);
            Header.setVal(Obj.DictKeyNames.Timezone, Obj.Timezone);
            Header.setVal(Obj.DictKeyNames.Filter, Obj.Filter);
            Header.setVal(Obj.DictKeyNames.FieldId, Obj.FieldId);
            Header.setVal(Obj.DictKeyNames.CropId, Obj.CropId);
            Header.setVal(Obj.DictKeyNames.ImageType, Obj.ImageType);
            Header.setVal(Obj.DictKeyNames.ImageLevel, Obj.ImageLevel);
            Header.setVal(Obj.DictKeyNames.ImageSubLevel, Obj.ImageSubLevel);
            Header.setVal(Obj.DictKeyNames.ImageProduct, Obj.ImageProduct);
            Header.setVal(Obj.DictKeyNames.ImageVer, Obj.ImageVer);
            Header.setVal(Obj.DictKeyNames.FileType, Obj.FileType);            
            
            Result = true;
        end        
        
        
        function Result = readFromDb(Obj, Query)
            % Read data from database table (common_image_path table)
            arguments
                Obj
                Query io.db.DbQuery
            end            
            
            Obj.msgLog(LogLevel.Debug, 'readFromDb: ');
            st = Query.getRecord();
            Result = Obj.readFromStruct(st);
        end
        
        
        function Result = writeToDb(Obj, Query, Args)
            % Insert/update data to database table (common_image_path table)
            arguments
                Obj
                Query io.db.DbQuery
                Args.Insert = True;
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
            Obj.Telescope       = st.tel;
            Obj.Node            = st.node;
            Obj.Mount           = st.mount;
            Obj.Camera          = st.camera;
            Obj.JD              = st.jd;
            Obj.Timezone        = st.timezone;
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
            st.timezone = Obj.Timezone;
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

    
    methods % Helpers
        
        function st = makeUuid(Obj)
            % Create uuid
            if isempty(Obj.Uuid)
                Obj.Uuid = Component.MakeUuid();
            end
        end
    end
  
    
    methods(Static) % Make
        
        function Result = constructPath(Args)
            % Construct image/catalog file path based on the LAST/ULTRASAT standard
            % Options are:
            % /data/YYYY/MM/DD/raw/ - contains all the science raw data
            % /data/YYYY/MM/DD/log/  - contains all the log files
            % /data/YYYY/MM/DD/proc/ - contains all the single processed images including: image, mask, back (if provided), var (if provided), PSF (if provided), and catalogs.
            % /data/YYYY/MM/DD/calib/ - contains all the processed calibration images/variance/masks/catalogs
            % /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction products 
            % /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF
            % /data/coadd/area/ - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods)             
            
            % Example: Path=imUtil.util.file.construct_path
            %          Path=imUtil.util.file.construct_path('Level','ref','SubDir','x')
            %          Path=imUtil.util.file.construct_path('Level','proc','Type','bias')
            arguments
                Args.Base = '/home/last'
                Args.DataDir = 'data';  % Default is 'data'
                Args.SubDir = ''  % This is the area/location directory below the coadd/ref directory
                Args.ProjName
                Args.Time = []; %  - If empty, then will use current computer time, and
%                   will assume the time is in UTC.
%                   If numeric then assume the time is in JD, if char then
%                   assume the time is in the YYYY-MM-DDTHH:MM:SS.FFF
%                   format.Date, TimeZone} or {YYYY, MM, DD}, or []}
                Args.TimeZone = 2;  % Hours
                Args.Type = 'sci';
                Args.Level = 'raw';
                Args.FieldID % (may be required for the <area#> (char or number) 
                Args.FieldIDformat = '%05d'
                Args.Area
                Args.Visit
                Args.RefVersion = 1;    % Reference image version
                Args.FormatRefVersion = '%03d'  % Format for numeric reference version
            end
            

% convert date to JD
if isempty(InPar.Date)
    JD = celestial.time.julday;
else
    if ischar(InPar.Date) || iscellstr(InPar.Date)
        JD = convert.time(InPar.Date,'StrDate','JD');
    else
        % already in JD
        JD = InPar.Date;
    end    
end


switch InPar.Level
    case 'ref'
        if isnumeric(InPar.RefVersion)
            InPar.RefVersion = sprintf(InPar.FormatRefVersion,InPar.RefVersion);
        end

        DateDir = sprintf('%s%s%s%s%s',filesep,...
                                   InPar.Level,...
                                   filesep,...
                                   'version',...
                                   InPar.RefVersion,...
                                   filesep,...
                                   InPar.SubDir);
    case 'coadd'
        DateDir = sprintf('%s%s%s%s%s',filesep,...
                                   InPar.Level,...
                                   filesep,...
                                   InPar.SubDir);
    otherwise
        

        % check TimeZone!
        % convert to JD
        %JD = convert.time(InPar.Date,'StrDate','JD');
        [Y,M,D]=imUtil.util.file.date_directory(JD,InPar.TimeZone);
        
        %LocalJD = JD + InPar.TimeZone./24;
        %FloorLocalJD = floor(LocalJD);
        %FloorDate = celestial.time.jd2date(FloorLocalJD);
        
        
        
        YearDir = sprintf('%04d',Y);
        MDir   = sprintf('%02d',M);
        DDir   = sprintf('%02d',D);
        MDDir  = sprintf('%s%s%s',MDir,filesep,DDir);
        
        
        
        if strcmp(InPar.Level,'proc') && ~strcmp(InPar.Type,'sci')
            % calibration
            Level = 'calib';
        else
            Level = InPar.Level;
        end
        
        DateDir = sprintf('%s%s%s%s%s%s',filesep,...
                                YearDir,...
                                filesep,...
                                MDDir,...
                                filesep,...
                                Level);
end


Path = sprintf('%s%s%s',InPar.Base,...
                        filesep,...
                        InPar.DataDir,...
                        DateDir);
   
            % Clean path from multiple /
            Path = regexprep(Path, sprintf('%s{2,5}', filesep), '/');
            msgLog(LogLevel.Debug, 'Path: %s', Path);
            Result = Path;            
        end

        
        function Result = constructFile(Args)
            %
            % Returns: string containing image name 
            % Returns: string containing image path (if nargout>1, call constructPath)
            arguments
                Args.ProjName
                Args.Date
                {TimeZone} or {YYYY, MM, DD} or [] (required for path) [Save in 2 properties: JD & DateStr]
                Args.Filter (char)
                Args.FieldID (char or number) [Saved as char]
                Args.FieldID format - default is %05d [Not in DB]
                Args.Counter (number) - if the user didn’t supply then apply auto increase (if AutoIncrease = true)
                Args.AutoIncrease - increase counter - default is true. [Not in DB]
                Args.Type (char)
                Args.Level (char)
                Args.SubLevel (char) - default is ‘n’ [note that n will be replaced by “”, without “.” seperator)
                Args.Product (char)
                Args.Version (char or number) [saved as char]
                Args.Version format - default is %03d. [Not in DB]
                Args.FileType - default is ‘fits’.
                    end
                end
                        
                
                
                
function [FileName,Path] = construct_filename(varargin)
% Construct image/catalog file name based on the LAST/ULTRASAT standard
% Package: +imUtil/+util/+file
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


InPar = inputParser;
addOptional(InPar,'ProjName','LAST.0.1');
addOptional(InPar,'Date',[]); % if empty use now | JD | full string
addOptional(InPar,'Filter','clear');
addOptional(InPar,'FieldID','');
addOptional(InPar,'FormatFieldID','%06d');
addOptional(InPar,'Type','sci');
addOptional(InPar,'Level','raw');
addOptional(InPar,'SubLevel','');
addOptional(InPar,'Product','im');
addOptional(InPar,'Version',1);
addOptional(InPar,'FormatVersion','%03d');
addOptional(InPar,'FileType','fits');
addOptional(InPar,'TimeZone',2);
addOptional(InPar,'RefVersion',1);
addOptional(InPar,'FormatRefVersion','%03d');
addOptional(InPar,'SubDir','');
addOptional(InPar,'DataDir','data');
addOptional(InPar,'Base','/home/last');
parse(InPar,varargin{:});
InPar = InPar.Results;


%<ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>


if isempty(InPar.Date)
    InPar.Date = celestial.time.julday;
end

if isnumeric(InPar.Date)
    % assume JD
    InPar.Date = convert.time(InPar.Date,'JD','StrDate');
    InPar.Date = InPar.Date{1};
end

if isnumeric(InPar.FieldID)
    InPar.FieldID = sprintf(InPar.FormatFieldID,InPar.FieldID);
end



        Path = .construct_path('Date', Args.Date,...
                                     'TimeZone', Args.TimeZone,...
                                     'Level', Args.Level,...
                                     'Type', Args.Type,...
                                     'RefVersion', Args.RefVersion,...
                                     'FormatRefVersion', Args.FormatRefVersion,...
                                     'Base', Args.Base,
                                    'DataDir', Args.DataDir,...
                                     'SubDir', Args.SubDir,
                                     
                                     
                Path = sprintf('%s%s', Path, filesep);

                % Clean from multiple '/'
                Path = regexprep(Path, sprintf('%s{2,5}', filesep), '/');

                                     
                % Verify Type
                switch Args.Type
                    case { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'sci', 'wave'}
                        % Ok
                    otherwise
                        error('Unknown Type option: %s', Args.Type);
                end

                % Verify Level
                switch Args.Level
                    case {'log', 'raw', 'proc', 'stack', 'ref', 'coadd'}
                        % Ok
                    otherwise
                        error('Unknown Level option: %s', Args.Level);
                end

                if isempty(InPar.SubLevel)
                    MergedLevel = InPar.Level;
                else
                    MergedLevel = sprintf('%s.%s',InPar.Level,InPar.SubLevel);    
                end

                % Verify Product
                switch Args.Product
                    case { 'im', 'back', 'var', 'exp', 'nim', 'psf', 'cat', 'spec', 'pixflag', 'imflag' }
                        % Ok
                    otherwise
                        error('Unknown Product option: %s', Args.Product);
                end

                if isnumeric(Args.Version)
                    Args.Version = sprintf(Args.FormatVersion, Args.Version);
                end

            % Remove '-' and ':' from date
            if iscellstr(InPar.Date)
                InPar.Date = InPar.Date{1};
            end
            InPar.Date = strrep(InPar.Date, '-', '');
            InPar.Date = strrep(InPar.Date, 'T', '.');
            InPar.Date = strrep(InPar.Date, ':' ,'');


            FileName = sprintf('%s_%s_%s_%s_%s_%s_%s_%s.%s', Args.ProjName,...
                           Args.Date,...
                           Args.Filter,...
                           Args.FieldID,...
                           Args.Type,...
                           MergedLevel,...
                           Args.Product,...
                           Args.Version,...
                           Args.FileType);
                       
                  msgLog(LogLevel.Debug, 'FileName: %s', FileName);
                  Result = FileName;
                       
                       
                
                
                
        
        % <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>
        % <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>               
        
        % /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction products 
        % /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF
        % /data/coadd/area/ - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods) 
        
        function [FileName, Path] = makeFileName(Obj)
            
            % Need to take care of separate folder names ??? @Eran
            TimeUTC = datetime(Obj.JD, 'convertfrom', 'juliandate');
            if Obj.Timezone ~= 0
                TimeUTC = TimeUTC + Obj.Timezone;
            end
            TimeStr = datestr(TimeUTC, 'yyyymmdd.HHMMSS.FFF');
            
            PrefixStr = sprintf('%s' , Obj.Telescope);
            
            % Optional fields
            if ~isempty(Obj.Mount)
                PrefixStr = [PrefixStr, '_', Obj.Mount];
            end            
            if ~isempty(Obj.Camera)
                PrefixStr = [PrefixStr, '_', Obj.Camera];
            end            
            if ~isempty(Obj.Filter)
                PrefixStr = [PrefixStr, '_', Obj.Filter];
            end            
            if ~isempty(Obj.FieldId)
                PrefixStr = [PrefixStr, '_', Obj.FieldId];
            end            
            if ~isempty(Obj.CropId)
                PrefixStr = [PrefixStr, '_', Obj.CropId];
            end
            
            % Image fields
            ImageStr = sprintf('%s_%s.%s_%s_%s', Obj.ImageType, Obj.ImageLevel, Obj.ImageSubLevel, Obj.ImageProduct, Obj.ImageVer);
            
            FileName = [PrefixStr, '_', TimeStr, '_', ImageStr, '.', Obj.FileType];
       
            Obj.FileName    = FileName;
            
            if ~isempty(Obj.Store)
                Path = Obj.Store.getDataPath(Obj);
                Obj.Path = Path;
                Obj.FullName = sprintf('%s%s%s', Path, filesep, FileName);
            else
                Path = '';
                Obj.FullName = FileName;
                io.msgLog(LogLevel.Warning, 'ImagePath: Store is not set');
            end

            
            Obj.Path        = Path;
            
        end
 
                end
    
    
        methods(Static) % Parsers
              
        function Result = path2struct(path)
            %(static) - Convert a string containing a path to a structure with all available information (e.g., date, type, level, fieldID, ProjName)
        end
        
        
        function Result = filename2struct(fname)
            %Convert a file name string to a structure with all available information
        end

            end
            
            
            methods(Static)
        function Result = createFromDbQuery(Obj, Query)
            % Create ImagePath
            % @Todo
                
        end
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'ImagePath test started\n');
    
            % Default
            IP = ImagePath();
            FileName = IP.makeFileName();
            assert(~isempty(FileName));
            
            IP.setTestData();
            
            s = IP.writeToStruct();
            disp(s);
            
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'ImagePath test passed')
            Result = true;
        end
    end    
    
end

