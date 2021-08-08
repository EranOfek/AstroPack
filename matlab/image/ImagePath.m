
classdef ImagePath < Base
    
    properties
        
        Store = [] % db.AstroStore = []     % 
        
        % Each property is mapped to field in common_image_path table


        % Instrument
        Telescope       = 'USAT';   % (ProjName) - source instrument (last_xx/ultrasat) - LAST.<#>.<#>.<#> or ???
        Node            = '';       % Optional
        JD              = 0;        % UTC start of exposure @Eran - UTC or JD???
        Timezone        = 0;        % Bias in hours, to generate folder name        
        Mount           = '';       % Optional
        Camera          = '';       % Optional

        % Filter and Field
        Filter          = 'clear';  % Optional
        FieldId         = '';       % Optional
        CropId          = '';       % Optional

        % Image - Mandatoty fields
        ImageType       = 'sci';    % sci, bias, dark, domeflat, twflat, skyflat, fringe
        ImageLevel      = 'raw';    % log, raw, proc, stack, coadd, ref.
        ImageSubLevel   = '';       % Sub level
        ImageProduct    = 'im';     % Product: im, back, var, imflag, exp, Nim, psf, cat, spec.
        ImageVer        = '1';      % Version (for multiple processing)
        FileType        = 'fits';   % fits / hdf5 / fits.gz

        % Debug? or have it?
        BasePath        = '/data/store';    % 
        FileName
        Path
        FullName        = '';       %
        
        % Optional (@Todo discuss with @Eran)
        SrcFileName     = '';
        Title           = '';       % Text description
        Metadata        = '';       % Other textual data
        
        
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
            Obj.setDefault();
        end
        
        
        function setDefault(Obj)
            % Set default values
            
            DateStr = '2021-01-02 13:14:15.678';
            DateUTC = datetime(DateStr, 'InputFormat', 'yyyy-MM-dd HH:mm:ss.SSS'); 
            Obj.JD = juliandate(DateUTC);

            % For unit test
            Obj.Mount = 'mnt01';
            Obj.Camera = 'cam01';
            Obj.FieldId = 'fld1';
            Obj.CropId = 'crop001-001';
        end

    end
    
    
    methods
        function Result = readFromHeader(Obj, Header)
            % Read data from AstroHeader
            % @TODO: @Eran - Validate field names in FITS header
            arguments
                Obj
                Header AstroHeader
            end
            
            Obj.msgLog(LogLevel.Debug, 'readFromHeader: ');
            
            Obj.Telescope       = Header.Key.TEL;
            Obj.Node            = Header.Key.NODE;
            Obj.Mount           = Header.Key.MOUNT;
            Obj.Camera          = Header.Key.CAMERA;
            Obj.JD              = Header.Key.JD; 
            Obj.Timezone        = Header.Key.TIMEZONE;
            Obj.Filter          = Header.Key.FILTER;
            Obj.FieldId         = Header.Key.FIELDID;
            Obj.CropId          = Header.Key.CROPID;
            Obj.ImageType       = Header.Key.IMTYPE;
            Obj.ImageLevel      = Header.Key.IMLEVEL;
            Obj.ImageSubLevel   = Header.Key.IMSLEVEL;
            Obj.ImageProduct    = Header.Key.IMPROD;
            Obj.ImageVer        = Header.Key.IMVER;
            Obj.FileType        = Header.Key.FILETYPE;

            Result = true;
        end
        
        
        function Result = writeToHeader(Obj, Header)
            % Write data to AstroHeader            
            arguments
                Obj
                Header AstroHeader
            end
            
            Obj.msgLog(LogLevel.Debug, 'writeToHeader: ');
            
            Header.Key.TEL      = Obj.Telescope;
            Header.Key.NODE     = Obj.Node;
            Header.Key.MOUNT    = Obj.Mount;
            Header.Key.CAMERA   = Obj.Camera;
            Header.Key.JD       = Obj.JD;
            Header.Key.TIMEZONE = Obj.Timezone;
            Header.Key.FILTER   = Obj.Filter;
            Header.Key.FIELDID  = Obj.FieldId;
            Header.Key.CROPID   = Obj.CropId;
            Header.Key.IMTYPE   = Obj.ImageType;
            Header.Key.IMLEVEL  = Obj.ImageLevel;
            Header.Key.IMSLEVEL = Obj.ImageSubLevel;
            Header.Key.IMPROD   = Obj.ImageProduct;
            Header.Key.IMVER    = Obj.ImageVer;
            Header.Key.FILETYPE = Obj.FileType;
            
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
        
        
        function Result = writeToDb(Obj, Query, Args)
            % Insert/update data to database table (common_image_path table)
            arguments
                Obj
                Query io.db.DbQuery
                Args.Insert = True;
            end
            
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

            if Args.Insert
                Obj.msgLog(LogLevel.Debug, 'writeToDb: insert: ');
                Query.insertRecord(Query.TableName, st);                
            else
                % @Todo
                % Obj.msgLog(LogLevel.Debug, 'writeToDb: update: ');
                %Query.updateRecord(Query.TableName, st);                
            end
            Result = true;
        end        
    end
        
    % setters and getters
    methods       

    end
    
    
    methods
        
        % <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>
               
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
            
            %fn = p.FullName;
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'ImagePath test passed')
            Result = true;
        end
    end    
    
end

