% AstroFileName - A class for generating stand sgenFiltoring image/path names
%       for ULTRASAT and LAST.
%
% File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
%
%
% Example:
% A=AstroFileName;
% A.ProjName = {'LAST',1,2,3};   % will convert to: "LAST.01.02.03"
%
% % constructor:
% A = AstroFileName([2 4]);  % create an empty object
% % read all files in a directory according to file papptern:
% A = AstroFileName('LAST.01.*fits','Path','.')
% % read specific file names
% A = AstroFileName("LAST.01.08.02_20240109.143054.460_clear_001+30_001_001_001_sci_raw_Image_1_fits")
% read from a table:
% T = table; T.CropID=[1 2].';
% T.Product=["Image","PSF"].' ; T.JD=2451545+(0:1).';
% A = AstroFileName(T)
% A = AstroFileName(T, 'ReadJD',true, 'JD2Time',true)
%
% % time operations A.julday    % get JD % populate Time string from JD
% A.julday2time % select entry nearest to JD [I,JD]=A.selectNearest2JD %
% selected latest JD [I,JD]=A.selectLastJD A.sunAlt
%
% % number of files
% A.nFiles
%
% A=AstroFileName('LAST.01.*fits','Path','.');
%
% % get property
% A.getProp('Product')
% A.getProp('Product',[1 2])
%
% % generate file names, path, products
% A.genFile
% A.genPath
% A.genFull
% A.genProducts
%
% % headers
% A.write2header(AH)
% A.readFromHeader(AH)
% 
% select and sort
% A.sortBy('JD')
% A.reorderEntries([2 1])
% A.selectByPropVal('Type','raw')
% A.selectByDate(2451545, 2460000)
%
% % grouping
% A.groupByCounter
% A.groupByTimeGaps
%
% % move/copy/delete images
% A.moveImages
%
% % Example with tables - read from visit table:
% load LAST_Visits.mat;                    
% F=strcmp(OT.FieldID,"1441") &OT.CropID==10 & OT.FWHM<2.8;
% A=AstroFileName(OT(F,:),'JDCol','MIDJD');
% A.genPath([],'AddSubDir',true)
%

classdef AstroFileName < Component
    % Construct and parse (@Todo) image path used in storage, database, and headers.
    % For storage and database, we should implement ImagePathDb class
    % The file path is described in the LAST/ULTRASAT file naming convension document.
    %
    % Path format (@Todo add from gdoc):
    % /base/data/YYYY/MM/DD/raw/ - contains all the science and calibration raw data
    %
    % File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID.<TranIndex>>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
    % Example: 'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw.sub_im_ver1.fits'
    
    %properties (Dependent)
    %    JD
    %end
    
    properties       
        % These fields are the input parameters for getPath() and getFileName()
        %<ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
        ProjName            = "LAST.01.01.01";
        
        Time                = [];
        JD
        Filter              = ["clear"];
        FieldID             = [""];
        Counter             = [];
        CCDID               = [];
        CropID              = [];
        Type                = ["sci"];
        Level               = ["raw"];
        Product             = ["Image"];
        Version             = ["1"];
        FileType            = ["fits"];
        %
        SubDir              = "";
        BasePath            = "/lastdata"; %"/marvin"; %{'2022', '/marvin'; '2023','/marvin'; '2024','/marvin'; '2025', '/euclid'}; %"/marvin";    % or cell array of : year, path (e.g., {'2022', '/marvin'; '2025', '/euclid'})
        BasePathRef         = "/lastdata/ref"; % "/marvin/ref";
        
        %
        Path                = [];
        
        BasePathIncludeProjName logical = true;
        AddSubDir logical               = true;
        
        TimeZone            = 2;
        
    end
    
    properties (Hidden, Constant)
        % Fields formatting
        %FormatFieldID   = '%06d';       % Used with FieldID
        FormatCounter   = "%03d";       % Used with Counter  
        FormatFieldID   = "%d";
        FormatCCDID     = "%03d";       % Used with CCDID
        FormatCropID    = "%03d";       % Used with CropID
        FormatVersion   = "%d"; %'%03d';       % Used with Version
        
        FormatProjName  = "%02d";
        FormatTime      = "yyyymmdd.HHMMSS.FFF";
    end

    properties (Hidden, SetAccess=protected, GetAccess=public)
     
    end
    
    properties (Hidden, Constant)
        ListType        = ["", "bias", "dark", "flat", "domeflat", "twflat", "skyflat", "fringe", "focus", "sci", "wave", "type" , "log", "test"];
        ListLevel       = ["", "raw", "proc", "stack", "ref", "coadd", "merged", "calib", "junk", "proc.zogyD","coadd.zogyD", "report"];
        ListProduct     = ["", "Image", "Back", "Var", "Exp", "Nim", "PSF", "Cat", "Spec", "Mask", "Evt", "MergedMat", "Asteroids","Asteroids.Known","Asteroids.Fast","Pipeline", "TransientsCat"];
        SEPERATOR       = "_";
        FIELDS          = ["ProjName", "Time", "Filter", "FieldID", "Counter", "CCDID", "CropID", "Type", "Level", "Product", "Version", "FileType"];
        PATH_FIELDS     = ["SubDir", "BasePath", "BasePathRef", "Path"];
    end
    
    
    methods % Constructor
       
        function Obj = AstroFileName(Files, Args)
            % Constructor for FileNames
            % Input : - If 'Method'='Files' then this is a cell array,
            %           a string array, or char array of file names that will be parsed.
            %           If 'Method'='Template', then this is a file pattern
            %           to read from the 'Path' directory.
            %           If this is a numeric array, then will
            %           create an AstroFileName object of this size.
            %           If this is a table (or AstroCatalog), then attempt to populate the
            %           file names based on the table columns (e.g., if the
            %           table have a 'CropID' column, then its content will
            %           be stored in the AstroFileName CropID property).
            %           If this is an AstroFileName object, then it will be
            %           returned as is as output.
            %          * ...,key,val,...
            %            'Path' - cd to this path prior to start the file
            %                   name ingestion (e.g., files location).
            %                   If empty, then do not cd.
            %                   If 'Path' is not empty, then 'Method' is
            %                   overide to 'Template'.
            %                   Default is [].
            %            'Method' - Method to use for file names
            %                   construction. Options are:
            %                   'Template' - Use the first input argument
            %                           as a template name (e.g.,
            %                           'LAST.01*.fits). All files with the
            %                           specified template will be searched
            %                           in the 'Path' directory and upload
            %                           as file names.
            %                   'Files' - Treat the first input as a cell array,
            %                           a string array, or char array of
            %                           file names that will be parsed.
            %                   'auto' - If files contains '*', then will
            %                           set to 'template', otherwise 'files'.
            %                   Default is 'auto'.
            %            'ReadJD' - If input is a table, then this is a
            %                   logical indicating if to try and read a
            %                   table column nmae JD, into the JD property.
            %                   Default is true.
            %            'JD2Time' - If input is a table and JD was read,
            %                   then this is a logical indicating if to
            %                   convert the JD into Time string.
            %                   Default is false.
            %            'TableCol' - If input is table or AstroCatalog,
            %                   then this is a cell array or strings array
            %                   of column names corresponding to the FIELDS
            %                   property. If empty, then use te FIELDS
            %                   property. Note that JD is not translated.
            %                   Default is [].
            %            'JDCol' - If input is table or AstroCatalog,
            %                   then this is the JD column name in the
            %                   table.
            %                   Alternatively, if this is a 3 elements
            %                   string array, then the elements corresponds
            %                   to the year, month, day column names -
            %                   e.g., ["Year", "Month", "Day"]
            %                   Default is 'JD'.
            %            'SubDirCol' - If input is table or AstroCatalog,
            %                   then this is the SubDir column name in the
            %                   table. Default is 'Visit'.
            %            'ProjNameBase' - A string containing the base
            %                   ProjName. This string will be concat to the
            %                   columns in ProjNameCols to create the
            %                   ProjName. Default is 'LAST'.
            %            'ProjNameCols' - A string array of column names
            %                   containing the literals of project names
            %                   following the base.
            %                   If ProjNameBase and ProjNameCols are empty,
            %                   then ProjName will not be populated.
            %                   Default is ["Node","Mount","Camera"]
            %
            % Output : - An AstroFileName object.
            % Author : Eran Ofek (Oct 2024)
            % Example: A = AstroFileName;
            %          A = AstroFileName(2);
            %          A = AstroFileName([2 3]);
            %          A = AstroFileName('LAST.01.*fits','Path','.')
            %          A = AstroFileName("LAST.01.08.02_20240109.143054.460_clear_001+30_001_001_001_sci_raw_Image_1_fits")
            %          T = table; T.CropID=[1 2].';
            %          T.Product=["Image","PSF"].' ; T.JD=2451545+(0:1).';
            %          A = AstroFileName(T)
            %          A = AstroFileName(T, 'ReadJD',true, 'JD2Time',true)
            %          A = AstroFileName(OT(F,:), 'JD2Time',true, 'JDCol',["Year", "Month", "Day"]);
            
            arguments
                Files                = 1;  
                Args.Path            = [];
                Args.Method          = 'auto'; %'files';
                Args.ReadJD logical  = true;
                Args.JD2Time logical = false;
                Args.TableCol        = [];
                Args.JDCol           = 'JD'; % 'MIDJD', if Triplet ["Year", "Month", "Day"] then use these columns
                Args.SubDirCol       = 'Visit';
                Args.ProjNameBase    = 'LAST';
                Args.ProjNameCols    = ["Node","Mount","Camera"];
            end
          

            if ~isempty(Args.Path)
                Args.Method = 'Template';
            end
            
            if isnumeric(Files)   
                % numeric input - create an empty AstroFileName object.
                for I=1:1:prod(Files)
                    Obj(I).Time = "";
                end
                if numel(Files)>1
                    Obj = reshape(Obj, Files);
                end
            
            elseif istable(Files)
                % Input is a table
              
                Fields = AstroFileName.FIELDS;
                
                if isempty(Args.TableCol)
                    TableCol = Fields;
                else
                    TableCol = Args.TableCol;
                end
                
                Nfield=numel(Fields);
                for Ifield=1:1:Nfield
                    if tools.table.isColumn(Files, TableCol{Ifield})
                        Obj.(Fields{Ifield}) = Files.(TableCol{Ifield});
                    end
                end
                % Read JD
                if Args.ReadJD
                    Field = 'JD';
                    if numel(Args.JDCol)==3
                        % assume JDCol is a triplet of Year, Month, Day
                        Year  = Files.(Args.JDCol(1));
                        Month = Files.(Args.JDCol(2));
                        Day   = Files.(Args.JDCol(3));

                        Obj.JD = celestial.time.julday([Day Month Year]);
                        if Args.JD2Time
                            Obj.julday2time;
                        end

                    else
                        if tools.table.isColumn(Files, Args.JDCol)
                            Obj.(Field) = Files.(Args.JDCol);
                            if Args.JD2Time
                                Obj.julday2time;
                            end
                        end
                    end
                end
                % Read SubDir
                Field = 'SubDir';
                if tools.table.isColumn(Files, Args.SubDirCol)
                    Obj.(Field) = Files.(Args.SubDirCol);
                end
                % Read ProjName
                if ~isempty(Args.ProjNameBase) && ~isempty(Args.ProjNameCols)
                    % Return a cell array of numeric columns:
                    CellArray = tools.table.table2cellOfCols(Files(:,Args.ProjNameCols));
                    
                    Obj.ProjName = [Args.ProjNameBase, CellArray];
                end

                
            elseif isa(Files, 'AstroCatalog')
                % Input is an AstroCatalog
              
                Fields = AstroFileName.FIELDS;
                
                if isempty(Args.TableCol)
                    TableCol = Fields;
                else
                    TableCol = Args.TableCol;
                end
                
                Nfield=numel(Fields);
                for Ifield=1:1:Nfield
                    if isColumn(Files, TableCol{Ifield})
                        Obj.(Fields{Ifield}) = Files.getCol(TableCol{Ifield});
                    end
                end
                if Args.ReadJD
                    Field = 'JD';
                    if isColumn(Files, Args.JDCol)
                        Obj.(Field) = Files.getCol(Args.JDCol);
                        if Args.JD2Time
                            Obj.julday2time;
                        end
                    end
                end
            elseif isa(Files, 'AstroFileName')
                Obj = Files;
            else
                % Input is a string, char, cell
                if strcmpi(Args.Method, 'auto')
                    if any(contains(Files, '*'))
                        Args.Method = 'template';
                    else
                        Args.Method = 'files';
                    end
                end

                switch lower(Args.Method)
                    case 'files'
                        [Obj] = AstroFileName.parseString2AstroFileName(Files, true);
                        
                    case 'template'
                        if ~isempty(Args.Path)
                            PWD = pwd;
                            cd(Args.Path);
                        end
                        
                        Obj = AstroFileName.dir(Files);
                        
                        if ~isempty(Args.Path)
                            cd(PWD);
                        end
                        
                    otherwise
                        error('Unknown Method option');
                end
            end
            
            % Load defaults from configuration
            % i.e.
            %Obj.BasePath = Obj.Config.Data.System.ImagePath.BasePath;
            %Obj.DataDir  = Obj.Config.Data.System.ImagePath.DataDir;
            %Obj.TimeZone = Obj.Config.Data.System.Time.TimeZone;
            
            % Load header key names mapping from configuation
            %Obj.DictKeyNames = Dictionary.getDict('Header.ImagePath.KeyNames');
            
        end
             
        function Obj=setTestData(Obj)
            % Set data for unit-test and debugging, return expected result
            
            Obj.ProjName        = "USAT";            
            Obj.Time            = 2451545;
            Obj.TimeZone        = 2;
            Obj.Filter          = "clear";
            Obj.FieldID         = "fld";
            Obj.Counter         = 1;
            Obj.CCDID           = 1;
            Obj.CropID          = 1;
            Obj.Type            = "sci";
            Obj.Level           = "raw";
            Obj.Product         = "Image";
            Obj.Version         = 1;
            Obj.FileType        = "fits";
            Obj.SubDir          = "subdir";
            
            % Debug? or have it?
            Obj.BasePath        = "/home/last";
           
        end
    end
    
    methods % setter/getters

        function Obj = set.Type(Obj, Val)
            % Setter for Type
            if ischar(Val) || iscell(Val)
                Val = string(Val);
            end
            Obj.Type = Val(:);
            Obj.validateType;
        end
        function Obj = set.Level(Obj, Val)
            % Setter for Level
            if ischar(Val) || iscell(Val)
                Val = string(Val);
            end
            Obj.Level = Val(:);
            Obj.validateLevel;
        end
        function Obj = set.Product(Obj, Val)
            % Setter for Product
            if ischar(Val) || iscell(Val)
                Val = string(Val);
            end
            Obj.Product = Val(:);
            Obj.validateProduct;
        end
        
        function Obj = set.ProjName(Obj, Val)
            % Setter for ProjName
            % Example: AFN.ProjName={'LAST',1,2,3}

            if ischar(Val)
                Val = string(Val);
            end
            if iscell(Val)
                Val = AstroFileName.formatCellProjName(Val, Obj.FormatProjName);
            end
            Obj.ProjName = Val(:);
        end
        
        function Obj = set.Filter(Obj, Val)
            % Setter for Filter
            if ischar(Val) || iscell(Val)
                Val = string(Val);
            end
            Obj.Filter = Val(:);
        end
        
        function Obj = set.FieldID(Obj, Val)
            % Setter for FieldID

            if isnumeric(Val)
                Val = tools.string.sprintf2string(Obj.FormatFieldID, Val);
            else
                if ischar(Val) || iscell(Val)
                    Val = string(Val);
                end
            end
            Obj.FieldID = Val(:);
            
        end
        
        function Obj = set.Counter(Obj, Val)
            % Setter for Counter
           
            if isnumeric(Val)
                Val = tools.string.sprintf2string(Obj.FormatCounter, Val);
            else
                if ischar(Val) || iscell(Val)
                    Val = string(Val);
                end
            end
            Obj.Counter = Val(:);
                
        end
        
        function Obj = set.CCDID(Obj, Val)
            % Setter for CCDID
           
            if isnumeric(Val)
                Val = tools.string.sprintf2string(Obj.FormatCCDID, Val);
            else
                if ischar(Val) || iscell(Val)
                    Val = string(Val);
                end
            end
            Obj.CCDID = Val(:);
                
        end
        
        function Obj = set.CropID(Obj, Val)
            % Setter for CropID
           
            if isnumeric(Val)
                Val = tools.string.sprintf2string(Obj.FormatCropID, Val);
            else
                if ischar(Val) || iscell(Val)
                    Val = string(Val);
                end
            end
            Obj.CropID = Val(:);
                
        end
        
        function Obj = set.Version(Obj, Val)
            % Setter for Version
           
            if isnumeric(Val)
                Val = tools.string.sprintf2string(Obj.FormatVersion, Val);
            else
                if ischar(Val) || iscell(Val)
                    Val = string(Val);
                end
            end
            Obj.Version = Val(:);
                
        end
        
        function Obj = set.Path(Obj, Val)
            % Setter for path (convert char to string)

            if ischar(Val)
                Val = string(Val);
            end
            Obj.Path = Val;
        end
    end
      
    methods (Static) % construction
        % DONE
        function Result=formatCellProjName(ProjName, Format)
            % Format project name strings array
            %   convert {"LAST",1,2,3} to: "LAST.01.02.03"
            % Input  : - Cellarray ProjName. E.g., {"LAST",1,2,3}
            %            Numeric values can be either scalars or vectors.
            %            If vectors then will generate an array of
            %            ProjName.
            %          - Format. E.g., "%02d"
            % Output : - ProjName string
            % Author : Eran Ofek (Oct 2024)
            % Example: AstroFileName.formatCellProjName({"LAST",1,2,3},"%02d")
            %          A.ProjName = {'LAST',1,[2 1],[1 2]}

            if ~iscell(ProjName)
                error('Works only on cell ProjName input');
            end
            
            N = numel(ProjName);
            
            if N>1
                IsNumeric = cellfun(@isnumeric, ProjName);
                Nel       = cellfun(@numel, ProjName);
                NelNum    = IsNumeric.*Nel;
                Nproj     = max(1, max(NelNum));

                Format = sprintf("%%s.%s", Format);

                Result = strings(Nproj,1);
                for Iproj=1:1:Nproj
                    Result(Iproj) = ProjName{1};

                    for I=2:1:N
                        Iline = min(NelNum(I), Iproj);
                        Result(Iproj) = sprintf(Format, Result(Iproj), ProjName{I}(Iline));
                    end
                end
            end
        end

        % DONE
        function Result=parseString2literals(FileNameString, Seperator, SplitLast, SeperatorLast)
            % Parse strings to array of literals
            % Input  : - A string array, cell array or char array with file
            %            names.
            %          - Seperator. Default is "_".
            %          - A logical indicating if to split the last literal
            %            by dot and to sepearte it to Version
            %            and FileType. Default is true.
            %          - Seperator for last literal. Default is ".".
            % Output : - A string array, in which each row corresponds to
            %            file name and columns corresponds to literals.
            % Author : Eran Ofek (Oct 2024)
            % Example: 
            % A=AstroFileName.parseString2literals('LAST.01.10.01_20231214.233948.237_clear_074+55_020_001_021_sci_proc_PSF_1.fits')
            
            arguments
                FileNameString
                Seperator          = "_";
                SplitLast logical  = true;
                SeperatorLast      = ".";
            end
            
            if ischar(FileNameString)
                FileNameString = string(FileNameString).';
            end
            
            if iscell(FileNameString)
                FileNameString = string(FileNameString);
            end
            N = numel(FileNameString);
            
            Result = split(FileNameString(:), Seperator);
            if N==1
                Result = Result.';
            end
                
            if SplitLast
                if N==1
                    Result = [Result(:,1:end-1), split(Result(:,end), SeperatorLast).'];
                else
                    Result = [Result(:,1:end-1), split(Result(:,end), SeperatorLast)];
                end
            end
                
        end
        
        % DONE
        function [Result]=parseString2AstroFileName(FileNameString, IsSinglePath, Seperator)
            % Convert file names strings into an AstroFileName object.
            % Input  : - A cell array, string array, or char array of file
            %            names. Alternatively, a struct array which is the
            %            output of the dir function.
            %          - A logical indicating if to use a single path
            %            (true), or path per file (false).
            %            Default is true.
            %          - Seperator. Default is "_".
            % Output : - An AstroFileName object populated with the file
            %            name literals.
            % Author : Eran Ofek (Oct 2024)
            % Example: R=AstroFileName.parseString2AstroFileName(A)
            
            arguments
                FileNameString
                IsSinglePath logical = true;
                Seperator            = "_";
            end
        
            if iscell(FileNameString)
                FileNameString = string(FileNameString);
            end
            if isstring(FileNameString)
                FileNameString = FileNameString(:);
            end

            Result = AstroFileName;
            if isstruct(FileNameString)
                if IsSinglePath
                    if isempty(FileNameString)
                        Result.Path = "";
                    else
                        Result.Path = string(FileNameString(1).folder);
                    end
                else
                    Result.Path = string({FileNameString.folder});
                end
                FileNameString = {FileNameString.name};
            else
                [P1,P2,P3] = fileparts(FileNameString);
                FileNameString = join([P2, P3], '', 2);

                Result.Path = P1;
            end
            
            Literals = AstroFileName.parseString2literals(FileNameString, Seperator);
            
            if ~isempty(FileNameString)
                Nfields = numel(Result.FIELDS);
                for Ifield=1:1:Nfields
                    Result.(Result.FIELDS(Ifield)) = Literals(:,Ifield);
                end
            end
            
        end
        
        % DONE
        function [Result,DirSt]=dir(varargin)
            % dir like function that returns a populated AstroFileName object.
            % Input  : - File template name.
            % Output : - An AstroFileName object.
            %          - A structure array of directories.
            % Author : Eran Ofek (Oct 2024)
            % Example: R=AstroFileName.dir('LAST.01.*.fits')
            
            DirSt = dir(varargin{:});
            if isempty(DirSt)
                Result = AstroFileName;
            else
                Result = AstroFileName.parseString2AstroFileName(DirSt);
            end
        end
        
        function Result=fileTemplate(Args)
            % Generate (a single) file template name with optional wild cards based on specific literals.
            % Input  : * ...,key,val,...
            %            All the literals with their values:
            %            'ProjName' - Default is 'LAST.*'
            %            'Time' - Default is '*'
            %            'Filter' - Default is '*'
            %            'FieldID' - Default is '*'
            %            'Counter' - Default is '*'
            %            'CCDID' - Default is '*'
            %            'CropID' - Default is '*'
            %            'Type' - Default is 'sci'
            %            'Level' - Default is 'coadd'
            %            'Product' - Default is 'Image'
            %            'Version' - Default is '*'
            %            'FileType' - Default is 'fit*'
            %             
            %            'Path' - If not empty, then will concat this path
            %                   to the file name. Default is ''.
            % Output : - A char array of template file name.
            % Author : Eran Ofek (Nov 2024)
            % Example: AstroFileName.fileTemplate
            %          AstroFileName.fileTemplate('Level','proc')

            arguments
                Args.ProjName = "LAST.*"
                Args.Time     = '*';
                Args.Filter   = '*';
                Args.FieldID  = '*';
                Args.Counter  = '*';
                Args.CCDID    = '*';
                Args.CropID   = '*';
                Args.Type     = 'sci';
                Args.Level    = 'coadd';
                Args.Product  = 'Image';
                Args.Version  = '*';
                Args.FileType = 'fit*';
                Args.Path     = '';
            end

            %<ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>

            Result = sprintf('%s_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.%s', Args.ProjName, Args.Time, Args.Filter, Args.FieldID, Args.Counter, Args.CCDID, Args.CropID, Args.Type, Args.Level, Args.Product, Args.Version, Args.FileType);

            if ~isempty(Args.Path)
                Result = sprintf('%s%s%s',Args.Path, filesep, Result);
            end
        end

        function [Result,FileTemplate] = dirLiteral(Args)
            % dir like function with user provided literals that returns a populated AstroFileName.
            %   While in the dir command the user specify the file
            %   template, in this command the user specify required literals
            %   e.g., 'Level'='proc'.
            % Input  : * ...,key,val,...
            %            All the literals with their values:
            %            'ProjName' - Default is 'LAST.*'
            %            'Time' - Default is '*'
            %            'Filter' - Default is '*'
            %            'FieldID' - Default is '*'
            %            'Counter' - Default is '*'
            %            'CCDID' - Default is '*'
            %            'CropID' - Default is '*'
            %            'Type' - Default is 'sci'
            %            'Level' - Default is 'coadd'
            %            'Product' - Default is 'Image'
            %            'Version' - Default is '*'
            %            'FileType' - Default is 'fit*'
            %             
            %            'Path' - If not empty, then will concat this path
            %                   to the file name. Default is ''.
            % Output : - An AstroFileName object array with element per
            %            requested directory in path.
            %          - A char array of template file name.
            % Author : Eran Ofek (Nov 2024)
            % Example: AstroFileName.dirLiteral
            %          AstroFileName.dirLiteral('Level','proc')
            %          AstroFileName.dirLiteral('Level','proc', 'Path',P)
            
            arguments
                Args.ProjName = "LAST.*"
                Args.Time     = '*';
                Args.Filter   = '*';
                Args.FieldID  = '*';
                Args.Counter  = '*';
                Args.CCDID    = '*';
                Args.CropID   = '*';
                Args.Type     = 'sci';
                Args.Level    = 'coadd';
                Args.Product  = 'Image';
                Args.Version  = '*';
                Args.FileType = 'fit*';
                Args.Path     = '';
            end
            
            if isnumeric(Args.CropID)
                % convert to string
                Args.CropID = sprintf(AstroFileName.FormatCropID,Args.CropID);
            end

            ArgsCell = namedargs2cell(Args);

            if isempty(Args.Path)
                
                FileTemplate = AstroFileName.fileTemplate(ArgsCell{:});
                [Result] = AstroFileName.dir(FileTemplate);
            else
                if ischar(Args.Path)
                    Args.Path = {Args.Path};
                end
                Npath = numel(Args.Path);
                
                PWD = pwd;
                
                FileTemplate = AstroFileName.fileTemplate(ArgsCell{:}, 'Path','');

                for Ipath=1:1:Npath
                    cd(Args.Path{Ipath});
                    
                    [Result(Ipath)] = AstroFileName.dir(FileTemplate);
                end
                cd(PWD);
            end

        end

        

        % Done
        function Result=getValFromFileName(File,Prop)
            % Get specific proprty from file name
            % Input  : - A char array containing a single file name.
            %          - Property to retrieve (e.g.,
            %            'Time','JD','Type',...). Default is 'JD'.
            % Output : - Property value.
            % Author : Eran Ofek (Jan 2023)
            % Example: FileNames.getValFromFileName('LAST.01.02.01_20221229.212126.937_clear_050+09_050_001_001_sci_raw_Image_1.fits');
            
            arguments
                File
                Prop  = 'JD';
            end

            if ~(isstring(File) || ischar(File))
                error('File must be char or string');
            end
                
            SplitName = regexp(File,'_','split');
            switch Prop
                case 'ProjName'
                    Result = SplitName{1};
                case 'Time'
                    Result = SplitName{2};
                case 'JD'
                    DateVec = SplitName{2};
                    DateVec = convert.strFN2date(DateVec);
                    Result  = celestial.time.julday(DateVec(:,[3 2 1 4 5 6]));
                case 'Filter'
                    Result = SplitName{3};
                case 'FieldID'
                    Result = SplitName{4};

                case 'Counter'
                    Result = SplitName{5};

                case 'CCDID'
                    Result = SplitName{6};

                case 'CropID'
                    Result = SplitName{7};

                case 'Type'
                    Result = SplitName{8};

                case 'Level'
                    Result = SplitName{9};

                case 'Product'
                    Result = SplitName{10};

                case 'Version'
                    TmpSplit = split(SplitName{11},'.');
                    Result = TmpSplit{1};
                
                case 'FileType'
                    TmpSplit = split(SplitName{11},'.');
                    Result = TmpSplit{2};
                
                otherwise
                    error('Unknown property name');
            end

        end

        % Done
        function Str = julday2timeString(JD)
            % Convert JD to time string with format "yyyymmdd.HHMMSS.FFF"
            % Input  : - JD
            % Output : - String with time.
            % Author : Eran Ofek (Oct 2024)
            % Example: AstroFileName.julday2timeString(2451545+[0 1])
            
            Date = celestial.time.jd2date(JD(:),'H');
            N = numel(JD);
            Str = strings(N,1);
            for I=1:1:N
                Str(I)  = sprintf('%04d%02d%02d.%02d%02d%06.3f',Date(I,[3 2 1 4 5 6]));
            end
            
        end
    end
    
    
    
    
    methods % validation

        % DONE
        function [Result,Flag]=validateType(Obj, ErrorIfWrong)
            % Validate Type property
            % Input  : - FileNames object.
            %          - Logical indicating if to result in error
            %            if validation failed. Default is true.
            % Output : - A logical indicating if Type was validated.
            %          - A vector of logical indicating, for each Type in
            %            cell, if validated correctly.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                ErrorIfWrong logical  = true;
            end
            
            Flag = ismember(Obj.Type, Obj.ListType);
            if all(Flag)
                Result = true;
            else
                Result = false;
                if ErrorIfWrong
                    II=find(Flag,1);
                    error('Illegal value found in Type property (element %d): %s',II, Obj.Type);
                end
            end
            
        end
        
        % DONE
        function [Result,Flag]=validateLevel(Obj, ErrorIfWrong)
            % Validate Level property
            % Input  : - FileNames object.
            %          - Logical indicating if to result in error
            %            if validation failed. Default is true.
            % Output : - A logical indicating if Level was validated.
            %          - A vector of logical indicating, for each Type in
            %            cell, if validated correctly.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                ErrorIfWrong logical  = true;
            end
            
            Flag = ismember(Obj.Level, Obj.ListLevel);
            if all(Flag)
                Result = true;
            else
                Result = false;
                if ErrorIfWrong
                    II=find(Flag,1);
                    error('Illegal value found in Level property (element %d)',II);
                end
            end
            
        end
        
        % DONE
        function [Result,Flag]=validateProduct(Obj, ErrorIfWrong)
            % Validate Product property
            % Input  : - FileNames object.
            %          - Logical indicating if to result in error
            %            if validation failed. Default is true.
            % Output : - A logical indicating if Product was validated.
            %          - A vector of logical indicating, for each Type in
            %            cell, if validated correctly.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                ErrorIfWrong logical  = true;
            end
            
            Flag = ismember(Obj.Product, Obj.ListProduct);
            if all(Flag)
                Result = true;
            else
                Result = false;
                if ErrorIfWrong
                    II=find(Flag,1);
                    error('Illegal value found in Product property (element %d)',II);
                end
            end
            
        end
        
        % DONE
        function [Result, Flag] = validate(Obj, ErrorIfWrong)
            % Validate Product property
            % Input  : - FileNames object.
            %          - Logical indicating if to result in error
            %            if validation failed. Default is true.
            % Output : - A logical indicating if Product was validated.
            %          - A vector of logical indicating, for each Type in
            %            cell, if validated correctly.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                ErrorIfWrong logical  = true;
            end
            
            [Result(1),Flag1] = validateType(Obj,    ErrorIfWrong);
            [Result(2),Flag2] = validateLevel(Obj,   ErrorIfWrong);
            [Result(3),Flag3] = validateProduct(Obj, ErrorIfWrong);
            
            Flag = Flag1 & Flag2 & Flag3;
            Result = all(Result);
            
        end
        
    end
    
    methods (Static) % static time utilities
        function BaseFieldID=getBaseFieldID(FieldID)
            % Extract the base fieldid from a full fieldid (i.e., the first dot-seperated literal).
            % Input  : - A string vector of FieldID.
            % Output : - A string vector of base FieldID.
            % Author : Eran Ofek (Jul 2025)
            % Example: AstroFileName.getBaseFieldID('1632.c')

            
            if numel(FieldID)>1
                % Find entries without a dot
                NoDot = cellfun(@isempty, regexp(FieldID, '\.'));

                % Add dot to those entries
                FieldID(NoDot) = FieldID(NoDot) + ".";
            end
            
            TmpFieldID  = split(FieldID(:),'.',2); 
            BaseFieldID = TmpFieldID(:,1);
        end

        function [Date, DateDir] = jd2datedir(JD, TimeZone)
            % convert JD to date dir of the form: YYYY/MM/DD (changed at local noon)
            % Input  : - JD
            %          - Time Zond (days). Default is 2./24
            % Output : - Date vector.
            %          - Date dir of the form: YYYY/MM/DD
            % Author : Eran Ofek (Mar 2025)
            % Example: [Date, DateDir] = AstroFileName.jd2datedir(2451545.5+[0;100])

            arguments
                JD
                TimeZone  = 2./24;
            end

            JD = JD + TimeZone./24;
            JD = floor(JD);
            %JD = JD(Ind);
            Date = celestial.time.jd2date(JD);

            if nargout>1
                N = numel(JD);
                DateDir = strings(N,1);
                for I=1:1:N
                    DateDir(I) = sprintf("%04d%s%02d%s%02d",Date(I,3),filesep,Date(I,2),filesep,Date(I,1));
                end
            end

        end
    end

    methods % time utilities
        % DONE
        function JD = julday(Obj, TimeFormat)
            % Convert Time property to JD (not populating the JD property)
            % Input  : - An AstroFileName object.
            %          - A time format.
            %            If empty, use the FormatTime property.
            %            Default is [].
            % Output : - JD
            % Author : Eran Ofek (Oct 2024)
            % Example: R.julday
            
            arguments
                Obj
                TimeFormat = [];
            end
            
            if isempty(TimeFormat)
                TimeFormat = Obj.FormatTime;
            end
            
            TimeMat = datevec(Obj.Time, TimeFormat);
            JD = celestial.time.julday(TimeMat(:,[3 2 1 4 5 6]));
            
        end
        
        % DONE
        function Obj = julday2time(Obj, JD)
            % Populate Time from JD
            % Input  : - self.
            %          - JD. If empty, then use JD property.
            %            Default is [].
            % Output : - An AstroFileName object with Time property
            %            populated wtith time string corresponding to the JD.
            % Author : Eran Ofek (Oct 2024)
            % Example: R.julday2time(2451545)
            
            arguments
                Obj
                JD    = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            else
                Obj.JD = JD;
            end
            
            DateStr = AstroFileName.julday2timeString(JD);
            Obj.Time = DateStr;
        end
            
        % DONE
        function JD = juldayFun(Obj, Fun)
            % Apply a function on the JD of each element of a FileNames object.
            % Input  : - A FileNames object.
            %          - Function handle. Default is @min.
            % Output : - An array of FUN(JD) for each element.
            %            For empty elements return NaN.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Fun   = @min;
            end

            Nobj = numel(Obj);
            JD   = zeros(size(Obj));
            for Iobj=1:1:Nobj
                Tmp = Fun(Obj(Iobj).julday);
                if isempty(Tmp)
                    JD(Iobj) = NaN;
                else
                    JD(Iobj) = Tmp;
                end
            end


        end
        
        % DONE
        function Result = getDateDir(Obj, Ind, Args)
            % Get the date directory names for te file names.
            %   The DateDir is the YYYY/MM/DD directory structure for
            %   storing data.
            % Input  : - self.
            %          - Indices of lines (file names) for which to
            %            generate DateDir. If empty, generate for all
            %            lines. Default is 1.
            %          * ...,key,val,...
            %            'BreakToYMD' - If true then will break to [YYYY,
            %                   MM, DD] strings. If false then return [YYYYMMDD]
            %                   strings. Default is true.
            %            'UseJD' - If true, then calculate the DateDir from the JD.
            %                   Otherwise, cut it from the Time strings.
            %                   Default is false.
            % Output : - A string array of the YYYY, MM, DD directories.
            %            If BreakToYMD is true, then the output have 3
            %            columns, otherwise, one column.
            % Author : Eran Ofek (Oct 2025)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.getDateDir([],'BreakToYMD',true, 'UseJD',true)
            %          A.getDateDir(1)
            
            arguments
                Obj
                Ind                     = 1;
                Args.BreakToYMD logical = true;
                Args.UseJD logical      = false;  % false; - with false no TimeZone treatment
            end
            
            if isempty(Ind)
                Nfile = Obj.nFiles;
                Ind   = (1:1:Nfile).';
            end
            Nind = numel(Ind);            
            
            if Args.UseJD
                JD = Obj.julday;
                JD = JD + Obj.TimeZone./24;
                JD = floor(JD);
                JD = JD(Ind);
                Date = celestial.time.jd2date(JD);
                
                if Args.BreakToYMD
                    Result = strings(Nind,3);
                    for I=1:1:Nind
                        Result(I,1) = sprintf("%04d", Date(I,3));
                        Result(I,2) = sprintf("%02d", Date(I,2));
                        Result(I,3) = sprintf("%02d", Date(I,1));
                    end
                else
                    Result = strings(Nind,1);
                    for I=1:1:Nind
                        Result(I) = sprintf("%04d%02d%02d",Date(I,[3 2 1]));
                    end
                end
            else
                if Args.BreakToYMD
                    Result = [extractBetween(Obj.Time(Ind), 1, 4), extractBetween(Obj.Time(Ind), 5, 6), extractBetween(Obj.Time(Ind), 7, 8)];
                else
                    Result = extractBefore(Obj.Time(Ind), 9);
                end
            end
            
        end
        
        % DONE
        function Result=validTimes(Obj)
            % Return a vector of logical indicating if Time argument is valid
            % Input  : - A single elemnent AstroFileName object.
            % Output : - A vector of logical which length equal to the
            %            number of file names. False if Time is NaN, [],
            %            or 'NaN'.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.validTimes

            arguments
                Obj(1,1)
            end

            N  = Obj.nFiles;
            JD = Obj.julday;
            if isempty(JD) || any(isnan(JD)) || N~=numel(JD)
                Result = false;
            else
                Result = true;
            end

        end
        
        % DONE
        function [SunAlt] = sunAlt(Obj, Args)
            % Calculate Sun Altitude for images in an AstroFileName object
            % Input  : - A single element AstroFileName object
            %          * ...,key,val,...
            %            'GeoPos' - Geodetic position [Lon, Lat] in deg.
            %                   Default is [35 30].
            % Output : - An array of Sun altitude (deg) for each image
            %            entry.
            % Author : Eran Ofek (May 2022)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.sunAlt
            
            arguments
                Obj(1,1)
                Args.GeoPos    = [35 30];
            end
            
            RAD = 180./pi;
            
            VecJD    = Obj.julday;
            VecJD    = VecJD(:);
            LST      = celestial.time.lst(VecJD, Args.GeoPos(1)./RAD);  % frac of day
            [RA,Dec] = celestial.SolarSys.suncoo(VecJD, 'j'); % [rad]
            HA       = LST.*2.*pi - RA;                       % [rad]
            [SunAz,SunAlt] = celestial.coo.hadec2azalt(HA, Dec, Args.GeoPos(2)./RAD); % [rad]
            SunAlt   = SunAlt.*RAD; 
            
        end
        
        % DONE
        function [Ind, NearJD, Result]=selectNearest2JD(Obj, TargetJD)
            % Return index of image which JD is nearest to some JD/date.
            % Input  : - A single element AstroFileName object
            %          - JD or date [D M Y [H M S]].
            %            The function will select the file name with JD
            %            nearest to this date.
            %            If empty, use current JD.
            %            Default is [].
            % Output : - Index of image with nearest JD to TargetJD
            %          - JD of image with nearest JD.
            %          - A (new copy) FileNames object with the image with the nearest JD.
            % Author : Eran Ofek (Mar 2023)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.selectNearest2JD(2451546)

            
            arguments
                Obj(1,1)
                TargetJD = [];
            end
            
            if isempty(TargetJD)
                TargetJD = celestial.time.julday;
            else
                if size(TargetJD,2)>2
                    TargetJD = celestial.time.julday(TargetJD);
                end
            end
            
            JD = Obj.julday;
            [~, Ind] = min(abs(JD-TargetJD));
            NearJD   = JD(Ind);
            
            if nargout>2
                Result = reorderEntries(Obj, Ind, 'CreateNewObj',true);
            end
        end
        
        % DONE
        function [Ind, LastJD, DT, Result]=selectLastJD(Obj)
            % Return index of image with largest JD
            % Input  : - A single element AstroFileName object
            % Output : - Index of image with largest JD
            %          - JD of image with largest JD
            %          - Time elpased since image with largest JD was taken
            %            I.e., CurrentJD - LastJD [days].
            %          - A FileNames object with the image with latest JD.
            % Author : Eran Ofek (Mar 2023)
            
            JD = Obj.julday;
            [LastJD, Ind] = max(JD);
            if nargout>2
                DT = celestial.time.julday - LastJD;
            end
            
            if nargout>3
                Result = reorderEntries(Obj, Ind, 'CreateNewObj',true);
            end
        end
    
        % DONE
        function Result = updateTime(Obj,Args)
            % Update time according to actual file real time stamp.
            %   Given an AstroFileName, cd to dir and look for file name with
            %   wild card time. Replace the Time stamp with the actual
            %   time.
            % Input  : - A single element AstroFileName object.
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object. Default is false.
            % Output : - An updated AstroFileName object.
            % Author : Eran Ofek (Feb 2025)
            % Example: AFN.updateTime

            arguments
                Obj(1,1)
                Args.CreateNewObj    = false;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            AllPath = Obj.genPath([]);
            AllFile = Obj.insertWildCards([], 'List',"Time");

            PWD = pwd;

            Nfile   = numel(AllFile);
            for Ifile=1:1:Nfile
                cd(AllPath{Ifile});
                File = dir(AllFile{Ifile});
                if numel(File)==1
                    Literals = AstroFileName.parseString2literals(File.name);
                    Result.Time(Ifile,:) = Literals(2);
                end
            end

            cd(PWD);

        end
    end
    
    methods % utilities
        
        function Result = getPropBasePath(Obj, Ind)
            % Get the BasePath property value including Path cell treatment
            % Input  : - self.
            %          - Index of image.
            %          * ...,key,val,...
            % Output : - Property value.
            % Author : Eran Ofek (Oct 2024)
            % Example: A.getPropBasePath

            arguments
                Obj
                Ind                  = [];
            end
            ColPath = 2;
            Prop    = 'BasePath';

            % Path contains a directory per year format
            YMD = Obj.getDateDir(Ind);
                
            [~,YearInd] = ismember(YMD(:,1), Obj.(Prop)(:,1));
            Result      = string(Obj.(Prop)(YearInd, ColPath));
            

        end

        % DONE
        function Result = getProp(Obj, Prop, Ind, Args)
            % Get specific property value and entry (index)
            % Input  : - self.
            %          - Property name.
            %          - Index. If empty, get all. Default is [].
            %          * ...,key,val,...
            %            'RepMat' - If true, then if the output string
            %                   contains one element and the numbre of files
            %                   (lines) in the object is >1, then the output will
            %                   be replicated using repmat to contains the
            %                   number of requested lines.
            % Output : - Property value.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.getProp('Type',[])
            %          A.getProp('Type',1)
            %          A.getProp('Path',[], 'RepMat',true)
            %          A.getProp('Path',1:2, 'RepMat',true)
            %          A.getProp('Path',1:2, 'RepMat',false)
           
            arguments
                Obj
                Prop                 = "Time";
                Ind                  = [];
                Args.RepMat logical  = false;
            end
            
            if strcmp(Prop, 'BasePath') && iscell(Obj.BasePath) && numel(Obj.BasePath)>1
                Result = Obj.getPropBasePath(Ind);
            else

                if Args.RepMat
                    % make sure that the length of the property is exactly like
                    % Time
                    Nfile = Obj.nFiles;
                    if isempty(Ind)
                        Ind = (1:1:Nfile).';
                    end
                    Nind = numel(Ind);
                    
                    Nprop = numel(Obj.(Prop));
                    if Nprop==1 && Nind>1
                        % repmat
                        Result = repmat(Obj.(Prop), Nind, 1);
                    else
                        if Nprop==1
                            Result = Obj.(Prop);
                        else
                            Result = Obj.(Prop)(Ind);
                        end
                    end
                    
                else
                    if isempty(Ind)
                        Result = Obj.(Prop);
                    else
                        Nprop = numel(Obj.(Prop));
                        if Nprop<max(Ind)
                            Nind = numel(Ind);
                            Result = repmat(Obj.(Prop), Nind, 1);
                        else
                            Result = Obj.(Prop)(Ind);
                        end
                    end
                end
            end
        end
       
        % DONE
        function Result = nFiles(Obj)
            % Return the number of files (times) in the object.
            % Input  : - self.
            % Output : - Number of files (times).
            % Author : Eran Ofek (Oct 2024)
            % Example: nFiles(A)

            Nobj = numel(Obj);
            Result = zeros(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = numel(Obj(Iobj).Time);
                if Result==1
                    if isempty(char(Obj(Iobj).Time))
                        Result(Iobj) = 0;
                    end
                end
            end
        end
          
        % DONE
        function Result = isemptyFile(Obj)
            % Check if all elements of FileNames object contain no files
            % Input  : - An AstroFileName object.
            % Output : - A vector of logical (per object element).
            %            Contains true if the element contains no
            %            lines/files.
            % Author : Eran Ofek (May 2023)
            % Exampe: A=AstroFileName.dir('LAST.01.*fits');
            % A.isemptyFile
            
            Nobj = numel(Obj);
            Result = false(size(Obj));
            for Iobj=1:1:Nobj
                if Obj.nFiles==0
                    Result(Iobj) = true;
                end
            end

        end
    
        function repProp(Obj)
            % Make sure that the length of all FIELDS and PATH_FIELDS is the same by repmat these fields.
            % Input  : - self.
            % Output : - An AstroFileName object in which all the file/path
            %            literal fields have the same length as the 'Time'
            %            field.
            % Author : Eran Ofek (Nov 2024)
            % Example: A.repProp

            AllFields = [AstroFileName.FIELDS, AstroFileName.PATH_FIELDS];
            Nf        = numel(AllFields);

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Ntime = nFiles(Obj(Iobj));
                for If=1:1:Nf
                    if numel(Obj(Iobj).(AllFields{If}))==1
                        Obj(Iobj).(AllFields{If}) = repmat(Obj(Iobj).(AllFields{If}), Ntime, 1);
                    end
                end
            end

        end
    
        function Result = isCompressed(Obj, Args)
            % Check if file name corresponds to compressed file
            % Input  : - A single element AstroFileName object.
            %          * ...,key,val,...
            %            'CompressionType' - Compression type to search.
            %                   Default is '.fz'.
            % Output : - A vector of logicals indicating if each file is
            %            compressed.
            % Author : Eran Ofek (Sep 2025)

            arguments
                Obj(1,1)
                Args.CompressionType = '.fz';
            end

            Iobj = 1;
            Result = contains(Obj(Iobj).FileType, Args.CompressionType);

        end

        function Obj=compress(Obj, Type)
            % Compress files in AstroFileName object and modify FileType accordingly.
            % Input  : - self.
            %          - Compression type. Options are:
            %            'fz' | 'gz' | 'bz2'
            %            Default is 'fz'.
            % Output : - Updated AstroFileName object.
            % Author : Eran Ofek (Sep 2025)

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Files = Obj(Iobj).genFile();
                Nf = numel(Files);
                for If=1:1:Nf
                    switch lower(Type)
                        case 'fz'
                            system(sprintf("fpack %s", Files{If}));
                        case 'gz'
                            system(sprintf("gzip %s", Files{If}));
                        case 'bz2'
                            system(sprintf("bzip2 %s", Files{If}));
                        otherwise
                            error('Unsupported compression type: %s',Type);
                    end
                end
                Obj(Iobj).FileType = append(Obj(Iobj).FileType, ['.',Type]);
                
            end

        end

        function Obj=uncompress(Obj, Type)
            % Uncompress files in AstroFileName object and modify FileType accordingly.
            % Input  : - self.
            %          - Compression type. Options are:
            %            'fz' | 'gzip' | 'bzip2'
            %            Default is 'fz' (with -D option).
            % Output : - Updated AstroFileName object.
            % Author : Eran Ofek (Sep 2025)

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Files = Obj(Iobj).genFile();
                Nf = numel(Files);
                for If=1:1:Nf
                    switch lower(Args.Type)
                        case 'fz'
                            system(sprintf("funpack -D %s", Files{If}));
                        case 'gz'
                            system(sprintf("gunzip %s", Files{If}));
                        case 'bz2'
                            system(sprintf("bunzip2 %s", Files{If}));
                        otherwise
                            error('Unsupported compression type: %s',Type);
                    end
                end
                Obj(Iobj).FileType = strrep(Obj(Iobj).FileType, ['.',Type], '');
            end

        end


        
        function Result = isFileInList(Obj, FileName)
            % Check if File name is contained within the object file names
            %   For physical existince of the file see: isFileExist
            % Input  : - self.
            %          - File Name.
            % Output : - A logical array (size of input object), indicating
            %            if each object element contain the file name 
            % Author : Eran Ofek (Sep 2025)
            % Example: AFN.isFileInList('LAST.01.01.01_20250915.213856.596_clear_1240_020_001_012_sci_proc_Image_1.fits');

            Nobj = numel(Obj);
            Result = false(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = any(strcmp(Obj(Iobj).genFile, FileName));
            end

        end

        % DONE
        function Result = isFileExist(Obj)
            % Check if files in AstroFileName object exist in their path
            %   For logical existence of the file name see: isFileInList
            % Input  : - self.
            % Output : - A vector of logicals indicating if each file exist
            %            in its path (pgysical existence).
            % Author : Eran Ofek (Feb 2025)
            % Example: AFN.isFileExist

            AllFiles = Obj.genFull([]);
            Result   = isfile(AllFiles);

        end
    end

    methods % generate file names and path
        % DONE
        function Result=genFile(Obj, Ind, Args)
            % Generate file names from literals in AstroFileName object.
            %   Optionally, replace some literals with user provided
            %   strings.
            % Input  : - self.
            %          - Indices of lines (file names) in the AstroFileName
            %            object for which to generate file names.
            %            If empty, use all. Default is [].
            %          * ...,key,val,...
            %            * Any of the literals (e.g., 'ProjName',
            %              'Time',...).
            %               The user can provide a char array or string
            %               array of literals to replace the ones in the
            %               AstroFileName object (the object will not
            %               change).
            % Output : - A string array of file names.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST*.fits');
            %          A.genFile     % generate for all files
            %          A.genFile(1:10)   % only first 10 files
            %          A.genFile([],'Level','raw')  % replace Level to 'raw'
            %          A.genFile([],'Level','raw','Version',2)  % replace Level to 'raw' and version to 2.

            arguments
                Obj
                Ind            = [];
                Args.ProjName  = [];
                Args.Time      = [];
                Args.Filter    = [];
                Args.FieldID   = [];
                Args.Counter   = [];
                Args.CCDID     = [];
                Args.CropID    = [];
                Args.Type      = [];
                Args.Level     = [];
                Args.Product   = [];
                Args.Version   = [];
                Args.FileType  = [];
            end
            
            Nfile = Obj.nFiles;
            if isempty(Ind)
                Ind = (1:1:Nfile).';
            end
            Nind = numel(Ind);

            Nfields  = numel(Obj.FIELDS);
            Literals = strings(Nind, Nfields);

            % prepare an array of literals
            % line per file, column per literal
            for I=1:1:Nfields
                % for each literal in file name
                if isempty(Args.(Obj.FIELDS(I)))
                    % use data from object
                    TmpCol = Obj.(Obj.FIELDS(I));
                else
                    % use data from user input
                    % convert to string if needed
                    TmpCol = string(Args.(Obj.FIELDS(I)));
                end
                switch numel(TmpCol)
                    case 1
                        Tmp = repmat(TmpCol, Nind, 1);
                    case 0
                        Tmp = strings(Nind,1);
                    otherwise
                        Tmp = TmpCol(Ind);
                end
                Literals(:,I) = Tmp;
            end

            Delim = [repmat(Obj.SEPERATOR,1,Nfields-2), "."];
            Result = join(Literals, Delim); %Obj.SEPERATOR);

        end

        % DONE
        function Result=genRefPath(Obj, Ind, Args)
            % Generate RefPath (path for reference images)
            % Input  : - self.
            %          - Indices of lines (file names) for which to
            %            generate ref image path. If empty, then generate
            %            for all images. Default is 1.
            %          * ...,key,val,...
            %            'CleanFieldID' - A logical indicating if to clean
            %                   the FieldID such it will contain only the first
            %                   dot seperated literal. Default is true.
            % Output : - A string array of reference image path.
            %            Construted from BasePathRef/FieldID.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.genRefPath
            %          A.genRefPath([])
            
            arguments
                Obj
                Ind    = 1;
                Args.CleanFieldID = true;
            end
            
            if isempty(Ind)
                Nfile = Obj.nFiles;
                Ind   = (1:1:Nfile).';
            end
            Nind = numel(Ind);
            if Nind>1
                TmpBasePathRef = repmat(Obj.BasePathRef, Nind, 1);
            else
                TmpBasePathRef = Obj.BasePathRef;
            end
            TmpFieldID = Obj.FieldID(Ind);
            if Args.CleanFieldID
                TmpFieldID = AstroFileName.getBaseFieldID(TmpFieldID);
            end
            
            Result = join([TmpBasePathRef, TmpFieldID], filesep);
            
        end
        
        % DONE
        function Result=genPath(Obj, Ind, Args)
            % Generate path for file names.
            % Input  : - self.
            %          - Indices of lines (file names) in the object.
            %            If empty, then get all lines. Default is 1.
            %          * ...,key,val,...
            %            'PathType' - If the object "Path" property is
            %                   populated, then the path will be retrieved
            %                   from this property. Otherwise, will be
            %                   work according to one of the following
            %                   options:
            %                   'proc' - Path of the form:
            %                           /BasePath/ProjName/YYYY/MM/DD/proc/SubDir
            %                   'raw' - Path of the form:
            %                           /BasePath/ProjName/YYYY/MM/DD/raw
            %                   'new'|'calib'|'failed' - Path of the form:
            %                           /BasePath/ProjName/<new>
            %                   'ref' - Get the reference images file names
            %                           using genRefPath.
            %                           Answer is of the form:
            %                           /RefBasePath/FieldID
            %                   Default is 'proc'.
            %            'BasePathIncludeProjName' - A logical indicating
            %                   if the path includes the ProjName string.
            %                   If empty, then read it from the object
            %                   property.
            %                   Default is [].
            %            'AddSubDir' - Like BasePathIncludeProjName but for
            %                   adding the SubDir string. Default is [].
            % Output : - A string array of path.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits'); A.Path = [];
            %          A.genPath;
            %          A.genPath([])
            %          A.genPath(1,'BasePathIncludeProjName',false);
            %          A.genPath(1,'AddSubDir',false);
            %          A.genPath([], 'PathType','raw')
            %          A.genPath([], 'PathType','new')
            %          A.genPath(1:2, 'PathType','calib')
            %          A.genPath(3, 'PathType','failed')
            %          A.genPath(3:4, 'PathType','ref')
            %          A.genPath([], 'PathType','ref')
           
            arguments
                Obj(1,1)
                Ind                   = 1;
                Args.PathType         = 'proc';  % 'new'|'calib'|'failed'|'proc'|'raw'|'ref'
                Args.BasePathIncludeProjName = [];
                Args.AddSubDir               = [];
            end
            if isempty(Args.BasePathIncludeProjName)
                BasePathIncludeProjName = Obj.BasePathIncludeProjName;
            else
                BasePathIncludeProjName = Args.BasePathIncludeProjName;
            end
            if isempty(Args.AddSubDir)
                AddSubDir = Obj.AddSubDir;
            else
                AddSubDir = Args.AddSubDir;
            end
            
            if isempty(Obj.Time)
                Obj.julday2time;
            end

            if isempty(Obj.Path)
                % Construct Path based on available properties:
                
                if isempty(Ind)
                    Nfile = Obj.nFiles;
                    Ind   = (1:1:Nfile).';
                end
                Nind = numel(Ind);
                
                switch lower(Args.PathType)
                    case 'proc'                
                        %BasePath/ProjName/YYYY/MM/DD/proc/visit
                        YMD         = Obj.getDateDir(Ind, 'BreakToYMD',true, 'UseJD',true);
                        ProjStr     = Obj.getProp("ProjName", Ind, 'RepMat',true);
                        VisitStr    = Obj.getProp("SubDir", Ind, 'RepMat',true);
                        BasePathStr = Obj.getProp("BasePath", Ind, 'RepMat',true);
                       
                        if BasePathIncludeProjName
                            ProjStr     = Obj.getProp("ProjName", Ind, 'RepMat',true);
                            if Args.AddSubDir
                                Result = join([BasePathStr, ProjStr, YMD, repmat("proc", Nind, 1), VisitStr], filesep);
                            else
                                Result = join([BasePathStr, ProjStr, YMD, repmat("proc", Nind, 1)], filesep);
                            end
                                
                        else
                            if Args.AddSubDir
                                Result = join([BasePathStr, YMD, repmat("proc", Nind, 1), VisitStr], filesep);
                            else
                                Result = join([BasePathStr, YMD, repmat("proc", Nind, 1)], filesep);
                            end
                        end
                    case {'new','calib','failed'}
                        %BasePath/ProjName/[new]
                        StrPathType = string(Args.PathType);
                        
                        BasePathStr = Obj.getProp("BasePath", Ind, 'RepMat',true);
                        
                        if BasePathIncludeProjName
                            ProjStr     = Obj.getProp("ProjName", Ind, 'RepMat',true);
                            Result = join([BasePathStr, ProjStr, repmat(StrPathType, Nind, 1)], filesep);
                        else
                            Result = join([BasePathStr, repmat(StrPathType, Nind, 1)], filesep);
                        end
                    case 'raw'
                        %BasePath/ProjName/YYYY/MM/DD/raw
                        
                        YMD         = Obj.getDateDir(Ind, 'BreakToYMD',true, 'UseJD',false);
                        BasePathStr = Obj.getProp("BasePath", Ind, 'RepMat',true);
                        
                        if BasePathIncludeProjName
                            ProjStr     = Obj.getProp("ProjName", Ind, 'RepMat',true);
                            Result = join([BasePathStr, ProjStr, YMD, repmat("raw", Nind, 1)], filesep);
                        else
                            Result = join([BasePathStr, YMD, repmat("raw", Nind, 1)], filesep);
                        end
                    case 'ref'
                        Result = Obj.genRefPath(Ind);
                    otherwise
                        error('Unknown PathType option');
                end
                
                
            else
                % use Path in AstroFileName object
                
                Nfile = Obj.nFiles;
                Npath = numel(Obj.Path);
                if isempty(Ind)
                    if Nfile==Npath
                        Result = Obj.Path;
                    else
                        if Npath==1
                            Result = repmat(Obj.Path, Nfile, 1);
                        else
                            error('Npath not 1 and not Nfile');
                        end
                    end
                else
                    % Ind is provided
                    %Ind  = (1:1:Nfile).';
                    Nind = numel(Ind);
                    
                    if Npath==1
                        Result = repmat(Obj.Path, Nind, 1);
                    else
                        Result = Obj.Path(Ind);
                    end
                end
            end
            
        end
        
        % DONE
        function Result = genFull(Obj, Ind, Args)
            % Generate full path+fill names using genFile and genPath
            % Input  : - self.
            %          - Indices of lines (file names) in the object.
            %            If empty, then get all lines. Default is [].
            %          * ...,key,val,...
            %            'genFileArgs' - A cell array of additional key,val
            %                   arguments to pass to genFile.
            %                   Default is {}.
            %            'genPathArgs' - A cell array of additional key,val
            %                   arguments to pass to genPath.
            %                   Default is {}.
            
            arguments
                Obj
                Ind                = [];
                Args.genFileArgs   = {};
                Args.genPathArgs   = {};
            end
           
            Result = join([genPath(Obj, Ind, Args.genPathArgs{:}),...
                           genFile(Obj, Ind, Args.genFileArgs{:})], filesep);
            
            
        end
    
        % DONE
        function Result = genProducts(Obj, Ind, Args)
            % Given file names, generate all associated products
            %   Associate products refer to 'Image','Mask','PSF','Cat'.
            % Input  : - A single element AstroFileName object.
            %          - Indices of lines (file names) in the object.
            %            If empty, then get all lines. Default is [].
            %          * ...,key,val,...
            %            'OutProduct' - Output product names to generate.
            %                   Default is ["Image", "Mask", "PSF", "Cat"]
            %            'AddPath' - A logical indicating if to add the
            %                   full path to the file name.
            %                   Default is true.
            % Output : - Given that Nfile is the number of requested files
            %            (i.e., numel(Ind)), and Nprod is the number of
            %            products, then return a Nfile X Nprod strings
            %            array. Columns corresponds to different products
            %            and rows to different file names.
            % Author : Eran Ofek (Oct 2024).
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.genProducts(1)
            %          A.genProducts([])
            %          A.genProducts(1:2, 'OutProduct',["PSF"]);
            %          A.genProducts(1, 'AddPath',false)
            
            arguments
                Obj(1,1)
                Ind                  = [];
                Args.OutProduct      = ["Image", "Mask", "PSF", "Cat"];
                Args.AddPath logical = true;
            end
            
            if isempty(Ind)
                Nfile = Obj.nFiles;
                Ind   = (1:1:Nfile).';
            end
            Nind = numel(Ind);
            
            Nprod = numel(Args.OutProduct);
            Result = strings(Nind, Nprod);
            for Iprod=1:1:Nprod
                if Args.AddPath
                    Result(:,Iprod) = genFull(Obj, Ind, 'genFileArgs', {'Product', Args.OutProduct{Iprod}});
                else
                    Result(:,Iprod) = genFile(Obj, Ind, 'Product', Args.OutProduct{Iprod});
                end
            end
            
        end
    
        function Result = insertWildCards(Obj, Ind, Args)
            % Generate file names with wild cards at specific literals
            % Input  : - self.
            %          - Indices of lines (file names) in the AstroFileName
            %            object for which to generate file names.
            %            If empty, use all. Default is [].
            %          * ...,key,val,...
            %            * 'List' - A string array or cell array of the literals (e.g., 'ProjName',
            %                   'Time',...) - Each one of the literals in
            %                   this list will be replaced with the wild card character (default is "*").
            %              'WildCard' - Wild crad character.
            %                   Default is "*".
            % Output : - A string array of file names.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST*.fits');
            %          A.genFile     % generate for all files
            
            arguments
                Obj
                Ind            = [];
                Args.List      = ["Time","Counter"];
                % ProjName, Time, Filter, FieldID, Counter, CCDID, CropID,
                % Type, Level, Product Version, FileType
                Args.WildCard  = "*";
               
            end
            
            N = numel(Args.List);
            ArgsList = strings(1,2.*N);
            for I=1:2:2.*N-1
                IndL = (I + 1).*0.5;
                ArgsList(I)   = string(Args.List{IndL});
                ArgsList(I+1) = Args.WildCard;
            end
            
            Result = genFile(Obj, Ind, ArgsList{:});
            
        end
        
        function Result = findImagesOfVisit(Obj, Ind, Args)
            % Given a single image name, find in the current dir all images in the same visit.
            %   Here visit is defined to images with take at roughly the
            %   same time and that their counter is running from 1 to N
            % Input  : - An AstroFileName object.
            %          - A scalar for one specific image in the object.
            %            Default is 1.
            %          * 'Path' - If [], then look for images in the
            %                   current dir.
            %                     If NaN, then use genPath.
            %                     If char array, then dir name.
            %            'genPathArgs' - Cell array of additional arguments
            %                   to send to genPath. Default is {}.
            %            'MinInGroup' - Minimum number of elements in
            %                   group. Default is 10.
            %            'MaxInGroup' - Maximum number of elements in
            %                   group. Default is 20.
            % Output : - An AstroFileName object with all the images
            %            belongs to the requested group.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          G=A.findImagesOfVisit(1);
            
            arguments
                Obj(1,1)
                Ind(1,1)                 = 1;
                Args.Path                = [];  % [] - current dir, NaN - genPath
                Args.genPathArgs cell    = {};
                Args.MinInGroup          = 10;
                Args.MaxInGroup          = 20;
            end
            
            if isempty(Args.Path)
                % read from current dir
                Path = pwd;
            elseif isnan(Args.Path)
                Path = genPath(Obj, Ind, Args.genPathArgs{:});
            else
                Path = Args.Path;
            end
            
            PWD = pwd;
            cd(Path);
            
            VisitPat = insertWildCards(Obj, Ind, 'List', ["Time","Counter"]);
            % get JD of the requested image
            JD       = Obj.julday;
            JD       = JD(Ind);
            
            % get all images in current dir
            ImagesInDir = AstroFileName.dir(VisitPat);       
            % group by counter
            [~, GroupResult] = groupByCounter(ImagesInDir, 'MinInGroup',Args.MinInGroup, 'MaxInGroup',Args.MaxInGroup, 'CreateNewObj',true);
            
            GroupMinJD = B.juldayFun(@min);
            GroupMaxJD = B.juldayFun(@max);
            
            % look for relevant group
            Igr = find(JD>=GroupMinJD & JD<=GroupMaxJD);
            Result = GroupResult(Igr);
            
            cd(PWD);
            
        end
    end

    methods % SubDir (Visit) utilities
        % DONE
        function [Result, Obj] = generateSubDir(Obj, Args)
            % Given an AstroFileName with all images belonging to visit, generate Visit (SubDir) dir name.
            % Input  : - self.
            %          * ...,key,val,...
            %            'Method' - Method for visit naming:
            %                   'funjd' - run function (defined in 'FunJD')
            %                           on JD vector, create
            %                           an HHMMSS string and optionaly add
            %                           version - e.g., '001223v1'.
            %                           Version if HHMMSS already exist,
            %                           then version will be advanced by
            %                           one.
            %                   'number' - Naming by running index.
            %                           Will look for largest index, and
            %                           advance by one.
            %                   Default is 'funjd'.
            %            'FunJD' - Function to apply on JD.
            %                   Default is @min.
            %            'AddVersion' - Add version to HHMMSS format.
            %                   Default is true.
            %            'UpdateSubDir' - Update AstroFileName object with
            %                   the generated SubDir. Default is true.
            %            'CreateDir' - Create the SubDir in the proper
            %                   place. Default is false.
            % Output : - SubDir (Visit) string.
            %          - Updated AstroFileName object (with generated SubDir)
            % Author : Eran Ofek (Oct 2024)
            % Example: A.generateSubDir
            
            arguments
                Obj
                Args.Method               = 'funjd'
                Args.FunJD                = @min;
                Args.AddVersion logical   = true;  % for Method='funjd'
                
                Args.UpdateSubDir logical = true;
                Args.CreateDir logical    = false;
            end
            
            PWD = pwd;
            PathAboveVisit = Obj.genPath(1, 'PathType','proc', 'AddSubDir',false);
            cd(PathAboveList);
            DirList = io.files.dirDir;
            
            switch lower(Args.Method)
                case 'funjd'
                    FJD = Args.FunJD(Obj.JD);
                    HMS = celestial.time.jd2date(FJD, 'H');
                    Result = sprintf('%02d%02d%02d',HMS(4:6));
                    
                    if Args.AddVersion
                        % search existing StrHMS
                        StrFolder = string({DirList.folder});
                        FlagContain = contains(StrFolder, StrHMS);
                        Tmp = regexp(StrFolder(FlagContain), '\d{6}v(\d+)', 'tokens');
                        AllVersions = str2double(cellfun(@(x) x{1}{1}, Tmp, 'UniformOutput', false));
                        MaxVersion  = max(AllVersions);
                        
                        if isempty(MaxVersion)
                            MaxVersion = 0;
                        end
                        Result = sprintf("%sv%d",Result,MaxVersion+1);
                    end
                        
                case 'number'
                    MaxNumber = max(str2double({DirList.folder}));
                    if isnan(MaxNumber)
                        error('Max number is NaN');
                    else
                        Result    = sprintf("%d",MaxNumber+1);
                    end
            end   
            
            if Args.UpdateSubDir
                Obj.SubDir = Result;
            end
            
            if Args.CreateDir
                mkdir(Result);
            end
            
            cd(PWD);
        end
                
    end

    methods % header utilities        
        
        % DONE
        function AIH=write2header(Obj, AIH, Args)
            % Update header with properties info
            %   Given an AstroFileName with entries corresponding to input
            %   Headers (either AstroHeader or AstroImage), for each
            %   property in 'FieldsToUpdate', update the property value in
            %   the header.
            %   
            % Input  : - self.
            %          - An AstoHeader or AstroImage object.
            %            The number of elements must be equal to the number
            %            of files (lines) in the AstroFileName object.
            %          * ...,key,val,...
            %            'Fields' - A cell array or string array of
            %                   properties in the AstroFuileName object.
            %                   Each such property and its value will be
            %                   inserted (replaced) in the corresponding
            %                   header. The keywords name will be take as
            %                   upper case.
            %                   Default is AstroFileName.FIELDS.
            % Output : - An updated AstroHeader/AstroImage object.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          AI=AstroImage('LAST.01.*fits');
            %          A.write2header(AI)
            %          A.write2header(AI, 'Fields',{'SubDir'});
           
            arguments
                Obj
                AIH
                Args.Fields = AstroFileName.FIELDS;
            end
            
            Nfu = numel(Args.Fields);
            
            
            Nfile = Obj.nFiles;
            Nhead = numel(AIH);
            if Nfile~=Nhead
                error('Number of elements in header and AstroFileNames must be the same');
            end
            
            for I=1:1:Nfile
                if isa(AIH, 'AstroHeader')
                    Head = AIH(I);
                else
                    % assume an AstroImagee
                    Head = AIH(I).HeaderData;
                end
                
                for Ifu=1:1:Nfu
                    Key = Args.Fields{Ifu};
                    Val = Obj.getProp(Key, Ifu);
                    Head.replaceVal(upper(Key), Val{1});
                end
                
                if isa(AIH, 'AstroHeader')
                    AIH(I) = Head;
                else
                    AIH(I).HeaderData = Head;
                end
                
            end
            
        end
        
        % DONE
        function Result = readFromHeader(Obj, AIH, Args)
            % Read AstroFileName properties from header.
            % Input  : - An AstroFileName object (existing values will be
            %            discarded).
            %          - An AstroImage or AstroHeader object.
            %          * ...,key,val,...
            %            'Fields' - A cell array or string array of fields
            %                   to read from header (uppre case will be
            %                   taken).
            %                   Default is AstroFileName.FIELDS.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object of the AstroFileName.
            %                   Default is false.
            % Output : - An updated AstroFileName object with its
            %            properties taken from the headers.
            % Author : Eran Ofek (Oct 2024)
            % Example: AI=AstroImage('LAST.01.*fits');
            %          A=AstroFileName;
            %          A.readFromHeader(AI)
           
            arguments
                Obj
                AIH
                Args.Fields = AstroFileName.FIELDS;
                Args.CreateNewObj logical   = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
        
            Nfu = numel(Args.Fields);
            
            %Nfile = Obj.nFiles;
            Nhead = numel(AIH);
   
            UpperFields = upper(Args.Fields);
            St = AIH.getStructKey(UpperFields);
                            
            for I=1:1:Nfu
                Result.(Args.Fields{I}) = strings(Nhead,1);
                for Ihead=1:1:Nhead                  
                    if ~isnan(St(Ihead).(UpperFields{I}))
                        Result.(Args.Fields{I})(Ihead) = string(St(Ihead).(UpperFields{I}));
                    end
                end
            end
        end
    end
        
    methods % sort/select utilities
        % DONE
        function Result = reorderEntries(Obj, Ind, Args)
            % Reorder lines/file names by index or flag.
            % Input  : - self.
            %          - A vector of lines indices, or logical flags of
            %            entries to select.
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is false.
            % Output : - An AstroFileName with the selected entries.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.reorderEntries(1:2);
            
            arguments
                Obj(1,1)
                Ind                        = [];
                Args.CreateNewObj logical  = false;
            end
        
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            if ~isempty(Ind)
                Nfield = numel(Obj.FIELDS);
                for Ifield=1:1:Nfield
                    Result.(Obj.FIELDS{Ifield}) = Obj.(Obj.FIELDS{Ifield})(Ind);
                end
            end
            
        end
    
        % DONE
        function [Obj, SI] = sortBy(Obj, Prop, Args)
            % Sort entries in FileNames object by a property.
            % Input  : - A FileNames object (multi-element possible).
            %          - Property to sort by. Default is 'JD'.
            %          * ...,key,val,...
            %            'Direction' - Sort direction: 'ascend' (default), or 'descend'.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the object. Default is false.
            % Output : - A FileNames object in which the entries are sorted
            %            by the selected property.
            %          - Sorted indices for the last element of the
            %            AstroFileName object.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.sortBy;                            
            %          A.sortBy('Type');  
            
            arguments
                Obj
                Prop                      = 'JD';
                Args.Direction            = 'ascend';
                Args.CreateNewObj logical = false;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                switch Prop
                    case 'JD'
                        [~,SI] = sort(Obj(Iobj).julday);
                    otherwise
                        [~, SI] = sort(Obj(Iobj).getProp(Prop), Args.Direction);                
                end
                Obj(Iobj) = reorderEntries(Result(Iobj), SI, 'CreateNewObj',false);
            end
        end
        
        % DONE
        function [Obj, Flag] = selectByPropVal(Obj, Prop, Val, Args)
            % Select lines/files names by property comparison with some value
            %   The comparison is done using a user specified function
            %   e.g., @strcmpi
            % Input  : - self.
            %          - Property name.
            %          - Property value. Compare the property content with
            %            this value using the 'Operator' function.
            %          * ...,key,val,...
            %            'Operator' - Comparison operator.
            %                   (e.g., @strcmpi, @strcmp, @contains)
            %                   Default is @strcmpi.
            %            'Str2Double' - Content property content to double
            %                   (using str2double) before applying the
            %                   operator. Default is false.
            %            'SelectNot' - A logical indicating if to select
            %                   lines that NOT follows the comparison criterion.
            %                   Default is false (do not use NOT).
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the object. Default is false.
            %            'SelectEntries' - If false then do not modify the
            %                   object. Default is true.
            % Output : - An updated AstroFileName object, with the selected
            %            lines/file names.
            %          - For the last element in the AstroFileName, return
            %            aslo the vector of flags of selected lines.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.selectByPropVal('Product','Image')
            %          A.selectByPropVal('Product','Image', 'SelectNot',true)
            %          A.selectByPropVal('Counter',1, 'Str2Double',true,'Operator',@eq)
            
            arguments
                Obj
                Prop
                Val
                Args.Operator               = @strcmpi;
                Args.Str2Double logical     = false;
                Args.SelectNot logical      = false;
                Args.CreateNewObj logical   = false;
                Args.SelectEntries logical  = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if Args.Str2Double
                    PropVal = str2double(Result(Iobj).(Prop));
                else
                    PropVal = Result(Iobj).(Prop);
                end
                Flag   = Args.Operator(PropVal, Val);
                if Args.SelectNot
                    Flag = ~Flag;
                end
                if Args.SelectEntries
                    Result(Iobj) = reorderEntries(Result(Iobj), Flag, 'CreateNewObj',false);
                end
            end
            
        end
        
        % DONE
        function [Result,Flag] = selectByDate(Obj, MinJD, MaxJD, Args)
            % Select entries by JD in some range
            % Input  : - A FileNames object.
            %          - Min JD or date [D M Y H M S].
            %          - Max JD or date.
            %          * ...,key,val,...
            %            'SelectNot' - Select files which are not the
            %                   required val. Default is false.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is true.
            % Output : - A FileNames object with the selected entries.
            %          - A vector of logicals indicating the selected
            %            entries, for the last element of the object.
            % Author : Eran Ofek (Jan 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.selectByDate([1 1 2000],[1 1 2020])
            %          A.selectByDate(2451545, 2469900, 'SelectNot',true)
            
            arguments
                Obj
                MinJD                       = -Inf;
                MaxJD                       = Inf;
                Args.SelectNot logical   = false;   
                Args.CreateNewObj logical   = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
        
            if size(MinJD,2)>1
                MinJD = celestial.time.julday(MinJD);
            end
            if size(MaxJD,2)>1
                MaxJD = celestial.time.julday(MaxJD);
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                JD = Result.julday;
                Flag = JD>MinJD & JD<MaxJD;
                if Args.SelectNot
                    Flag = ~Flag;
                end
                Result = reorderEntries(Result, Flag);
            end
        end

    end
    
    methods % grouping
        % DONE
        function [Groups, Result] = groupByCounter(Obj, Args)
            % Group entries according to running counter groups.
            %   Given the Counter entry in an AstroFileNames object, create
            %   groups of entries by running counter, only for groups that
            %   contains at least MinInGroup and not more than MaxInGroup
            %   entries.
            % Input  : - A single element AstroFileName object.
            %            Note that after running this function the input
            %            object will be sorted by JD.
            %          * ...,key,val,...
            %            'MinInGroup' - Minimum number of elements in
            %                   group. Default is 10.
            %            'MaxInGroup' - Maximum number of elements in
            %                   group. Default is 20.
            %            'BasePath' - Base path to replace the existing
            %                   BasePath in the FileNames object.
            %                   If empty, use original.
            %                   Default is [].
            %            'CreateNewObj' - A logical indicating if to save
            %                   the grouped elements in a new object.
            %                   Default is true.
            % Output : - A struct array of all groups, with the following
            %            fields:
            %            .I1 - start index.
            %            .I2 - end index.
            %            .Ind - Vector of all indices.
            %            .N - Number of elements
            %          - An AstroFileName object with multiple elements.
            %            Each element corresponds to a FileNames object for
            %            each one of the groups.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj(1,1)
                Args.MinInGroup             = 10;
                Args.MaxInGroup             = 20;
                Args.BasePath               = [];
                Args.CreateNewObj logical   = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            if Obj.nFiles()==0
                % do nothing
                Groups = [];
            else
                Result     = Result.sortBy('JD');
                JD         = Result.julday;
                CounterVec = str2double(Result.Counter);
                
                Groups = tools.find.groupCounter(CounterVec, 'MinInGroup',Args.MinInGroup, 'MaxInGroup',Args.MaxInGroup);
                
                if nargout>1
                    Ngr    = numel(Groups);
                    if Ngr==0
                        Result = AstroFileName;
                    else
                        for Igr=1:1:Ngr
                            Result(Igr) = Obj.reorderEntries(Groups(Igr).Ind, 'CreateNewObj',true);
                            if ~isempty(Args.BasePath)
                                Result(Igr).BasePath = Args.BasePath;
                            end
                        end
                    end
                end
            end
        end
        
        % DONE
        function [Groups, Result] = groupByTimeGaps(Obj, Args)
            % Group FileNames images by groups seperated by some time gaps.
            %   The input object will be sorted by time.
            % Input  : - A single element AstroFileName object.
            %          * ...,key,val,...
            %            'TimeGap' - The minimum time gaps between observations
            %                   that will result in sepeartion into a
            %                   group. Default is 1./24 [day].
            %            'MinInGroup' - Minimum number of files in a group.
            %                   Groups with less images will be removed.
            %                   Default is 5.
            % Output : - A structure array of groups, with the following
            %            fields:
            %            .I1 - start index.
            %            .I2 - end index.
            %            .Ind - Vector of all indices.
            %            .N - Number of elements
            %          - A new copy of AstroFileName object with element per group.
            % Author : Eran Ofek (Apr 2023)


            arguments
                Obj(1,1)
                Args.TimeGap      = [1./24];  % smallest time gap [day]
                Args.MinInGroup   = 5;
            end

            % sort FileNames object by JD
            Obj = sortBy(Obj, 'JD');

            JD = Obj.julday;

            Gaps = [Inf; diff(JD(:))];

            Flag = Gaps>Args.TimeGap;

            N        = numel(JD);
            I1       = find(Flag);
            I2       = [I1(2:end); N];
            Ngr      = numel(I1);
            K = 0;
            Groups = [];
            for Igr=1:1:Ngr
                Ind = (I1(Igr):I2(Igr));
                if numel(Ind)>=Args.MinInGroup
                    K = K + 1;

                    Groups(K).I1 = I1(Igr);
                    Groups(K).I2 = I2(Igr);
                    Groups(K).Ind = Ind;
                    Groups(K).N   = numel(Ind);
                end
            end
            Ngr = numel(Groups);

            if nargout>1
                if isempty(Groups)
                    Result = [];
                else
                    Result = FileNames(Ngr);
                    for Igr=1:1:Ngr
                        Result(Igr) = Obj.reorderEntries(Groups(Igr).Ind, 'CreateNewObj',true);
                    end
                end
            end

        end

        
    end
        
        
    methods % move, copy, delete files
        
         % DONE
         function moveImages(Obj, Args)
            % move/delete images specified by FileNames object
            % Input  : - A single element AstroFileName object.
            %          * ...,key,val,...
            %            'SrcPath' - The path name of the source files to
            %                   move or delete. If empty, then use current
            %                   directory. Default is [].
            %            'DestPath' - The path name of the files
            %                   destination. If empty, then use the genPath
            %                   method to construct the path.
            %                   Default is [].
            %            'Operator' - Options are:
            %                   'move' - move files.
            %                   'delete' - delete files.
            %                   'keep' - do nothing.
            %                   Default is 'move'.
            %            'OutName' - A cell array of optional file names in
            %                   the destination. If empty, then use source
            %                   file names. Default is [].
            %            'Product' - Select only file names of this
            %                   product (e.g. 'Image' | 'Mask' | ...).
            %                   If empty, do not use this selection.
            %                   Default is [].
            %            'Type' - Select only file names of this
            %                   type (e.g. 'sci' | 'twflat' | ...).
            %                   If empty, do not use this selection.
            %                   Default is [].
            %            'Level' - Select only file names of this
            %                   level (e.g. 'proc' | 'raw' | ...).
            %                   If empty, do not use this selection.
            %                   Default is [].
            % Output : null
            % Author : Eran Ofek (Apr 2023)

            arguments
                Obj(1,1)
                Args.SrcPath      = []; % if empty, use pwd
                Args.DestPath     = []; % if empty use genPath
                Args.Operator     = 'move';   % 'move'|'delete'
                Args.OutName      = [];
                Args.Product      = [];
                Args.Type         = [];
                Args.Level        = [];
            end

            KEYS_SELECT = {'Product','Type','Level'};
            Nks         = numel(KEYS_SELECT);

            if ~strcmp(Args.Operator, 'keep') && nfiles(Obj)>0

                % selectBy keys - do not modify the input object
                for Iks=1:1:Nks
                    if ~isempty(Args.(KEYS_SELECT{Iks}))
                        Obj = Obj.selectBy(KEYS_SELECT{Iks}, Args.(KEYS_SELECT{Iks}), 'CreateNewObj',true, 'SelectNotVal',false);
                    end
                end

                if isempty(Args.SrcPath)
                    SrcPath = pwd;
                else
                    SrcPath = Args.SrcPath;
                end
    
                if isempty(Args.DestPath)
                    DestPath = Obj.genPath;
                else
                    DestPath = Args.DestPath;
                end                
    
                switch Args.Operator
                    case 'move'
                        io.files.moveFiles(Obj.genFile(), Args.OutName, SrcPath, DestPath);
                    case 'delete'
                        PWD = pwd;
                        cd(SrcPath);
                        io.files.delete_cell(Obj.genFile());
                        cd(PWD);
                    case 'copy'
                        error('copy not yet implemented');
                    otherwise
                        error('Unknown Operator argument option');
                end
            end
        end
         
    end
      
    
    
    methods % search and utilities
     
        
        

        function [Obj, SI] = sortByFunJD(Obj, Direction, Fun)
            % Sort elements in FileNames object by mean/min JD of entries in each element.
            % Input  : - A FileNames object (multi-element possible).
            %          - Sort direction: 'ascend' (default), or 'descend'.
            %          - Function. Default is @min.
            % Output : - A FileNames object in which the entries are sorted
            %            by JD.
            %          - A vector of sorted indices.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                Direction = 'ascend';
                Fun       = @min;
            end

            JD = Obj.juldayFun(Fun);
            [~,SI] = sort(JD, Direction);
            Obj = Obj(SI);
            
        end

        
        
       
        function Obj=updateIfNotEmpty(Obj, Args)
            % Update FileNames properties if provided and not empty
            % Input  : - A FileNames object.
            %          * ...,key,val,...
            %            Any of the properties of FileNames (e.g.,
            %            'ProjName, Time, ..., SubDir).
            %            If proprty is not given or empty, then it will not
            %            be updated with the provided value.
            % Output : - An updated FileNames object.
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Obj
                Args.ProjName = [];
                Args.Time     = [];
                Args.Filter   = [];
                Args.FieldID  = [];
                Args.Counter  = [];
                Args.CCDID    = [];
                Args.CropID   = [];
                Args.Type     = [];
                Args.Level    = [];
                Args.Product  = [];
                Args.Version  = [];
                Args.FileType = [];
                Args.FullPath = [];
                Args.BasePath = [];
                Args.SubDir   = [];
            end
        
            Fields = fieldnames(Args);
            Nf     = numel(Fields);
            Nobj   = numel(Obj);
            
            for Iobj=1:1:Nobj
                for If=1:1:Nf
                    if ~isempty(Args.(Fields{If}))
                        Obj(Iobj).(Fields{If}) = Args.(Fields{If});
                    end
                end
            end
        end
        
        function I = findFirstLast(Obj, IsLast, ProductName)
            % find image, of some product type, with latest/earliest JD
            % Input  : - A FileNames object.
            %          - Search for last (true), first (false) image name.
            %          - Select only products of this type.
            %            Default is 'Image'.
            % Output : - The index of the selected FileNames element.
            %            Empty if no JD, or no images.
            % Author : Eran Ofek (Jan 2022)
            % Example: IP = FileNames(2);
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
        
        
        
        
    end
        
        
        
        
            
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
