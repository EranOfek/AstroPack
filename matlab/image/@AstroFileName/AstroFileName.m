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
        Version             = [1];
        FileType            = ["fits"];
        %
        SubDir              = "";
        BasePath            = "/marvin";
        BasePathRef         = "/marvin/ref";
        
        %
        Path                = [];
        
        BasePathIncludeProjName logical = true;
        AddSubDir logical               = true;
        
        TimeZone            = 2;
        
    end
    
    properties (Hidden)
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
        ListType        = ["", "bias", "dark", "flat", "domeflat", "twflat", "skyflat", "fringe", "focus", "sci", "wave", "type" , "log"];
        ListLevel       = ["", "raw", "proc", "stack", "ref", "coadd", "merged", "calib", "junk", "proc.zogyD","coadd.zogyD"];
        ListProduct     = ["", "Image", "Back", "Var", "Exp", "Nim", "PSF", "Cat", "Spec", "Mask", "Evt", "MergedMat", "Asteroids","Pipeline", "TransientsCat"];
        SEPERATOR       = "_";
        FIELDS          = ["ProjName", "Time", "Filter", "FieldID", "Counter", "CCDID", "CropID", "Type", "Level", "Product", "Version", "FileType"];
        
    end
    
    
    methods % Constructor
       
        function Obj = FileNames(Pars)
            % Constructor for FileNames
            % Input  : - Either a:
            %            1. Scalar indicting the number of elements in the
            %            empty ImagePath object that will be created.
            %            2. A cell array of file nsmaes that will be
            %            parsed.
            %            3. A structure array returned from the dir
            %            command.
            % Output : - An ImagePath object.
            % Author : Eran Ofek (Jan 2022)
            % Example: IP = ImagePath(2);
            %          IP = ImagePath({'LAST_20220118.193339.010_clear_____sci_raw_Image_1.fits'});
            
            arguments
                Pars = 1;
            end
            
            if iscell(Pars) || ischar(Pars)
                % Pars is a list of image names
                Obj = FileNames.generateFromFileName(Pars);

            elseif isnumeric(Pars)
                % length of ImagePath object
                N = Pars;
                for I=1:1:N
                    Obj(I).ProjName = '';
                end
            elseif isstruct(Pars)
                List = fullfile({Pars.folder}, {Pars.name});
                Obj = FileNames.generateFromFileName(List);
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
        
    end
      
    methods (Static) % construction
        % DONE
        function Result=formatCellProjName(ProjName, Format)
            % Format project name strings array
            %   convert {"LAST",1,2,3} to: "LAST.01.02.03"
            % Input  : - Cellarray ProjName. E.g., {"LAST",1,2,3}
            %          - Format. E.g., "%02d"
            % Output : - ProjName string
            % Author : Eran Ofek (Oct 2024)
            % Example: AstroFileName.formatCellProjName({"LAST",1,2,3},"%02d")

            if ~iscell(ProjName)
                error('Works only on cell ProjName input');
            end
            
            N = numel(ProjName);
            if N>1
                Result = ProjName{1};
                Format = sprintf("%%s.%s", Format);
                for I=2:1:N
                    Result = sprintf(Format, Result, ProjName{I});
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
            
            if iscell(FileNameString) || ischar(FileNameString)
                FileNameString = string(FileNameString);
            end
            
            Result = split(FileNameString(:), Seperator);
            if SplitLast
                Result = [Result(:,1:end-1), split(Result(:,end), SeperatorLast)];
            end
                
        end
        
        % DONE
        function [Result,Path]=parseString2AstroFileName(FileNameString, IsSinglePath, Seperator)
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
            %          - A string array of path. This is obtained only if
            %            the first input is a structure array as returned by
            %            the dir command.
            % Author : Eran Ofek (Oct 2024)
            % Example: R=AstroFileName.parseString2AstroFileName(A)
            
            arguments
                FileNameString
                IsSinglePath logical = true;
                Seperator            = "_";
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
                Result.Path = "";
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
        function Result=dir(varargin)
            % dir like function that returns a populated AstroFileName object.
            % Input  : - File template name.
            % Output : - An AstroFileName object.
            % Author : Eran Ofek (Oct 2024)
            % Example: R=AstroFileName.dir('LAST.01.*.fits')
            
            DirSt = dir(varargin{:});
            Result = AstroFileName.parseString2AstroFileName(DirSt);
            
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
                Args.UseJD logical      = false;
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
                    Result = [extractBetween(Obj.Time(Ind), 1, 4), extractBetween(Obj.Time(Ind), 4, 5), extractBetween(Obj.Time(Ind), 6, 7)];
                else
                    Result = extractBefore(Obj.Time(Ind), 9);
                end
            end
            
        end
            
    end
    
    methods % utilities
        
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
       
        % DONE
        function Result = nFiles(Obj)
            % Return the number of files (times) in the object.
            % Input  : - self.
            % Output : - Number of files (times).
            % Author : Eran Ofek (Oct 2024)
            % Example: nFiles(A)

            Result = numel(Obj.Time);
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

            Result = join(Literals, Obj.SEPERATOR);

        end

        % DONE
        function Result=genRefPath(Obj, Ind)
            % Generate RefPath (path for reference images)
            % Input  : - self.
            %          - Indices of lines (file names) for which to
            %            generate ref image path. If empty, then generate
            %            for all images. Default is 1.
            % Output : - A string array of reference image path.
            %            Construted from BasePathRef/FieldID.
            % Author : Eran Ofek (Oct 2024)
            % Example: A=AstroFileName.dir('LAST.01.*fits');
            %          A.genRefPath
            %          A.genRefPath([])
            
            arguments
                Obj
                Ind    = 1;
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
                Obj
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
                        YMD         = Obj.getDateDir(Ind, 'BreakToYMD',true, 'UseJD',false);
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
        
        
        
        
      
    
    
    methods % search and utilities
        function Result = reorderEntries(Obj, Ind, Args)
            % Reorder/select all the entries in FileNames object.
            % Input  : - A FileNames object.
            %          - Indices of entries as they should appear in the
            %            output.
            %          * ...,key,val,...
            %            'PropToOrder' -  A cell array of properties to order.
            %                   Default is: 
            %                   {'ProjName','Time','Filter','FieldID','Counter','CCDID','CropID','Type','Level','Product','Version','FileType'}
            %            'CreateNewObj' - Create a new copy.
            %                   Default is false.
            % Output : - A FileNames object in which the entries are in the
            %            order specified in Indices.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                Ind
                Args.PropToOrder = {'ProjName','Time','Filter','FieldID','Counter','CCDID','CropID','Type','Level','Product','Version','FileType'};
                Args.CreateNewObj logical = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            
            NInd = numel(Ind);
            Nf   = numel(Args.PropToOrder);
            for If=1:1:Nf
                if numel(Obj.(Args.PropToOrder{If}))==1
                    % skip reorder
                else
                    %if numel(Obj.(Args.PropToOrder{If}))==NInd
                        % reorder
                    Result.(Args.PropToOrder{If}) = Obj.(Args.PropToOrder{If})(Ind);
                    %else
                    %    error('Number of entries in property %s must be either 1 or %d',Args.PropToOrder{If},NInd);
                    %end
                end
            end
            
        end
        
        function [Obj, SI] = sortByJD(Obj, Direction)
            % Sort entries in FileNames object by JD
            % Input  : - A FileNames object (multi-element possible).
            %          - Sort direction: 'ascend' (default), or 'descend'.
            % Output : - A FileNames object in which the entries are sorted
            %            by JD.
            %          - A vector of sorted indices.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                Direction = 'ascend';
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                JD = Obj(Iobj).julday;
                [~,SI] = sort(JD, Direction);
                Obj(Iobj) = reorderEntries(Obj(Iobj), SI);
            end
            
        end

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

        function [SunAlt] = sunAlt(Obj, Args)
            % Calculate Sun Altitude for images in FileNames object
            % Input  : - An FileNames object
            %          * ...,key,val,...
            %            'GeoPos' - Geodetic position [Lon, Lat] in deg.
            %                   Default is [35 30].
            % Output : - An array of Sun altitude (deg) for each image
            %            entry.
            % Author : Eran Ofek (May 2022)
            % Example: 
            
            arguments
                Obj
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
        
        function [Result,Flag] = selectBy(Obj, PropName, PropVal, Args)
            % Select entries that have proprty value of some value or in some range.
            % Input  : - A FileNames object.
            %          - Property name by which to select entries.
            %            Default is 'Product'.
            %          - Value to select. This is either a char array of
            %            property type (e.g., 'Image'),
            %            a cell array of property values.
            %            If a cell array then will select entries that are
            %            equal to one of these property values.
            %            Alternatively, a numeric scalar
            %            or a numeric vector of [min max]. In the latter,
            %            will select all values within range.
            %          * ...,key,val,...
            %            'SelectNotVal' - Select files which are not the
            %                   required val. Default is false.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is true.
            % Output : - A FileNames object with the selected entries.
            %          - A vector of logicals indicating the selected
            %            entries.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                PropName char               = 'Product';
                PropVal                     = 'Image';
                Args.SelectNotVal logical   = false;   
                Args.CreateNewObj logical   = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
        
            if ischar(PropVal) || iscell(PropVal)
                if ~iscell(Obj.(PropName))
                    error('PropName %s must contain a cell array',PropName);
                end
                Flag = ismember(Obj.(PropName), PropVal);
            elseif isnumeric(PropVal)
                if ~isnumeric(Obj.(PropName))
                    error('PropName %s must contain a numeric array',PropName);
                end
                if numel(PropVal)==1
                    % equal numeric value
                    Flag = Obj.(PropName) == PropVal;
                else
                    % numeric value in range
                    Flag = Obj.(PropName)>min(PropVal) & Obj.(PropName)<max(PropVal);
                end
            else
                error('PropVal must be either a char array or numeric');
            end
            
            if Args.SelectNotVal
                Flag = ~Flag;
            end

            Nt     = numel(Obj.Time);
            Flag   = Flag(:).' | false(1,Nt);
            Result = reorderEntries(Result, Flag);
        end
        
        function [Result,Flag] = selectByDate(Obj, MinJD, MaxJD, Args)
            % Select entries by JD in some range
            % Input  : - A FileNames object.
            %          - Min JD or date [D M Y H M S].
            %          - Max JD or date.
            %          * ...,key,val,...
            %            'SelectNotVal' - Select files which are not the
            %                   required val. Default is false.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is true.
            % Output : - A FileNames object with the selected entries.
            %          - A vector of logicals indicating the selected
            %            entries.
            % Author : Eran Ofek (Jan 2024)
            
            arguments
                Obj
                MinJD                       = -Inf;
                MaxJD                       = Inf;
                Args.SelectNotVal logical   = false;   
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

            JD = Result.julday;
            Flag = JD>MinJD & JD<MaxJD;
            if Args.SelectNotVal
                Flag = ~Flag;
            end
            
            Result = reorderEntries(Result, Flag);
        end


        function [Groups, Result] = groupByCounter(Obj, Args)
            % Group entries according to running counter groups.
            %   Given the Counter entry in an FileNames object, create
            %   groups of entries by running counter, only for groups that
            %   contains at least MinInGroup and not more than MaxInGroup
            %   entries.
            % Input  : - A FileNames object.
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
            %          - A FileNames object with multiple elements.
            %            Each element corresponds to a FileNames object for
            %            each one of the groups.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
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

            if Obj.nfiles()==0
                % do nothing
                Groups = [];
            else
                Result     = Result.sortByJD;
                JD         = Result.julday;
                CounterVec = Result.Counter;
                
                Groups = tools.find.groupCounter(CounterVec, 'MinInGroup',Args.MinInGroup, 'MaxInGroup',Args.MaxInGroup);
                
                if nargout>1
                    Ngr    = numel(Groups);
                    for Igr=1:1:Ngr
                        Result(Igr) = Obj.reorderEntries(Groups(Igr).Ind, 'CreateNewObj',true);
                        if ~isempty(Args.BasePath)
                            Result(Igr).BasePath = Args.BasePath;
                        end
                    end
                end
            end
        end
        
        function [Groups, Result] = groupByTimeGaps(Obj, Args)
            % Group FileNames images by groups seperated by some time gaps.
            %   The input object will be sorted by time.
            % Input  : - A FileNames object.
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
            %          - A FileNames object with element per group.
            % Author : Eran Ofek (Apr 2023)


            arguments
                Obj
                Args.TimeGap      = [1./24];  % smallest time gap [day]
                Args.MinInGroup   = 5;
            end

            % sort FileNames object by JD
            Obj = sortByJD(Obj);

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

        function [Ind, LastJD, DT, Result]=selectLastJD(Obj)
            % Return index of image with largest JD
            % Input  : - A FileNames object
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
        
        function [Ind, NearJD, Result]=selectNearest2JD(Obj, TargetJD)
            % Return index of image which JD is nearest to some JD/date.
            % Input  : - A FileNames object
            %          - JD or date [D M Y [H M S]].
            %            The function will select the file name with JD
            %            nearest to this date.
            %            If empty, use current JD.
            %            Default is [].
            % Output : - Index of image with largest JD
            %          - JD of image with nearest JD
            %          - A (new copy) FileNames object with the image with latest JD.
            % Author : Eran Ofek (Mar 2023)
            % Example: F=FileNames; F.Time=2451545;
            %          F.selectNearest2JD(2451546)

            
            arguments
                Obj
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
        
        function Result=validTimes(Obj)
            % Return a vector of logical indicating if Time argument is valid
            % Input  : - A single elemnent FileNames object.
            % Output : - A vector of logical which length equal to the
            %            number of file names. False if Time is NaN, [],
            %            or 'NaN'.
            % Author : Eran Ofek (Sep 2023)

            arguments
                Obj(1,1)
            end

            if iscell(Obj.Time)
                Nt = numel(Obj.Time);
                Result = true(Nt,1);
                for It=1:1:Nt
                    if isempty(Obj.Time{It}) || any(isnan(Obj.Time{It})) || strcmpi(Obj.Time{It},'nan')
                        Result(It) = false;
                    end
                end
            else
                Result = ~isnan(Obj.Time);
            end

        end

        function Result=nfiles(Obj)
            % Return number of files in a FileNames object
            % Input  : - A FileNames object
            % Output : - Number of files in each FileNames element.
            % Author : Eran Ofek (Apr 2023)

            Nobj = numel(Obj);
            Result = zeros(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = numel(Obj(Iobj).Time);
            end
        end

        function moveImages(Obj, Args)
            % move/delete images specified by FileNames object
            % Input  : - A FileNames object.
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
                Obj
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
        
        function Result = isemptyFile(Obj)
            % Check if all elements of FileNames object contain no files
            % Input  : - A FileName object.
            % Output : - Return true if no files in object.
            % Author : Eran Ofek (May 2023)

            if sum(Obj.nfiles)==0
                Result = true;
            else
                Result = false;
            end

        end
        
        
    end
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        %%% OBSOLETE:
        
        % function Ind = getAllProductsFromImageName(Obj, FileName)
        %     % Given an ImagePath and an image name (or index), return all the corresponding products 
        %     % Input  : - An ImagePath object.
        %     %          - A file name or an ImagePath element index.
        %     % Output : - The indices in the ImagePath object of elements for
        %     %            which the image name, with the exception of the
        %     %            Product name, is identical to the input FileName.
        %     % Author : Eran Ofek (Jan 2022)
        %     % Example: IP = ImagePath;
        %     %          FN  = IP.genFile;
        %     %          FN1 = strrep(FN, '_Image_', '_Mask_');
        %     %          IP.Filter = 'AA'; FN2 = IP.genFile;
        %     %          F   = {FN, FN1, FN2};
        %     %          IP  = ImagePath.parseFileName(F);
        %     %          Ind = getAllProductsFromImageName(IP, FN)
        % 
        %     if ischar(FileName)
        %         Ind = find(strcmp({Obj.FileName}, FileName));
        %     else
        %         % FileName is already an image index
        %         if isinf(FileName)
        %             Ind = numel(Obj);
        %         else
        %             Ind = FileName;
        %         end
        %     end
        % 
        %     Obj.genFile;
        %     AllNames    = {Obj.FileName};
        %     Splitted    = split(AllNames{Ind}, '_');
        %     % Replace the Product name with '\w*'
        %     Splitted{10} = '\w*';
        %     Template     = tools.string.unsplit(Splitted, '_');
        % 
        %     Match = regexp(AllNames, Template, 'match');
        %     Ind = find(~cellfun(@isempty, Match));
        % 
        % end
        
%         function [Ind, IndCounter] = getAllInCounterSeries(Obj, FileName, ModNumber)
%             % Return indices of a series of images belonging to the same counter series (sorted by JD).
%             % Given an ImagePath and a file name or an index of an element
%             %   in ImagePath:
%             %   sort by JD,
%             %   search for the indices (in the sorted list) of all the
%             %   images that belongs to the same counter series.
%             %   For example, given a set of images with counter = [19    20     1     2     3     4] 
%             %   and the last counter is an ancor, will return 3:6.
%             % Input  : - An ImagePath object.
%             %          - Either a file name, or an index of the image in
%             %            the ImagePath object.
%             %            If Inf then will set the index to the last element
%             %            in ImagePath.
%             %            If this is empty, then the output index will be
%             %            empty.
%             %          - A scalar. For the calculations the counter will be
%             %            replace by the mod(Counter, This_Number).
%             %            Default is Inf.
%             % Output : - Indices of all the elements in the sorted
%             %            ImagePath that belong to the series that contains
%             %            the input image name. The input image name is
%             %            always the last element in the series.
%             %            NOTE that the ImagePath object will be sorted by
%             %            JD.
%             %          - Vector of image counters.
%             % Author : Eran Ofek (Jan 2022)
%             % Example: 
% 
%             arguments
%                 Obj
%                 FileName
%                 ModNumber = Inf;
%             end
% 
%             Obj.genFile;
% 
%             [~,SI] = Obj.sortByJD;
%             if ischar(FileName)
%                 Ind = find(strcmp({Obj.FileName}, FileName));
%             else
%                 % FileName is already an image index
%                 if isinf(FileName)
%                     Ind = numel(Obj);
%                 else
%                     %Ind = FileName;
%                     Ind = SI(FileName);
%                 end
%             end
% 
%             if ~isempty(Ind)
%                 AllCounter = mod([Obj(1:Ind).Counter], ModNumber+1);
% 
%                 AllCounter = [Inf, AllCounter, 0];
%                 I = find(diff(AllCounter)<0, 2, 'last');
%                 Ind = (I(1):I(2)-1);
% 
%                 IndCounter = [Obj(Ind).Counter];
%             end
%         end
% 
%         function Obj = setAllVal(Obj, Prop, Val)
%             % Set the value of one of the properties in all the elements.
%             % Input  : - An ImagePath object.
%             %          - Property name.
%             %          - Value to set.
%             % Output : - The updated ImagePath object.
%             % Author : Eran Ofek (Jan 2022)
%             % Example: Obj = setAllVal(Obj, 'FormatCounter', '%d');
% 
%             Nobj = numel(Obj);
%             for Iobj=1:1:Nobj
%                 Obj(Iobj).(Prop) = Val;
%             end
%         end
% 
% 
%     end
% 
%     methods % raed/write from stuct
%         function Result = readFromStruct(Obj, st)
%             % Read data from struct or DbRecord (common_image_path table)
%             % Struct field names should match database fields
%             Obj.Telescope       = st.tel;
%             Obj.Node            = st.node;
%             Obj.Mount           = st.mount;
%             Obj.Camera          = st.camera;
% 
%             Obj.JD              = st.jd;
%             Obj.TimeZone        = st.timezone;
%             Obj.Filter          = st.filter;
%             Obj.FieldID         = st.field_id;
%             Obj.CropID          = st.crop_id;
%             Obj.ImageType       = st.imtype;
%             Obj.ImageLevel      = st.imlevel;
%             Obj.ImageSubLevel   = st.imslevel;
%             Obj.ImageProduct    = st.improd;
%             Obj.ImageVer        = st.imver;
%             Obj.FileType        = st.filetype;
%             Result = true;
%         end
% 
%         function st = writeToStruct(Obj)
%             % Write data fields to struct (common_image_path table)            
%             st = struct;
%             st.tel      = Obj.Telescope;
%             st.node     = Obj.Node;
%             st.mount    = Obj.Mount;
%             st.camera   = Obj.Camera;
%             st.jd       = Obj.JD;
%             st.timezone = Obj.TimeZone;
%             st.filter   = Obj.Filter;
%             st.field_id = Obj.FieldID;
%             st.crop_id  = Obj.CropID;
%             st.imtype   = Obj.ImageType;
%             st.imlevel  = Obj.ImageLevel;
%             st.imslevel = Obj.ImageSubLevel;
%             st.improd   = Obj.ImageProduct;
%             st.imver    = Obj.ImageVer;
%             st.filetype = Obj.FileType;
%         end        
%     end
% 
% 
%     methods % Helpers (for internal use)
% 
% 
%     end    
% 
%     methods (Static) % Parsers
% 
%         %function Result = parsePath(Obj, Path)
%         %    % Convert a string containing a path to a structure with all available information (e.g., date, type, level, fieldID, ProjName)
%         %end
% 
% 
%         function Obj = parseFileName(FileName)
%             % Populate ImagePath object from file name.
%             % Input  : - A file name, or a cell array of file names.
%             % Output : - A populated ImagePath object based on the file
%             %            name only.
%             % Author : Eran Ofek (Jan 2022)
%             % Example:
%             % IP=ImagePath.parseFileName('LAST_20220118.193339.010_clear_____sci_raw_Image_1.fits')
% 
%             if ischar(FileName)
%                 FileName = {FileName};
%             end
%             Nf = numel(FileName);
% 
%             Obj = ImagePath(Nf);
%             for If=1:1:Nf
%                 [Path, File, Ext]  = fileparts(FileName{If});
%                 Obj(If).FileName = File;
%                 Obj(If).FullName = FileName{If};
%                 Obj(If).Path     = Path;
% 
%                 Obj(If).FileType = Ext(2:end);
%                 Parts = split(File,'_');
%                 % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
%                 Obj(If).ProjName = Parts{1};
%                 if numel(Parts)>1
%                     VecTime = datevec(Parts{2}, 'yyyymmdd.HHMMSS.FFF');
%                     Obj(If).Time     = celestial.time.julday(VecTime([3 2 1 4 5 6]));
%                 else
%                     Obj(If).Time     = [];
%                 end
%                 if isempty(Parts{3})
%                     Obj(If).Filter = '';
%                 else
%                     Obj(If).Filter   = Parts{3};
%                 end
%                 if isempty(Parts{4})
%                     Obj(If).FieldID = '';
%                 else
%                     Obj(If).FieldID  = Parts{4};
%                 end
%                 Obj(If).Counter  = str2double(Parts{5});
%                 Obj(If).CCDID    = Parts{6};
%                 Obj(If).CropID   = Parts{7};
%                 Obj(If).Type     = Parts{8};
%                 PartsLevel   = split(Parts{9}, '.');
%                 Obj(If).Level    = PartsLevel{1};
%                 if numel(PartsLevel)>1
%                     Obj(If).SubLevel = PartsLevel{2};
%                 else
%                     Obj(If).SubLevel = '';
%                 end
%                 Obj(If).Product  = Parts{10};
%                 Obj(If).Version  = Parts{11};
% 
%             end
%         end
% 
%         function [List, Flag] = selectByDate(Files, StartDate, EndDate)
%             % Select files in some range of dates and times.
%             % Input  : - A file name with possible wild cards, or a cell
%             %            array of file names.
%             %            Default is '*.fits'.
%             %          - Start date (JD, [Y M D H M S], or date string).
%             %            Default is -Inf.
%             %            If empty, then select all.
%             %          - End date (JD, [Y M D H M S], or date string).
%             %            Default is Inf.
%             % Output : - A list of selected files which dates is between
%             %            the start and end dates.
%             %          - A logical vector of selected files.
%             % Author : Eran Ofek (Jun 2022)
%             % Example: ImagePath.selectByDate('PTF*.fits');
% 
%             arguments
%                 Files       = '*.fits';
%                 StartDate   = -Inf;
%                 EndDate     = Inf;
%             end
% 
%             if ischar(Files)
%                 List = io.files.filelist(Files);
%             else
%                 List = Files;
%             end
% 
%             if isempty(StartDate)
%                 % do nothing
%                 Flag = [];
%             else
%                 if numel(StartDate)==1
%                     StartJD = StartDate;
%                 else
%                     StartJD = celestial.time.julday(StartDate);
%                 end
%                 if numel(EndDate)==1
%                     EndJD = EndDate;
%                 else
%                     EndJD = celestial.time.julday(EndDate);
%                 end
% 
%                 IP = ImagePath.parseFileName(List);
% 
%                 Flag = [IP.Time]>StartJD & [IP.Time]<EndJD;
%                 List = List(Flag);
%             end
%         end
% 
%         function [IP, List, Flag] = selectByProp(Files, PropVal, Prop, IsVal)
%             % Select files with ImagePath format by some property (e.g., Type).
%             % Input  : - If this is a char array than use io.files.filelist
%             %            to generate a cell array of file names.
%             %            Alternatively, this is a cell array of file names.
%             %          - Image property value to select (or ignore).
%             %            Default is {'sci','science'}.
%             %          - Property name to select by (e.g., 'Type','Level').
%             %            Default is 'Type'.
%             %          - A logical flag indicating if to use files of
%             %            the specified property value. If false, then
%             %            ignore this value and select all the rest.
%             %            Default is true.
%             % Output : - An ImagePath pbject with the selected image.
%             %          - A cell array of selected files.
%             %          - A vector of logical flags indicating the selected
%             %            files.
%             % Author : Eran Ofek (Aug 2022)
%             % Example: [IP,List] = ImagePath.selectByProp('LAST*.fits', {'focus'}, 'Type')
%             % Example: [IP,List] = ImagePath.selectByProp('LAST*.fits', {'focus'}, 'Type',false)
% 
%             arguments
%                 Files
%                 PropVal              = {'sci','science'};
%                 Prop                 = 'Type';
%                 IsVal logical        = true;
%             end
% 
%             if ischar(Files)
%                 List = io.files.filelist(Files);
%             else
%                 List = Files;
%             end
% 
%             IP   = ImagePath.parseFileName(List);
%             Flag = ismember({IP.(Prop)}, PropVal);
%             if ~IsVal
%                 Flag = ~Flag;
%             end
% 
%             IP   = IP(Flag);
%             List = List(Flag);
% 
%         end
% 
% 
%     end
% 
% 
%     methods % write product
%         function ObjIP = writeProduct(ObjIP, ObjProduct, Args)
%             % Write an array of data prodicts to disk.
%             % Input  : - An ImagePath object array.
%             %          - A data product (e.g., an AstroImage). If this is
%             %            an AstroImage then the number of elements must be
%             %            equal to the number of elements in the first input
%             %            argument.
%             %          * ...,key,val,...
%             %            'Save' - A logical indicating if to save the
%             %                   products. Default is true.
%             %            'SaveFields' - Fields to save.
%             %                   Default is {'Image','Mask','Cat'}.
%             %            'WriteFun' - Default is @write1.
%             %                   Fun(Object, Name, Field, WriteFunArgs{:})
%             %            'WriteFunArgs' - Cell array of arguments to pass
%             %                   to the WriteFun.
%             %                   Default is {'WriteMethodImages','Simple',
%             %                   'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false}.
%             %            'Product' - For a non AstroImage object. This is
%             %                   the data type that will be written.
%             %                   Default is 'Asteroids'.
%             %            'Level' - Overide level. If empty, do not
%             %                   override. Default is [].
%             % Output : - The ImagePath object (without changes).
%             % Author : Eran Ofek (Feb 2022)
%             %
% 
%             arguments
%                 ObjIP
%                 ObjProduct
%                 Args.Save
%                 Args.SaveFields                = {'Image','Mask','Cat'};
%                 Args.WriteFun function_handle  = @write1;
%                 Args.WriteFunArgs cell         = {'WriteMethodImages','Simple','FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
%                 Args.Product                   = 'Asteroids';   %'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat', 'Asteroids'
%                 Args.Level                     = []; % override Level
%                 Args.FileType                  = []; % override FileType
%             end
% 
%             if Args.Save
%                 % verify that directory exist
%                 mkdir(ObjIP(1).genPath);
% 
%                 Nfield  = numel(Args.SaveFields);
%                 Nprod   = numel(ObjProduct);
%                 if isa(ObjProduct, 'AstroImage')
%                     for Iprod=1:1:Nprod
%                         if ~isempty(Args.Level)
%                             ObjIP(Iprod).Level = Args.Level;
%                         end
%                         if ~isempty(Args.FileType)
%                             ObjIP(Iprod).FileType = Args.FileType;
%                         end
%                         for Ifield=1:1:Nfield
%                             ObjIP(Iprod).Product = Args.SaveFields{Ifield};
%                             Args.WriteFun(ObjProduct(Iprod), ObjIP(Iprod).genFull, Args.SaveFields{Ifield}, Args.WriteFunArgs{:});
%                         end
%                     end
%                 elseif isa(ObjProduct, 'AstroCatalog')
%                     % save AstroCatalog
%                     switch Args.SaveFields{1}
%                         case 'Cat'
%                             for Iprod=1:1:Nprod
%                                 if ~isempty(Args.Level)
%                                     ObjIP(Iprod).Level = Args.Level;
%                                 end
%                                 if ~isempty(Args.FileType)
%                                     ObjIP(Iprod).FileType = Args.FileType;
%                                 end
%                                 ObjIP(Iprod).Product = 'Cat';
%                                 Args.WriteFun(ObjProduct(Iprod), ObjIP(Iprod).genFull, Args.WriteFunArgs{:});
%                             end
%                     end
%                 elseif isa(ObjProduct, 'MatchedSources')
%                     % save MatchedSources
%                     switch Args.SaveFields{1}
%                         case 'Cat'
%                             for Iprod=1:1:Nprod
%                                 if ~isempty(Args.Level)
%                                     ObjIP(Iprod).Level = Args.Level;
%                                 end
%                                 if ~isempty(Args.FileType)
%                                     ObjIP(Iprod).FileType = Args.FileType;
%                                 end
%                                 ObjIP(Iprod).Product = 'MergedMat';
%                                 Args.WriteFun(ObjProduct(Iprod), ObjIP(Iprod).genFull, Args.WriteFunArgs{:});
%                             end
%                     end
%                 else
%                     % save as MAT file
%                     IP1 = ObjIP(1).copy;
%                     IP1.Counter   = 0;
%                     IP1.CropID    = 0;
%                     IP1.Product   = Args.Product;
%                     IP1.FileType  = 'mat';
% 
%                     save('-v7.3',IP1.genFull, 'ObjProduct');
%                 end
%             end
%         end  
% 
%         function [Future, ObjIP] = saveProduct(ObjIP, ObjProduct, Args) 
%             % Save product to disk
%             % Input  : - An ImagePath object
%             %          - An object to save (e.g., AstroImage)
%             %          * ...,key,val,...
%             %            'Save' - A logical indicating if to save the
%             %                   pdoducts. Default is true.
%             %            'ParEval' - Use parfeval. Default is false.
%             %            'SaveFun' - A function handle which is a method of
%             %                   the the object product used to save products.
%             %                   Default is @write1.
%             %            'SaveFunArgs' - A cell array of additional
%             %                   arguments to pass to the 'SaveFun'.
%             %                   Default is {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false}
%             %            'PropFromHeader' - Attempt to populate the
%             %                   ImagePath properties from the Header.
%             %                   Default is true.
%             %            'CropID_FromInd' - If true, then CropID is taken
%             %                   from object element index. Default is false.
%             %            'SetProp' - Pairs of additional ImagePath properties to
%             %                   populated (key,val). This is overriding the
%             %                   Header properties.
%             %                   Default is {'Product','Image'}.
%             %            'DataDirFromProjName' - Set DataDir value from
%             %                   ProjName. Default is false.
%             % Output : - A future object (for parfeval).
%             %          - The updated ImagePath object.
%             % Author : Eran Ofek (Jan 2022)
% 
% 
%             arguments
%                 ObjIP
%                 ObjProduct
%                 Args.Save logical              = true;
%                 Args.ParEval logical           = false;
%                 Args.SaveFun function_handle   = @write1;
%                 Args.SaveFunArgs cell          = {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
%                 Args.PropFromHeader logical    = true;
%                 Args.CropID_FromInd logical    = false;
%                 Args.SetProp cell              = {'Product','Image'};   % overide header
%                 Args.DataDirFromProjName logical = false;
%             end
%             Future = [];
%             if Args.Save
% 
%                 % FFU:
%                 % create dir
%                 % use parfeval
% 
%                 if Args.ParEval
%                     Future = parfeval(@ImagePath.saveProductBlocking, 0, ObjIP, ObjProduct, 'SaveFun',Args.SaveFun, 'SaveFunArgs',Args.SaveFunArgs, 'PropFromHeader',Args.PropFromHeader, 'SetProp',Args.SetProp);
%                 else
%                     ObjIP = ImagePath.saveProductBlocking(ObjIP, ObjProduct,...
%                                                          'SaveFun',Args.SaveFun,...
%                                                          'SaveFunArgs',Args.SaveFunArgs,...
%                                                          'PropFromHeader',Args.PropFromHeader,...
%                                                          'CropID_FromInd',Args.CropID_FromInd,...
%                                                          'SetProp',Args.SetProp,...
%                                                          'DataDirFromProjName',Args.DataDirFromProjName);
%                 end
% 
%             end
%         end
%     end
% 
%     methods (Static) % utilities
%         function ObjIP = saveProductBlocking(ObjIP, ObjProduct, Args)
%             % Save product to disk - utility blocking function
%             %   This function is for the internal use by
%             %   ImagePath/saveProduct
%             % Input  : - A single-element ImagePath object.
%             %          - Object product to save (e.g., an AstroImage)
%             %          * ...,key,val,...
%             %            'SaveFun' - Function handle for saving object
%             %                   Default is @write1
%             %            'SaveFunArgs' - A cell array of additional arguments to pass
%             %                   to the SaveFun function.
%             %                   Default is {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
%             %            'PropFromHeader' - A logical indicating if to
%             %                   popuAlate ImagePath properties from image header.
%             %                   Default is true.
%             %            'CropID_FromInd' - If true, then CropID is taken
%             %                   from object element index. Default is false.
%             %            'SetProp' - A cell array of pairs of additional
%             %                   ImagePath properties to set (override
%             %                   header). These are ...Prop,val,...
%             %                   Default is  {'Product','Image'}
%             %            'DataDirFromProjName' - Set DataDir value from
%             %                   ProjName. Default is false.
%             % Output : The updated ImagePath object.
%             % Author : Eran Ofek (Jan 2022)
%             % Example: 
% 
%             arguments
%                 ObjIP(1,1) ImagePath
%                 ObjProduct
%                 Args.SaveFun function_handle   = @write1;
%                 Args.SaveFunArgs cell          = {'Image',  'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false};
%                 Args.PropFromHeader logical    = true;
%                 Args.CropID_FromInd logical    = false;
%                 Args.SetProp cell              = {'Product','Image'};   % overide header
%                 Args.DataDirFromProjName logical = false;
%             end
% 
%             NsetProp = numel(Args.SetProp);
%             if (0.5.*NsetProp)~=floor(0.5.*NsetProp)
%                 % odd number of SetProp
%                 error('Number of elements in SetProp argument must be even (key,val)');
%             end
% 
%             Nprod = numel(ObjProduct);
%             for Iprod=1:1:Nprod
%                 if Args.PropFromHeader
%                     ObjIP.readFromHeader(ObjProduct(Iprod));  
%                 end
%                 for Iset=1:2:NsetProp
%                     ObjIP.(Args.SetProp{Iset}) = Args.SetProp{Iset+1};
%                 end
%                 if Args.CropID_FromInd
%                     ObjIP.CropID = Iprod;
%                 end
%                 if Args.DataDirFromProjName
%                     ObjIP.DataDir = ObjIP.ProjName;
%                 end
% 
%                 FullName = ObjIP.genFull;
%                 if Iprod==1
%                     % create Dir
%                     Path = fileparts(FullName);
%                     if ~isfolder(Path)
%                         %exist(Path,'dir')==0
%                         % create dir
%                         mkdir(Path);
%                     end
%                 end
% 
%                 Args.SaveFun(ObjProduct(Iprod), FullName, Args.SaveFunArgs{:});
%             end
% 
%         end
% 
% 
% 
%         function ObjIP = generateImagePathFromProduct(ObjProduct, Args)
%             % Generate an ImagePath object from product (e.g., AstroImage)
%             % Input  : - A product object (e.g., AstroImage).
%             %          * ...,key,val,...
%             %            'PropFromHeader' - A logical indicating if to
%             %                   popuAlate ImagePath properties from image header.
%             %                   Default is true.
%             %            'CropID_FromInd' - If true, then CropID is taken
%             %                   from object element index. Default is false.
%             %            'SetProp' - A cell array of pairs of additional
%             %                   ImagePath properties to set (override
%             %                   header). These are ...Prop,val,...
%             %                   Default is  {'Product','Image'}
%             %            'DataDirFromProjName' - Set DataDir value from
%             %                   ProjName. Default is false.
%             % Output : - An ImagePath object populated based on product
%             %            data.
%             % Author : Eran Ofek (Feb 2022)
%             % Example: IP = ImagePath.generateImagePathFromProduct(AllSI(1));
% 
%             arguments
%                 ObjProduct
%                 Args.PropFromHeader logical      = true;
%                 Args.CropID_FromInd logical      = false;
%                 Args.SetProp cell                = {'Product','Image'};   % overide header
%                 Args.DataDirFromProjName logical = false;
%                 Args.AutoSubDir logical          = true;
%             end
% 
%             NsetProp = numel(Args.SetProp);
%             if (0.5.*NsetProp)~=floor(0.5.*NsetProp)
%                 % odd number of SetProp
%                 error('Number of elements in SetProp argument must be even (key,val)');
%             end            
% 
%             Nprod = numel(ObjProduct);
%             ObjIP = ImagePath(Nprod);
%             CopySubDirFromPrevious = false;
%             for Iprod=1:1:Nprod
%                 if Args.PropFromHeader
%                     ObjIP(Iprod).readFromHeader(ObjProduct(Iprod));  
%                 end
%                 for Iset=1:2:NsetProp
%                     ObjIP(Iprod).(Args.SetProp{Iset}) = Args.SetProp{Iset+1};
%                 end
%                 if Args.CropID_FromInd
%                     ObjIP(Iprod).CropID = Iprod;
%                 end
%                 if Args.DataDirFromProjName
%                     ObjIP(Iprod).DataDir = ObjIP.ProjName;
%                 end
% 
%                 if Args.AutoSubDir && ~CopySubDirFromPrevious
%                     % automatically set the SubDir directory : +1 to
%                     % largest existing dir
%                     FullPath = ObjIP(Iprod).genPath;
%                     FullPath = strrep(FullPath,sprintf('%sNaN%s',filesep,filesep),'');
% 
%                     if isfolder(FullPath)
%                         FL       = dir(FullPath);
%                         Flag     = [FL.isdir]' & ~strcmp({FL.name}.','.') & ~strcmp({FL.name}.','..');
%                         FL       = FL(Flag);
%                         if isempty(FL)
%                             % no dirs
%                             ObjIP(Iprod).SubDir = '1';
%                         else
%                             Max = max(str2double({FL.name}));
%                             if isnan(Max)
%                                 ObjIP(Iprod).SubDir = '1';
%                             else
%                                 ObjIP(Iprod).SubDir = sprintf('%d',Max + 1);
%                             end
%                         end
% 
%                     else
%                         ObjIP(Iprod).SubDir = '1';
%                     end
%                 else
%                     if CopySubDirFromPrevious
%                         ObjIP(Iprod).SubDir = ObjIP(Iprod-1).SubDir;
%                     end
%                 end
% 
% 
% %                FullName = ObjIP.genFull;
% %                 if Iprod==1
% %                     % create Dir
% %                     Path = fileparts(FullName);
% %                     %if exist(Path,'dir')==0
% %                     %    % create dir
% %                     %    mkdir(Path);
% %                     %end
% %                 end
%             end
%         end
% 
%     end
            
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
