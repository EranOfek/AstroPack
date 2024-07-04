% ImagePath - A class for generating stand sgenFiltoring image/path names
%       for ULTRASAT and LAST.
%
% File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>


classdef FileNames < Component
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
        %<ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
        ProjName            = '';
        Time                = [];
        Filter              = {'clear'};
        FieldID cell        = {};
        Counter             = [];
        CCDID               = [];
        CropID              = [];
        Type                = {'sci'};
        Level               = {'raw'};
        Product             = {'Image'};
        Version             = [1];
        FileType cell       = {'fits'};
        
        
        %
        FullPath            = '';
        
        BasePath            = '/euler1/archive/LAST';
        BasePathIncludeProjName logical = true;
        BasePathRef         = '/euler1/archive/LAST';
        SubDir              = '';
        TimeZone            = 2;
        
    end
    
    properties (Hidden)
        % Fields formatting
        %FormatFieldID   = '%06d';       % Used with FieldID
        FormatCounter   = '%03d';       % Used with Counter        
        FormatCCDID     = '%03d';       % Used with CCDID
        FormatCropID    = '%03d';       % Used with CropID
        FormatVersion   = '%d'; %'%03d';       % Used with Version
        
    end

    properties (Hidden, SetAccess=protected, GetAccess=public)
     
    end
    
    properties (Hidden, Constant)
        ListType        = { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'focus', 'sci', 'wave', 'type' , 'log'};
        ListLevel       = { 'raw', 'proc', 'stack', 'ref', 'coadd', 'merged', 'calib', 'junk', 'proc.zogyD','coadd.zogyD'};
        ListProduct     = { 'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat', 'Asteroids','Pipeline', 'TransientsCat'};
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
            
            Obj.ProjName{1}     = 'USAT';            
            Obj.Time            = 2451545;
            Obj.TimeZone        = 2;
            Obj.Filter{1}       = 'clear';
            Obj.FieldID{1}      = 'fld';
            Obj.Counter         = 1;
            Obj.CCDID           = 1;
            Obj.CropID          = 1;
            Obj.Type{1}         = 'sci';
            Obj.Level{1}        = 'raw';
            Obj.Product{1}      = 'Image';
            Obj.Version         = 1;
            Obj.FileType{1}     = 'fits';
            Obj.SubDir          = 'subdir';
            
            % Debug? or have it?
            Obj.BasePath        = '/home/last';
           
        end
    end
    
    methods % setter/getters
        function Obj = set.Type(Obj, Val)
            % Setter for Type
            if ischar(Val)
                Val = {Val};
            end
            Obj.Type = Val;
            Obj.validateType;
        end
        function Obj = set.Level(Obj, Val)
            % Setter for Level
            if ischar(Val)
                Val = {Val};
            end
            Obj.Level = Val;
            Obj.validateLevel;
        end
        function Obj = set.Product(Obj, Val)
            % Setter for Product
            if ischar(Val)
                Val = {Val};
            end
            Obj.Product = Val;
            Obj.validateProduct;
        end
        
        function Obj = set.ProjName(Obj, Val)
            % Setter for ProjName
            if ischar(Val)
                Val = {Val};
            end
            Obj.ProjName = Val;
        end
        
        function Obj = set.Filter(Obj, Val)
            % Setter for Filter
            if ischar(Val)
                Val = {Val};
            end
            Obj.Filter = Val;
        end
        
        
        
        
    end
      
    methods (Static) % construction
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

        function Obj=generateFromFileName(List, Args)
            % Generate a FileNames object from a list of file names
            %   Given file names which name structure obeys the
            %   LAST-ULTRASAT file name convention.
            % Input  : - A list of file names.
            %            Either a char array or a cell array.
            %            If the input is a char array, then the function
            %            will use the io.files.filelist to search for all
            %            existing files that have this name (wild cards are
            %            allowed). In this case the file must exist.
            %            If a cell array, then the file name in each element of
            %            the cell will be used as is. In this case, the
            %            file doesn't need to exist.
            %          * ...,key,val,...
            %            'FullPath' - Directory to populate FullPath, if true,
            %                   then use current directory. If false or
            %                   empty, use class default.
            %                   Default is true.
            %            'WarningIfEmpty' - A logical indicating if to
            %                   print a warning in case the outpout FileNames
            %                   object contains no files.
            %                   Default is true.
            %            'CheckExist' - A logical indicating if to check
            %                   that all the files exist. Only operational when the
            %                   input is a cell. If some files does not
            %                   exist then the function will act according
            %                   to the options in the 'DoNotExist'
            %                   argument.
            %                   Default is false.
            %            'DoNotExist' - What to do in case a file doesn't
            %                   exist: 'error'|'warnning'
            %                   Default is 'error'.
            %            'FullPathFromFileName' - If the user provided file
            %                   name contains a path and this argument is
            %                   true, then the FullPath property will be
            %                   populated with the user-provided path.
            %                   Default is false.
            % Output : - A FileNames object containing the file names.
            %          If you will use this with the char array option when
            %          the file doesn't exist, then the returned structure
            %          will contain no Time entries (i.e., FN.nfiles will
            %          be zero).
            % Author : Eran Ofek (Dec 2022)
            % Example: FN=FileNames.generateFromFileName('LAST*.fits');
            
            
            arguments
                List
                Args.FullPath                     = true;
                Args.WarningIfEmpty logical       = true;
                Args.CheckExist logical           = false;
                Args.DoNotExist                   = 'error';
                Args.FullPathFromFileName logical = false;
            end
                        
            if ischar(List)
                List = io.files.filelist(List, 'AddPath',false);
            elseif iscell(List)
                List = List;
                if Args.CheckExist
                    if ~all(isfile(List))
                       switch lower(Args.DoNotExist)
                           case 'error'
                               Ierr = find(~isfile(List), 1);
                               error('File name: %s does not exist',List{Ierr});
                           case 'warnning'
                               Ierr = find(~isfile(List), 1);
                               warnning('File name: %s does not exist',List{Ierr});
                           otherwise
                               error('Unknown option for DoNotExist argument');
                       end
                    end
                end
            else
                error('List must be either a char array or cell array');
            end
            
            Obj = FileNames;
            
            if islogical(Args.FullPath)
                if Args.FullPath
                    Obj.FullPath = pwd;
                end
            elseif isempty(Args.FullPath)
                % do nothing
            else
                Obj.FullPath = Args.FullPath;
            end

            Nlist = numel(List);
            for Ilist=1:1:Nlist
                SplitName = regexp(List{Ilist},'_','split');
                if numel(SplitName)~=11
                    error('Illegal file name - number of underline-seperators in file name %s is not 11',List{Ilist});
                end
                % make sure the first splitted term doesn't contain the
                % path
                %[SplitNameCell] = split(SplitName{1}, filesep);
                [UserPath, Tmp1,Tmp2] = fileparts(SplitName{1});
                SplitName{1} = sprintf('%s%s',Tmp1,Tmp2);
                if Args.FullPathFromFileName && ~isempty(UserPath)
                    Obj.FullPath = UserPath;
                end
                Obj.ProjName{Ilist} = SplitName{1}; %Cell{end};
                Obj.Time{Ilist}     = SplitName{2};
                Obj.Filter{Ilist}   = SplitName{3};
                Obj.FieldID{Ilist}  = SplitName{4};
                Obj.Counter(Ilist)  = str2double(SplitName{5});
                Obj.CCDID(Ilist)    = str2double(SplitName{6});
                Obj.CropID(Ilist)   = str2double(SplitName{7});
                Obj.Type{Ilist}     = SplitName{8};
                Obj.Level{Ilist}    = SplitName{9};
                Obj.Product{Ilist}  = SplitName{10};
                TmpSplit = split(SplitName{11},'.');
                Obj.Version(Ilist)  = str2double(TmpSplit{1});
                Obj.FileType{Ilist} = TmpSplit{2};
                                
            end
            
            if Args.WarningIfEmpty
                if Obj.nfiles==0
                    warning('No files matching the requested names were found - output FileNames object contain no files');
                end
            end

        end
    end
    
    methods % utilities
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
        
        function JD = julday(Obj)
            % Convert Time to JD
            % Input  : - An FileNames object
            % Output : - A vector of JD, one per time in object.
            % Author : Eran Ofek (Dec 2022)
            

            if isempty(Obj.Time)
                Obj.Time = [];
                JD       = [];
            else
                if isnumeric(Obj.Time)
                    JD = Obj.Time;
                else
                    FlagN   = cellfun(@ischar, Obj.Time,'UniformOutput',true);
                    DateVec = convert.strFN2date(Obj.Time(FlagN));
                    JD      = nan(numel(FlagN),1);
                    JD(FlagN)  = celestial.time.julday(DateVec(:,[3 2 1 4 5 6]));
                    %JD(~FlagN) = NaN;
                end
            end
                
        end

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

        
        function Obj = jd2str(Obj)
            % Convert JD in Time property to file name strings
            % Input  : - An FileNames object.
            % Output : - The FileNames object in which the Time property is
            %            in file name string format: yyyymmdd.HHMMSS.FFF
            % Author : Eran Ofek (Dec 2022)
            
            if isnumeric(Obj.Time)
                N         = numel(Obj.Time);
                FlagN     = isnan(Obj.Time);
                Obj.Time(FlagN) = 2451545;
                DateVec   = celestial.time.jd2date(Obj.Time, 'H');
                DateArray = datestr(DateVec(:,[3 2 1 4 5 6]),'yyyymmdd.HHMMSS.FFF');
                Str       = cell(N,1);
                for I=1:1:N
                    Str{I} = DateArray(I,:);
                end
                if sum(FlagN)>0
                    IndN = find(FlagN);
                    for In=1:1:numel(IndN)
                        Str{IndN(In)} = NaN;
                    end
                end
                Obj.Time = Str;
            end
        end
        
        function Result = getProp(Obj, Prop, Ind, Args)
            % get property (all or single by index).
            % Input  : - A FileNames object.
            %          - Property to retrieve. Default is 'Time'.
            %          - An index of element in array to retrieve
            %            If empty, then get all, in array or cell array.
            %            If scalar, then get a single element (not in
            %            cell).
            %            If scalar is larger than the number of elements,
            %            then return the first element only.
            %          * ...,key,val,...
            %            'jd2str' - Default is true.
            % Output : - Value.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                Prop = 'Time';
                Ind  = [];
                Args.Jd2str logical  = true;
                Args.CreateNewObj logical = true;
            end

            if Args.Jd2str && strcmp(Prop, 'Time')
                if isnumeric(Obj.(Prop))
                    Obj.jd2str;
                end
            end
            
            if isempty(Ind)
                Result = Obj.(Prop);
            else
                if isempty(Obj.(Prop))
                    Result = '';
                else
                    if iscell(Obj.(Prop))
                        if Ind>numel(Obj.(Prop))
                            Result = Obj.(Prop){1};
                        else
                            Result = Obj.(Prop){Ind};
                        end
                    elseif isnumeric(Obj.(Prop))
                        if Ind>numel(Obj.(Prop))
                            Result = Obj.(Prop)(1);
                        else
                            Result = Obj.(Prop)(Ind);
                        end
                    elseif ischar(Obj.(Prop))
                        Result = Obj.(Prop);
                    else
                        error('Unknown FileNames property format');
                    end
                end
            end
            
        end
        
        function DateDir = getDateDir(Obj, Ind, ReturnChar)
            % Return date directory name from file name properties
            % Input  : - An FileNames object.
            %          - Index of time in object for which to generate date
            %            dir name. If empty, generate for all times.
            %            Default is [].
            %          - A logical indicating if to return a char array
            %            instead of cell array. Default is false.
            % Output : - A cell array of date directories for each file
            %            time stamp.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                Ind   = [];
                ReturnChar logical = false;
            end
            
            JD = Obj.julday;
            JD = JD + Obj.TimeZone./24;
            JD = floor(JD);
            Date = celestial.time.jd2date(JD);
            
            if isempty(Ind)
                N = numel(JD);
                DateDir = cell(N,1);
                for I=1:1:N
                    DateDir{I} = sprintf('%s%04d%s%02d%s%02d',filesep,Date(I,3), filesep,Date(I,2), filesep, Date(I,1));
                end
            else
                Date = Date(Ind,:);
                DateDir = {sprintf('%s%04d%s%02d%s%02d',filesep,Date(3), filesep,Date(2), filesep, Date(1))};
            end
                
            if numel(DateDir)==1 && ReturnChar
                DateDir = DateDir{1};
            end
        end
    end
    
    methods % file/path names
        function FileName = genFile(Obj, Ind, Args)
            % Generate a cell array of file names from a FileNames object
            % Input  : - An FileNames object.
            %          - If empty, then will return all file names.
            %            If scalar, then will return only the file name
            %            that corresponds to the Ith time in the object.
            %          * ..., key,val,...
            %            'ReturnChar' - A logical indicating if to return a char array
            %                   instead of cell array in case that a single file is
            %                   returned. Default is false.
            %            'Product' - If given (e.g., 'Image') will override
            %                   Product name (but will not modify the
            %                   object). Default is '';
            %            'Level' - If given (e.g., 'proc') will override
            %                   Level name (but will not modify the
            %                   object). Default is '';
            %            'IsLog' - A logical indicating if to generate a
            %                   log file name. Default si false;
            % Output : - A cell array of file names.
            % Author : Eran Ofek (Dec 2022)
        
            arguments
                Obj
                Ind = [];
                Args.ReturnChar logical = false;
                Args.Product            = '';
                Args.Level              = '';
                Args.IsLog logical      = false;
            end
            
            if isempty(Ind)
                if ischar(Obj.Time)
                    Ntime = 1;
                else
                    Ntime = numel(Obj.Time);
                end
            else
                Ntime = 1;
            end
            
            
            FileName = cell(Ntime,1);
            for Itime=1:1:Ntime
                if ~isempty(Ind)
                    Itime = Ind;
                end
                
                if isempty(Args.Product)
                    ProductStr = Obj.getProp('Product',Itime);
                else
                    ProductStr = Args.Product;
                    if iscell(ProductStr)
                        ProductStr = ProductStr{1};
                    end
                end

                if Args.IsLog
                    % generate log files
                    OnlyDate    = Obj.getDateDir(1,true);
                    OnlyDate    = strrep(OnlyDate, filesep, '');
                    
                    FileName{Itime} = sprintf('%s_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.%s',...
                                            Obj.getProp('ProjName',Itime),...
                                            OnlyDate,...
                                            '',...
                                            '',...
                                            '',...
                                            '',...
                                            '',...
                                            'log',...
                                            '',...
                                            ProductStr,...
                                            '',...
                                            'log');
                else
                                            
                    FilterStr = Obj.getProp('Filter',Itime);
                    if isnumeric(FilterStr)
                        if isnan(FilterStr)
                            FilterStr = '';
                        else
                            FilterStr = sprintf('%d',FilterStr);
                        end
                    end
                    FieldIDStr = Obj.getProp('FieldID',Itime);
                    if isnumeric(FieldIDStr) && ~isnan(FieldIDStr)
                        if isnan(FieldIDStr)
                            FieldIDStr = '';
                        else
                            FieldIDStr = sprintf(Obj.FormatFieldID,FieldIDStr);
                        end
                    end
                    CounterStr = Obj.getProp('Counter',Itime);
                    if isnumeric(CounterStr)
                        if isnan(CounterStr)
                            CounterStr = '';
                        else
                            CounterStr = sprintf(Obj.FormatCounter,CounterStr);
                        end
                    end
                    CCDIDStr = Obj.getProp('CCDID',Itime);
                    if isnumeric(CCDIDStr)
                        if isnan(CCDIDStr)
                            CCDIDStr = '';
                        else
                            CCDIDStr = sprintf(Obj.FormatCCDID,CCDIDStr);
                        end
                    end
                    CropIDStr = Obj.getProp('CropID',Itime);
                    if isnumeric(CropIDStr)
                        if isnan(CropIDStr)
                            CropIDStr = '';
                        else
                            CropIDStr = sprintf(Obj.FormatCropID,CropIDStr);
                        end
                    end
                    VersionStr = Obj.getProp('Version',Itime);
                    if isnumeric(VersionStr)
                        VersionStr = sprintf(Obj.FormatVersion,VersionStr);
                    end

                    

                    if isempty(Args.Level)
                        LevelStr = Obj.getProp('Level',Itime);
                    else
                        LevelStr = Args.Level;
                    end

                    % <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
                    FileName{Itime} = sprintf('%s_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.%s',...
                                                Obj.getProp('ProjName',Itime),...
                                                Obj.getProp('Time',Itime),...
                                                FilterStr,...
                                                FieldIDStr,...
                                                CounterStr,...
                                                CCDIDStr,...
                                                CropIDStr,...
                                                Obj.getProp('Type',Itime),...
                                                LevelStr,...
                                                ProductStr,...
                                                VersionStr,...
                                                Obj.getProp('FileType',Itime));
                end
            end
            if Args.ReturnChar && Ntime==1
                FileName = FileName{1};
            end
            
            
        end
        
        function Path = genPath(Obj, Ind, Args)
            % Generate path for FileNames object.
            %       If the FullPath [property in the object is populated
            %       than it is returned.
            %       If FullPath or BasePath are given then they are used
            %       but not propagated to the object.
            % Input  : - A FileNames object.
            %          - Index of time stamp in the object for which to
            %            generate the path. If empty, then for all (slow).
            %            Default is 1.
            %          * ...,key,val,...
            %            'ReturnChar' - A logical indicatibf if to return the path in a
            %                   char array (true) or cell (false).
            %                   Default is true.
            %            'AddSubDir' - Add SubDir to path. Default is true.
            %            'BasePath' - Base path to insert into the object
            %                   BasePath property. If empty, use object default.
            %                   Default is [].
            %            'FullPath' - A full path to insert into the object
            %                   FullPath property. If empty, use object default.
            %                   Default is [].
            %            'Level' - Image level. If empty, use object Level.
            %                   Default is [].
            % Output : - A path.
            % Author : Eran Ofek (Dec 2022) 
            
            arguments
                Obj(1,1)
                Ind = 1;
                Args.ReturnChar logical = true;
                Args.AddSubDir logical  = true;
                Args.BasePath = [];
                Args.FullPath = [];
                Args.Level    = [];

            end
            
            if nfiles(Obj)==0
                Path = {''};
            else

                if isempty(Ind)
                    if ischar(Obj.Time)
                        Ntime = 1;
                    else
                        Ntime = numel(Obj.Time);
                    end
                else
                    Ntime = 1;
                end
                
                if ~isempty(Args.BasePath)
                    %Obj.BasePath = Args.BasePath;
                    BasePath = Args.BasePath;
                else
                    BasePath = Obj.BasePath;
                end
    
                if ~isempty(Args.FullPath)
                    %Obj.FullPath = Args.FullPath;
                    FullPath = Args.FullPath;
                else
                    FullPath = Obj.FullPath;
                end
                
                if isempty(FullPath)
                    Path = cell(Ntime,1);
                    for Itime=1:1:Ntime
                        if ~isempty(Ind)
                            Itime = Ind;
                        end
    
                        if isempty(Args.Level)
                            % get Level from object:
                            Level = getProp(Obj, 'Level', Itime);
                        else
                            % Level supplied by arguments
                            Level = Args.Level;
                        end
    
                        % /euler1/archive/LAST/<ProjName>/new
                        % /euler1/archive/LAST/<ProjName>/2022/12/01/raw
                        % /euler1/archive/LAST/<ProjName>/2022/12/01/proc
                        % /euler1/archive/LAST/<ProjName>/2022/12/01/proc/1
                        DateDir = getDateDir(Obj, Itime, true);
                        if Obj.BasePathIncludeProjName
                            % BasePath already include the ProjName
                            Path{Itime} = sprintf('%s%s%s%s%s%s%s%s',...
                                            BasePath, filesep, ...
                                            DateDir, filesep, ...
                                            Level);
                        else
                            Path{Itime} = sprintf('%s%s%s%s%s%s%s%s',...
                                            BasePath, filesep, ...
                                            getProp(Obj, 'ProjName', Itime),...
                                            DateDir, filesep, ...
                                            Level);
                        end
    
                        if Args.AddSubDir
                            Path{Itime} = sprintf('%s%s%s',Path{Itime},filesep,...
                                                           getProp(Obj, 'SubDir', Itime));
                        end
                    end
    
                else
                    Path = {FullPath};
                end
            end

            if Args.ReturnChar && numel(Path)==1
                Path = Path{1};
            end
            
        end
        
        function FullName = genFull(Obj, Ind, Args)
            % Generate a full path and file name for all files
            % Input  : - An FileNames object
            %          - Index of time for file. If empty, generate full
            %            name for all files. Default is [].
            %          * ...,key,val,...
            %            'IndDir' - Index of file for path. If empty, generate full
            %                   name for all files. Default is 1.
            %            'ReturnChar' - Logical indicating if to return char instead of
            %                   cell (only if a single file is returned).
            %                   Default is false.
            %            'BasePath' - Base path to insert into the object
            %                   BasePath property. If empty, use object default.
            %                   Default is [].
            %            'FullPath' - A full path to insert into the object
            %                   FullPath property.
            %                   If FullPath is not empty then it will
            %                   replace the automatic path construction.
            %                   If empty, use object default.
            %                   Default is [].
            %            'Product' - See genFile.
            %            'Level' - Level to add to file and path.
            %                   If empty, use object Level.
            %                   Default is ''.
            %            'LevelPath' - Seperate Level for path.
            %                   If numeric empty, then use 'Level' argument.
            %                   Default is [].
            %            'AddSubDir' - A logical indicating if to
            %                   automatically add numerical SubDir.
            %                   Default is true.
            %            'RemoveLeadingStr' - If not empty, then this is a
            %                   string that will be searched in the
            %                   FullName output and will be replaced with
            %                   ''. Default is [].
            % Output : - A cell array of full file name and path.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                Ind           = [];
                Args.IndDir   = 1;
                Args.ReturnChar logical = false;
                Args.BasePath  = [];
                Args.FullPath  = [];
                Args.Product   = '';
                Args.Level     = '';
                Args.LevelPath = [];
                Args.AddSubDir logical = true;
                Args.RemoveLeadingStr = [];
            end
            
            if isempty(Args.LevelPath) && isnumeric(Args.LevelPath)
                Args.LevelPath = Args.Level;
            end

            FileName = genFile(Obj, Ind, 'ReturnChar',Args.ReturnChar, 'Product',Args.Product, 'Level',Args.Level);
            Path     = genPath(Obj, Args.IndDir, 'ReturnChar',Args.ReturnChar, 'BasePath',Args.BasePath, 'FullPath',Args.FullPath, 'Level',Args.LevelPath, 'AddSubDir',Args.AddSubDir);
            
            Nfn      = numel(FileName);
            Np       = numel(Path);
            FullName = cell(Nfn,1);
            for Ifn=1:1:Nfn
                Ip = min(Np,Ifn);
                FullName{Ifn} = sprintf('%s%s%s',Path{Ip},filesep,FileName{Ifn});
            end
            
            if Args.ReturnChar && numel(FullName)==1
                FullName = FullName{1};
            end

            if ~isempty(Args.RemoveLeadingStr)
                FullName = strrep(FullName, Args.RemoveLeadingStr, '');
            end

            
        end
        
        function [Result,Obj] = nextSubDir(Obj, Args)
            % Given SubDir which is a running numeric index, check which
            %   directories exist in FileNames path and return the next
            %   SubDir name
            % Input  : - A FileNames object from which a path can be
            %            generated using genPath.
            %          * ...,key,val,...
            %            'OneIfEmpty' - A logical indicating if to return
            %                   SubDir='1', if directory does not exist
            %                   (if false will return []).
            %                   Default is true.
            %            'UseTime' - A logical indicating if the SubDir
            %                   string is a time stamp (true), or number (false).
            %                   If true, then the SubDir will be of the
            %                   format HHMMSSv#, where number indicate if
            %                   there is more than one dir with this time
            %                   stampe. First dir alwas have v0.
            %                   Default is false.
            % Output : - A char array containing the suggested SubDir name
            %            that does not exist in path. 
            %          - Only if the second argument is requested, then the
            %            will update and return the FileNames object with
            %            the new SubDir directory.
            % Author : Eran Ofek (Dec 2022)
           
            arguments
                Obj
                Args.OneIfEmpty logical   = true;
                Args.UseTime logical      = true;
            end

            Path = Obj.genPath(1, 'AddSubDir',false); % Path without SubDir
            Dir  = dir(Path);
            
            if Args.UseTime
                % SubDir is a time stamp
                %File = Obj.genFile;
                Obj.jd2str;
                SpTime  = split(Obj.Time{1},'.');
                Result  = SpTime{2};
                Flag    = contains({Dir.name}, Result);
                Version = sum(Flag);
                Result  = sprintf('%sv%d',Result, Version);
            else
                % SUbDir is a number
                if isempty(Dir) && Args.OneIfEmpty
                    Result = '1';
                else
                    % select non-hidden directories
                    Flag = [Dir.isdir] & ~startsWith({Dir.name}, '.');
                    
                    NumDir = str2double({Dir(Flag).name});
                    if isempty(NumDir) && Args.OneIfEmpty
                        Result = '1';
                    else
                        Result = sprintf('%d',max(NumDir) + 1);
                    end
                end
            end

            if nargout>1
                % update SubDir
                Obj.SubDir = Result;
            end
        
                        
        end
        
    end
    
    
    methods % Read/Write from Header
        
        function Obj = readFromHeader(Obj, Input, DataProp)
            % Read FileNames parameters from header.
            % Input  : - An FileNames object.
            %          - AstroImage or AstroHeader.
            %          - Either data property name or product name.
            % Output : - A populated FileNames object.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj(1,1)
                Input        % AstroHeader | AstroImage
                DataProp    = 'Image';    % DataProp in AstroImage or Product name
                
            end
            
            %Obj.msgLog(LogLevel.Debug, 'readFromHeader: ');
                         
            Nim = numel(Input);
            for Iim=1:1:Nim

                if isa(Input, 'AstroHeader')
                    Header = Input(Iim);
                elseif isa(Input, 'AstroImage')
                    Header = Input(Iim).HeaderData;
                else
                    error('Input must be an AstroHeader or AstroImage');
                end
                  
                Obj.ProjName{Iim}        = Header.getVal({'INSTRUME','PROJNAME'}); %Obj.DictKeyNames.PROJNAME);
                Obj.Time(Iim)            = julday(Header);  %.getVal('JD');
                Obj.TimeZone(Iim)        = Header.getVal('TIMEZONE');
                Obj.Filter{Iim}          = Header.getVal('FILTER');
                Obj.FieldID{Iim}         = Header.getVal('FIELDID');
                Obj.Counter{Iim}         = Header.getVal('COUNTER');
                Obj.CCDID(Iim)           = Header.getVal('CCDID');
                Obj.CropID(Iim)          = Header.getVal('CROPID');
                Obj.Type{Iim}            = Header.getVal('IMTYPE');
                Obj.Level{Iim}           = Header.getVal('LEVEL');
                if isempty(DataProp)
                    Obj.Product{Iim}         = Header.getVal('PRODUCT');
                else
                    Obj.Product{Iim}         = DataProp;
                end
                Obj.Version(Iim)         = Header.getVal('VERSION');
                Obj.FileType{Iim}        = 'fits';
                Obj.SubDir{Iim}          = Header.getVal('SUBDIR');
                if ~iscell(Obj.SubDir) && isnan(Obj.SubDir)
                    Obj.SubDir{Iim} = '';
                end
            end

        end
        
        function Input = writeToHeader(Obj, Input, KeysToWrite)
            % Write data to AstroHeader, DictKeyNames is used to get the
            % correct key names  
            % Input  : - A FileNames object.
            %          - An AstroHeader or AstroImage object.
            %            Number of elements must be equal to the number of
            %            times in FileNames.
            %          - A cell arrays of properties in FileNames to write
            %            to header.
            %            Default is : {'TimeZone','Filter','FieldID','Counter','CCDID','CropID','Type','Level','Product','Version','FileType','SubDir'}
            % Output : - The input AstroHeader or AstroImage with the
            %            updated header.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                Obj
                Input  % AstroHeader | AstroImage
                KeysToWrite = {'TimeZone','Filter','FieldID','Counter','CCDID','CropID','Type','Level','Product','Version','FileType','SubDir'};
            end

            Nk = numel(KeysToWrite);
            Nt = numel(Obj.Time);
            Nh = numel(Input);
            if Nt~=Nh
                error('Number of header elements must be equal to the number of times in FileNames');
            end
                 
            for Ih=1:1:Nh
                if isa(Input, 'AstroHeader')
                    Header = Input(Ih);
                else
                    % assuming AstroImage
                    Header = Input(Ih).Header;
                end
                
                for Ik=1:1:Nk
                    error('BUG')
                    %Header.replaceVal(Obj.Config.Data.Header.ImagePath.KeyNames.(KeysToWrite{Ik}{1}, Obj.getProp(KeysToWrite{Ik},Ih);
                end
              
                if isa(Input, 'AstroImage')
                    Input(Ih).Header = Header;
                end
            end
            
        end        

        function Result=updateFromObjectInfo(Obj, DataObj, Args)
            % Update an FileNames object using the metadata
            %   Update the Time, CropID, and Counter in a FileNames object
            %   from the header information of an AstroImage object, or JD
            %   and counters of AstroCatalog and MatchedSources objects.
            % Input  : - A FileNames object.
            %            For size restriction see the 'SelectFirst'
            %            argument.
            %          - A AstroImage/AstroCatalog/MatchedSources object.
            %          * ...,key,val,...
            %            'CreateNewObj' - Create a new copy of the input
            %                   object. Default is true.
            %            'SelectFirst' - A logical indicating if to take
            %                   the rest of the FileNames properties from
            %                   the first file in FileNames.
            %                   Default is true.
            %                   If false, then number of elements in
            %                   FileNames must be 1 or equal to the number
            %                   of elements in the AstroImage object.
            %            'GetHeaderJD' - Update JD from header. Default is true.
            %            'AI_CropID_FromHeader' - Update CropID from AstroImage header.
            %                   Default is true.
            %            'CropID_FromIndex' - For non AstroImage inputs,
            %                   update CropID from object element index.
            %                   Default is true.
            %            'AI_Counter_FromHeader' - Update Counter from AstroImage header.
            %                   Default is true.
            %            'Counter_Zero' - For non AstroImage inputs,
            %                   update Counter to 0.
            %                   Default is true.
            %            'KeyCropID' - Header keyword containing the
            %                   CropID. Default is 'CROPID'.
            %            'KeyCounter' - Header keyword containing the
            %                   Counter. Default is 'COUNTER'.
            % Output : - An updated FileNames object.
            %            Number of files equal and corresponding to the
            %            number of AstroImage elements.
            % Author : Eran Ofek (Apr 2023)
            % Example: Result = updateForAstroImage(FN_Sci_GroupsProc(Igroup), AllSI)

            arguments
                Obj
                DataObj

                Args.CreateNewObj logical   = true;
                Args.SelectFirst logical    = true;
                
                Args.GetHeaderJD logical       = true;

                Args.AI_CropID_FromHeader logical  = true;
                Args.CropID_FromIndex logical      = true;

                Args.AI_Counter_FromHeader logical = true;
                Args.Counter_Zero logical          = true;

                Args.KeyCropID              = 'CROPID';
                Args.KeyCounter             = 'COUNTER';
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            if Args.SelectFirst
                Result.reorderEntries(1);
            end

            Nfiles = Result.nfiles;
            Ndo    = numel(DataObj);

            if ~(Nfiles==1 || Ndo==Nfiles)
                error('FileNames object number of files must be 1 or equal to the number of elements in the data object');
            end

            if Args.GetHeaderJD
                switch class(DataObj)
                    case 'MatchedSources'
                        JD = DataObj.juldayFun('mid');
                    case {'AstroImage','AstroCatalog'}
                        JD = DataObj.julday;
                    otherwise
                        error('Unknwon class of DataObj');
                end
            else
                JD = [];
            end
            JD = JD(:);

            U_JD    = nan(Ndo,1);
            CropID  = nan(Ndo,1);
            Counter = nan(Ndo,1);


            switch class(DataObj)
                case {'AstroImage'}
                    for Ido=1:1:Ndo
                        if Args.AI_CropID_FromHeader 
                            CropID(Ido) = DataObj(Ido).HeaderData.getVal(Args.KeyCropID);
                        else
                            CropID = [];
                        end
                        if Args.AI_Counter_FromHeader
                            Counter(Ido) = DataObj(Ido).HeaderData.getVal(Args.KeyCounter);
                        else
                            Counter = [];
                        end
                    end


                case {'MatchedSources','AstroCatalog'}
                    if Args.CropID_FromIndex
                        CropID = (1:1:Ndo).';
                    else
                        CropID = [];
                    end
                    
                    if Args.Counter_Zero
                        Counter = 0;
                    else
                        Counter = [];
                    end


                otherwise
                    error('Unknown class of DataObj');
            end

            Result.updateIfNotEmpty('Counter',Counter, 'CROPID',CropID, 'Time',JD);
            
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
