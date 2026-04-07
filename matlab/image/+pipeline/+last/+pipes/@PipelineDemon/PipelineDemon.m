% ImagePath - A class for generating stand storing image/path names
%       for ULTRASAT and LAST.
%
% File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
% Example:
%          D = pipeline.last.pipes.PipelineDemon;
%          D.CalibPath = <put here the output directory of the calibration images>
%          D.prepMasterDark
%

classdef PipelineDemon < Component
    % 
            
    properties       
        %
        CI CalibImages   = CalibImages;    % CalibImages

        % These fields are the input parameters for getPath() and getFileName()
        ProjectName  = 'LAST';
        Node         = 1;
        DataDir      = 1;
        CamNumber    = [];
        HostName     = [];

        BasePath     = [];
   
        NewPath      = []; %'new';    % if start with '/' then abs path
        CalibPath    = []; %'calib';  % if start with '/' then abs path
        FailedPath   = []; %'failed'; % if start with '/' then abs path
        LogPath      = []; %'log';    % if start with '/' then abs path

        SciPath      = []; %'science';

        AutoPath     = [];  % 'LAST',...

        RefPath      = [];
        

        ObsCoo       = [35.04073 30.05298 415];  % [deg deg m]
    end
    
    properties (Hidden)
        % Fields formatting
        %FormatFieldID   = '%06d';       % Used with FieldID
        FormatCounter   = '%03d';       % Used with Counter        
        FormatCCDID     = '%03d';       % Used with CCDID
        FormatCropID    = '%03d';       % Used with CropID
        FormatVersion   = '%03d';       % Used with Version
        

        
        DefNewPath      = 'new';    % if start with '/' then abs path
        DefCalibPath    = 'calib';  % if start with '/' then abs path
        DefFailedPath   = 'failed'; % if start with '/' then abs path
        DefLogPath      = 'log';    % if start with '/' then abs path
        DefRefPath      = 'data/references';   %/last01e/data/refreences'

        FieldList       = pipeline.DemonLAST.fieldsListLAST;
    end

    properties (Hidden, SetAccess=protected, GetAccess=public)
     
    end
    
    properties (Hidden, Constant)
        ListType        = { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'focus', 'sci', 'wave', 'type' };
        ListLevel       = {'log', 'raw', 'proc', 'stack', 'ref', 'coadd', 'merged', 'calib', 'junk'};
        ListProduct     = { 'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat', 'Asteroids'};
    end
    
    
    methods % Constructor
       
        function Obj = DemonLAST(Args)
            % Constructor for DemonLAST

            
        end
        
    end
    
    methods % setter/getters
        function Obj=set.AutoPath(Obj, Val)
            % Setter for AutoPath
            
            if ~isempty(Val)
                if isempty(Obj.BasePath)
                    % BasePath is empty - first populate it:
                    Obj.BasePath = pipeline.last.pipes.PipelineDemon.getBasePath('DataDir',Obj.DataDir, 'CamNumber',Obj.CamNumber, 'ProjectName',Obj.ProjectName, 'Node',Obj.Node);
                end
                Obj.autoDetectPath(Val);
                Obj.AutoPath = Val;
            end

        end

        %function Result=get.BasePath(Obj)            
            % getter for BasePath

            % if isempty(Obj.BasePath)
            %     if isempty(Obj.DataDir) && isempty(Obj.CamNumber)
            %         Result = [];
            %     else
            %         % get base path
            %         Result = pipeline.DemonLAST.getBasePath('DataDir',Obj.DataDir, 'CamNumber',Obj.CamNumber, 'ProjectName',Obj.ProjectName, 'Node',Obj.Node);
            %     end
            % else
            %     Result         = Obj.BasePath;
            %     Obj.NewPath    = Obj.DefNewPath;
            %     Obj.CalibPath  = Obj.DefCalibPath;
            %     Obj.FailedPath = Obj.DefFailedPath;
            %     Obj.LogPath    = Obj.DefLogPath;
            % end
        %end

        function Result=get.NewPath(Obj)
            % getter fore NewPath

            if isempty(Obj.NewPath)
                Result = [];
            else
                if strcmp(Obj.NewPath(1),filesep)
                    % NewPath contains full dir name
                    Result = Obj.NewPath;
                else
                    % NewPath contains relative path (relative to BasePath)
                    Obj.NewPath = fullfile(Obj.BasePath,Obj.NewPath);
                    Result      = Obj.NewPath;
                end
            end

        end

        function Result=get.CalibPath(Obj)
            % getter fore CalibPath

            if isempty(Obj.CalibPath)
                Result = [];
            else
                if strcmp(Obj.CalibPath(1),filesep)
                    % CalibPath contains full dir name
                    Result = Obj.CalibPath;
                else
                    % CalibPath contains relative path (relative to BasePath)
                    Obj.CalibPath = fullfile(Obj.BasePath,Obj.CalibPath);
                    Result      = Obj.CalibPath;
                end
            end

        end

        function Result=get.FailedPath(Obj)
            % getter fore FailedPath

            if isempty(Obj.FailedPath)
                Result = [];
            else
                if strcmp(Obj.FailedPath(1),filesep)
                    % FailedPath contains full dir name
                    Result = Obj.FailedPath;
                else
                    % FailedPath contains relative path (relative to BasePath)
                    Obj.FailedPath = fullfile(Obj.BasePath,Obj.FailedPath);
                    Result      = Obj.FailedPath;
                end
            end

        end

        function Result=get.LogPath(Obj)
            % getter fore LogPath

            if isempty(Obj.LogPath)
                Result = [];
            else
                if strcmp(Obj.LogPath(1),filesep)
                    % FailedPath contains full dir name
                    Result = Obj.LogPath;
                else
                    % FailedPath contains relative path (relative to BasePath)
                    Obj.LogPath = fullfile(Obj.BasePath,Obj.LogPath);
                    Result      = Obj.LogPath;
                end
            end

        end

        function Result=get.SciPath(Obj)
            % getter fore SciPath

            if isempty(Obj.SciPath)
                Result = [];
            else
                if strcmp(Obj.SciPath(1),filesep)
                    % FailedPath contains full dir name
                    Result = Obj.SciPath;
                else
                    % FailedPath contains relative path (relative to BasePath)
                    Obj.SciPath = fullfile(Obj.BasePath,Obj.SciPath);
                    Result      = Obj.SciPath;
                end
            end

        end


        function set.DataDir(Obj, Val)
            % Set DataDir and modify BasePath
            
            Obj.DataDir = Val;
            Obj.BasePath = Obj.getPath;
            
        end
        
        function Result=get.RefPath(Obj)
            % getter for RefPath

            if isempty(Obj.RefPath)
                Obj.RefPath = Obj.populateRefPath;
            end
%                 [~,~,~,HostName] = Obj.getPath;
%                 Obj.RefPath = fullfile(filesep, HostName, 'data', 'reference');
%             end
            Result = Obj.RefPath;

        end
    end
      
    methods (Static) % path and files
           

        function Result = constructProjectName(Project,Node,Mount,Camera)
            % Construct project name of the form LAST.01.02.03
            % Input  : - Project name.
            %          - Node number
            %          - Mount number (integer or string).
            %          - Camera number
            % Output : - Projet name
            % Author : Eran Ofek (Mar 2023)

            if ~ischar(Mount)
                Mount = sprintf('%02d',Mount);
            end
            Result = sprintf('%s.%02d.%s.%02d',Project, Node, Mount, Camera);
        end

        function [MountNumberStr, MountNumber]=getMountNumber            
            % Get mount number from computer name
            % Output : - Mount number string.
            %          - Mount number. NaN if not xxxx## name.
            % Author : Eran Ofek (Mar 2023)

            HostName = tools.os.get_computer;
            if numel(HostName)==6
                MountNumberStr = HostName(5:6);
                if nargout>1
                    MountNumber = str2double(MountNumberStr);
                end
            else
                MountNumberStr = '';
                MountNumber    = NaN;
            end
        end

        function [DataNumber, DataDir]=camNumber2dataDir(CameraNumber)
            % Camera number to data dir
            % Input  : - Camera number (1 to 4)
            % Output : - Data dir number (1 or 2)
            %          - Data dir name (e.g., 'data1')
            % Author : Eran Ofek (Mar 2023)
            
            DataNumber = mod(CameraNumber-1,2)+1;
            if nargout>1
                DataDir = sprintf('data%d',DataNumber);
            end
        end

        function Side=camNumber2computerSide(CameraNumber)
            % Camera number to computer side
            % Input  : - camera number (1 to 4)
            % Output : - computer side ('e'|'w')
            % AUthor : Eran Ofek (Mar 2023)

            switch ceil(CameraNumber./2)
                case 1
                    Side = 'e';
                case 2
                    Side = 'w';
                otherwise
                    error('Unknown Computer side');
            end
        end

        function [CameraNumber,Side]=dataDir2cameraNumber(DataDirNum,HostName)
            % data dir number to camera number and computer side
            % Input  : - DataDir number (1 or 2)
            %          - Host name (e.g., 'last02w'). If empty, then will
            %            get from computer host name.
            %            Default is [].
            % Output : - Camera number (1 to 4)
            %          - Computer side ('e'|'w')
            % Author : Eran Ofek (Mar 2023)

            arguments
                DataDirNum
                HostName = [];
            end
            if isempty(HostName)
                HostName = tools.os.get_computer;
            end
            Side = HostName(7);

            switch Side
                case 'e'
                    CameraNumber = DataDirNum;
                case 'w'
                    CameraNumber = DataDirNum + 2;
                otherwise
                    error('Unknown Side')
            end

        end

        function [BasePath,CameraNumber,Side,HostName,ProjName,MountNumberStr]=getBasePath(Args)
            % get base path for LAST computers
            % Input  : * ...,key,val,...
            %            'DataDir' - DataDir number (1 or 2). If given than
            %                   superceed the CameraNumber. Default is [].
            %            'CamNumber' - Camera number (1 to 4).
            %                   Default is [].
            %            'HostName' - Computer host name. If empty, then
            %                   get from computer host name.
            %                   Default is [].
            %            'ProjectName' - e.g., 'LAST'. Default is ''.
            %            'Node' - e.g., 1. Default is [].
            % Output : - Base path - e.g., '/last02e/data1/archive/LAST.01.02.01'
            %          - Camera number
            %          - Computer side
            %          - Computer host name
            %          - Project name
            %          - Mount number string
            % Author : Eran Ofek (Mar 2023)

            arguments
                Args.DataDir     = [];
                Args.CamNumber   = [];
                Args.HostName    = [];
                Args.ProjectName = '';
                Args.Node        = [];
            end

            if isempty(Args.CamNumber)
                if isempty(Args.DataDir)
                    error('DataDir or CamNumber must be supplied');
                else
                    % only DataDir is given
                    DataDir = Args.DataDir;
                end
            else
                if isempty(Args.DataDir)
                    % only CamNumber is given
                    [DataDir] = pipeline.DemonLAST.camNumber2dataDir(Args.CamNumber);

                else
                    % both DataDir and CamNumber are given - use DataDir
                    DataDir = Args.DataDir;
                end
            end

            if isempty(Args.HostName)
                HostName = tools.os.get_computer;
            else
                HostName = Args.HostName;
            end

            %'need to remove this line when going to production - getBasePath'
            %HostName = 'last02w'

            if numel(HostName)<6
                HostName       = 'last01e';
                warning('HostName has less than 6 chars');
            end
            MountNumberStr = HostName(5:6);
            
         
            [CameraNumber,Side] = pipeline.DemonLAST.dataDir2cameraNumber(DataDir,HostName);
            ProjName            = pipeline.DemonLAST.constructProjectName(Args.ProjectName, Args.Node, MountNumberStr, CameraNumber);

            % e.g., '/last02e/data1/archive/LAST.01.02.01'

            DataStr = sprintf('data%d',DataDir);
            BasePath = fullfile(filesep,HostName,DataStr,'archive',ProjName);

        end

        function [Path, SubDir] = getArchivePath(FN, SubDir, Args)
            % get archive (proc/raw) images directory from FileNames object.
            % Input  : - A FileNames object containing images from which we
            %            want to find the proc images directory.
            %          - A SubDir indicating additional directory in the
            %            proc/ dir in which the data resides.
            %            If numeric empty (i.e., []) then automatically
            %            find the SubDir by incrimenting the largest
            %            existing SubDir by 1.
            %            If '' or chra array, then use it as a SubDir.
            %          * ...,Key,Val,...
            %            'BasePath' - Override the BasePath in the input
            %                   FileNames object. If empty, use currently
            %                   available BasePath in FileNames object.
            %                   Default is [].
            % Output : - Path for proc images, including SubDir.
            %          - SubDir char array.
            % Author : Eran Ofek (Apr 2023)

            arguments
                FN FileNames
                SubDir           = [];  % [] - auto find ; '' - SubDir is '' 
                Args.BasePath    = [];
                %Args.DataDir     = [];
                %Args.CamNumber   = [];
                %Args.HostName    = [];
                %Args.ProjectName = '';
                %Args.Node        = [];
            end

            if ~isempty(Args.BasePath)
                FN.BasePath = Args.BasePath;
            end
            %[BasePath,CameraNumber,Side,HostName,ProjName,MountNumberStr] = getBasePath('DataDir',Args.DataDir, 'CamNumber',Args.CamNumber, 'HostName',Args.HostName, 'ProjectName',Args.ProjectName, 'Node',Args.Node);

            PathProc = FN.genPath;

            if ischar(SubDir)
                % SubDir is provided by user
            else
                if isempty(SubDir)
                    SubDir = nextSubDir(FN);
                else
                    error('Unknown SubDir option');
                end
            end

            Path = fullfile(PathProc,SubDir);

        end
        
        
    
    end

    methods (Static)  % fields related utilities
        
        function List = fieldsListLAST(Args)
            % (Static) Return a table with list of LAST predefined field indices
            % Input  : * ...,key,val,...
            %            'N_LonLat' - Arguments for celestial.grid.tile_the_sky
            %                   Default is [85 28]
            % Output : - A table with the LAST predefined fields.
            % Author : Eran Ofek (Jul 2023)
            % Example: List = pipeline.DemonLAST.fieldsListLAST

            arguments
                Args.N_LonLat   = [88 30] %[85 28];
            end

            RAD = 180./pi;
            
            [TileList,TileArea] = celestial.grid.tile_the_sky(Args.N_LonLat(1), Args.N_LonLat(2));
            try
                Ebv      = astro.extinction.sky_ebv(TileList(:,1),TileList(:,2));
            catch
                Ebv      = nan(size(TileList,1),1);
            end
            
            TileList = TileList.*RAD;
            
            List = table(TileList(:,1), TileList(:,2), TileList(:,3), TileList(:,4), TileList(:,5), TileList(:,6), Ebv, TileArea);
            List.Properties.VariableNames = {'RA','Dec','MinRA','MaxRA','MinDec','MaxDec','Ebv','Area'};
            List.Properties.VariableUnits = {'deg','deg','deg','deg','deg','deg','mag','sr'};

        end

        function Str = radec2str(RA,Dec, InUnits)
            % Convert RA/Dec to RA/Dec degree accuracy string (e.g., 045-01)
            % Input  : - RA
            %          - Dec
            %          - Input units. Default is 'deg'.
            % Output : - String of decimal degrees- e.g., 351-12
            % Author : Eran Ofek (Jul 2023)
            % Example: Str = pipeline.DemonLAST.radec2str([101.8 0.8],[+12.1;-1.1])
           
            arguments
                RA
                Dec
                InUnits   = 'deg';
            end
            
            Conv = convert.angular(InUnits, 'deg');
            RA   = RA.*Conv;
            Dec  = Dec.*Conv;
            
            RA   = mod(RA, 360);
            
            RA   = round(RA);
            Dec  = round(Dec);
            
            Ncoo = numel(RA);
            Str  = cell(Ncoo,1);
            for Icoo=1:1:Ncoo
                Str{Icoo} = sprintf('%03d%+03d',RA(Icoo), Dec(Icoo));
            end
            
        end
        
        function [RA, Dec]=str2radec(Str, OutUnits)
            % Convert RA/Dec decimal string (e.g., '351-09') to RA/Dec.
            % Input  : - String
            %          - Output units. Default is 'deg'
            % Output : - RA
            %          - Dec
            % Author : Eran Ofek (Jul 2023)
            % Example: [RA, Dec]=pipeline.DemonLAST.str2radec({'351-09','009-01'})
            
            arguments
                Str
                OutUnits = 'deg';
            end
            
            if ischar(Str)
                Str = {Str};
            end
            Nstr = numel(Str);
            RA   = zeros(Nstr,1);
            Dec  = zeros(Nstr,1);
            for Istr=1:1:Nstr
                RA(Istr)  = str2double(Str{Istr}(1:3));
                Dec(Istr) = str2double(Str{Istr}(4:6));
            end
            
            Conv = convert.angular('deg',OutUnits);
            RA   = RA.*Conv;
            Dec  = Dec.*Conv;
            
        end
        
        function [ID, MinDist, List] = searchFieldLAST(List, RA, Dec)
            % Search coordinates in fields list
            % Input  : - List of fields generated by:
            %            pipeline.DemonLAST.fieldsListLAST
            %            If empty, will generate the default list.
            %            Default is [].
            %          - RA (deg, sexagesimal string, or RA+Dec string
            %            (e.g., '045-12')
            %          - Dec. If empty, then RA is in RA+Dec string format.
            %            Default is empty.
            % Output : - ID of fields.
            %          - Min dist to field center (radians).
            %          - The table of fields.
            % Author : Eran Ofek (Jul 2023)
            % Example: [ID,M]=pipeline.DemonLAST.searchFieldLAST([],1,1)
            %          [ID,M]=pipeline.DemonLAST.searchFieldLAST([],'045-01')
            %          [ID,M]=pipeline.DemonLAST.searchFieldLAST([],'05:12:01','-01:19:10')
            
            
            arguments
                List
                RA
                Dec    = [];
            end
            RAD = 180./pi;
            
            if isempty(List)
                List = pipeline.DemonLAST.fieldsListLAST;
            end

            if ~isempty(Dec) && isnumeric(Dec)
                % assume RA and Dec are in deg, convert to radian
                RA  = RA./RAD;
                Dec = Dec./RAD;
            end            
            if isempty(Dec)
                % assume RA is string of the format '045-01'
                [RA, Dec]=pipeline.DemonLAST.str2radec(RA, 'rad');
            end
            if iscell(Dec) || ischar(Dec)
                % assume RA and Dec are in sexagesinal
                RA  = celestial.coo.convertdms(RA, 'SH', 'r');
                Dec = celestial.coo.convertdms(Dec, 'SD', 'R');
            end

            % RA/Dec are in radians
            CatRA  = List.RA./RAD;
            CatDec = List.Dec./RAD;
            Nra = numel(RA);
            MinDist = zeros(Nra,1);
            ID      = zeros(Nra,1);
            
            for Ira=1:1:Nra
                Dist = celestial.coo.sphere_dist_fast(RA(Ira), Dec(Ira), CatRA, CatDec);
                [MinDist(Ira), ID(Ira)] = min(Dist);
            end

        end

        function [CamNum,Mount,Node] = getCamNumFromProjName(ProjName,Convert2Num)
            % get camera number from project name
            % Input  : - Project name - e.g., 'LAST.01.02.03'
            %          - Convert to number. Default is true.
            % Output : - Camera number (e.g., '03')
            %          - Mount
            %          - Node
            % Author : Eran Ofek (Oct 2023)
            % Example: [C,M,N]=pipeline.DemonLAST.getCamNumFromProjName('LAST.01.02.03');
            
            arguments
                ProjName
                Convert2Num logical  = true;
            end
            
            Node   = ProjName(6:7);
            Mount  = ProjName(9:10);
            CamNum = ProjName(12:13);
            
            if Convert2Num
                Node   = str2double(Node);
                Mount  = str2double(Mount);
                CamNum = str2double(CamNum);
            end
            
        end        
    
        function NewVal = PrepSaveProductArg(Val)
            % Convert SaveEpochProduct/SaveVisitProduct char to cell array
            %   Given the value of the aveEpochProduct/SaveVisitProduc if
            %   it is char ('all'|'cat') then convert it to cell array of
            %   data products to save.
            % Input  : - Argument value.
            % Output : - Updated argument value.
            % Author : Eran Ofek (Dec 2023)

            if ischar(Val)
                switch Val
                    case 'all'
                        NewVal  = {'Image','Mask','Cat','PSF'};
                    case 'cat'
                        NewVal  = {[],[],'Cat',[]};
                    otherwise
                        error('Unknown Val option');
                end
            else
                NewVal = Val;
            end
        end
    
        function prep_zSpecialInstruction(File, Mount, Camera, OutFile, Args)
            % prep a zSpecialInst.txt file from a list
            %   Given a file of format: fieldName, mount, cam, Nimages, StartJD, EndJD
            %   prep a zSpecial.txt file
            % Input  : - Input File name
            %          - Mount
            %          - Camera
            %          - Output file name.
            %          * ...,key,val,...
            %            See code.
            % Output : - A zSpecialInst.txt file
            % Author : Eran Ofek (Jan 2024)
            % Example: pipeline.DemonLAST.prep_zSpecialInstruction('flares_m5',5,[1 2],'zSpecial.txt_05')

            arguments
                File
                
                Mount
                Camera
                OutFile
                Args.ColName   = 'SNName'
                Args.ColMount  = 'Mount';
                Args.ColCamera = 'Camera';
                Args.ColStart  = 'StartJD';
                Args.ColEnd    = 'EndJD';
                Args.Parameter = 'SaveEpochProduct'
                Args.AddName logical = false;
            end
            
            T = readtable(File);

            T = sortrows(T, 'StartJD');
        
            Flag = T.(Args.ColMount) == Mount & any(T.(Args.ColCamera) == Camera, 2);
            T    = T(Flag,:);
            [~,Iu]    = unique(T.StartJD);
            T         = T(Iu,:);

            % remove overlapping times
          %  FlagDup = T.StartJD(2:end) < T.EndJD(1:end-1);
          %  T       = T(~FlagDup,:);

            FID = fopen(OutFile,'w');
            Nt = size(T,1);
            for It=1:1:Nt
                Name = T.(Args.ColName){It};
                Start = T.(Args.ColStart)(It) - 20./86400;
                End   = T.(Args.ColEnd)(It) + 20./86400;

                DateStart = celestial.time.jd2date(Start,'H');
                DateEnd   = celestial.time.jd2date(End,'H');
            
                if Args.AddName
                    fprintf(FID,'%02d %02d %04d %02d %02d %04.1f  %s %s  %% %s\n', DateStart, Args.Parameter, 'all', Name);
                    fprintf(FID,'%02d %02d %04d %02d %02d %04.1f  %s %s  %% %s\n', DateEnd,   Args.Parameter, 'cat', Name);
                else
                    fprintf(FID,'%02d %02d %04d %02d %02d %04.1f  %s %s  \n', DateStart, Args.Parameter, 'all');
                    fprintf(FID,'%02d %02d %04d %02d %02d %04.1f  %s %s  \n', DateEnd,   Args.Parameter, 'cat');
                end
            end
            fclose(FID);
        end
        
    end

    
    methods % utilities
        function Obj=autoDetectPath(Obj, Type)
            % Auto detect path for various directories
            % Input  : - self.
            %          - One of the following options:
            %            'LAST' - Path on some LAST computer (in site).
            %            'test/data' - Path for local AstroPack data dir.
            % Output : - Updated object.
            % Author : Eran Ofek (Jan 2026)

            arguments
                Obj
                Type  = 'LAST';
            end

            switch lower(Type)
                case 'last'

                    if ~isempty(Obj.BasePath)
                        Obj.NewPath    = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefNewPath);
                        Obj.CalibPath  = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefCalibPath);
                        Obj.FailedPath = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefFailedPath);
                        Obj.LogPath    = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefLogPath);
                    
                        Obj.RefPath    = sprintf('%s%s%s',tools.os.get_computer, filesep, Obj.DefRefPath);
                    end
                case 'marvin'
                    % Obj.BasePath = '/marvin';
                    % 
                    % Obj.NewPath    = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefNewPath);
                    % Obj.CalibPath  = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefCalibPath);
                    % Obj.FailedPath = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefFailedPath);
                    % Obj.LogPath    = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefLogPath);
                    % 
                    % Obj.RefPath    = '/lastdata/references/v4';

                case 'test/data'
                    Obj.BasePath = sprintf('%s/matlab/data/pipeline/LAST',tools.os.get_userhome);
                    
                    Obj.NewPath    = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefNewPath);
                    Obj.CalibPath  = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefCalibPath);
                    Obj.FailedPath = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefFailedPath);
                    Obj.LogPath    = sprintf('%s%s%s',Obj.BasePath, filesep, Obj.DefLogPath);
                    
                    Obj.RefPath    = '/lastdata/references/v4';


                otherwise
                    error('Unknown Type option');
            end

        end

        function Result=getBasePathWithOutProjName(Obj)
            % Get the BasePath without the last directory
            % Input  : - self
            % Output : - BasePath from which the last dir is removed.
            % Author : Eran Ofek (Jun 2023)
            
            Ind = strfind(Obj.BasePath, filesep);
            if isempty(Ind)
                Result = Obj.BasePath;
            else
                Result = Obj.BasePath(1:Ind(end)-1);
            end

        end

        function [Path,CameraNumber,Side,HostName,ProjName,MountNumberStr]=getPath(Obj, SubDir, Args)
            % get base path, computer, data, camera,...
            % Input  : - A pipeline.DemonLAST object
            %          - Sub directory to concat to base path
            %            (e.g., 'new'). Default is ''.
            %          * ...,key,val,...
            %            'DataDir' - A data dir number. If given will
            %                   overwrite the pipeline.DemonLAST property.
            %                   If empty, will use the pipeline.DemonLAST property.
            %                   Superceed the CamNumber.
            %                   Default is [].
            %            'CamNumber' - A camera number. If given will
            %                   overwrite the pipeline.DemonLAST property.
            %                   If empty, will use the pipeline.DemonLAST property.
            %                   Default is [].
            %            'HostName' - set the host name by hand and not 
            %                       from the actual machine name (for pipeline runs outside the LAST nodes)
            % Output : - Path
            %          - Camera number
            %          - Computer side
            %          - Computer host name
            %          - Project name
            %          - Mount number string
            % Author : Eran Ofek (Mar 2023)
            arguments
                Obj
                SubDir          = '';
                Args.DataDir    = [];
                Args.CamNumber  = [];
                Args.HostName   = [];                
            end

            if ~isempty(Args.DataDir)
                Obj.DataDir = Args.DataDir;
            end
            if ~isempty(Args.CamNumber)
                Obj.CamNumber = Args.CamNumber;
            end

            [BasePath,CameraNumber,Side,HostName,ProjName,MountNumberStr] = pipeline.DemonLAST.getBasePath('DataDir',Obj.DataDir,...
                                        'CamNumber',Obj.CamNumber,...
                                        'Node',Obj.Node,...
                                        'ProjectName',Obj.ProjectName,...
                                         'HostName',Args.HostName);
            Path = fullfile(BasePath,SubDir);
        end

        function [FN, Flag]=deleteDayTimeImages(Obj, Args)
            % Delete science images taken when the Sun is above the horizon
            % Input  : - A pipeline.DemonLAST object.
            %          * ...,key,val,...
            %            'TempFileName' - File name template to select.
            %                   Default is '*.fits'.
            %            'Type' - Type of images to delete.
            %                   Default is {'sci','science'}.
            %            'SunAlt' - Sun altitude threshold above to delete
            %                   the images. Default is 0.
            %            'Delete' - Logical indicating if to delete the
            %                   images. Default is true.
            % Output : - FileNames object of all found images
            %          - Logical indicating, for each image, if Alt>0 
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Obj
                Args.TempFileName = '*.fits';
                Args.Type         = 'sci'; %{'sci','science'};
                Args.SunAlt       = 0;
                Args.Delete logical = true;
                
            end
            
            PWD = pwd;
            cd(Obj.NewPath);
            
            FN = AstroFileName(Args.TempFileName);
            FN.selectByPropVal('Type',Args.Type);
            
            [SunAlt] = FN.sunAlt('GeoPos',Obj.ObsCoo(1:2));
            Flag     = SunAlt>Args.SunAlt;
            
            FN.reorderEntries(Flag);
            if Args.Delete
                io.files.delete_cell(FN.genFile());
            end
            
            cd(PWD);
        end
        
       
        function Obj=setPath(Obj, BasePath, Args)
            % set the BasePath and other paths of pipeline.DemonLAST object
            % Input  : - A pipeline.DemonLAST object.
            %          - BasePath.
            %          * ...,key,val,...
            %            'NewPath' - If empty, do not set NewPath.
            %                   Default is 'new'.
            %            'CalibPath' - If empty, do not set CalibPath.
            %                   Default is 'calib'.
            %            'FailedPath' - If empty, do not set FailedPath.
            %                   Default is 'failed'.
            % Output : - A pipeline.DemonLAST object in which the paths are
            %            updated.
            % Author : Eran Ofek (Apr 2023)

            arguments
                Obj
                BasePath
                Args.NewPath    = 'new';
                Args.CalibPath  = 'calib';
                Args.FailedPath = 'failed';
            end

            FIELDS = {'NewPath','CalibPath','FailedPath'};
            Nf     = numel(FIELDS);

            if ~isempty(BasePath)
                Obj.BasePath = BasePath;
            end
            for If=1:1:Nf
                if ~isempty(Args.(FIELDS{If}))
                    Obj.(FIELDS{If}) = Args.(FIELDS{If});
                end
            end

        end

    end
    
    methods % status file, lock file, special files

        function writeStatus(Obj, Path, Args)
            % Write ready-to-transfer in status file
            % Input  : - A pipeline.DemonLAST object
            %          - Path in which to write the file
            %          * ...,key,val,...
            %            'FileName' - Default is '.status'.
            %            'Msg' - Default is 'ready-to-transfer'.
            % Output : null
            % Author : Eran Ofek (Apr 2023)

            arguments
                Obj
                Path
                Args.FileName = '.status';
                Args.Msg      = 'ready-for-transfer';
            end

            FileName = fullfile(Path,Args.FileName);
            FID = fopen(FileName,'a+');
            fprintf(FID,'%s %s\n',datestr(now,'yyyy-mm-ddTHH:MM:SS'),Args.Msg);
            fclose(FID);

        end


        function ResultOK = lockFile(Obj, Args)
            % Create, check and manage lock file for pipelines
            % Author : Eran Ofek (Mar 2024)

            arguments
                Obj
                Args.LockDir   = '/var/run/1001';
                Args.LockFileBase = 'pipeline';
                
            end

            ResultOK = true;

            LockFile = sprintf('%s%s%s%d', Args.LockDir, filesep, Args.LockFileBase, Obj.DataDir);
            Pid      = tools.os.getPid;
            if isfile(LockFile)
                % lock file exist
                % read Pid from lock file
                FID = fopen(LockFile);
                LockPid = fscanf(FID,'%d');
                fclose(FID);

                % Check if LockPid exist in system
                [~,OutStr] = system(sprintf('ps %d',LockPid));
                if contains(OutStr,'matlab')
                    % Pid exist
                    ResultOK = false;

                    Obj.setLogFile('HostName',Args.HostName);
                    Msg = sprintf('Lock File exist: %s - clear and try again',LockFile);
                    fprintf(Msg);
                    Obj.writeLog(Msg);

                else
                    % Pid doesnt exist - delete LockFile
                    delete(LockFile);

                    % create new lock file
                    FID = fopen(LockFile, 'w');
                    fprintf(FID,'%d',Pid);
                    fclose(FID);
                end

            else
                % no lock file - create
                FID = fopen(LockFile, 'w');
                fprintf(FID,'%d',Pid);
                fclose(FID);
            end


        end

        function writeLog(Obj, Msg, Level, Args)
            % write a log message to screen and log file
            % Input  : - A pipeline.DemonLAST object.
            %          - One of the following:
            %            Char array containing message to print/log.
            %            A cell array of messages.
            %            As truct array with messages in the .Msg field.
            %            An MException object.
            %            Empty (do nothing).
            %          - A LogLevel object with the specified message
            %            level. See LogLeve.<tab> for options.
            %            Default is LogLevel.Info
            %          * ...,key,val,...
            %            'WriteLog' - write log file. Default is true.
            %            'WriteDev' - write to screen. Default is true.
            %            'ConcatenateImageEmpty' - instead of N "image is empty" lines put out just a summary
            % Output : null
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Obj
                Msg
                Level LogLevel           = LogLevel.Info; % All       Assert    Debug     DebugEx   Error     Fatal     Info      None      Perf      Test      unitTest  Verbose   Warnin
                Args.WriteLog logical    = true;
                Args.WriteDev logical    = false;
                Args.ConcatenateImageEmpty logical = true;
            end

            if ~isempty(Msg)
                if ischar(Msg)
                    Lines{1} = Msg;
                elseif isstruct(Msg)
                    Lines = squeeze(struct2cell(Msg));
                elseif isa(Msg, 'MException')
                    Nst      = numel(Msg.stack);
                    Lines    = cell(1+Nst,1);
                    Lines{1} = sprintf('Exception: id=%s msg=%s',Msg.identifier, Msg.message);
                    
                    for Ist=1:1:Nst
                        Lines{Ist+1} = sprintf('stack: Ind=%d; FunName=%s; line=%d',Ist, Msg.stack(Ist).name, Msg.stack(Ist).line);
                    end
                elseif iscell(Msg)
                    % do nothing - already in cell format
                    Lines = Msg;
                else
                    error('Unknown Msg option');
                end
                
                %instead of N "image is empty" lines put out just a summary
                NumEmpty = sum(contains(Lines, "image is empty"));
                if NumEmpty > 0 && Args.ConcatenateImageEmpty
                    Lines = {sprintf('%d empty images were not written', NumEmpty)};
                end
    
                Nl = numel(Lines);
                for Il=1:1:Nl
                    Lines{Il} = {[Obj.HostName ': ' Lines{Il}]};
                    if Args.WriteDev
                        fprintf('%s\n', Lines{Il});
                    end
                    if Args.WriteLog
                        Obj.Logger.msgLog(Level, Lines{Il});
                    end
                end
            end
        end

        function Obj=setLogFile(Obj, Args)
            % Set the log file name according to current date
            % Input  : - A pipeline.DemonLAST object.
            %          * ...,key,val,...
            %            'ProjName' - Project name. If empty, read from
            %                   system. Default is [].
            %            'HostName' - Host name. If empty, get from OS
            % Output : - A pipeline.DemonLAST object in which the Logger
            %            property is updated with the log file name.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Args.ProjName = [];
                Args.HostName = []; 
            end

            if isempty(Args.ProjName)
            end

            [~,~,~,~,ProjName,~]=getPath(Obj,'HostName',Args.HostName);

            FN = FileNames;
            FN.ProjName = ProjName;
            FN.Time     = celestial.time.julday;
            LogFileName = FN.genFile('IsLog',true, 'ReturnChar',true, 'Product','Pipeline');
            Path        = Obj.LogPath;
            mkdir(Path);
            LogFileName = fullfile(Path, LogFileName);

            Obj.Logger.LogF.FileName = LogFileName;

        end
        
        function SpecialArgs=specialInstruction(Obj, JD, InArgs, Args)
            % Check for pipeline special instructions and modify arguments using new instructions
            %   If the 'zSpecialInst.txt' file exist in the new/ dir, then
            %   will be read. This file contains lines of:
            %   D M Y HH MM SS Key Val, where Key is a main pipeline
            %   argument name, and Val is its value.
            %   E.g., 1 1 2023 10 0 0   SaveEpochProduct all
            %   allways a last line of Inf JD is added.
            %   The code checks if there are any special instructions for
            %   the time range containing the JD input argument.
            % Input  : - A pipeline.DemonLAST object
            %          - Date [D M Y H M S] or JD in which to check if
            %            there are any special instructions.
            %          - A structure array of input argumnets passed to the
            %            pipeline.
            %          * ...,key,val,...
            %            See code for options
            % Output : - Updated structure array of input arguments.
            % Author : Eran Ofek (Dec 2023)
            % Example: SA=D.specialInstruction([2 1 2023], Args)

            arguments
                Obj
                JD
                InArgs
                Args.InstructionFileName = 'zSpecialInst.txt';  % [D M Y HH MM SS  | SaveEpochProduct all/cat
                Args.InstTable           = [];
            end

            if numel(JD)>1
                JD = celestial.time.julday(JD);
            end

            if isempty(Args.InstTable)
                InstFile = fullfile(Obj.NewPath, Args.InstructionFileName);
                if isfile(InstFile)
                    T        = readtable(InstFile);
                else
                    T        = [];
                end
            else
                T        = Args.InstTable;
            end

            SpecialArgs = InArgs;
            if isempty(T)
                % do nothing
            else
                TableJD  = celestial.time.julday([T.Var1 T.Var2 T.Var3 T.Var4 T.Var5 T.Var6]);
                TableJD  = [TableJD; Inf];
                Flag     = JD>TableJD(1:end-1) & JD<TableJD(2:end);
    
                if any(Flag)
                    if sum(Flag)>1
                        error('InstFile=%s contains conflicting dates',InstFile);
                    end
                    Ncol = size(T,2);
                    
                    I = 0;
                    for Icol=7:2:Ncol
                        I = I + 1;
                        Tmp1 = table2array(T(Flag,Icol));
                        Tmp2 = table2array(T(Flag,Icol+1));
                        I = I + 1;
                        SpecialArgs.(Tmp1{1}) = Tmp2{1};
                    end
                else
                    % do nothing
                end
            end

        end
    end

    methods % cleanup utilities
        function moveRaw2New_AndDeleteProc(Obj, Args)
            % Move raw images back to new/ dir and delete the proc/ dir
            % Input  : - see code for options
            % Output : null
            % Author : Eran Ofek (Feb 2024)
            % Example: D=pipeline.DemonLAST;
            %          D.DataDir = 1 % 2
            %          D.moveRaw2New_AndDeleteProc

            arguments
                Obj
                Args.YearList  = {'2023','2024'};
                
                Args.StartJD       = -Inf;           % refers only to Science observations: JD, or [D M Y]
                Args.EndJD         = Inf; 

                Args.DeleteFocus logical   = true;
                Args.DeleteProc logical    = true;
                Args.DeleteRawDir logical  = true;
            end

            cd(Obj.BasePath);
            
            if numel(Args.StartJD)>1
                Args.StartJD = celestial.time.julday(Args.StartJD);
            end
            if numel(Args.EndJD)>1
                Args.EndJD = celestial.time.julday(Args.EndJD);
            end


            Nyear = numel(Args.YearList);
            for Iy=1:1:Nyear
                cd(Args.YearList{Iy});
                
                DirMonth = io.files.dirDir;
                Nm       = numel(DirMonth);
                for Im=1:1:Nm
                    cd(DirMonth(Im).name);

                    DirDay   = io.files.dirDir;
                    Nd       = numel(DirDay);

                    for Id=1:1:Nd
                        cd(DirDay(Id).name);

                        %Simone: loop to check daterange and unpack .fz
                        %files before moving
                        if ~isempty(Args.StartJD)
                            % Check if date is in allowed range (in
                            % DateRange)
                            FN_JD  = celestial.time.julday([str2num(DirDay(Id).name) str2num(DirMonth(Im).name) str2num(Args.YearList{Iy})]);
                            
                            FlagJD = FN_JD>Args.StartJD & FN_JD<Args.EndJD;
                            %disp([FlagJD Args.StartJD Args.EndJD FN_JD])
                            if FlagJD
      
                                Execute = true; 
                            else
                                Execute = false;
                            end
                        else
                            Execute = false;
                        end

                        if isfolder('raw') && Execute
                            cd ('raw');
                            
                            if Args.DeleteFocus
                                delete('LAST*focus*.fits');
                            end
                            
                            % funpack images...
                            CompFiles = dir('*.fz');
                            if ~isempty(CompFiles)
                                for k = 1:length(CompFiles)
                                    baseFileName = CompFiles(k).name;

                                    system(append('funpack ',baseFileName))

                                end
                            end

                            % move raw to new
                            !mv LAST*.fits ../../../../new/.

                            %List=io.files.filelist('LAST*.fits', 'UseRegExp',false, 'AddPath',false);
                            %io.files.moveFiles(List,[],'',Obj.NewPath);

                            cd ..
                            % delete raw dir
                            if Args.DeleteRawDir
                                %rmdir('raw');
                                !rm -rf raw/
                            end
                        end

                        if Args.DeleteProc
                            if isfolder('proc')
                                %rmdir('proc','s');
                                !rm -rf proc/
                            end
                        end
                        cd ..
                    end
                    cd ..
                end
                cd ..
            end
        end
        
        function removeUnderlineFromFileName(Obj, Args)
            % Remove _ from file name and fix header
            % Input  : - A pipeline.DemonLAST object
            %          * ...,key,val,...
            %            See code.
            % Output : null
            % Author : Eran Ofek (Mar 2024)
            
            arguments
                Obj
                Args.BasePath          = [];
                Args.Nsplit            = 10;
                Args.UnderLine         = '_';
                Args.Modify logical    = false;
            end
            
            PWD = pwd;
            
            cd(Obj.NewPath);
            
            List = io.files.filelist('LAST*.fits');
            List = io.files.removeFilePath(List);
            
            Nlist = numel(List);
            Count = 0;
            for Ilist=1:1:Nlist
                Tmp = split(List{Ilist}, Args.UnderLine);
                if (numel(Tmp)-1)~=Args.Nsplit
                    % file name problem
                    if (numel(Tmp)-1)==(Args.Nsplit+1)
                        
                        Count = Count + 1;
                        if Args.Modify
                            FieldID = sprintf('%s%s', Tmp{4}, Tmp{5});
                            TmpNew  = [Tmp(1:3); FieldID; Tmp(6:end)];

                            NewFileName = join(TmpNew, Args.UnderLine);
                            OldFileName = List{Ilist};

                            % delete OBJECT key
                            FITS.delete_keys(OldFileName,'OBJECT');
                            % add OBJECT key
                            FITS.write_keys(OldFileName, {'OBJECT',FieldID,''});

                            % delete FILENAME
                            try
                                FITS.delete_keys(OldFileName,'FILENAME');
                                % add FILENAME key
                                FITS.write_keys(OldFileName, {'FILENAME',NewFileName,''});
                            end
                            % move file
                            io.files.moveFiles(OldFileName, NewFileName);
                        end
                        
                    else
                        error('Unknown problem with file name %s',List{Ilist});
                    end
            
                end
            end
            
            cd(PWD);
            
        end
        
    end
    
    methods (Static)  % find all files
        function Result=findAllVisitsDir(Args)
            % Get all proc visits directories under some base path
            % Input  : * ...,key,val,...
            %            'BasePath' - Default is '/marvin/LAST.01.01.01'
            %            'YearPat' - Year pattern to scan. Default is '20*'.
            %            'MinNfile' - Min. number of images in visit.
            %                   Default is 10.
            % Output : - A strings array of dir names.
            % Author : Eran Ofek (Jun 2025)
            % Example: S=pipeline.DemonLAST.findAllVisitsDir;

            arguments
                Args.BasePath             = '/marvin/LAST.01.01.01';
                Args.YearPat              = '20*';
                Args.MinNfile             = 10;
            end

            D = pipeline.DemonLAST;
            D.BasePath = Args.BasePath;
            
            [St] = D.findAllVisits('YearPat',Args.YearPat, 'MinNfile',Args.MinNfile, 'ReadHeader',false);
            
            Nst = numel(St);
            Result = strings(1,Nst);
            for Ist=1:1:Nst
                Result{Ist} = fullfile(St(Ist).BasePath, St(Ist).Year, St(Ist).Month, St(Ist).Day, 'proc', St(Ist).Visit);
            end

        end
    end

    
    methods % ref image utilities
         function Path=populateRefPath(Obj, Args)
            % Get path for reference imags location on the LAST computers
            % Input  : - A pipeline.DemonLAST object
            %          * ...,key,val,...
            %            'HostName' - HostName. If empty, get it from OS.
            %                   Default is [].
            %            'DefRefPath' - Reference path above the host name.
            %                   Default is 'data/references'.
            % Output : - Base path for reference images dir.
            % Author : Eran Ofek (Oct 2023)
            % Example: Path=D.populateRefPath
            
            arguments
                Obj
                Args.HostName     = [];
                Args.DefRefPath   = 'data/references';
            end
            
            if isempty(Args.HostName)
                HostName = tools.os.get_computer;
            else
                HostName = Args.HostName;
            end
            
            Path = fullfile(filesep, HostName, Args.DefRefPath);
        end

        function [Path, File, AI] = getRefImage(Obj, Args)
            % Get reference images corresponding to some field
            % Input  : * ...,key,val,...
            %            'FN' - Either [], a file name char array,
            %                   a cell array of char arrays, or a FileNames
            %                   object.
            %                   If empty, then the image details are provided
            %                   in the other argumnets.
            %                   If given, then the other argumnets are
            %                   overwritten.
            %                   Default is [].
            %            'Camera' - Camera index. Default is 1.
            %            'Filter' - Filter name. Default is 'clear'.
            %            'CropID' - CropID. If empty, then will attemt to
            %                   read all possible crop IDs.
            %                   default is [].
            %            'Version' - Ref. version. If Inf, then will
            %                   attempt to read the latest available
            %                   version.
            %                   Default is Inf.
            %            'AddProduct' - Add the following products to the
            %                   AstroImage, in addition to the 'Image' product.
            %                   Default is {'Mask','Cat','PSF'}
            %            'RefPath' - RefPath (to overwrite the object
            %                   Refpath). If empty, then will use the
            %                   object RefPath.
            %                   Default is [].
            % Output : - Path for the reference images.
            %          - File names of the refernce images.
            %          - An AstroImage object with all the ref images.
            % Author : Eran Ofek & Nora (Oct 2023)
            % Example:
            %
            % FN=FileNames.generateFromFileName({'LAST.01.01.01_20220826.182338.681_clear_297+41_20_001_021_sci_proc_Image_001.fits'});
            % D=pipeline.DemonLAST;
            % [Path,File,AI]=D.getRefImage('FN',FN)
            % or
            % [Path,File,AI]=D.getRefImage('Camera',1,'CropID',10,'FieldID',1220)
            
            arguments
                
                Obj
                Args.FN           = []; % FileNames object
                Args.Camera       = 1;
                Args.Filter       = 'clear';
                Args.FieldID      = []; % ID or RA+Dec str
                Args.CropID       = [];
                Args.Version      = Inf;
                
                Args.AddProduct   = {'Mask','Cat','PSF'};  
                Args.RefPath      = [];
            end
            
            
            % FN is provided - overwrite other argumnets
            if ~isempty(Args.FN)
                if ~isa(Args.FN, 'FileNames')
                    Args.FN = FileNames.generateFromFileName(Args.FN);
                end
                

                Args.Filter   = Args.FN.Filter;
                Args.FieldID  = Args.FN.FieldID;
                Args.CropID   = Args.FN.CropID;
                Args.Version  = Args.FN.Version;
                
                Args.Camera   = pipeline.DemonLAST.getCamNumFromProjName(Args.FN.ProjName{1});  % return a number
                
            end
            
            if isempty(Args.RefPath)
                Args.RefPath = Obj.RefPath;
            end
            
            
            if ~isnumeric(str2double(Args.FieldID))
                % FieldID is likely in RA+Dec str - convert to FieldID
                % index
                [Args.FieldID, ~, ~] = pipeline.DemonLAST.searchFieldLAST(Obj.FieldList, Args.FieldID);
                
            elseif ~isnumeric(Args.FieldID)
                Args.FieldID = str2double(Args.FieldID);
            end
                               
            Path = fullfile(Args.RefPath, string(Args.FieldID), sprintf('%d',Args.Camera));
            
            if nargout>1
                % construct FileName using a FileNames object
                % treat Version=Inf as the last available version
                %RefFN = copy(Args.FN);
                RefFN = FileNames;
                RefFN.ProjName = {'LAST.01*'};
                RefFN.Time = {'*'};
                RefFN.FieldID = {string(Args.FieldID)};
                RefFN.Counter = 1;
                RefFN.Level = 'coadd';
                RefFN.FullPath = Path;
                RefFN.CCDID = 1;
                RefFN.CropID = Args.CropID;
                
                
                % check whether reference image exists
                AbsFile = fullfile(Path, RefFN.genFile);
                RefName = dir(AbsFile);
                
                if isempty(RefName)
                    File = [];
                    AI = [];
                    fprintf('No reference image found.')
                    return;
                elseif length(RefName)>1
                    fprintf('Found several reference images. Using the first one.')
                    File = RefName.name;
                else
                    % do nothing
                    File = RefName.name;
                end
                
                if nargout>2
                    % read all files into an AstroImage object
                    FullRefName = char(fullfile(Path, File));
                    RefFN = FileNames.generateFromFileName(FullRefName);
                    RefFN.FullPath = Path;

                    AI = AstroImage.readFileNamesObj(RefFN,'AddProduct',Args.AddProduct);
                    
                    
                end
            end
            
        end     
        
    end


    methods % preperations
        function DB = connectDB(Obj, Args)
            % Prepare and connect to local DB
            % Input  : - self.
            %          - demon ARgs structure
            % Output : - db.Db object
            % Author : Eran Ofek (Sep 2025)
            
            try
                if Args.Insert2DB

                    Configuration.getSingleton().loadFile(Args.AstroDBPassFile); % tell the PM where to look for passwords
                    PM = PasswordsManager;    
    
                
                    if isempty(Args.DB)

                        DB          = db.Db;
                        DB.Host     = Args.DBHost;
                        DB.DbName   = Args.DbName;
                        DB.User     = Args.DbUser;
                        DB.Password = PM.search(Args.DbName).Pass;
                        DB.connect;
                    else
                        DB = Args.DB;
                        
                    end
                

                    if DB.isConnected
                        MsgLog = 'DB connected sucessfully';
                        LogL = LogLevel.Info;
                    else
                        MsgLog = 'DB connection failed';
                        LogL = LogLevel.Error;
                        DB = [];
                    end
                else
                    MsgLog = 'DB InsertTransients2DB is false';
                    LogL = LogLevel.Info;
                    DB = [];
                end
            catch ME_DB
                Obj.writeLog(ME_DB, LogLevel.Error);
                DB = [];
            end
          
        end


        function [Args] = prepSolarSystemEphem(Obj, Args)
            % Populate GeoPos, OrbEl, INPOP arguments
            % Input  : - self
            %          - The demon Args structure
            % Output : - The updated demon Args structure
            % Author : Eran Ofek (Jun 2025)

            RAD = 180./pi;

            if isempty(Args.GeoPos)
                ObsCooSt = celestial.earth.observatoryCoo('Name','LAST');
                Args.GeoPos = [ObsCooSt.Lon./RAD, ObsCooSt.Lat./RAD, ObsCooSt.Height];
            end
            if isempty(Args.OrbEl)
                Args.OrbEl  = celestial.OrbitalEl.loadSolarSystem('merge');
            end
            if isempty(Args.INPOP)
                Args.INPOP = celestial.INPOP;
                Args.INPOP.populateTables('all', 'MaxOrder',5);
                Args.INPOP.populateTables({'Ear','Sun'}, 'FileData','vel', 'MaxOrder',5);
            end
        end
    
        function Args = prepPath(Obj, Args)
            % Path preparations
            % Input  : - self
            %          - Args structure.
            % Output : - Updated Args structure
            % Author : Eran Ofek (Sep 2025)
        
            switch Args.ReductionMode
                case 'last'
                    % do nothing
                case 'local'
                    if isempty(Args.CalibPath)
                        % use CalibBasePath
                        SplittedNewPath = split(Args.NewPath, filesep);
                        Fc = contains(SplittedNewPath, 'LAST.');
                        ProjName = SplittedNewPath(Fc);
                        Args.CalibPath = sprintf('%s%s%s%s%s', Args.CalibBasePath, filsesep, ProjName, filesep, 'calib');
                    end

                    Obj.BasePath    = Args.LocalPath;
                    Obj.NewPath     = Args.NewPath;
                    Obj.CalibPath   = Args.CalibPath;
                    Obj.LogPath     = sprintf('%s%s%s',Args.LocalDir, filesep, 'log');
                    Obj.FailedPath  = sprintf('%s%s%s',Args.LocalDir, filesep, 'failed');
                    Obj.RefPath     = Args.RefPath;
                case 'test'
                    Obj.BasePath = sprintf('%s/matlab/data/pipeline/LAST',tools.os.get_userhome);
                    Obj.RefPath     = Args.RefPath;

                otherwise
                    error('Unknown ReductionMode option');
            end
        end


        function cleanNewDir(Obj)
            % Clean 0 size files and day time images

            % remove files with zero size (claening)
            Fsize0 = io.files.deleteZeroSizeFiles(true);
            if numel(Fsize0)>0
                warning('Files with zero size where found in new/ dir (N=%d) - deleted',numel(Fsize0));
            end
           
            % remove day time images (non-dark)
            Obj.deleteDayTimeImages;

        end

        function [IndStartGroup]=selectVisitForReduction(Obj, FN_Sci_Groups, Args)
            % Select visit for reduction
            % Input  : - self.
            %          - AstroFileName objects of all identified groups
            %          - Structure of arguments
            % Output : - Index of selected visit for reduction
            % Author : Eran Ofek (Sep 2025)

            arguments
                Obj
                FN_Sci_Groups
                Args
            end
            SEC_DAY = 86400;

            % number of groups
            NinGroup = FN_Sci_Groups.nFiles;
            Ngroup   = numel(NinGroup);
            if Ngroup==0
                % wait for more images
                pause(200);   % pause for 200 s (i.e., 10x20 s)
                Skip        = true;
            else
                Skip = false;
            end

            %--- Chose visit for reduction ---
            if ~Skip
                % at least one group was found
                IndStartGroup = find(NinGroup==Args.MaxInGroup, 1, 'first');
            else
                IndStartGroup = [];
            end


            if ~isempty(Args.ReduceVisitContainingImage)
                % Select only the visit that contains this image name
                IndStartGroup = find(FN_Sci_Groups.isFileInList(Args.ReduceVisitContainingImage),1);
                if isempty(IndStartGroup)
                    error('ReduceVisitContainingImage file name: %s was not found - Abort', Args.ReduceVisitContainingImage);
                end
            else
            
                if ~Skip && Ngroup>0 && isempty(IndStartGroup)
                    % at least one group was found, but less than
                    % Args.MaxInGroup (20) images in group.

                    MaxJDPerGroup = FN_Sci_Groups.juldayFun(@max);
                    if Ngroup==1
                        % get max JD of each sequence:
                        
                        TimeSinceLastImage = celestial.time.julday() - MaxJDPerGroup;
                        if TimeSinceLastImage<(40./SEC_DAY)
                            % Don't wait for more images
                        else
                            % wait for more images
                            pause((Args.MaxInGroup - NinGroup).*Args.ExpTime+5);
                            Skip = true;
                        end
                    else
                        switch lower(Args.SortDirection)
                            case 'ascend'
                                IndStartGroup = Ngroup - 1;
                            case 'descend'
                                IndStartGroup = 2;
                            otherwise
                                Msg = sprintf('Unknown SortDirection option : %s', Args.SortDirection);
                                Obj.writeLog(Msg, LogLevel.Error);
            
                                error('Unknown SortDirection option');
                        end

                    end

                end
            end

        end

        function [Obj,Args]=prepArgs(Obj, Args)
            % Prepare arguments for main pipeline function

            %---Update input arguments ---
            Args = Obj.prepPath(Args);
           
            % Update argumenst from configuration file:
            Args = tools.args.updateParFromConfig(Args, Obj.Config, Args.ArgsConfigName);
            
            if isempty(Args.HostName)
                Args.HostName = tools.os.get_computer;            
            end
            Obj.HostName = Args.HostName;


            % Prepare and connect to DB:
            Args.DB = connectDB(Obj, Args);


            if Args.SelectKnownAsteroid
                Args = Obj.prepSolarSystemEphem(Args);
            end
              
           
            % convert 'all'|'cat' to cell array of data products
            Args.SaveEpochProduct = pipeline.DemonLAST.PrepSaveProductArg(Args.SaveEpochProduct);
            Args.SaveVisitProduct = pipeline.DemonLAST.PrepSaveProductArg(Args.SaveVisitProduct);            


            if numel(Args.StartJD)>1
                Args.StartJD = celestial.time.julday(Args.StartJD);
            end
            if numel(Args.EndJD)>1
                Args.EndJD = celestial.time.julday(Args.EndJD);
            end
            if Args.EndJD<0 && ~isinf(Args.EndJD)
                Args.EndJD = Args.StartJD - Args.EndJD;
                Args.StopWhenDone = true;
            end


        end
    end


    methods % sub pipelines
        
        function [Obj, FN, FN_Master]=prepMasterDark(Obj, Args)
            % prepare master dark images
            %   Given a (D=) pipeline.DemonLAST object select files in either
            %   current dir or D.NewPath, and create master dark images
            %   from images which belongs to the same counter series.
            %   The function also checks if the last dark image was
            %   obtained in the last 1minute, then wait for more images.
            %   The master dark image, mask and variance are saved in the
            %   D.CalibPath directory.
            % Input  : - A ipeline.DemonLAST object.
            %          * ...,key,val,...
            %            'FileList' - list of files to select.
            %                   Default is '*dark*.fits'.
            %            'BiasArgs' - A cell array of additional arguments
            %                   to pass to the CalibImages/createBias
            %                   function. Default is {}.
            %            'MinDT' - Minimum time since the last image was
            %                   taken. Default is 1./1440 [days].
            %            'WaitForMoreImages' - Wait time for additional
            %                   images. Default is 10 [s].
            %            'MinInGroup' - Minimum number of images in counter
            %                   group from which to construct the master dark.
            %                   Default is 9.
            %            'move2newPath' - A logical indicating if to move
            %                   to the D.NewPath before running the function.
            %                   Default is true (i.e., working in current
            %                   dir).
            %            'Repopulate' - A logical indicating if to
            %                   repopulate the dark image even if it is already
            %                   exist.
            %            'ClearVar' - Clear the Var image from the
            %                   CalibImages. Default is true.
            %            'Move2raw' - Move images to raw dir after
            %                   processing. Default is true.
            % Output : - A pipeline.DemonLAST with updated CI property.
            %          - FileNames object of raw dark images.
            %          - FileNames object of proc dark master image.
            %          * Write files to disk
            % Author : Eran Ofek (Apr 2023)
            % Example: D = pipeline.DemonLAST;
            %          D.CalibPath = <put here the output directory of the calibration images>
            %          D.prepMasterDark

            arguments
                Obj
                Args.FilesList            = '*dark*.fits';
                Args.BiasArgs             = {};
                Args.MinDT                = 1./1440;  % [d]
                Args.WaitForMoreImages    = 10;   % [s]
                Args.MinInGroup           = 9;
                Args.move2newPath logical = true;
                Args.Repopulate logical   = true;
                Args.ClearVar logical     = true;
                Args.Move2raw logical     = true;
            end

            FN        = [];
            FN_Master = [];
            NoImages = false;
            if Args.Repopulate || ~exist(Obj.CI,'Bias',{'Image','Mask'})
                % create a master Bias

                if Args.move2newPath
                    cd(Obj.NewPath);
                end
    
                WaitForMoreImages = true;
                while WaitForMoreImages
                    
                    if isa(Args.FilesList,'FileNames')
                        FN = Args.FN;
                        WaitForMoreImages = false;
                    else
                        % generate file names
                        % find all images in directory
                        FN = FileNames.generateFromFileName(Args.FilesList);
                        FN.BasePath = Obj.BasePath;
                    end
    
                    % identify new bias images
                    [FN_Dark,Flag] = selectBy(FN, 'Type', {'dark','bias'}, 'CreateNewObj',true);
    
                    % check that the dark images are ready and group by night
                    [Ind, LastJD, DT] = selectLastJD(FN_Dark);
                    if isempty(Ind)
                        % no images
                        WaitForMoreImages = false;
                        NoImages = true;
                    else
                        if DT>Args.MinDT
                            % prep master dark
                            PrepMaster = true;
                        else
                            % wait for more images
                            PrepMaster = false;
                        end
                    
                        if PrepMaster==true
                            WaitForMoreImages = false;
                        else
                            pause(Args.WaitForMoreImages)
                        end
                    end
                end
                
                if ~NoImages
                    % group dark images by time groups
                    [~, FN_Dark_Groups] = groupByCounter(FN_Dark, 'MinInGroup',Args.MinInGroup);
                    
                    Ngr = numel(FN_Dark_Groups);
                    if Ngr>0
                        for Igr=1:1:Ngr
                            DarkList = FN_Dark_Groups(Igr).genFull([]);


                            % prepare master bias
                            CI = CalibImages;

                            CI.createBias(DarkList, 'BiasArgs',Args.BiasArgs, 'Convert2single',true);

                            % save processed bias images in raw/ dir

                            %readFromHeader(FN_Dark_Groups(Igr), CI.Bias)


                            %Obj = readFromHeader(, Input, DataProp

                            % check if bias/dark is good
                            if Obj.checkMasterDark(CI.Bias)

                                % write file
                                JD = CI.Bias.julday;
                                FN_Master = FileNames;
                                FN_Master.readFromHeader(CI.Bias);
                                FN_Master.Type     = {'dark'};
                                FN_Master.Level    = {'proc'};
                                FN_Master.Product  = {'Image'};
                                FN_Master.Version  = [1];
                                FN_Master.FileType = {'fits'};    

                                % values for LAST dark images
                                FN_Master.FieldID  = {''};
                                FN_Master.Counter  = {''};
                                FN_Master.CCDID    = {''};
                                FN_Master.CropID   = {''};
                                FN_Master.ProjName = FN_Dark.ProjName{1};

                                if ~isfolder(Obj.CalibPath)
                                    mkdir(Obj.CalibPath);
                                end

                                FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                                write1(CI.Bias, FileN{1}, 'Image');
                                FN_Master.Product  = {'Mask'};
                                FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                                write1(CI.Bias, FileN{1}, 'Mask');
                                FN_Master.Product  = {'Var'};
                                FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                                write1(CI.Bias, FileN{1}, 'Var');

                                % keep in CI.Bias 
                                Obj.CI.Bias = CI.Bias;

                                if Args.ClearVar
                                    Obj.CI.Bias.Var = [];
                                end
                            end

                        end
                    else
                        % read master image from disk
                        Obj = Obj.loadCalib('ReadProduct',{'Bias'});
                    end
                else
                    % read master image from disk
                    Obj = Obj.loadCalib('ReadProduct',{'Bias'});
                end
            else % ~Repopulate...
                % no need to create a Master bias - already exist       
                % read master image from disk
                Obj = Obj.loadCalib('ReadProduct',{'Bias'});
            end

            % check that was able to read master image
            if ~Obj.CI.exist('Bias')
                error('No Bias image found - can not run pipeline');
            end
            
            if Args.Move2raw && FN.nfiles>0
                RawList = FN.genFull; 
                FN.FullPath = [];
                % copy files to: FN_Dark.genPath();
                io.files.moveFiles(RawList, FN.genFull);
            end

        end
        
        function [Obj, FN, FN_Master]=prepMasterFlat(Obj, Args)
            % prepare master flat images
            %   Given a (D=) pipeline.DemonLAST object select files in either
            %   current dir or D.NewPath, and create master flat images
            %   from images which belongs to the same counter series.
            %   The function also checks if the last flat image was
            %   obtained in the last 1minute, then wait for more images.
            %   The master flat image, mask and variance are saved in the
            %   D.CalibPath directory.
            % Input  : - A ipeline.DemonLAST object.
            %          * ...,key,val,...
            %            'FileList' - list of files to select.
            %                   Default is '*flat*.fits'.
            %            'debiasArgs' - A cell array of additional
            %                   arguments to pass to imProc.dark.debias.
            %                   Default is {}.
            %            'FlatArgs' - A cell array of additional arguments
            %                   to pass to the CalibImages/createFlat
            %                   function. Default is {}.
            %            'MinDT' - Minimum time since the last image was
            %                   taken. Default is 1./1440 [days].
            %            'WaitForMoreImages' - Wait time for additional
            %                   images. Default is 10 [s].
            %            'MinInGroup' - Minimum number of images in counter
            %                   group from which to construct the master flat.
            %                   Default is 6.
            %            'move2newPath' - A logical indicating if to move
            %                   to the D.NewPath before running the function.
            %                   Default is false (i.e., working in current
            %                   dir).
            %            'Repopulate' - A logical indicating if to
            %                   repopulate the dark image even if it is already
            %                   exist.
            %            'ClearVar' - Clear the Var image from the
            %                   CalibImages. Default is true.
            %            'Move2raw' - Move images to raw dir after
            %                   processing. Default is true.
            %            'Convert2single' - A logical indicating if to
            %                   convert the images to single. Default is true.
            %            'OverWrite' - Over write files.
            %                   Default is true.
            % Output : - A pipeline.DemonLAST with updated CI property.
            %          - FileNames object of raw flat images.
            %          - FileNames object of proc flat master image.
            %          * Write files to disk
            % Author : Eran Ofek (Apr 2023)
            % Example: D = pipeline.DemonLAST;
            %          D.CalibPath = <put here the output directory of the calibration images>
            %          D.D.prepMasterFlat

            arguments
                Obj
                Args.FilesList   = '*flat*.fits';
                Args.BiasImage   = [];  % if not given use Demon.CI
                Args.debiasArgs  = {};
                Args.FlatArgs    = {};
                Args.MinDT       = 1./1440;  % [d]
                Args.WaitForMoreImages = 10;   % [s]
                Args.MinInGroup  = 6;
                Args.move2newPath logical = false;
                Args.Repopulate logical   = true;
                Args.ClearVar logical     = true;
                Args.Move2raw logical     = true;
                Args.Convert2single logical = true;
                Args.OverWrite logical      = true;
            end

            if ~exist(Obj.CI, 'Bias',{'Image','Mask'})
                error('Can not execute prepMasterFlat without bias/dark images');
            end

            PWD = pwd;
            cd(Obj.NewPath);

            FN        = [];
            FN_Master = [];
            NoImages = false;
            if Args.Repopulate || ~exist(Obj.CI,'Flat',{'Image','Mask'})
                % create a master Flat

                if Args.move2newPath
                    cd(Obj.NewPath);
                end
    
                WaitForMoreImages = true;
                while WaitForMoreImages
                    
                    if isa(Args.FilesList,'FileNames')
                        FN = Args.FN;
                        WaitForMoreImages = false;
                    else
                        % generate file names
                        % find all images in directory
                        FN = FileNames.generateFromFileName(Args.FilesList);
                        FN.BasePath = Obj.BasePath;
                    end
    
                    % identify new flat images
                    [FN_Flat,Flag] = selectBy(FN, 'Type', {'twflat','flat'}, 'CreateNewObj',true);
    
                    % check that the dark images are ready and group by night
                    [Ind, LastJD, DT] = selectLastJD(FN_Flat);
                    if isempty(Ind)
                        % no images
                        WaitForMoreImages = false;
                        NoImages = true;
                    else
                        if DT>Args.MinDT
                            % prep master dark
                            PrepMaster = true;
                        else
                            % wait for more images
                            PrepMaster = false;
                        end
                    
                        if PrepMaster==true
                            WaitForMoreImages = false;
                        else
                            pause(Args.WaitForMoreImages)
                        end
                    end
                end
                
                % group flat images time

                if ~NoImages

                    [~, FN_Flat_Groups] = groupByTimeGaps(FN_Flat, 'MinInGroup',Args.MinInGroup);
                    
                    Ngr = numel(FN_Flat_Groups);
                    if Ngr>0
                        for Igr=1:1:Ngr
                            FlatList = FN_Flat_Groups(Igr).genFull([]);


                            % prepare master flat

                            % read the images
                            AI = AstroImage(FlatList);

                            % subtract bias/dark
                            if Args.Convert2single
                                AI.cast('single');
                            end
                            AI = imProc.dark.debias(AI, Obj.CI.Bias, Args.debiasArgs{:});

                            CI = CalibImages;

                            CI.createFlat(AI, 'FlatArgs',Args.FlatArgs, 'Convert2single',true);

                            % check if flat is good
                            if Obj.checkMasterFlat(CI.Flat)

                                % write file
                                JD = CI.Flat.julday;
                                FN_Master = FileNames;
                                FN_Master.readFromHeader(CI.Flat);
                                FN_Master.Type     = {'twflat'};
                                FN_Master.Level    = {'proc'};
                                FN_Master.Product  = {'Image'};
                                FN_Master.Version  = [1];
                                FN_Master.FileType = {'fits'};    

                                % values for LAST dark images
                                FN_Master.FieldID  = {''};
                                FN_Master.Counter  = {''};
                                FN_Master.CCDID    = {''};
                                FN_Master.CropID   = {''};
                                FN_Master.ProjName = FN_Flat.ProjName{1};


                                FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                                write1(CI.Flat, FileN{1}, 'Image', 'Overwrite',Args.OverWrite);
                                FN_Master.Product  = {'Mask'};
                                FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                                write1(CI.Flat, FileN{1}, 'Mask', 'Overwrite',Args.OverWrite);
                                FN_Master.Product  = {'Var'};
                                FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                                write1(CI.Flat, FileN{1}, 'Var', 'Overwrite',Args.OverWrite);

                                % keep in CI.Bias 
                                Obj.CI.Flat = CI.Flat;

                                if Args.ClearVar
                                    Obj.CI.Flat.Var = [];
                                end
                            end % if Obj.checkMasterFlat(CI.Flat)
                        end % for Igr=1:1:Ngr
                        if isempty(FN_Master)
                            Obj = Obj.loadCalib('ReadProduct',{'Flat'});
                        end
                    else
                        % read master image from disk
                        Obj = Obj.loadCalib('ReadProduct',{'Flat'});
                    end % if Ngr>0
                    
                else
                    % read master image from disk
                    Obj = Obj.loadCalib('ReadProduct',{'Flat'});
                end
            else
                % no need to create a Master bias - already exist
                % read master image from disk
                Obj = Obj.loadCalib('ReadProduct',{'Flat'});
            end

            % check that was able to read master image
            if ~Obj.CI.exist('Flat')
                error('No Flat image found - can not run pipeline');
            end
            
            if Args.Move2raw && FN.nfiles>0
                RawList = FN.genFull; 
                FN.FullPath = [];
                % copy files to: FN_Dark.genPath();
                io.files.moveFiles(RawList, FN.genFull);
            end

            cd(PWD);
        end
        
        function [Flag, Info,AI]=checkMasterDark(Obj, AI, Args)
            % Check if MasterDark image is good
            % Input  : - A pipeline.DemonLAST object
            %          - Either an AstroImage containing dark image,
            %            or a char array of template file name. Will look
            %            for files in the CalibPath dir.
            %          * ...,key,val,...
            %            See code.
            % Output : - A vector of logical flags indicating, for each
            %            image, if its good dark image.
            %          - A structure array with information per image.
            % Author : Eran Ofek (Feb 2024)
            % Example: D.checkMasterDark;  % check all images in Calib dir
            %          D.checkMasterDark(D.CI.Bias) % check current dark
            
            arguments
                Obj
                AI                = 'LAST*_dark_proc_Image*.fits';
                Args.MedianRange  = [20 200];
                Args.RStdRange    = [1 3];
                Args.StdRange     = [0 60];
                Args.MaxNaN       = 20000;
            end
            
            PWD = pwd;
            cd(Obj.CalibPath);
            if ischar(AI) || isstring(AI)
                AI = AstroImage.readFileNamesObj(AI);
            else
                % AI is supplied by user
            end
            
            Nai = numel(AI);
            Flag = false(Nai,1);
            for Iai=1:1:Nai
                Info(Iai).Median  = imProc.stat.median(AI(Iai));
                Info(Iai).Std     = imProc.stat.std(AI(Iai));
                Info(Iai).RStd    = imProc.stat.rstd(AI(Iai));
                Info(Iai).CountNaN = sum(isnan(AI(Iai).Image(:)));
                
                Info(Iai).FileName = AI(Iai).ImageData.FileName;
                if Info(Iai).Median>Args.MedianRange(1) && Info(Iai).Median<Args.MedianRange(2) && ...
                        Info(Iai).Std>Args.StdRange(1) && Info(Iai).Std<Args.StdRange(2) && ...
                        Info(Iai).RStd>Args.RStdRange(1) && Info(Iai).RStd<Args.RStdRange(2) && ...
                        Info(Iai).CountNaN<Args.MaxNaN
                    Flag(Iai) = true;
                end
                    
            end
                            
            
            cd(PWD);
        end
        
        function [Flag, Info,AI]=checkMasterFlat(Obj, AI, Args)
            % Check if MasterFlat image is good
            % Input  : - A pipeline.DemonLAST object
            %          - Either an AstroImage containing flat image,
            %            or a char array of template file name. Will look
            %            for files in the CalibPath dir.
            %          * ...,key,val,...
            %            See code.
            % Output : - A vector of logical flags indicating, for each
            %            image, if its good flat image.
            %          - A structure array with information per image.
            % Author : Eran Ofek (Feb 2024)
            % Example: D.checkMasterFlat;  % check all images in Calib dir
            %          D.checkMasterFlat(D.CI.Flat) % check current flat
            
            arguments
                Obj
                AI                = 'LAST*_twflat_proc_Image*.fits';
                Args.MedianRange  = [0.95 1.05];
                Args.RStdRange    = [0.01 0.05];
                Args.StdRange     = [0 1];
                Args.MaxNaN       = 40000;
                Args.MaxAbsGrad   = 0.05;
                Args.MaxNmaxGrad  = 50000;
            end
            
            PWD = pwd;
            cd(Obj.CalibPath);
            if ischar(AI) || isstring(AI)
                AI = AstroImage.readFileNamesObj(AI);
            else
                % AI is supplied by user
            end
            
            Nai = numel(AI);
            Flag = false(Nai,1);
            for Iai=1:1:Nai
                Info(Iai).Median  = imProc.stat.median(AI(Iai));
                Info(Iai).Std     = imProc.stat.std(AI(Iai));
                Info(Iai).RStd    = imProc.stat.rstd(AI(Iai));
                Info(Iai).CountNaN = sum(isnan(AI(Iai).Image(:)));

                [Gx] = gradient(AI(Iai).Image);
                NmaxGrad = sum(abs(Gx(:))>Args.MaxAbsGrad);
                Info(Iai).NmaxGrad = NmaxGrad;

                Info(Iai).FileName = AI(Iai).ImageData.FileName;
                if Info(Iai).Median>Args.MedianRange(1) && Info(Iai).Median<Args.MedianRange(2) && ...
                        Info(Iai).Std>Args.StdRange(1) && Info(Iai).Std<Args.StdRange(2) && ...
                        Info(Iai).RStd>Args.RStdRange(1) && Info(Iai).RStd<Args.RStdRange(2) && ...
                        Info(Iai).CountNaN<Args.MaxNaN && Info(Iai).NmaxGrad<Args.MaxNmaxGrad
                    Flag(Iai) = true;
                end
                    
            end
                            
            
            cd(PWD);
        end
                
        function Obj=loadCalib(Obj, Args)
            % load CalibImages into the pipeline.DemonLAST object
            % Input  : - A pipeline.DemonLAST object.
            %          * ...,key,val,...
            %            'CalibPath' - A path to the calib images. If
            %                   empty, use the object CalibPath property.
            %                   Default is [].
            %            'ReadProduct' - A cell array of products to read.
            %                   Default is {'bias','flat'}.
            %            'BiasTemplate' - File name template of bias images.
            %                   Default is '*dark_proc_Image*.fits'.
            %            'FlatTemplate' - File name template of flat images.
            %                   Default is '*flat_proc_Image*.fits'.
            %            'AddImages' - Additional products to be loaded, in
            %                   addition to the 'Image' product.
            %                   Default is {'Mask'}.
            %            'BiasNearJD' - If given this is a date [D M Y [H M S]]
            %                   or JD, and will select the nearest in time
            %                   bias file.
            %                   Default is [].
            %            'FlatNearJD' - Like 'BiasNearJD', but for flat
            %                   file. Default is [].
            %            'ForceReload' - A logical indicating if to force
            %                   reload bias/flat images. Default is false.
            %
            % Output : - A ipeline.DemonLAST object in which the CI
            %            property is populated with bias, flat, and
            %            linearity data.
            % Author : Eran Ofek (Apr 2023)
            % Example: D=pipeline.DemonLAST;
            %          D.loadCalib

            arguments
                Obj
                Args.CalibPath       = [];
                Args.ReadProduct     = {'bias','flat'}
                Args.BiasTemplate    = '*dark_proc_Image*.fits';
                Args.FlatTemplate    = '*twflat_proc_Image*.fits';
                Args.AddImages       = {'Mask'};
                Args.BiasNearJD      = [];
                Args.FlatNearJD      = [];

                Args.ForceReload     = false;
            end

            PWD = pwd;
            if ~isempty(Args.CalibPath)
                cd(Args.CalibPath)
            else
                cd(Obj.CalibPath);
            end
            
            %fprintf('\n\nAvailable storage space:\n')
            %unix('df -h | grep data');
            %fprintf('\n\nPlease only run the pipeline if disk less than 80percent full.\n\n')

            
            IsBiasEmpty = Obj.CI.Bias.isemptyImage;
            IsFlatEmpty = Obj.CI.Flat.isemptyImage;


            % read latest bias image
            if ismember('bias',lower(Args.ReadProduct))
                if Args.ForceReload || IsBiasEmpty
                    FN_Bias = FileNames.generateFromFileName(Args.BiasTemplate);
                    if isempty(Args.BiasNearJD)
                        [~,~,~,FN_Bias] = FN_Bias.selectLastJD;
                    else
                        [~,~,FN_Bias] = FN_Bias.selectNearest2JD(Args.BiasNearJD);
                    end
                        
                    Obj.CI.Bias = AstroImage.readFileNamesObj(FN_Bias, 'AddProduct',Args.AddImages);
                    Obj.writeLog(sprintf('Using dark: %s\n', char(FN_Bias.genFile)), LogLevel.Info);
                    %fprintf('\nUsing dark: %s\n', char(FN_Bias.genFile))
                end
            end

            % read latest flat image
            if ismember('flat',lower(Args.ReadProduct))
                if Args.ForceReload || IsFlatEmpty
                    FN_Flat = FileNames.generateFromFileName(Args.FlatTemplate);
                    if isempty(Args.FlatNearJD)
                        [~,~,~,FN_Flat] = FN_Flat.selectLastJD;
                    else
                        [~,~,FN_Flat] = FN_Flat.selectNearest2JD(Args.FlatNearJD);
                    end
                    Obj.CI.Flat = AstroImage.readFileNamesObj(FN_Flat, 'AddProduct',Args.AddImages);
                    Obj.writeLog(sprintf('Using flat: %s\n', char(FN_Flat.genFile)), LogLevel.Info);
                    %fprintf('Using flat: %s\n\n', char(FN_Flat.genFile))
                end
            end

            % Read linearity file
            % Result = populateLinearity(Obj.CI, Name, Args)
          
            cd(PWD);

        end


        



        function [Status, RawImageListFinal, AllSI, MS, Coadd, OnlyMP, AllForcedPhot]=runPipelineI(Obj, RawImageList, FN_I, Args)
            % Reduce + save + error catching a single visit  
            
            arguments
                Obj
                RawImageList
                FN_I
                Args   % struct of Args
            end


            Status.PipeI   = false;
            Status.WriteI  = false;
            Status.MoveRaw = false;
            
            try
                Tstart = clock;

                % executing pipelineI
                [TableRaw, AllSI, MS, Coadd, OnlyMP] = pipeline.last.pipes.pipelineI(RawImageList, Obj.CI, Args.pipelineIArgs{:});
                ProcImageList = TableRaw.FileNames;                
                RunTime = etime(clock, Tstart);
                MsgF{1} = sprintf('pipeline.last.pipes.PipelineDemon/pipelineI finished executing pipeline for visit');
                MsgF{2} = sprintf('pipeline run time : %f', RunTime);
                Obj.writeLog(MsgF, LogLevel.Info);

                Status.PipeI = true;

                % Notify watchdog that process is running
                tools.systemd.mex.notify_watchdog;

                % saving data products of pipelineI
                Tstart = clock;
                AllForcedPhot = []; % TEMPORARY
                [FN_I, FN_C, FN_A, FN_MS, FN_Raw, FN_FP] = saveDataProductsI(Obj, FN_I, TableRaw, AllSI, MS, Coadd, OnlyMP, AllForcedPhot, Args);
                RunTime = etime(clock, Tstart);
                MsgF{1} = sprintf('pipeline.last.pipes.PipelineDemon/pipelineI finished saving products for visit');
                MsgF{2} = sprintf('pipeline run time : %f', RunTime);
                Obj.writeLog(MsgF, LogLevel.Info);

                
                Status.WriteI = true;

                % Move images to raw/ dir                
                if Args.MoveNew2Raw     
                    RawImageListFinal = FN_I.genPath('PathType','raw');
                    io.files.moveFiles(RawImageList, [], '', RawImageListFinal);
                    
                    Status.MoveRaw = true;
                else
                    RawImageListFinal = FN_I.genPath;
                    Status.MoveRaw = false;
                end
                    

                % Pipeline II

                %Status.PipeII = true;

                % write data products II

                % Status.WriteII = true;

            catch MEp
                % pipeline failed
               
                RawImageListFinal = [];
                AllSI = [];
                MS    = [];
                Coadd = [];
                OnlyMP = [];
                AllForcedPhot = [];

                ErrorMsg = sprintf('Pipeline I failed: %s / funname: %s @ line: %d', MEp.message, MEp.stack(1).name, MEp.stack(1).line);
                Obj.writeLog(ErrorMsg, LogLevel.Error);
                Obj.writeLog(MEp, LogLevel.Info);

            end % try



        end


        function [FN_I, FN_C, FN_A, FN_MS, FN_Raw, FN_FP] = saveDataProductsI(Obj, FN_I, TableRaw, AllSI, MS, Coadd, OnlyMP, AllForcedPhot, Args)
            % Save data products for pipeline I

          


            % save products
            % Debug: FN_I=AstroFileName.dir('LAST*.fits');
            FN_I.BasePathIncludeProjName = false;
            FN_I.BasePath = Obj.BasePath;
            FN_I.Path     = [];
            %
            FN_C = FN_I.reorderEntries(1, 'CreateNewObj',true);
            [Nim, Nsub] = size(AllSI);
            FN_I.reorderEntries(TableRaw.SelectedImages);
            FN_I.Level  = repmat("proc", Nim, 1);
            FN_I.duplicateCrop(Nsub);
            FN_I.JD = FN_I.julday;
            % Coadd
            FN_C.JD = Coadd(1).julday('KeyJD','MIDJD');
            FN_C.julday2time;
            FN_C.Level = "coadd";
            FN_C.duplicateCrop(Nsub);

            [~,FN_I] = imProc.io.saveProductImage(AllSI, FN_I, 'BasePath',Obj.BasePath, 'OutProduct',Args.SaveEpochProduct, 'WriteHeader',Args.SaveEpochHeader, 'WriteMethodImages',Args.WriteMethodImages, 'WriteMethodTables',Args.WriteMethodTables, 'CompressedOutput', Args.CompressedOutput);  % 20 s

            % Coadd
            FN_C.SubDir  = FN_I.SubDir;
            FN_C.Counter = 0;
            imProc.io.saveProductImage(Coadd, FN_C, 'BasePath',Obj.BasePath, 'OutProduct',Args.SaveVisitProduct, 'WriteHeader',Args.SaveVisitHeader, 'CompressedOutput', Args.CompressedOutput);  % 3 s
            
            % Asteroids:
            if OnlyMP.sizeCatalog>0 && Args.SaveVisitAsteroids
                FN_A = FN_C.reorderEntries(1, 'CreateNewObj',true);
                FN_A.CropID = 0;
                FN_A.Product = "Asteroids.Known";
                FN_A.FileType = 'mat';
                AI_tmp = AstroImage;
                AI_tmp.CatData = OnlyMP;
                imProc.io.saveProductMat(AI_tmp, FN_A, 'BasePath',Obj.BasePath);
            else
                FN_A = [];
            end

            % MatchedSources
            if Args.SaveMergedMat
                FN_MS = FN_C.copy;
                FN_MS.Level    = repmat("merged", Nsub, 1);
                FN_MS.Product  = repmat("MergedMat", Nsub, 1);
                FN_MS.FileType = repmat("hdf5", Nsub, 1);
                [~,FN_MS]   = imProc.io.saveProductMatchedSources(MS, FN_MS, 'BasePath',Obj.BasePath);
            else
                FN_MS = [];
            end

            % TableRaw
            if Args.SaveTableRaw
                FN_Raw = FN_I.reorderEntries(1, 'CreateNewObj',true);
                FN_Raw.FileType = "mat";
                FN_Raw.Level    = "raw";
                FN_Raw.Product  = "Cat";
                FN_Raw.Counter  = 0;
                FN_Raw.CropID   = 0;
                [~,FN_Raw] = imProc.io.saveProductMat(TableRaw, FN_Raw, 'BasePath',Obj.BasePath);
            else
                FN_Raw = [];
            end

            % AllForcedPhot
            FN_FP = FN_I.reorderEntries(1, 'CreateNewObj',true);
            % FN_FP.FileType = "fits";
            % FN_FP.Level    = "proc";
            % FN_FP.Product  = "Cat.forced";
            % FN_FP.Counter  = 0;
            % FN_FP.CropID   = 0;
            % imProc.io.saveProductImage(AllForcedPhot, FN_FP, 'BasePath',DD.BasePath, 'OutProduct',["Cat"], 'WriteHeader',false);
        end

        function transientDetectionPipeline(Obj, Coadd, FN_Proc, UpArgs)
            % excute transients detection pipeline

            % Run the transients pipeline
            UpArgs.DoTransientsDetection = true;
            if Args.DoTransientsDetection 
                %&& strcmp(tools.os.get_computer, 'last01e')
                Msg{1} = sprintf('pipeline.DemonLAST - Transients detection / group %d', Igroup);
                Obj.writeLog(Msg, LogLevel.Info);

                % Transients detection
                try
                    [~,TransientCutouts, TCL1, TranPipeStatus] = ...
                        pipeline.last.transients.runTransientsPipe(...
                            Coadd, 'SavePath',FN_Proc.genPath, 'RefPath',Obj.RefPath, 'SaveProducts',true, ...
                            'Product',{'Image','Mask','Cat','PSF'},'WriteHeader',[true,false,true,false]);
                    Obj.writeLog(sprintf('pipeline.DemonLAST / Transients detection - %s', TranPipeStatus), LogLevel.Info);
                catch MEtran
                    Msg{1} = sprintf('pipeline.DemonLAST - Transients detection / Failed');
                    Obj.writeLog(Msg, LogLevel.Info);
                end

                % Match to multi-epochs via DB
                if exist('TransientCutouts','var')
                    Msg{1} = sprintf('pipeline.DemonLAST - Transients match multi epoch / group %d', Igroup);
                    Obj.writeLog(Msg, LogLevel.Info);

                    try
                        [TransientCutouts, TCL2, MultiEpochStatus] = pipeline.last.transients.matchTransientsToMultiEpochs(...
                            TransientCutouts, TCL1, 'DbHost', Args.DBHost, 'DB', DB);
                        Obj.writeLog(sprintf('pipeline.DemonLAST / Transients match multi epoch - %s', MultiEpochStatus), LogLevel.Info);
                    catch MEtran
                        Msg{1} = sprintf('pipeline.DemonLAST - Transients match multi epoch / Failed');
                        Obj.writeLog(Msg, LogLevel.Info);
                    end
                end
                
                % Send transients alerts
                if exist('TransientCutouts','var') && Args.SendTransientAlerts && IsRunningOnLAST
                    Msg{1} = sprintf('pipeline.DemonLAST - Transients alerting / group %d', Igroup);
                    Obj.writeLog(Msg, LogLevel.Info);
                    try
                        TranAlertStatus = pipeline.last.transients.sendTransientsAlert(TransientCutouts, 'SaveProducts', true, ...
                                'SavePath', FN_Proc.genPath,'UseLASTtools', true);
                        Obj.writeLog(sprintf('pipeline.DemonLAST / Transients alerting - %s', TranAlertStatus), LogLevel.Info);
                    catch MEtran
                        Msg{1} = sprintf('pipeline.DemonLAST - Alerting / Failed');
                        Obj.writeLog(Msg, LogLevel.Info);
                    end
                end
                RunTime = etime(clock, Tstart);
                Msg{1} = sprintf('pipeline.DemonLAST - Transients / RunTime: %.1f', RunTime);
                Obj.writeLog(Msg, LogLevel.Info);
            end


            % Insert pipeline products to the DB
            if Args.Insert2DB 
                Msg{1} = sprintf('pipeline.DemonLAST started preparing DB data for group %d',Igroup);
                Obj.writeLog(Msg, LogLevel.Info);
                if isempty(ADB) % && ( ~Args.DB_ImageBulk || ~Args.DB_CatalogBulk) % connect to DB
                    ADB = db.AstroDb(Args.AstroDBArgs{:});
                end
                try                                                                                                                                                             
                    Obj.insert2DB(ADB, RawHeader, AllSI, Coadd, RawImageListFinal, FN_I, FN_Proc, FN_Coadd, ...
                        'DB_ImageBulk',Args.DB_ImageBulk,'DB_CatalogBulk',Args.DB_CatalogBulk,...
                        'DB_Table_Raw',Args.DB_Table_Raw,'DB_Table_Proc',Args.DB_Table_Proc,'DB_Table_Coadd',Args.DB_Table_Coadd,...
                        'UpdateStatusFile',Args.UpdateStatusFile,'Tstart',Tstart);                                     
                catch DBMsg
                    DBErrorMsg = sprintf('pipeline.DemonLAST try error: %s / funname: %s @ line: %d', DBMsg.message, DBMsg.stack(1).name, DBMsg.stack(1).line);
                    Obj.writeLog(DBErrorMsg, LogLevel.Error);
                    Obj.writeLog(DBMsg, LogLevel.Error);
                end
            end
            % Insert transients to the transients' DB (on the fly)
            if Args.InsertTransients2DB && ~isempty(TCL2)
                if ~TCL2.isemptyCatalog
                    Err = [];
                    try
                        Err = pipeline.last.insertDB.insertTransients2DB(TCL2, [Coadd.HeaderData],'DbHost',Args.DBHost,'DB',DB);
                    catch ME
                        Obj.writeLog(ME, LogLevel.Error);
                    end
                    if ~isempty(Err)
                        Obj.writeLog(Err, LogLevel.Error);
                    end
                    RunTime = etime(clock, Tstart);
                    Msg{1} = sprintf('pipeline.DemonLAST finished injecting transients to the DB for group %d / RunTime: %.1f', Igroup, RunTime);
                    Obj.writeLog(Msg, LogLevel.Info);
                end
            end
            %
            RunTime = etime(clock, Tstart); % toc;

        end



        function moveImagesToFailedDir(Obj, RawImageList)
            % move images to failed directory

            % extract errors
            %ErrorMsg = sprintf('PipelineI try error: %s / funname: %s @ line: %d', ME.message, ME.stack(1).name, ME.stack(1).line);
            %warning(ErrorMsg);
            %Obj.writeLog(ErrorMsg, LogLevel.Error);

            %Obj.writeLog(ME, LogLevel.Error);
            
            % write log file
            ErrorMsg = sprintf('PipelineI %d images moved to failed directory',numel(RawImageList));
            Obj.writeLog(ErrorMsg, LogLevel.Error);

            % move images to failed/ dir
            io.files.moveFiles(RawImageList, [], '', Obj.FailedPath);           

            Msg{1} = sprintf('PipelineI summary line - Failed - First image: %s', RawImageList{1});
            Obj.writeLog(Msg, LogLevel.Info);
        end


    end

    methods % main

 
        function Obj=main(Obj, Args)
            % The main LAST pipeline demon.
            %   The demon waits for images in the new/ directory, analyze
            %   the images and distribute them.
            % Input  : - self.
            %          * ...,key,val,...
            %            See code.
            % Output : - Updated object.
            % Author : Eran Ofek (Jul 2023)
            % Example: cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/new
            %          D=pipeline.DemonLAST;
            %          D.setPath('/raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02')
            %          
            %          D.main('StartJD',[24 4 2023],'EndJD',[26 4 2023], 'StopWhenDone',true, 'Insert2DB',false, 'SaveEpochProduct',{'Image','Mask','Cat','PSF'});

            arguments
                Obj

                Args.ArgsConfigName         = 'LAST.Pipeline.PipeDemon';

                Args.DataDir       = 1;              % LAST data dir: 1|2
                Args.CamNumber     = [];             % Camera number: 1|2|3|4
                
                Args.NewSubDir     = 'new';          % new sub dir
                Args.NonStandardNew= '';             % non-standard new dir
                
                Args.AbortFileName = '~/abortPipe';  % if this file exit, then abort.
                Args.StopButton logical = true;      % Display stop button
                Args.StopDiskFull  = [];             % e.g., use 95 to abort if disk storage is above 95%
                
                Args.multiRaw2procCoaddArgs = {'DoCoadd',true};

                Args.pipelineIArgs  = {'UseParfor',true};

                Args.StopWhenDone logical = false;   % If true, then will not look for new images (i.e., images that were created after the function started)
                
                

                Args.FocusTreatment  = 'move';           % 'move'|'keep'|'delete' 
                

                Args.MinNumImageVisit  = 10;
                Args.PauseDay          = 100;
                Args.PauseNight        = 30;
               
                 % DataBase
                
                Args.DB_Table_Raw      = 'raw_images';
                Args.DB_Table_Proc     = 'proc_images';
                Args.DB_Table_Coadd    = 'coadd_images';
                Args.DB_Table_ProcCat  = 'proc_src_catalog';
                Args.DB_Table_CoaddCat = 'coadd_src_catalog';
                Args.DB_ImageBulk   logical = true;  % whether to use bulk or direct injection method
                Args.DB_CatalogBulk logical = true;  % whether to use bulk or direct injection method
                Args.AstroDBArgs cell  = {'Host','10.23.1.25','DatabaseName','last_operational','Port',5432};
                Args.AstroDBPassFile   = '~/.astropack/Passwords.yml';
                
                Args.InsertTransients2DB = true;
                Args.DBHost              = '10.23.1.25';
                Args.DbName              = 'last';
                Args.DbUser              = 'default';
                

                %% NEW


                Args.HostName          = []; 

                Args.ReductionMode = 'last';
                Args.ReduceVisitContainingImage = []; % if provided this is an image name that must exist in the visit to reduce
                Args.LocalPath     = [];
                Args.NewPath       = [];
                Args.CalibBasePath = '/marvin';
                Args.CalibPath     = [];
                Args.RefPath       = '/lastdata/references/v4';

                Args.Insert2DB         = false;              % Insert images data to LAST DB or prepare CSV dumps for further insertion
                Args.DB                = [];

                Args.SelectKnownAsteroid logical      = true;
                Args.GeoPos                           = [];    %[Lon (rad), Lat (rad), Height (m)].
                Args.OrbEl                            = [];
                Args.INPOP                            = [];
                Args.AsteroidSearchRadius             = 10;
                        
                

                Args.StartJD       = 0;           % refers only to Science observations: JD, or [D M Y]
                Args.EndJD         = Inf;         % if <0, then this is the number of nighst to reduce after StarJD
                
                Args.RegenCalib logical   = true; %false;     % Generate a new calib dark/flat images and load - if false: will be loaded once at the start
                
                Args.ReloadCalibTimeDiff   = 0.7;
                
                Args.DeleteSciDayTime logical = false;   % Delete 'sci' images taken during day time.
                Args.DeleteSunAlt  = 0;                  % SunAlt for previous argument
                
                Args.TempRawFocus    = '*_focus_raw_*.fit*';
                Args.TempRawSci      = '*_sci_raw_*.fit*';   % file name template to search

                Args.MinInGroup    = 10;             % min. number of images in visit/group to analyze.
                Args.MaxInGroup    = 20;             % max. number of images in visit/group to analyze.
                Args.SortDirection = 'descend';      % 'ascend'|'descend' - analyze last image first

                Args.ExpTime       = 20;

                Args.RepackRaw     = false;             % pack raw FITS images after processing 

                Args.PauseNotFound = 60;


                % Save data products
                Args.SaveEpochProduct  = ["Cat"]; %{[],[],'Cat',[]}; %{'Image','Mask','Cat','PSF'}; % {[],[],'Cat',[]}; %{[],[],'Cat',[]}; %{[],[],'Cat'};  %{'Image','Mask','Cat','PSF'};,  % 'all'
                Args.SaveVisitProduct  = ["Image","Mask","Cat","PSF"];      % 'all'
                Args.SaveEpochHeader  = true; %[true, false, false, true];
                Args.SaveVisitHeader  = [true, false, true, false];

                Args.SaveMergedCat     = false;
                Args.SaveMergedMat     = true;
                Args.SaveMergedAsteroids  = false;
                Args.SaveVisitAsteroids  = true;
                Args.SaveTableRaw        = true;

                Args.WriteMethodImages = 'ThreadedMex';     % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
                Args.WriteMethodTables = 'MexHeader';       % can be 'Standard' or 'MexHeader'  
                Args.UpdateStatusFile  = true;              % write update strings to the .status files in the output directories
                
                Args.CompressedOutput  = [];                % if empty, write FITS files ('fz' will lead to FITS.fz compression) 

                Args.SendTransientAlerts logical      = true;
                %Args.RunAsService logical  = false;
                
                Args.UncompressRaw     = false;             % we already know how to read compressed fits.fz, so no need to uncompress
                
                Args.MoveNew2Raw       = true;
                Args.DebugMode         = false;
            end
            RAD = 180./pi;
            SEC_DAY = 86400;
            
            % prep arguments
            [Obj, Args] = prepArgs(Obj, Args);
            


            % set Logger log file 
            Obj.setLogFile('HostName',Args.HostName);
            Obj.writeLog('******* pipeline.last.pipes.PipelineDemon started ********', LogLevel.Info);
            
            PWD = pwd;
            
            cd(Obj.NewPath);

            % clean Files from NewPath
            % remove files with zero size and day-time images:
            Obj.cleanNewDir;

            %IsRunningOnLAST = false;
            %if numel(Args.HostName)>=4
            %    if contains(Args.HostName,'last')
            %        IsRunningOnLAST = true;
            %    end
            %end


            % Stop GUI
            if all(get(0, 'ScreenSize')==1)
                % No display mode - set StopButton to false
                Args.StopButton = false;
            end

            if Args.StopButton
                GUI_Text = sprintf('Abort : Pipeline');
                [StopGUI, Hstop]  = tools.gui.stopButton('Msg',GUI_Text);
            end
            

            % loop indefently
            JDlastCalib = 0;
            Cont = true;
            MainLoopCounter = 0;
            while Cont
                MainLoopCounter = MainLoopCounter + 1;
                % Notify watchdog that process is running 
                tools.systemd.mex.notify_watchdog;
   
                cd(Obj.NewPath);

                if Args.RegenCalib
                    % prep Master dark and move to raw/ dir
                    [Obj, FN_Dark] = Obj.prepMasterDark('Move2raw',true);
                        
                    % prep Master flat and move to raw/ dir
                    [Obj, FN_Flat] = Obj.prepMasterFlat('Move2raw',true);
                else
                    % reload calib only if not already loaded in CI
                    [IsEmB, IsEmF] = Obj.CI.isemptyProp({'Bias','Flat'});
                    if IsEmB || IsEmF
                        Obj.loadCalib;
                    end
                end

                %--- Select group ---

                % delete test images taken during daytime
                if Args.DeleteSciDayTime
                    deleteDayTimeImages(Obj, 'SunAlt',Args.DeleteSunAlt);
                end

                % move focus images
                %FN_Foc   = FileNames.generateFromFileName(Args.TempRawFocus);
                FN_Foc   = AstroFileName(Args.TempRawFocus);
                FN_Foc.BasePath = Obj.BasePath;
                
                %FN_Foc.FullPath = [];
                % The empty argument in genPath is required for moving each image to the correct (date) directory. 
                if FN_Foc.nFiles>0
                    FN_Foc.moveImages('Operator',Args.FocusTreatment, 'SrcPath',[], 'DestPath', FN_Foc.genPath([]), 'Level','raw', 'Type','focus');
                end
                
                % look for new images
                FN_Sci   = AstroFileName(Args.TempRawSci);
                FN_Sci.JD=FN_Sci.julday;
               
                if FN_Sci.nFiles>Args.MinInGroup

                    % select observations by date
                    [FN_Sci] = FN_Sci.selectByDate(Args.StartJD, Args.EndJD, 'CreateNewObj',false);
                    FN_Sci   = FN_Sci.sortBy('JD', 'Direction', Args.SortDirection, 'CreateNewObj',false);  % sort by JD
    
                    % group images by counter
                    [Groups, FN_Sci_Groups] = groupByCounter(FN_Sci, 'MinInGroup',Args.MinInGroup, 'MaxInGroup',Args.MaxInGroup, 'CreateNewObj',true);
    
    
                    % sort groups by JD (be default last observed group is
                    % first)
                    FN_Sci_Groups = FN_Sci_Groups.sortByFunJD(Args.SortDirection);
    
                    % Select visit for reduction:
                    [IndStartGroup]=selectVisitForReduction(Obj, FN_Sci_Groups, Args);
                    
                    %--- End Select group ---
                    
                    % check if stop loop
                    if Args.StopButton && StopGUI()
                        Cont = false;
                    end
                    if isfile(Args.AbortFileName)
                        Cont = false;
                        delete(Args.AbortFileName);
                    end
    
                    if isempty(IndStartGroup)
                        % no files for reduction found
                        Msg = sprintf('Waiting for new visit - pause for %f seconds', Args.PauseNotFound);
                        Obj.writeLog(Msg, LogLevel.Info);
                        pause(Args.PauseNotFound);
                    else
                        % visit found
                        TstartAll = clock;
    
                        % --- files compression ---
                        % Check if images are compressed
                        if any(FN_Sci_Groups(IndStartGroup).isCompressed) && Args.UncompressRaw
                            % files are compressed
                            FN_Sci_Groups(IndStartGroup).uncompress('Type','fz');
                            FilesUncomp = true;
        
                            Msg = 'Uncompress visit';
                            Obj.writeLog(Msg, LogLevel.Info);
        
                        else
                            FilesUncomp = false;
                        end
                        RepackRaw = Args.RepackRaw && FilesUncomp;
                        
                        RawImageList   = FN_Sci_Groups(IndStartGroup).genFile();
                        NimagesInVisit = numel(RawImageList);
    
                        % log
                        MsgV{1} = sprintf('New visit found - Number of images in visit: %d', NimagesInVisit);
                        MsgV{2} = sprintf('New visit first image : %s', RawImageList{1});
                        MsgV{3} = sprintf('New visit last image : %s', RawImageList{end});
                        Obj.writeLog(MsgV, LogLevel.Info);
    
                        
                        % special instruction block
                        % FFU
                        UpArgs = Args;
    
                        % reload calibration files
                        FN_I = FN_Sci_Groups(IndStartGroup);
                        JDgr = FN_I.juldayFun(@mean);
                        if abs(JDgr-JDlastCalib)>Args.ReloadCalibTimeDiff
                            % if time difference between the last time
                            % calibration was loaded and current image >
                            % 0.7 days, then reload...
                            Obj.loadCalib('FlatNearJD',JDgr, 'BiasNearJD',JDgr, 'ForceReload',true);
                            JDlastCalib = JDgr(1);
    
                            Msg = 'Reload calibration files';
                            Obj.writeLog(Msg, LogLevel.Info);
                        end
    
                        % insert raw images to DB
                        % FFU
    
    
                        % visit found - start reduction
                        [Status, RawImageListFinal, AllSI, MS, Coadd, OnlyMP, AllForcedPhot] = runPipelineI(Obj, RawImageList, FN_I, UpArgs);
    
                        if ~Status.PipeI || ~Status.WriteI
                            % Move images to failed directory:
                            Obj.moveImagesToFailedDir(RawImageList);
                        end
    
                        if Status.PipeI && Status.WriteI && Status.MoveRaw
                            % continue to pipeline II
    
                        end
      
                            
                        % Write ready-to-transfer
                        if Args.UpdateStatusFile
                            writeStatus(Obj, FN_I.genPath);
                            writeStatus(Obj, RawImageListFinal{1});
                        end
    
    
                        Msg = sprintf('Pipeline summary status - PipeI: %d, Write: %d, Move: %d', Status.PipeI, Status.WriteI, Status.MoveRaw);
                        Obj.writeLog(Msg, LogLevel.Info);
                        
    
                        % check if stop loop
                        if Args.StopButton && StopGUI()
                            Cont = false;
                        end
                        if isfile(Args.AbortFileName)
                            Cont = false;
                            delete(Args.AbortFileName);
                        end
        
                        % check disk storage state
                        if ~isempty(Args.StopDiskFull)
                            [~,DiskP] = tools.os.df(sprintf('data%d',Obj.DataDir));
                            if DiskP>Args.StopDiskFull
                                Cont = false;
                                Msg = sprintf('Pipeline stopped because disk is full');
                                Obj.writeLog(Msg, LogLevel.Info);
                            end
                        end 
    
                        if RepackRaw
                            FN_Sci_Groups(IndStartGroup).compress('fz');
                            
                            Msg = 'Re-compress visit';
                            Obj.writeLog(MsgV, LogLevel.Info);  
                        end
                        
                        RunTime = etime(clock, TstartAll);    
                        Msg = sprintf('Visit total run time : %.1f',RunTime);
                        Obj.writeLog(Msg, LogLevel.Info);
    
                    end % if isempty(IndStartGroup)
    
                    if ~Cont
                        % exist the visit loop
                        break;
                    end
         
                    % stop when done
                    if Args.StopWhenDone
                        Cont = false;
                    end
                else
                    % slow down - 
                    if FN_Sci.nFiles<Args.MinInGroup
                        pause(20);
                    end
                end % if FN_Sci.nFiles>Args.MinInGroup

            end % while Cont
            cd(PWD);

        end % function main
        % delete abort msg box
        %Hstop.delete;
    end

    methods % image subtraction
        
        function [Path, FieldName, Telescope, CropID] = getRefPath(Obj, FN)
            % Get path for reference images
            % Input  : - A pipeline.DemonLAST object
            %          - One of the following options:
            %            1. A FileNames object.
            %            2. A cell array of {FieldName, Telescope, CROPID}
            %            3. A vector of [FieldID, Telescope, CROPID]
            %            4. A vector of [RA, Dec, Telescope, CROPID]
            % Output : - A path for the reference images
            %          - Field ID
            %          - Telescope ID
            %          - Crop ID
            % Author : Eran Ofek (Jul 2023)
            % Example: D.getRefPath([1 1 1]) 

            arguments
                Obj
                FN      % FileNames object or {FieldName, Telescope, CROPID} or [FieldID, Telescope, CROPID] or [RA, Dec, Telescope, CROPID]
            end

            if isa(FN, 'FileNames')
                FieldName  = FN.FieldID;
                SpProjName = split(FN.ProjName,'.');
                Telescope  = SpProjName{end};
                CropID     = FN.CropID;
            else
                if iscell(FN)
                    FieldName = FN{1};
                    Telescope = FN{2};
                    CropID    = FN{3};
                else
                    if numel(FN)==3
                        FieldName = FN(1);
                        Telescope = FN(2);
                        CropID    = FN(3);
                    elseif numel(FN)==4
                        RA  = FN(1);
                        Dec = FN(2);
                        FieldName = pipeline.DemonLAST.searchFieldLAST(Obj.FieldList, RA, Dec);
                        Telescope = FN(3);
                        CropID    = FN(4);
                    else
                        error('Numeric FN must contain 3 or 4 elements');
                    end
                end
            end
            if ischar(FieldName)
                % convert field name to field ID
                [FieldName, MinDist, List] = searchFieldLAST(Obj.FieldList, FieldName);
            end
            % FieldName now contains FieldID number
            Path = fullfile(Obj.RefPath, sprintf('%d',FieldName));

        end


    end

    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
