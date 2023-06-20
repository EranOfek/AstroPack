% ImagePath - A class for generating stand storing image/path names
%       for ULTRASAT and LAST.
%
% File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
% Example:
%          D = pipeline.DemonLAST;
%          D.CalibPath = <put here the output directory of the calibration images>
%          D.prepMasterDark
%



classdef DemonLAST < Component
    % 

    properties       
        %
        CI CalibImages   = CalibImages;    % CalibImages

        % These fields are the input parameters for getPath() and getFileName()
        ProjectName  = 'LAST';
        Node         = 1;
        DataDir      = 1;
        CamNumber    = [];
        %HostName     = [];

        BasePath     = [];
   
        NewPath      = 'new';    % if start with '/' then abs path
        CalibPath    = 'calib';  % if start with '/' then abs path
        FailedPath   = 'failed'; % if start with '/' then abs path
        LogPath      = 'log';    % if start with '/' then abs path

        ObsCoo       = [35 30 415];  % [deg deg m]
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

            arguments
                Args.BasePath    = [];
            end
            
            

        end
        
    end
    
    methods % setter/getters
        function Result=get.BasePath(Obj)            
            % getter for BasePath

            if isempty(Obj.BasePath)
                if isempty(Obj.DataDir) && isempty(Obj.CamNumber)
                    Result = [];
                else
                    % get base path
                    Result = pipeline.DemonLAST.getBasePath('DataDir',Obj.DataDir, 'CamNumber',Obj.CamNumber, 'ProjectName',Obj.ProjectName, 'Node',Obj.Node);
                end
            else
                Result         = Obj.BasePath;
                Obj.NewPath    = Obj.DefNewPath;
                Obj.CalibPath  = Obj.DefCalibPath;
                Obj.FailedPath = Obj.DefFailedPath;
                Obj.LogPath    = Obj.DefLogPath;
            end
        end

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

        function set.DataDir(Obj, Val)
            % Set DataDir and modify BasePath
            
            Obj.DataDir = Val;
            Obj.BasePath = Obj.getPath;
            
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
            %          - Mount number.
            % Author : Eran Ofek (Mar 2023)

            HostName = tools.os.get_computer;
            MountNumberStr = HostName(5:6);
            if nargout>1
                MountNumber = str2double(MountNumberStr);
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

    
    methods % utilities

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
                                        'ProjectName',Obj.ProjectName);
            Path = fullfile(BasePath,SubDir);
        end

        function Obj=deleteDayTimeImages(Obj, Args)
            % Delete science images taken when the Sun is above the horizon
            % Input  : - A pipeline.DemonLAST object.
            %          * ...,key,val,...
            %            'TempFileName' - File name template to select.
            %                   Default is '*.fits'.
            %            'Type' - Type of images to delete.
            %                   Default is {'sci','science'}.
            %            'SunAlt' - Sun altitude threshold above to delete
            %                   the images. Default is 0.
            % Output : null
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Obj
                Args.TempFileName = '*.fits';
                Args.Type         = {'sci','science'};
                Args.SunAlt       = 0;
                
            end
            
            PWD = pwd;
            cd(Obj.NewPath);
            
            FN = FileNames.generateFromFileName(Args.TempFileName);
            FN.selectBy('Type',Args.Type);
            
            [SunAlt] = Obj.sunAlt(Obj, 'ObsCoo',Obj.ObsCoo(1:2));
            Flag     = SunAlt>Args.SunAlt;
            
            FN.reorderEntries(Flag);
            io.files.delete_cell(FN.genFile());
            
            cd(PWD);
        end
        
        function moveToDestination(Obj, ListImages, Args)
            % Move list of files in the NewPath dir to destination
            % Input  : - A pipeline.DemonLAST object.
            %          - A file name template char array, or a cell array
            %            of file names, or an FileNames object.
            %          * ...,key,val,...
            %            'Type' - FileNames image Type to select by.
            %                   If empty, then skip. Default is [].
            %            'Level' - Like 'Type', but for 'Level'.
            %            'Product' - Like 'Type', but for 'Product'.
            %            'Destination - Destination to move files to.
            %                   Must be supplied.
            %            'SourcePath' - Source path from which to move files.
            %                   Default is Obj.NewPath.
            % Output : null
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Obj
                ListImages
                Args.Type          = [];
                Args.Level         = [];
                Args.Product       = [];
                Args.Destination
                Args.SourcePath    = Obj.NewPath;
            end
                
             
            if ~isempty(Args.Type) || ~isempty(Args.Level) || ~isempty(Args.Product)
                % select by Type/Level/Product
                if isa(ListImages, 'FileNames')
                    FN = FileNames;
                else
                    if iscell(ListImages)
                        FN = FileNames(ListImages);
                    else
                        FN = FileNames.generateFromFileName(ListImages);
                    end
                end   
                
                if ~isempty(Args.Type)
                    FN.selectByProp('Type',Args.Type, 'CreateNewObj',false);
                end
                if ~isempty(Args.Level)
                    FN.selectByProp('Type',Args.Level, 'CreateNewObj',false);
                end
                if ~isempty(Args.Product)
                    FN.selectByProp('Type',Args.Product, 'CreateNewObj',false);
                end
            else
                if ischar(ListImages)
                    % parse file names
                    ListImages = io.files.filelist(ListImages);
                end
                    
                FN = ListImages; 
            end
                
            if isa(FN, 'FileNames')
                ListImages = FN.genFile;
            else
                ListImages = FN;
            end            
            
            io.files.moveFiles(ListImages, [], Args.SourcePath, Args.Destination);
            
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
            FID = fopen(FileName,'w+');
            fprintf(FID,'%s %s\n',datestr(now,'yyyy-mm-ddTHH:MM:SS'),Args.Msg);
            fclose(FID);

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
            % Output : null
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Obj
                Msg
                Level LogLevel           = LogLevel.Info; % All       Assert    Debug     DebugEx   Error     Fatal     Info      None      Perf      Test      unitTest  Verbose   Warnin
                Args.WriteLog logical    = true;
                Args.WriteDev logical    = false;
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
    
                Nl = numel(Lines);
                for Il=1:1:Nl
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
            % Output : - A pipeline.DemonLAST object in which the Logger
            %            property is updated with the log file name.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Args.ProjName = [];
            end

            if isempty(Args.ProjName)
            end

            [~,~,~,~,ProjName,~]=getPath(Obj);

            FN = FileNames;
            FN.ProjName = ProjName;
            FN.Time     = celestial.time.julday;
            LogFileName = FN.genFile('IsLog',true, 'ReturnChar',true, 'Product','Pipeline');
            Path        = Obj.LogPath;
            mkdir(Path);
            LogFileName = fullfile(Path, LogFileName);

            Obj.Logger.LogF.FileName = LogFileName;

        end

    end

    methods % pipelines
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
                    for Igr=1:1:Ngr
                        DarkList = FN_Dark_Groups(Igr).genFull([]);
                    
                    
                        % prepare master bias
                        CI = CalibImages;
                        
                        CI.createBias(DarkList, 'BiasArgs',Args.BiasArgs, 'Convert2single',true);
        
                        % save processed bias images in raw/ dir
                        
                        %readFromHeader(FN_Dark_Groups(Igr), CI.Bias)
                        
                        
                        %Obj = readFromHeader(, Input, DataProp
                        
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
                    end
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
            end

            PWD = pwd;
            if ~isempty(Args.CalibPath)
                cd(Args.CalibDir)
            else
                cd(Obj.CalibPath);
            end

            % read latest bias image
            if ismember('bias',lower(Args.ReadProduct))
                FN_Bias = FileNames.generateFromFileName(Args.BiasTemplate);
                [~,~,~,FN_Bias] = FN_Bias.selectLastJD;
                Obj.CI.Bias = AstroImage.readFileNamesObj(FN_Bias, 'AddProduct',Args.AddImages);
            end

            % read latest flat image
            if ismember('flat',lower(Args.ReadProduct))
                FN_Flat = FileNames.generateFromFileName(Args.FlatTemplate);
                [~,~,~,FN_Flat] = FN_Flat.selectLastJD;
                Obj.CI.Flat = AstroImage.readFileNamesObj(FN_Flat, 'AddProduct',Args.AddImages);
            end

            % Read linearity file
            % Result = populateLinearity(Obj.CI, Name, Args)
          
            cd(PWD);

        end

        
        
        function Obj=main(Obj, Args)
            % The main LAST pipeline demon.
            %   The demon waits for images in the new/ directory, analyze
            %   the images and distribute them.
            % Example: cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/new
            %          D=pipeline.DemonLAST;
            %          D.setPath('/raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02')
            %

            arguments
                Obj
                Args.DataDir       = 1;
                Args.CamNumber     = [];
                Args.TempRawSci    = '*_sci_raw_*.fits';
                Args.NewSubDir     = 'new';
                Args.MinInGroup    = 5;
                Args.MaxInGroup    = 20;
                Args.SortDirection = 'descend';  % analyze last image first
                Args.AbortFileName = '~/abortPipe';
                Args.StopButton logical = true;
                Args.multiRaw2procCoaddArgs = {};

                Args.StartJD       = -Inf;    % refers onlt to Science observations: JD, or [D M Y]
                Args.EndJD         = Inf;
                Args.RegenCalib logical = false; % Generate a new calib dark/flat images and load - if false: will be loaded once at the start

                Args.DeleteSciDayTime logical = false;
                Args.DeleteSunAlt  = 0;

                Args.FocusTreatment  = 'move';   % 'move'|'keep'|'delete' 
                Args.TempRawFocus    = '*_focus_raw_*.fits';

                Args.MinNumIMageVisit  = 5;
                Args.PauseDay          = 60;
                Args.PauseNight        = 10;

                
                Args.SaveEpochProduct  = {[],[],'Cat'};  %{'Image','Mask','Cat'};
                Args.SaveVisitProduct  = {'Image','Mask','Cat','PSF'};
                Args.SaveMergedCat     = true;
                Args.SaveMergedMat     = true;
                Args.SaveAsteroids     = true;

                Args.Insert2DB         = false;
                Args.DB_InsertRaw      = false;
                Args.DB_Table_Raw      = 'test_raw_images';
                Args.DB_Table_Proc     = 'test_proc_images';
                Args.DB_Table_Coadd    = 'test_coadd_images';
                Args.DB_Table_Cat      = 'test_src_catalog';
                Args.AstroDBArgs cell  = {'Host','10.23.1.25','DatabaseName','last_operational','Port',5432};
            end
            
            ADB = [];

            % get path
            %[NewPath,CameraNumber,Side,HostName,ProjName,MountNumberStr]=getPath(Obj, Args.NewSubDir, 'DataDir',Args.DataDir, 'CamNumber',Args.CamNumber);
            %[BasePath] = getPath(Obj, '', 'DataDir',Args.DataDir, 'CamNumber',Args.CamNumber);
            NewPath    = Obj.NewPath;
            BasePath   = Obj.BasePath;
            FailedPath = Obj.FailedPath;


            PWD = pwd;
            cd(NewPath);

            if numel(Args.StartJD)>1
                Args.StartJD = celestial.time.julday(Args.StartJD);
            end
            if numel(Args.EndJD)>1
                Args.EndJD = celestial.time.julday(Args.EndJD);
            end

            % if ~Args.RegenCalib
            %     [IsEmB, IsEmF] = Obj.CI.isemptyProp({'Bias','Flat'});
            %     if IsEmB || IsEmF
            %         error('For ReloadCalib=false, a populated CalibImages (CI) with Bias and Flat images must be provided');
            %     end
            % end


            if Args.StopButton
                GUI_Text = sprintf('Abort : Pipeline');
                [StopGUI, Hstop]  = tools.gui.stopButton('Msg',GUI_Text);
            end

            Cont = true;
            while Cont
                % set Logger log file 
                Obj.setLogFile;

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
                
                % delete test images taken during daytime
                if Args.DeleteSciDayTime
                    deleteDayTimeImages(Obj, 'SunAlt',Args.DeleteSunAlt);
                end

                % move focus images
                FN_Foc   = FileNames.generateFromFileName(Args.TempRawFocus);
                FN_Foc.BasePath = Obj.BasePath;
                FN_Foc.FullPath = [];
                FN_Foc.moveImages('Operator',Args.FocusTreatment, 'SrcPath',[], 'DestPath', FN_Foc.genPath, 'Level','raw', 'Type','focus');
                
                % look for new images
                FN_Sci   = FileNames.generateFromFileName(Args.TempRawSci, 'FullPath',false);
                [FN_Sci] = selectBy(FN_Sci, 'Product', 'Image', 'CreateNewObj',false);
                [FN_Sci] = selectBy(FN_Sci, 'Type', {'sci','science'}, 'CreateNewObj',false);
                [FN_Sci] = selectBy(FN_Sci, 'Level', 'raw', 'CreateNewObj',false);

                % select observations by date
                FN_JD  = FN_Sci.julday;
                FlagJD = FN_JD>Args.StartJD & FN_JD<Args.EndJD;
                FN_Sci = reorderEntries(FN_Sci, FlagJD);

                [~, FN_Sci_Groups] = FN_Sci.groupByCounter('MinInGroup',Args.MinInGroup, 'MaxInGroup',Args.MaxInGroup);
                FN_Sci_Groups = FN_Sci_Groups.sortByFunJD(Args.SortDirection);
                Ngroup = numel(FN_Sci_Groups);
                
                MaxNfiles = max(FN_Sci_Groups.nfiles);
                if MaxNfiles<=Args.MinNumIMageVisit
                    Msg{1} = 'Waiting for more images to analyze';
                    Obj.writeLog(Msg, LogLevel.Info);
    
                    SunInfo = celestial.SolarSys.get_sun;
                    if SunInfo.Alt>0
                        % Sun above horizon
                        pause(Args.PauseDay)
                    else
                        pause(Args.PauseNight);
                    end
                end
                
                % check if stop loop
                if StopGUI()
                    Cont = false;
                end
                if isfile(Args.AbortFileName)
                    Cont = false;
                    delete(Args.AbortFileName);
                end
                

                for Igroup=1:1:Ngroup
                   
                    % for each visit
                    if FN_Sci_Groups(Igroup).nfiles>Args.MinNumIMageVisit

                        % set Logger log file 
                        Obj.setLogFile;

                        RawImageList = FN_Sci_Groups(Igroup).genFull('FullPath',NewPath);
    
                        FN_Sci_Groups(Igroup).BasePath = BasePath;
                        
                        % call visit pipeline
                        
                        Msg{1} = sprintf('pipline.DemonLAST executing pipeline for group %d - First image: %s',Igroup, RawImageList{end});
                        Obj.writeLog(Msg, LogLevel.Info);

                        try
                         
                            tic;

                            %AI = AstroImage(FilesList, Args.AstroImageReadArgs{:}, 'CCDSEC',Args.CCDSEC);
                            % Insert AI to DB


                            % Instead of AI, it used to be: RawImageList
                            [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd,RawHeader]=pipeline.generic.multiRaw2procCoadd(RawImageList, 'CalibImages',Obj.CI,...
                                                                       Args.multiRaw2procCoaddArgs{:},...
                                                                       'SubDir',NaN,...
                                                                       'BasePath', BasePath,...
                                                                       'SaveAll',false);
    
                            Msg{1} = sprintf('pipline.DemonLAST finish executing pipeline - start saving data',Igroup, RawImageList{end});
                            Obj.writeLog(Msg, LogLevel.Info);
                            
                            %CoaddTransienst = imProc.cat.searchExternalCatOrphans(Coadd);

                            % save data products
                            FN_I = FN_Sci_Groups(Igroup).reorderEntries(1, 'CreateNewObj',true);
        
                            %{'EpochImage', 'EpochMask', 'EpochCat', 'EpochPSF', 'VisitImage','VisitMask', 'VisitCat', 'VisitPSF', 'MergedCat', 'MergedMat', 'MergedAsteroids'};

                            % the following call also update the AllSI.ImageData.FileName
                            [FN_Proc,~,Status] = imProc.io.writeProduct(AllSI, FN_I, 'Product',Args.SaveEpochProduct, 'WriteHeader',[true false true],...
                                                   'Level','proc',...
                                                   'LevelPath','proc',...
                                                   'FindSubDir',true);
                            Obj.writeLog(Status, LogLevel.Info);
        
                            % the following call also update the Coadd.ImageData.FileName
                            [FN_Coadd,~,Status]=imProc.io.writeProduct(Coadd, FN_I, 'Product',Args.SaveVisitProduct, 'WriteHeader',[true false true false],...
                                                   'Level','coadd',...
                                                   'LevelPath','proc',...
                                                   'SubDir',FN_Proc.SubDir);
                            Obj.writeLog(Status, LogLevel.Info);

                            [~,~,Status]=imProc.io.writeProduct(MergedCat, FN_I, 'Product',{'Cat'}, 'WriteHeader',[false],...
                                                   'Save',Args.SaveMergedCat,...
                                                   'Level','merged',...
                                                   'LevelPath','proc',...
                                                   'SubDir',FN_Proc.SubDir);
                            Obj.writeLog(Status, LogLevel.Info);

                            [~,~,Status]=imProc.io.writeProduct(MatchedS, FN_I, 'Product',{'MergedMat'}, 'WriteHeader',[false],...
                                                   'Save',Args.SaveMergedMat,...
                                                   'Level','merged',...
                                                   'LevelPath','proc',...
                                                   'SubDir',FN_Proc.SubDir);
                            Obj.writeLog(Status, LogLevel.Info);

                            [~,~,Status]=imProc.io.writeProduct(ResultAsteroids, FN_I, 'Product',{'Asteroids'}, 'WriteHeader',[false],...
                                                   'Save',Args.SaveAsteroids,...
                                                   'Level','merged',...
                                                   'LevelPath','proc',...
                                                   'SubDir',FN_Proc.SubDir);
                            Obj.writeLog(Status, LogLevel.Info);


                            
                            % if CoaddTransienst.sizeCatalog>0
                            %     [~,~,Status]=imProc.io.writeProduct(CoaddTransienst, FN_I, 'Product',{'TransientsCat'}, 'WriteHeader',[false],...
                            %                            'Level','merged',...
                            %                            'LevelPath','proc',...
                            %                            'SubDir',FN_Proc.SubDir);
                            %     Obj.writeLog(Status, LogLevel.Info);
                            % end

                            % Write images and catalogs to DB
    
    
                            % move raw images to final location
                            RawImageListFinal = FN_Sci_Groups(Igroup).genFull;
                            io.files.moveFiles(RawImageList, RawImageListFinal);
                        
                            % Write ready-to-transfer
                            writeStatus(Obj, FN_Proc.genPath);
                            writeStatus(Obj, fileparts(RawImageListFinal{1}));


                            % Insert images to DB tables
                            % Insert raw images
                            
                            % 
                            if Args.Insert2DB
                                if isempty(ADB)
                                    % connect to DB
                                    ADB = db.AstroDb(Args.AstroDBArgs{:});
                                end
                                % insert raw images into the DB:
                                RawFileName = RawImageListFinal; % regexprep(RawImageList,'.*/','');
                                [ID_RawImage, OK] = ADB.insert(RawHeader, 'Table',Args.DB_Table_Raw, 'FileNames',RawFileName);
                                Msg{1} = sprintf('Insert images to LAST raw images table - success: %d', OK);
                                Obj.writeLog(Msg, LogLevel.Info);
                                                        
                                % insert proc images into the DB:
%                                 FN_Proc.genFull('RemoveLeadingStr', Obj.getBasePathWithOutProjName);
                                ProcFileName = FN_Proc.genFull;
                                [ID_ProcImage, OK] = ADB.insert(AllSI, 'Table',Args.DB_Table_Proc, 'Hash', 0, 'FileNames',ProcFileName);
                                Msg{1} = sprintf('Insert images to LAST proc images table - success: %d', OK);
                                Obj.writeLog(Msg, LogLevel.Info);
                                % there are ~N*24 ProcImages, and only N RawImages
                                ID_RawImage = repmat(ID_RawImage,1, 24);
                                ID_RawImage = ID_RawImage(:);
                                OK = ADB.updateByTupleID(ID_ProcImage, 'raw_image_id', ID_RawImage, 'Table',Args.DB_Table_Proc);

                                % insert coadd images into the DB:
%                                 FN_Coadd.genFull('RemoveLeadingStr', Obj.getBasePathWithOutProjName);
                                CoaddFileName = FN_Coadd.genFull('LevelPath','proc');
                                [ID_ProcImage, OK] = ADB.insert(Coadd, 'Table',Args.DB_Table_Coadd, 'FileNames',CoaddFileName);
                                Msg{1} = sprintf('Insert images to LAST proc images table - success: %d', OK);
                                Obj.writeLog(Msg, LogLevel.Info);
                                
                                % insert coadd catalogs into the DB:
%                                 CatFileName = FN_Cat.genFull('LevelPath','proc');
%                                 CatFileName = '';
%                                 [ID_ProcCat, OK] = ADB.insert(MergedCat, 'Table',Args.DB_Table_Cat, 'FileNames', CatFileName,'Type','cat');
%                                 Msg{1} = sprintf('Insert catalog objects to LAST catalog table - success: %d', OK);
%                                 Obj.writeLog(Msg, LogLevel.Info);

                            end

                            RunTime = toc;
                        catch ME
                             
                            
                            RunTime = toc;
    
                            % extract errors
                            ErrorMsg = sprintf('pipeline.DemonLAST try error: %s / funname: %s @ line: %d', ME.message, ME.stack(1).name, ME.stack(1).line);
                            %warning(ErrorMsg);
                            Obj.writeLog(ErrorMsg, LogLevel.Error);

                            Obj.writeLog(ME, LogLevel.Error);
                            
                            % write log file
    
                            % move images to failed/ dir
                            io.files.moveFiles(RawImageList, FN_Sci_Groups(Igroup).genFull('FullPath',FailedPath));
    
                            ErrorMsg = sprintf('pipeline.DemonLAST: %d images moved to failed directory',numel(RawImageList));
                            %warning(ErrorMsg);
                            Obj.writeLog(ErrorMsg, LogLevel.Error);
                        end
    
    
                        % write summary and run time to log
                        Msg{1} = sprintf('pipeline.DemonLAST / pipeline.generic.multiRaw2procCoadd analyzed %d images starting at %s',numel(RawImageList), FN_Sci_Groups(Igroup).Time{1});
                        Msg{2} = sprintf('pipeline.DemonLAST / pipeline.generic.multiRaw2procCoadd run time [s]: %6.1f', RunTime);
                        Obj.writeLog(Msg, LogLevel.Info);                        
    
                        
                        
                        
                        % check if stop loop
                        if Args.StopButton && StopGUI()
                            Cont = false;
                        end
                        if isfile(Args.AbortFileName)
                            Cont = false;
                            delete(Args.AbortFileName);
                        end
                        if ~Cont
                            % exist the visit loop
                            break;
                        end
                    end
                end
            end
            
            cd(PWD);

        end
        % delete abort msg box
        %Hstop.delete;


    end

    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
