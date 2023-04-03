% ImagePath - A class for generating stand storing image/path names
%       for ULTRASAT and LAST.
%
% File name format: <ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
% Example:
%          D = pipeline.DemonLAST;
%          D.CalibPath = <put here the output directory of the calibration images>
%          D.D.prepMasterDark
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

        BasePath     = [];
        NewPath      = 'new';    % if start with '/' then abs path
        CalibPath    = 'calib';  % if start with '/' then abs path
        FailedPath   = 'failed'; % if start with '/' then abs path
        
    end
    
    properties (Hidden)
        % Fields formatting
        %FormatFieldID   = '%06d';       % Used with FieldID
        FormatCounter   = '%03d';       % Used with Counter        
        FormatCCDID     = '%03d';       % Used with CCDID
        FormatCropID    = '%03d';       % Used with CropID
        FormatVersion   = '%03d';       % Used with Version
        
    end

    properties (Hidden, SetAccess=protected, GetAccess=public)
     
    end
    
    properties (Hidden, Constant)
        ListType        = { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'focus', 'sci', 'wave', 'type' };
        ListLevel       = {'log', 'raw', 'proc', 'stack', 'ref', 'coadd', 'merged', 'calib', 'junk'};
        ListProduct     = { 'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat', 'Asteroids'};
    end
    
    
    methods % Constructor
       
        function Obj = FileNames(Pars)
            % Constructor for DemonLAST
            
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
                Result = Obj.BasePath;
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
            HostName = 'last02w'
            MountNumberStr = HostName(5:6);
         
            [CameraNumber,Side] = pipeline.DemonLAST.dataDir2cameraNumber(DataDir,HostName);
            ProjName            = pipeline.DemonLAST.constructProjectName(Args.ProjectName, Args.Node, MountNumberStr, CameraNumber);

            % e.g., '/last02e/data1/archive/LAST.01.02.01'

            DataStr = sprintf('data%d',DataDir);
            BasePath = fullfile(filesep,HostName,DataStr,'archive',ProjName);

        end

    end

    
    methods % utilities

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

    end

    methods % pipelines
        function Obj=prepMasterDark(Obj, Args)
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
            %                   Default is false (i.e., working in current
            %                   dir).
            %            'Repopulate' - A logical indicating if to
            %                   repopulate the dark image even if it is already
            %                   exist.
            %            'ClearVar' - Clear the Var image from the
            %                   CalibImages. Default is true.
            % Output : - A pipeline.DemonLAST with updated CI property.
            %          * Write files to disk
            % Author : Eran Ofek (Apr 2023)
            % Example: D = pipeline.DemonLAST;
            %          D.CalibPath = <put here the output directory of the calibration images>
            %          D.D.prepMasterDark

            arguments
                Obj
                Args.FilesList            = '*dark*.fits';
                Args.BiasArgs             = {};
                Args.MinDT                = 1./1440;  % [d]
                Args.WaitForMoreImages    = 10;   % [s]
                Args.MinInGroup           = 9;
                Args.move2newPath logical = false;
                Args.Repopulate logical   = true;
                Args.ClearVar logical     = true;
            end

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
                    end
    
                    % identify new bias images
                    [FN_Dark,Flag] = selectBy(FN, 'Type', {'dark','bias'}, 'CreateNewObj',true);
    
                    % check that the dark images are ready and group by night
                    [Ind, LastJD, DT] = selectLastJD(FN_Dark);
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
                        pasue(Args.WaitForMoreImages)
                    end
                end
                
                % group dark images by time groups
                [~, FN_Dark_Groups] = groupByCounter(FN_Dark, 'MinInGroup',Args.MinInGroup);
                
                Ngr = numel(FN_Dark_Groups);
                for Igr=1:1:Ngr
                    DarkList = FN_Dark_Groups(Igr).genFull([]);
                
                
                    % prepare master bias
                    CI = CalibImages;
                    
                    CI.createBias(DarkList, 'BiasArgs',Args.BiasArgs);
    
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
                % no need to create a Master bias - already exist
            end
            
        end
        
        function Obj=prepMasterFlat(Obj, Args)
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
            % Output : - A pipeline.DemonLAST with updated CI property.
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
            end

            if ~exist(Obj.CI, 'Bias',{'Image','Mask'})
                error('Can not execute prepMasterFlat without bias/dark images');
            end

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
                    end
    
                    % identify new flat images
                    [FN_Flat,Flag] = selectBy(FN, 'Type', {'twflat','flat'}, 'CreateNewObj',true);
    
                    % check that the dark images are ready and group by night
                    [Ind, LastJD, DT] = selectLastJD(FN_Flat);
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
                        pasue(Args.WaitForMoreImages)
                    end
                end
                
                % group flat images time

                [~, FN_Flat_Groups] = groupByTimeGaps(FN_Flat, 'MinInGroup',Args.MinInGroup);
                
                Ngr = numel(FN_Flat_Groups);
                for Igr=1:1:Ngr
                    FlatList = FN_Flat_Groups(Igr).genFull([]);
                
                
                    % prepare master flat
                    
                    % read the images
                    AI = AstroImage(FlatList);

                    % subtract bias/dark
                    AI = imProc.dark.debias(AI, Obj.CI.Bias, Args.debiasArgs{:});

                    CI = CalibImages;
                    
                    CI.createFlat(AI, 'FlatArgs',Args.FlatArgs);
    
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
                    write1(CI.Flat, FileN{1}, 'Image');
                    FN_Master.Product  = {'Mask'};
                    FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                    write1(CI.Flat, FileN{1}, 'Mask');
                    FN_Master.Product  = {'Var'};
                    FileN = FN_Master.genFull('FullPath',Obj.CalibPath);
                    write1(CI.Flat, FileN{1}, 'Var');
                    
                    % keep in CI.Bias 
                    Obj.CI.Flat = CI.Flat;
                    
                    if Args.ClearVar
                        Obj.CI.Flat.Var = [];
                    end
                end
            else
                % no need to create a Master bias - already exist
            end

        end
        
        function Obj=loadCalib(Obj, Args)
            % load CalibImages into the pipeline.DemonLAST object
            % Input  : - A pipeline.DemonLAST object.
            %          * ...,key,val,...
            %            'CalibPath' - A path to the calib images. If
            %                   empty, use the object CalibPath property.
            %                   Default is [].
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
            FN_Bias = FileNames.generateFromFileName(Args.BiasTemplate);
            [~,~,~,FN_Bias] = FN_Bias.selectLastJD;
            Obj.CI.Bias = AstroImage.readFileNames(FN_Bias, 'AddProduct',Args.AddImages);

            % read latest flat image
            FN_Flat = FileNames.generateFromFileName(Args.FlatTemplate);
            [~,~,~,FN_Flat] = FN_Flat.selectLastJD;
            Obj.CI.Flat = AstroImage.readFileNames(FN_Flat, 'AddProduct',Args.AddImages);

            % Read linearity file
            % Result = populateLinearity(Obj.CI, Name, Args)
          
            cd(PWD);

        end

        function Obj=main(Obj, Args)
            %

            arguments
                Obj
                Args
            end

            GUI_Text = sprintf('Abort : Pipeline : %d.%d%d',Node, Mount, Camera);
            StopGUI  = tools.gui.stopButton('Msg',GUI_Text);
    %          % next you can check
    %          if StopGUI(), % do something; end

            Cont = true;
            while Cont


            end

        end




        function runPipeline(Obj, Args)
            %
            
            arguments
                Obj
                Args.List       = 'LAST*.fits';
                Args.DataDir    = [];
                Args.CamNumber  = [];

                Args.SubDir     = 'new';

                Args.Verbose    = true;
                Args.AbortFile  = '~/abortPipe';
            end

            StopGUI = tools.gui.stopButton('Msg','Terminate pipeline.DemonLAST/runPipeline');

            [Path,CameraNumber,Side,HostName,ProjName,MountNumberStr] = getPath(Obj, Args.SubDir, 'DataDir',Args.DataDir, 'CamNumber',Args.CamNumber);
            
            % cd to new files directory
%             if ~isfolder(Path)
%                 error('Folder %s doesnt exist on computer',Path);
%             end
%             cd(Path);

            Cont = true;

            Counter = 0;
            while Cont
                pause(1);
                Counter = Counter + 1;

                if Args.Verbose
                    fprintf('pipeline counter: %d\n',Counter);
                end

                % find all images in directory
                FN = FileNames.generateFromFileName(Args.List);

                % identify new bias images
                [FN_Dark,Flag] = selectBy(Obj, 'Type', {'dark','bias'}, "CreateNewObj",true);
                
                % prepare master bias

                % copy processed bias images to raw/ dir

                % identify new flat images

                % prepare master flat

                % copy processed flat images to raw/ dir

                % identify non science images

                % copy non science images to unproc/ dir

                % load calib file (if new exist)

                % group images

                try
                    % run pipeline on group

                catch
                    % move images to failed/ dir

                end

                % check if continue
                if StopGUI()
                    Cont = false;
                end
                if isfile(Args.AbortFile)
                    delete('Args.AbortFile');
                    Cont = false;
                end

            end


        end
    end

    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
