function runPipeLAST(DataNumber, Args)
    % Script for running the LAST pipeline
    % Input  : - data disk number from which to extract the new images.
    %            Default is 1.
    %          * ...,key,val,...
    %            See code
    % Author : Eran Ofek (Jan 2022)
    % Example: pipeline.last.runPipeLAST(1)
    
    
    arguments
        DataNumber                    = 1;
        Args.ProjName                 = [];  %'LAST.01.02.03';
        Args.NodeNumber               = 1;
        
        Args.GeoPos                   = [35 30 400];
        Args.MinNdark                 = 10;
        Args.MinNflat                 = 5;
        Args.multiRaw2procCoaddArgs cell   = {};
        
        Args.BaseArchive              = []; % '/last01e/data1/archive';
        Args.BasePath                 = []; % '/last01e/data1/archive/LAST.1.1.1';
        Args.DataDir                  = ''; % 
        Args.NewFilesDir              = []; %'/last01e/data1/archive/new';
        Args.DarkFlatDir              = []; %'/last01e/data1/archive/calib';
        Args.RegenerateCalib logical  = true;
        
        Args.TimeSinceLast            = 300./86400;
        
        Args.SearchStr                = '*.fits'; 
        Args.DarkSearchStr            = '*_dark_proc*_Image_*.fits';
        Args.FlatSearchStr            = '*flat_proc*_Image_*.fits';
        Args.ScienceSearchStr         = '*_*_clear_*_science_raw*Image*.fits';
        Args.NinBatch                 = 20;
        Args.DefBaseProjName          = 'LAST';
        
        Args.MountNumber              = []; % if given, override MountNumber
        Args.AbortFile                = 'abort';
    end
   
    RAD = 180./pi;
    
    [ProjName, MountNumber, CameraNumber, HostName] = pipeline.last.constructProjName(Args.ProjName, [], Args.MountNumber, DataNumber, Args.NodeNumber, Args.DefBaseProjName);
    

    BaseArchiveDefault =  fullfile(filesep, HostName, sprintf('%s%d','data', DataNumber), 'archive');
    BasePathDefault    =  fullfile(filesep, HostName, sprintf('%s%d','data', DataNumber), 'archive', ProjName);
    if isempty(Args.NewFilesDir)
        % generate NewFilesDir
        Args.NewFilesDir = fullfile(BasePathDefault, filesep, 'new');
    end
    if isempty(Args.DarkFlatDir)
        % generate DarkFlatDir
        Args.DarkFlatDir = fullfile(BasePathDefault, filesep, 'calib');
    end
    
    if isempty(Args.BasePath)
        % generate default BasePath - e.g., '/last01e/data1/archive/LAST.1.12.3'
        Args.BasePath = BasePathDefault;
    end
    if isempty(Args.BaseArchive)
        % generate default BasePath - e.g., '/last01e/data1/archive'
        Args.BaseArchive = BaseArchiveDefault;
    end
    
    StopFile = sprintf('%s%s%s',Args.NewFilesDir, filesep, Args.AbortFile);
    
    
    % switch to new files directory
    cd(Args.NewFilesDir);
    
    %<ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
    MostRecentDarkImage = '';
    MostRecentFlatImage = '';
    
    %CI = CalibImages;
    CI = CalibImages.loadFromDir(Args.DarkFlatDir);
    
    % wait for new images
    Cont = true;
    Counter = 0;
    FailedCounter = 0;
    while Cont
        % move focus files
        %io.files.moveFiles('*.focus*.\.fits',[], Args.NewFilesDir, ...
        
        Args.RegenerateCalib = false;
        if Args.RegenerateCalib || CI.isemptyProp('Bias') || CI.isemptyProp('Flat')
            % look for darks and create master dark
            CounterBias = pipeline.last.prepMasterDark('DataNum',DataNumber,'Node',Args.NodeNumber);
            % look for flats and created master flat
            CounterFlat = pipeline.last.prepMasterFlat('DataNum',DataNumber,'Node',Args.NodeNumber);
            if CounterFlat>0 || CounterBias>0
                % reload Dark/Flat
                CI = CalibImages.loadFromDir(Args.DarkFlatDir);
            end           

            if CI.isemptyProp('Bias') || CI.isemptyProp('Flat')
                error('No dark or flat found');
            end
        end
        

          

        % process images
        % 
        
        
        % move the focus images to the raw directory
        [IP_Foc, ListFoc] = ImagePath.selectByProp('LAST*.fits', {'focus'}, 'Type');
        %IP_Foc       = ImagePath.parseFileName(ListSci);
        IP_Foc.setAllVal('BasePath', Args.BasePath);
        IP_Foc.setAllVal('FormatCounter', '%03d');
        IP_Foc.setAllVal('DataDir','');
        ListFocRaw  = IP_Foc.genFullCell;
        Destination = io.files.moveFiles(ListFoc, ListFocRaw, '', '', 'MkDir',true);
        
        
        % move the focus images to the raw directory
        
        
        % get all files waiting for processing
        %SciFiles = dir(fullfile(Args.NewFilesDir,Args.SearchStr));
        %SciFiles = SciFiles(~[SciFiles.isdir]);
        % convert file names to ImagePath 
        [IP_Sci, ListSci] = ImagePath.selectByProp('LAST*.fits', {'sci','science'}, 'Type');
        IP_Sci       = ImagePath.parseFileName(ListSci);
        IP_Sci.setAllVal('BasePath', Args.BasePath);
        IP_Sci.setAllVal('FormatCounter', '%03d');
        IP_Sci.setAllVal('DataDir','');
        
        %%% NEED to make sure that the processed file are of the same field
        [Groups, ListG] = ImagePath.groupByCounter(IP_Sci);  % BUG
        
        % find the latest image
        %IP.setTime;   % make sure JD is populated
        %IP.sortByJD;
        %IndLatest = findFirstLast(IP, true, 'Image');
        %IndLatest = find([IP.Counter]==Args.NinBatch, 1, 'last');
        %Ind = getAllInCounterSeries(IP, IndLatest, Args.NinBatch);
        
        if ~isempty(Groups)
            % the raw images destination directory
            ListImagesRaw = ListG(Groups(end).I1:Groups(end).I2);
            % the new/ directory
            %ListImagesNew = regexprep(ListImagesRaw,'/\d\d\d\d/\d\d/\d\d/raw','/new');
            ListImagesNew = io.files.replaceFilePath(ListImagesRaw, Args.NewFilesDir);
            
            % execute the pipeline
            Counter = Counter + 1;
            %%%% problem: FieldID is wrong
            
            
            %LogFileName = sprintf('/home/ocs/%s_log_pipeline.log',ProjName);
            %FID = fopen(LogFileName, 'a+');
            
            try
                tic;
                pipeline.generic.multiRaw2procCoadd(ListImagesNew, 'CalibImages',CI, Args.multiRaw2procCoaddArgs{:}, 'SubDir',NaN, 'BasePath', Args.BaseArchive);
                toc
                
                Destination = io.files.moveFiles(ListImagesNew, ListImagesRaw, '', '', 'MkDir',true);
                
                fprintf('Sucess\n\n');

%                 PWD = pwd;
%                 cd(Destination)
%                 FID = fopen('.status','w+');
%                 fprintf('FID','%s ready-for-transfer',datestr(now,'yyyy-mm-ddTHH:MM:SS'));
%                 fclose(FID);
%                 cd(PWD);
                
            catch ME
                
                
                if exist('ME.mat','file')>0
                    load MEx.mat
                    FailedCounter = numel(MEx);
                end
                
                FailedCounter = FailedCounter + 1;
                
                MEx(FailedCounter).FailedCounter = FailedCounter;
                MEx(FailedCounter).ME            = ME;
                
                save -v7.3 MEx.mat MEx
                
                
                
                warning('Failed sequence');
                % failed
                % copy images to failed directory
                ListImagesFailed = regexprep(ListImagesNew,'/new','/failed');
                %DirFailed        = fileparts(ListImagesFailed{1});
                io.files.moveFiles(ListImagesNew, ListImagesFailed, '', '', 'MkDir',true);
            end
            
            
            Counter
            
            % move images to path
%            Destination = io.files.moveFiles(ListImages, ListImages, Args.NewFilesDir, Path, 'MkDir',true);
%             % move images to path
%             for Iim=1:1:Args.NinBatch
%                 Source      = sprintf('%s%s%s', Args.NewFilesDir, filesep, ListImages{Iim});
%                 Destination = sprintf('%s%s%s', Path, filesep, ListImages{Iim});
%                 % make sure diirectory exist
%                 mkdir(Path);
%                 % move file
%                 movefile(Source, Destination);
%             end
            
            % FFU: write log file
        
        else
            % wait for image
            Sun = celestial.SolarSys.get_sun([], Args.GeoPos(1:2)./RAD);
            if Sun.Alt>0
                % day time
                pause(60);
            else
                % night time
                pause(2);
            end
        end
        
        % check if Cont state changed
        if exist(StopFile, 'file')>0
            Cont = false;
        end
    end
end



