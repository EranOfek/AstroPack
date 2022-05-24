function runPipeLAST(DataNumber, Args)
    % Script for running the LAST pipeline
    % Input  : - data disk number from which to extract the new images.
    %            Default is 1.
    %          * ...,key,val,...
    %            See code
    % Author : Eran Ofek (Jan 2022)
    % Example: 
    
    
    arguments
        DataNumber                    = 1;
        Args.ProjName                 = [];  %'LAST.1.12.3';
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
    HostName = tools.os.get_computer;
    
    if isempty(Args.ProjName)
        % ProjName is empty - construct the default LAST camera address:
        switch HostName(end)
            case 'e'
                % East computer controls cameras 1 & 2
                CameraNumber = DataNumber + 0;
            case 'w'
                % East computer controls cameras 3 & 4
                CameraNumber = DataNumber + 2;
            otherwise
                error('Unknown host name template')
        end
        if isempty(Args.MountNumber)
            MountNumber = HostName(5:6);
        else
            if isnumeric(Args.MountNumber)
                Args.MountNumber = sprintf('%02d',Args.MountNumber);
            end
            MountNumber = Args.MountNumber;
        end
        Args.ProjName = sprintf('%s.%02d.%s.%02d', Args.DefBaseProjName, Args.NodeNumber, MountNumber, CameraNumber);
    end
    
    BaseArchiveDefault =  fullfile(filesep, HostName, sprintf('%s%d','data', DataNumber), 'archive');
    BasePathDefault    =  fullfile(filesep, HostName, sprintf('%s%d','data', DataNumber), 'archive', Args.ProjName);
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
        % generate default BasePath - e.g., '/last01e/data1/archive/LAST.1.12.3'
        Args.BaseArchive = BaseArchiveDefault;
    end
    
    StopFile = sprintf('%s%s%s',Args.NewFilesDir, filesep, Args.AbortFile);
    
    %<ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
    MostRecentDarkImage = '';
    MostRecentFlatImage = '';
    
    CI = CalibImages;
    
    % wait for new images
    Cont = true;
    Counter = 0;
    while Cont
        % move focus files
        %io.files.moveFiles('*.focus*.\.fits',[], Args.NewFilesDir, ...
        
        % look for darks and create master dark
        pipeline.last.prepDarkFlat('NewFilesDir',Args.NewFilesDir, 'CalibDir',Args.DarkFlatDir, 'BasePath',Args.BaseArchive);
        % look for flats and created master flat
        pipeline.last.prepDarkFlat('Type','flat','NewFilesDir',Args.NewFilesDir, 'CalibDir',Args.DarkFlatDir, 'BasePath',Args.BaseArchive);
        
        % check if there is a new Dark/Flat
        [FoundDark, RecentDarkImage, RecentDarkMask] = io.files.searchNewFilesInDir(Args.DarkFlatDir, Args.DarkSearchStr, '_Image_',{'_Mask_'});
        [FoundFlat, RecentFlatImage, RecentFlatMask] = io.files.searchNewFilesInDir(Args.DarkFlatDir, Args.FlatSearchStr, '_Image_',{'_Mask_'});
        
        % add full path
        if ~isempty(RecentDarkImage)
            RecentDarkImage = sprintf('%s%s%s',Args.DarkFlatDir, filesep, RecentDarkImage);
            RecentDarkMask  = sprintf('%s%s%s',Args.DarkFlatDir, filesep, RecentDarkMask{1});
        end
        
        if ~isempty(RecentFlatImage)
            RecentFlatImage = sprintf('%s%s%s',Args.DarkFlatDir, filesep, RecentFlatImage);
            RecentFlatMask  = sprintf('%s%s%s',Args.DarkFlatDir, filesep, RecentFlatMask{1});
        end
        
        
        if (~FoundDark || ~FoundFlat)
            % may be a probelm - Dark | Flat master images are not found on
            % disk
            
            if isempty(CI.Bias.Image) || isempty(CI.Flat.Image)
                warning('No flat or dark available');
                pause(30);
            end
            
            
        else
            if strcmp(MostRecentDarkImage, RecentDarkImage)
                % most recent dark is already loaded
            else
                % load dark image
                CI.Bias = AstroImage(RecentDarkImage, 'Mask',RecentDarkMask);
            end
            
            if strcmp(MostRecentFlatImage, RecentFlatImage)
                % most recent flat is already loaded
            else
                % load flat image
                CI.Flat = AstroImage(RecentFlatImage, 'Mask',RecentFlatMask);
            end
        end
          
        % get all files waiting for processing
        SciFiles = dir(fullfile(Args.NewFilesDir,Args.SearchStr));
        SciFiles = SciFiles(~[SciFiles.isdir]);
        % convert file names to ImagePath 
        IP       = ImagePath.parseFileName({SciFiles.name});
        IP.setAllVal('BasePath', Args.BasePath);
        IP.setAllVal('DataDir',  Args.DataDir);
        IP.setAllVal('ProjName', Args.ProjName);
        
        %%% NEED to make sure that the processed file are of the same field
        
        
        % find the latest image
        IP.setTime;   % make sure JD is populated
        IP.sortByJD;
        %IndLatest = findFirstLast(IP, true, 'Image');
        IndLatest = find([IP.Counter]==Args.NinBatch, 1, 'last');
        Ind = getAllInCounterSeries(IP, IndLatest, Args.NinBatch);
        
        
        if ~isempty(Ind)
            
            IP.setAllVal('FormatCounter', '%03d');
            IP(Ind).genFile;
            Path           = IP(1).genPath;
            ListImages     = {IP(Ind).FileName};
            ListImagesFull = io.files.addPathToFiles(ListImages, Args.NewFilesDir);
            
            % execute the pipeline
            Counter = Counter + 1;
            tic;
            pipeline.generic.multiRaw2procCoadd(ListImagesFull, 'CalibImages',CI, Args.multiRaw2procCoaddArgs{:}, 'SubDir',sprintf('%d',Counter), 'BasePath', Args.BaseArchive);
            toc
            
            % move images to path
            Destination = io.files.moveFiles(ListImages, ListImages, Args.NewFilesDir, Path, 'MkDir',true);
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



