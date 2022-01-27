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
        Args.GeoPos                   = [35 30 400];
        Args.MinNdark                 = 10;
        Args.MinNflat                 = 5;
        Args.multiRaw2procArgs cell   = {};
        
        Args.NewFilesDir              = []; %'/last01e/data1/archive/new';
        Args.DarkFlatDir              = []; %'/last01e/data1/archive/calib';
        
        
        Args.SearchStr                = '*.fits'; 
        Args.DarkSearchStr            = '*_dark_proc_*Image_*.fits';
        Args.FlatSearchStr            = '*_flat_proc_*Image_*.fits';
        Args.ScienceSearchStr         = '*_*_clear_*_science_raw*Image*.fits';
        Args.NinBatch                 = 20;
    end
   
    RAD = 180./pi;
    
    if isempty(Args.NewFilesDir)
        % generate NewFilesDir
        HostName = tools.os.get_computer;
        Args.NewFilesDir = sprintf('%s%s%s%s%d%s%s%s%s',filesep, HostName, filesep, 'data', DataNumber, filesep, 'archive',filesep,'new');
    end
    if isempty(Args.DarkFlatDir)
        % generate DarkFlatDir
        HostName = tools.os.get_computer;
        Args.DarkFlatDir = sprintf('%s%s%s%s%d%s%s%s%s',filesep, HostName, filesep, 'data', DataNumber, filesep, 'archive',filesep,'calib');
    end
    
    
    StopFile = sprintf('%s%s%s',Args.NewFilesDir, filesep, 'stop');
    
    %<ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
    MostRecentDarkImage = '';
    MostRecentFlatImage = '';
    
    CI = CalibImages;
    
    % wait for new images
    Cont = true;
    Counter = 0;
    while Cont
        % check if there is a new Dark/Flat
        [FoundDark, RecentDarkImage, RecentDarkMask] = io.files.searchNewFilesInDir(Args.DarkFlatDir, Args.DarkSearchStr, '_Image_',{'_Mask_'});
        [FoundFlat, RecentFlatImage, RecentFlatMask] = io.files.searchNewFilesInDir(Args.DarkFlatDir, Args.FlatSearchStr, '_Image_',{'_Mask_'});
        
        if (~FoundDark || ~FoundFlat)
            % may be a probelm - Dark | Flat master images are not found on
            % disk
            
            if isempty(CI.Bias.Image) || isempty(CI.Flat.Image)
                warning('No flat or dark available');
                pasue(30);
            end
            
            
        else
            if strcmp(MostRecentDarkImage, RecentDarkImage)
                % most recent dark is already loaded
            else
                % load dark image
                CI.Bias = AstroImage(RecentDarkImage, 'Mask',RecentDarkMask{1});
            end
            
            if strcmp(MostRecentFlatImage, RecentFlatImage)
                % most recent flat is already loaded
            else
                % load flat image
                CI.Flat = AstroImage(RecentFlatImage, 'Mask',RecentFlatMask{1});
            end
        end
          
        % get all files waiting for processing
        SciFiles = dir(Args.NewFilesDir);
        SciFiles = SciFiles(~[SciFiles.isdir]);
        % convert file names to ImagePath 
        IP       = ImagePath.parseFileName({SciFiles.name});
        % find the latest image
        IP.setTime;   % make sure JD is populated
        IP.sortByJD;
        %IndLatest = findFirstLast(IP, true, 'Image');
        IndLatest = find([IP.Counter]==Args.NinBatch, 1, 'last');
        Ind = getAllInCounterSeries(IP, IndLatest, Args.NinBatch);
        
        
        if ~isempty(Ind)
            
            IP.setAllVal(IP, 'FormatCounter', '%d');
            IP(Ind).genFile;
            ListImages = {IP(Ind).FileName};
            
            % execute the pipeline
            Counter = Counter + 1;
            pipeline.generic.multiRaw2proc(ListImages, 'CalibImages',CI, Args.multiRaw2procArgs{:}, 'SubDir',Counter);
            
            % move images to path
            for Iim=1:1:Args.NinBatch
                Source      = sprintf('%s%s%s', Args.NewFilesDir, filesep, ListImages{Iim});
                Destination = sprintf('%s%s%s', Path, filesep, ListImages{Iim});
                movefile(Source, Destination);
            end
        
        else
            % wait for image
            Sun = celestial.SolarSys.get_sun([], Args.GeoPos(1:2)./RAD);
            if Sun.Alt>0
                % day time
                pause(60);
            else
                % night time
                pasue(2);
            end
        end
        
        % check if Cont state changed
        if exist(StopFile, 'file')>0
            Cont = false;
        end
    end
end
