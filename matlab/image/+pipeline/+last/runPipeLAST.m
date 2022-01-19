function cd(Args)
    % Script for running the LAST pipeline
    % Input  : * ...,key,val,...
    %            See code
    % Author : Eran Ofek (Jan 2022)
    % Example: 
    
    arguments
        Args.GeoPos                   = [35 30 400];
        Args.MinNdark                 = 10;
        Args.MinNflat                 = 5;
        Args.multiRaw2procArgs cell   = {};
        
        Args.NewFilesDir              = '/last01e/data1/archive/new';
        Args.DarkFlatDir              = '/last01e/data1/archive/calib';
        
        Args.SearchStr                = '*.fits'; 
        Args.DarkSearchStr            = '*_dark_proc_*Image_*.fits';
        Args.FlatSearchStr            = '*_flat_proc_*Image_*.fits';
        Args.ScienceSearchStr         = '*_*_clear_*_20_*_science_raw*Image*.fits';
        Args.NinBatch                 = 20;
    end
   
    RAD = 180./pi;
    
    StopFile = sprintf('%s%s%s',Args.NewFilesDir, filesep, 'stop');
    
    
    %<ProjName>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<counter>_<CCDID>_<CropID>_<type>_<level>.<sublevel>_<product>_<version>.<FileType>
    MostRecentDarkImage = '';
    MostRecentFlatImage = '';
    
    CI = CalibImages;
    
    % wait for new images
    Cont = true;
    while Cont
        % check if there is a new Dark/Flat
        [FoundDark, RecentDarkImage, RecentDarkMask] = io.files.searchNewFilesInDir(Args.DarkFlatDir, Args.DarkSearchStr, '_Image_',{'_Mask_'});
        [FoundFlat, RecentFlatImage, RecentFlatMask] = io.files.searchNewFilesInDir(Args.FlatFlatDir, Args.FlatSearchStr, '_Image_',{'_Mask_'});
        
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
          
        % search for the last image (counter=20) in a sequence of images
        [FoundScience, RecentScienceImage] = io.files.searchNewFilesInDir(Args.NewFilesDir, Args.ScienceSearchStr);
        IP = ImagePath.parseFileName(RecentScienceImage);
        Path = IP.genPath;
        
        if FoundScience
            ListImages = cell(1,Args.NinBatch);
            for Iim=1:1:Args.NinBatch
                IP.Counter = Iim;
                ListImages{Iim} = IP.genFile;
            end
           
            % execute the pipeline
            pipeline.generic.multiRaw2proc(ListImages, 'CalibImages',CI, Args.multiRaw2procArgs{:});
            
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
