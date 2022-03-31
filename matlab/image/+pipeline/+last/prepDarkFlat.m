function Found = prepDarkFlat(Args)
    % Look for dark images and combine when ready
    % Example:
    % pipeline.last.prepDarkFlat('NewFilesDir','/last02w/data1/archive/LAST.01.01.03/new','CalibDir','/last02w/data1/archive/LAST.01.01.03/calib','BasePath','/last02w/data1/archive');
    % pipeline.last.prepDarkFlat('Type','flat','NewFilesDir','/last02w/data1/archive/LAST.01.01.03/new','CalibDir','/last02w/data1/archive/LAST.01.01.03/calib','BasePath','/last02w/data1/archive');
    
    
    
    arguments
        Args.Type                     = 'dark';  % 'dark' | 'flat' | 'twflat'
        Args.NewFilesDir              = [];
        Args.CalibDir                 = [];
        Args.BasePath                 = [];
        
        Args.SearchStr                = []; %'*_dark_raw*_Image_*.fits';
        Args.DarkSearchStr            = '*_dark_proc*_Image_*.fits'; % needed for the flat
        Args.ModNumber                = 20;
        Args.MinNimages               = 3; % 8; % 18;
        Args.WaitForMoreImages        = 40;   % [s]
        Args.KeyFilter                = 'FILTER';
        
        Args.Verbose logical          = false;
        
    end
    
    if isempty(Args.SearchStr)
        switch lower(Args.Type)
            case 'dark'
                Args.SearchStr = '*_dark_raw*_Image_*.fits';
            case {'flat','twflat'}
                Args.SearchStr = '*flat_raw*_Image_*.fits';
            otherwise
                error('Unknown Type option');
        end
    end
    % files of interest
    if isempty(Args.CalibDir) || isempty(Args.NewFilesDir) || isempty(Args.BasePath)
        error('CalibDir, NewFilesDir and BasePath must be provided');
    end
    
    PWD = pwd;
    mkdir(Args.CalibDir);
    mkdir(Args.NewFilesDir);
    cd(Args.NewFilesDir);
   
    
    Files = io.files.dirSortedByDate(Args.SearchStr);
    List  = {Files.name};
    if isempty(List)
        Found = false;
    else
        Cont = true;
        PrevNInd = 0;
        while Cont
            Files             = io.files.dirSortedByDate(Args.SearchStr);
            RecentImage       = Files(end).name;
            IP                = ImagePath.parseFileName(List);
            %IP.genFile
            [Ind, IndCounter] = getAllInCounterSeries(IP, RecentImage, Args.ModNumber);

            NInd = numel(Ind);
            if NInd>PrevNInd
                % number of images changed - wait more time
                pause(Args.WaitForMoreImages);
                PrevNInd = NInd;
            else
                Cont = false;
            end
            
        end
        
        Nip = numel(IP);
        [IP(1:Nip).BasePath] = deal(Args.BasePath);
        for I=1:1:numel(IP)
            IP(I).DataDir = IP(I).ProjName;
        end
        
        if NInd>=Args.MinNimages
            % found enough images
            % combine images
            
            CI = CalibImages;
            switch lower(Args.Type)
                case 'dark'
                    CI.createBias(Files);
                    
                    % save data
                    DarkIP = IP(1).copy;
                    DarkIP.Level   = 'proc';
                    DarkIP.Product = 'Image';
                    DarkName = [Args.CalibDir, filesep, DarkIP.genFile];
                    write1(CI.Bias, DarkName, DarkIP.Product);
                    
                    DarkIP.Product = 'Var';
                    DarkName = [Args.CalibDir, filesep, DarkIP.genFile];
                    write1(CI.Bias, DarkName, DarkIP.Product);
                    
                    DarkIP.Product = 'Mask';
                    DarkName = [Args.CalibDir, filesep, DarkIP.genFile];
                    write1(CI.Bias, DarkName, DarkIP.Product);
                    
                    % move files to date directory
                    Nfiles = numel(Files);
                    Path           = IP(1).genPath;
                    for Iim=1:1:Nfiles
                        Source      = sprintf('%s%s%s', Args.NewFilesDir, filesep, Files(Iim).name);
                        Destination = sprintf('%s%s%s', Path, filesep, Files(Iim).name);
                        % make sure diirectory exist
                        mkdir(Path);
                        % move file
                        movefile(Source, Destination);
                    end
                    
                case {'flat','twflat'}
                    % read most recent bias
                    [FoundDark, RecentDarkImage, RecentDarkMask] = io.files.searchNewFilesInDir(Args.CalibDir, Args.DarkSearchStr, '_Image_',{'_Mask_'});
                    % add full path
                    RecentDarkImage = sprintf('%s%s%s',Args.CalibDir, filesep, RecentDarkImage);
                    RecentDarkMask  = sprintf('%s%s%s',Args.CalibDir, filesep, RecentDarkMask{1});
        
                    CI.Bias = AstroImage(RecentDarkImage, 'Mask',RecentDarkMask);
                    
                    AI = AstroImage({Files.name});
                    FlatImages = CI.debias(AI);
                    %FlatImages.setKeyVal('FILTER','clear');
                    %StF = FlatImages(1).getStructKey(Args.KeyFilter);
                    
                    CI.createFlat(FlatImages); %, StF.(Args.KeyFilter));
                    
                    % save data
                    FlatIP = IP(1).copy;
                    FlatIP.Level   = 'proc';
                    FlatIP.Product = 'Image';
                    FlatName = [Args.CalibDir, filesep, FlatIP.genFile];
                    write1(CI.Flat, FlatName, FlatIP.Product);
                    
                    FlatIP.Product = 'Var';
                    FlatName = [Args.CalibDir, filesep, FlatIP.genFile];
                    write1(CI.Flat, FlatName, FlatIP.Product);
                    
                    FlatIP.Product = 'Mask';
                    FlatName = [Args.CalibDir, filesep, FlatIP.genFile];
                    write1(CI.Flat, FlatName, FlatIP.Product);
                    
                    % move files to date directory
                    Nfiles = numel(Files);
                    Path           = IP(1).genPath;
                    for Iim=1:1:Nfiles
                        Source      = sprintf('%s%s%s', Args.NewFilesDir, filesep, Files(Iim).name);
                        Destination = sprintf('%s%s%s', Path, filesep, Files(Iim).name);
                        % make sure diirectory exist
                        mkdir(Path);
                        % move file
                        movefile(Source, Destination);
                    end
                    
                    
                otherwise
                    error('Unknown Type option');
            end
        else
            % Not enough images - faulty series
            % go out
        end
    end    
    
    cd(PWD);

end
