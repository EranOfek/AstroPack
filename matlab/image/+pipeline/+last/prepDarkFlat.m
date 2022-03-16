function Found = prepDarkFlat(Args)
    % Look for dark images and combine when ready
    % Example: pipeline.last.prepDarkFlat
    
    
    arguments
        Args.Type                     = 'dark';  % 'dark' | 'flat'
        Args.NewFilesDir              = [];
        Args.CalibDir                 = [];
        Args.SearchStr                = '*_dark_raw*_Image_*.fits';
        Args.DarkSearchStr            = '*_dark_proc*_Image_*.fits';
        Args.ModNumber                = 20;
        Args.MinNimages               = 18;
        Args.WaitForMoreImages        = 40;   % [s]
    end
    
    % files of interest
    
    PWD = pwd;
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
            [Ind, IndCounter] = getAllInCounterSeries(IP, RecentImage, Args.ModNumber);

            NInd = numel(Ind);
            if NInd>PrevNInd
                % number of images changed - wait more time
                pause(Args.WaitForMoreImages);
            else
                Cont = false;
            end
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
                        Source      = sprintf('%s%s%s', Args.NewFilesDir, filesep, Files{Iim});
                        Destination = sprintf('%s%s%s', Path, filesep, Files{Iim});
                        % make sure diirectory exist
                        mkdir(Path);
                        % move file
                        movefile(Source, Destination);
                    end
                    
                case 'flat'
                    % read most recent bias
                    [FoundDark, RecentDarkImage, RecentDarkMask] = io.files.searchNewFilesInDir(Args.CalibDir, Args.DarkSearchStr, '_Image_',{'_Mask_'});
                    % add full path
                    RecentDarkImage = sprintf('%s%s%s',Args.DarkFlatDir, filesep, RecentDarkImage);
                    RecentDarkMask  = sprintf('%s%s%s',Args.DarkFlatDir, filesep, RecentDarkMask{1});
        
                    CI.Bias = AstroImage(RecentDarkImage, 'Mask',RecentDarkMask);
                    
                    
                    FlatImages = CI.debias(Files);
                    FlatImages.setKeyVal('FILTER','clear');

                    CI.createFlat;
                    
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
                        Source      = sprintf('%s%s%s', Args.NewFilesDir, filesep, Files{Iim});
                        Destination = sprintf('%s%s%s', Path, filesep, Files{Iim});
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
