function prepMasterDark(Args)
    % Look for new files and prepare master dark for LAST
    % Input  : - 
    % Example: pipeline.last.prepMasterDark
    
    
    arguments
        Args.DataNum                  = 1;   % disk data# number
        Args.Node                     = 1;
        Args.ProjName                 = [];
        Args.Type                     = 'dark';
        Args.FileTemplate             = [];  % if empty will be constructed: 'LAST.01.02.03_*_dark*.fits'
        
        Args.NewFilesDir              = [];  % '/last02w/data1/archive/LAST.01.02.03/new'
        Args.CalibDir                 = [];  % '/last02w/data1/archive/LAST.01.02.03/calib'
        Args.BasePath                 = [];  % '/last02w/data1/archive'
        
        Args.MaxTimeDiff              = 60;  % [s] - max time diff between images
        Args.SearchStr                = []; %'*_dark_raw*_Image_*.fits';
        Args.DarkSearchStr            = '*_dark_proc*_Image_*.fits'; % needed for the flat
        Args.MaxImages                = 20;
        Args.MinNimages               = 8; % 8; % 18;
        
        Args.GroupKeys                = {'EXPTIME','CAMOFFS'};
        
        Args.Verbose logical          = false;
    end
    
    
    SEC_DAY = 86400;
    
    [BasePath, CalibDir, NewFilesDir, ProjName] = pipeline.last.constructArchiveDir('DataNum',Args.DataNum,...
                                                                                    'Node',Args.Node,...
                                                                                    'ProjName',Args.ProjName,...
                                                                                    'NewFilesDir',Args.NewFilesDir,...
                                                                                    'CalibDir',Args.CalibDir,...
                                                                                    'BasePath',Args.BasePath);
    cd(NewFilesDir);
    
    if isempty(Args.FileTemplate)
        Args.FileTemplate = sprintf('%s_*_%s*.fits', ProjName, Args.Type);
    end
    
    % wait for new files
    NewDarkFilesExist = true;
    while NewDarkFilesExist
        FilesInNew  = io.files.dirSortedByDate(Args.FileTemplate);
        TimeSinceLastFile_sec = SEC_DAY.*min(now - FilesInNew.datenum);
    
        if TimeSinceLastFile_sec > Args.MaxTimeDiff
            NewDarkFilesExist = false;
        end
    end
    
    % read headers of all selected files
    % select images by additional criteria
    
    % read headers
    AH     = AstroHeader({Files.name});
    Groups = AH.groupByKeyVal(Args.GroupKeys);
    JD     = julday(AH);
    Ngroup = numel(Groups);
    
    for Igroup=1:1:Ngroup
        Files = FilesInNew(Groups(Igroup).ptr);
        JDF   = JD(Groups(Igroup).ptr);
        
        UniqueDays = unique(ceil(JDF));
        Nud        = numel(UniqueDays);
        for Iud=1:1:Nud
            IndDay = find(UniqueDays(Iud)==ceil(JDF));
            ListInDay = {FilesInNew(IndDay).name};
            
            Nid    = numel(IndDay);
            I1     = max(Nid - Args.MaxImages,1);
            I2     = Nid;
            SelectedInd = IndDay(I1:I2);
            Ind    = Groups(Igroup).ptr(SelectedInd);
            
            List   = {FilesInNew(Ind).name};
            
            % prep dark
            CI = CalibImages;
            IP = ImagePath.parseFileName(List);
            CI.createBias(Files);

            % save data
            MasterIP = IP(1).copy;
            MasterIP.Level   = 'proc';
            MasterIP.Product = 'Image';
            MasterName = [CalibDir, filesep, MasterIP.genFile];
            write1(CI.Bias, MasterName, MasterIP.Product);

            MasterIP.Product = 'Var';
            MasterName = [CalibDir, filesep, MasterIP.genFile];
            write1(CI.Bias, MasterName, MasterIP.Product);

            MasterIP.Product = 'Mask';
            MasterName = [CalibDir, filesep, MasterIP.genFile];
            write1(CI.Bias, MasterName, MasterIP.Product);

            
            % move files to date directory
            Nfiles = numel(ListInDay);
            Path           = IP(1).genPath;
            for Iim=1:1:Nfiles
                Source      = sprintf('%s%s%s', NewFilesDir, filesep, ListInDay{Iim});
                Destination = sprintf('%s%s%s', Path, filesep, ListInDay{Iim});
                % make sure diirectory exist
                mkdir(Path);
                % move file
                movefile(Source, Destination);
            end
            Found = true;
        
        end
    end
    
end