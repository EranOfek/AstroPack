function Counter = prepMasterDark(Args)
    % Look for new files and prepare master dark for LAST
    % Input  : * ...,key,val,...
    %            'DataNum' - Number of disk data (e.g., 'data1').
    %                   Default is 1.
    %            'Node' - LAST node. Default is 1.
    %            'ProjName' - e.g., 'LAST.01.02.03'. If empty, will be
    %                   constructed automatically from computer name.
    %                   Default is [].
    %            'Type' - Image type to search in image name.
    %                   Default is 'dark'.
    %            'FileTemplate' - File template to search.
    %                   If empty will be constructed from project name and
    %                   type (e.g., 'LAST.01.02.03_*_dark*.fits').
    %                   Default is [].
    %            'NewFilesDir' - e.g., '/last02w/data1/archive/LAST.01.02.03/new'
    %                   If empty, constrcut automatically. Default is [].
    %            'CalibDir' - e.g., '/last02w/data1/archive/LAST.01.02.03/calib'
    %                   If empty, constrcut automatically. Default is [].
    %            'BasePath' - e.g., '/last02w/data1/archive'
    %                   If empty, constrcut automatically. Default is [].
    %            'MaxTimeDiff' - Maximum time difference between images in
    %                   sequence. Default is 70 [seconds].
    %            'MaxImages' - Max. number of images in master.
    %                   Default is 20.
    %            'MinNimages' - Min. number of images in master.
    %                   Default is 8.
    %            'GroupKeys' - Cell array of image header keywords by which
    %                   to group the images.
    %                   Default is {'EXPTIME','CAMOFFS','CAMGAIN'}.
    %            'Verbose' - Default is false.
    % Output : - Number of dark images written to disk.
    % Author : Eran Ofek (Sep 2022)
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
        
        Args.MaxTimeDiff              = 70;  % [s] - max time diff between images
        Args.SearchStr                = []; %'*_dark_raw*_Image_*.fits';
        %Args.DarkSearchStr            = '*_dark_proc*_Image_*.fits'; % needed for the flat
        Args.MaxImages                = 20;
        Args.MinNimages               = 8; % 8; % 18;
        
        Args.GroupKeys                = {'EXPTIME','CAMOFFS','CAMGAIN'};
        
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
    
    Counter = 0;
    
    % wait for new files
    NewDarkFilesExist = true;
    while NewDarkFilesExist
        FilesInNew  = io.files.dirSortedByDate(Args.FileTemplate);
        TimeSinceLastFile_sec = SEC_DAY.*min(now - [FilesInNew.datenum]);
    
        if TimeSinceLastFile_sec > Args.MaxTimeDiff
            NewDarkFilesExist = false;
        else
            pause(max(Args.MaxTimeDiff - TimeSinceLastFile_sec, 1));
        end
    end
    
    % read headers of all selected files
    % select images by additional criteria
    
    % read headers
    AH     = AstroHeader({FilesInNew.name});
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
            ListInDay = {Files(IndDay).name};
            
            Nid    = numel(IndDay);
            I1     = max(Nid - Args.MaxImages,1);
            I2     = Nid;
            SelectedInd = IndDay(I1:I2);
            Ind    = Groups(Igroup).ptr(SelectedInd);
            
            if numel(Ind)>Args.MinNimages
            
                List   = {FilesInNew(Ind).name};

                % prep dark
                CI = CalibImages;
                IP = ImagePath.parseFileName(List);
                IP(1).DataDir  = ProjName;
                IP(1).BasePath = BasePath;
                IP(1).Level    = 'raw';
                
                CI.createBias(List);

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

                Counter = Counter + 1;
            end
            
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