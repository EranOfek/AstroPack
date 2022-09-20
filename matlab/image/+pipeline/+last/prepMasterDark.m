function prepMasterDark(Args)
    % Look for new files and prepare master dark for LAST
    % 
    
    arguments
        Args.DataNum                  = 1;   % disk data# number
        Args.Node                     = 1;
        Args.ProjName                 = [];
        Args.Type                     = 'dark';
        
        Args.NewFilesDir              = [];  % '/last02w/data1/archive/LAST.01.02.03/new'
        Args.CalibDir                 = [];  % '/last02w/data1/archive/LAST.01.02.03/calib'
        Args.BasePath                 = [];  % '/last02w/data1/archive'
        
        Args.MaxTimeDiff              = 60;  % [s] - max time diff between images
        Args.SearchStr                = []; %'*_dark_raw*_Image_*.fits';
        Args.DarkSearchStr            = '*_dark_proc*_Image_*.fits'; % needed for the flat
        Args.MaxImages                = 20;
        Args.MinNimages               = 3; % 8; % 18;
        
        Args.GroupKeys                = {'EXPTIME','CAMOFFS'};
        
        Args.Verbose logical          = false;
    end
    
    [BasePath, CalibDir, NewFilesDir, ProjName] = pipeline.last.constructArchiveDir('DataNum',Args.DataNum,...
                                                                                    'Node',Args.Node,...
                                                                                    'ProjName',Args.ProjName,...
                                                                                    'NewFilesDir',Args.NewFilesDir,...
                                                                                    'CalibDir',Args.CalibDir,...
                                                                                    'BasePath',Args.BasePath);
        

    
    
    
    
end