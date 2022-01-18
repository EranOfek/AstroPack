function runPipeLAST(Args)
    %
    
    arguments
        Args.BaseDir       = '/last01e/data1'
        Args.NewFilesDir   = 'archive/new';
        Args.SearchStr     = '*.fits'; 
        Args.DarkSearchStr = '*_dark_raw_*';
        Args.FlatSearchStr = '*_flat_raw_*';
    end
   
    PWD = pwd;
    cd(Args.BaseDir);
    cd(Args.NewFilesDir);
    
    % Check if new image arrived
    Files  = dir(Args.SearchStr);
    % sort files by time
    [~,SI] = sort([Files.datenum]);
    Files  = Files(SI);
    
    
    % Get image type
    
    % Check if this is the first image of a set (darks, flats, 20 science
    % images)
    
    
    
    switch lower(ImageType)
        case 'flat'
            
        case 'dark'
            
        case 'science'
            
        otherwise
            % do nothing
    end
            
    
end
