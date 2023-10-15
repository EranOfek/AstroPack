function Result = getUltrasatPath()

	UltrasatDir = 'ULTRASAT';

    if (ismac || isunix)
        % Linux / Mac
        HomeDir = getenv('HOME');
        UltrasatPath = getenv('ULTRASAT_PATH');
        if isempty(UltrasatPath)    
            UltrasatPath = fullfile(HomeDir, UltrasatDir);
        end
    else
        % Windows
        HomeDir = getenv('HOMEPATH');
        UltrasatPath = getenv('ULTRASAT_PATH');    
        if isempty(UltrasatPath)
            UltrasatPath = fullfile(HomeDir, UltrasatDir);
        end
    end

    if (isempty(HomeDir))
        %error('Can not find home directory environment variable - edit the startup.m file accordingly');
    end

    Result = UltrasatPath;
    
end
