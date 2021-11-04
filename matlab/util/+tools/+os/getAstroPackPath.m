function Result = getAstroPackPath()

    MatlabDir    = 'matlab';
    AstroPackDir = 'AstroPack';

    if (ismac || isunix)
        % Linux / Mac
        HomeDir = getenv('HOME');
        AstroPackPath = getenv('ASTROPACK_PATH');
        if isempty(AstroPackPath)    
            AstroPackPath = fullfile(HomeDir, MatlabDir, AstroPackDir);
        end
    else
        % Windows
        HomeDir = getenv('HOMEPATH');
        AstroPackPath = getenv('ASTROPACK_PATH');    
        if isempty(AstroPackPath)
            AstroPackPath = fullfile(HomeDir, MatlabDir, AstroPackDir);
        end
    end

    if (isempty(HomeDir))
        %error('Can not find home directory environment variable - edit the startup.m file accordingly');
    end

    Result = AstroPackPath;
    
end
