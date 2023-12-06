function Result = getAstroPackDataPath()

    MatlabDir    = 'matlab';
    AstroPackDir = 'AstroPack';

    if (ismac || isunix)
        % Linux / Mac
        HomeDir = getenv('HOME');
        AstroPackPath = getenv('ASTROPACK_PATH');
        if isempty(AstroPackPath)    
            AstroPackPath = fullfile(HomeDir, MatlabDir, AstroPackDir);
        end
        
        AstroPackDataPath = getenv('ASTROPACK_DATA_PATH');        
        if isempty(AstroPackPath)    
            AstroPackDataPath = fullfile(HomeDir, MatlabDir, 'data');
        end        
    else
        % Windows
        HomeDir = getenv('HOMEPATH');
        AstroPackDataPath = getenv('ASTROPACK_DATA_PATH');    
        if isempty(AstroPackDataPath)
            AstroPackDataPath = 'C:/AstroPack/matlab/data';  %fullfile(HomeDir, MatlabDir, AstroPackDir);
        end            
    end

    Result = AstroPackDataPath;
end
