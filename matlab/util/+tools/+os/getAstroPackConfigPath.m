function Result = getAstroPackConfigPath()

    % Configuation path, default is in repo config/ 
    AstroPackConfigPath = getenv('ASTROPACK_CONFIG_PATH');
    if isempty(AstroPackConfigPath)
        AstroPackConfigPath = fullfile(tools.os.getAstroPackPath(), 'config');
    end
    
    Result = AstroPackConfigPath;
end
