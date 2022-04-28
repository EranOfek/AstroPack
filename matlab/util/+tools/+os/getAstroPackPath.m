function Result = getAstroPackPath()

    AstroPackPath = getenv('ASTROPACK_PATH');
    
    if isempty(AstroPackPath)
        AstroPackDir = 'AstroPack';
        if (ismac || isunix)
            % Linux / Mac (forward slash)
                HomeDir = extractBefore(mfilename('fullpath'),'AstroPack/matlab');
        else
            % Windows (back slash)
                HomeDir = extractBefore(mfilename('fullpath'),'AstroPack\matlab');
        end
        AstroPackPath = fullfile(HomeDir, AstroPackDir);
    end

    Result = AstroPackPath;
    
end
