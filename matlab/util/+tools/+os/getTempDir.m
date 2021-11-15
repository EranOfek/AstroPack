function Result = getTempDir()
    %
    % Eaxmple: TempDir = tools.os.getTempDir
   
    if (ismac || isunix)
        Result = '/tmp/AstroPackTemp';
    else
        Result = 'C:/Temp/AstroPackTemp';        
    end
    
    % Create folder
    if ~isfolder(Result)
        mkdir(Result);    
    end
end
