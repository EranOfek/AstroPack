function Result = getULTRASATDataPath()

    MatlabDir    = 'matlab';
   
    if (ismac || isunix)
        % Linux / Mac       
        ULTRASATDataPath = getenv('ULTRASAT_DATA_PATH');        
        if isempty(ULTRASATDataPath)    
            HomeDir = getenv('HOME');
            ULTRASATDataPath = fullfile(HomeDir, MatlabDir, 'data/ULTRASAT/');
        end        
    else
        % Windows
        ULTRASATDataPath = getenv('ULTRASAT_DATA_PATH');    
        if isempty(ULTRASATDataPath)
            ULTRASATDataPath = 'C:/AstroPack/matlab/data/ULTRASAT/';  
        end            
    end

    Result = ULTRASATDataPath;
end
