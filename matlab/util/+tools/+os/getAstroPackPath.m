function Result = getAstroPackPath
    % Return the AstroPack toolbox path
    % Author : Eran Ofek (May 2022)
    % Example: Result = tools.os.getAstroPackPath
    
    ThisFunPath = mfilename('fullpath');
    
    Result = strrep(ThisFunPath, '/matlab/util/+tools/+os/getAstroPackPath','');
end
