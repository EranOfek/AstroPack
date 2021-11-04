function Result = getAstroPackExternalPath()
    
    % External packages root
    Result = fullfile(tools.os.getAstroPackPath(), 'matlab', 'external');

end
