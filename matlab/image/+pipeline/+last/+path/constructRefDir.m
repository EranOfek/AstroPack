function [Result] = constructRefDir(BasePath, FieldID)
    % Construct references image directory in LAST or storage machines 
    % Input  : - BasePath. If empty, then construct from host name (e.g., '/last01e/data')
    %            Default is '/marvin'
    %          - Optional fieldid to add to base path. Default is empty.
    % Output : - LAST reference images directory.
    % Author : Eran Ofek (2024 Dec) 
    % Example: pipeline.last.path.constructRefDir([]);
    %          pipeline.last.path.constructRefDir([], 1023);

    arguments
        BasePath               = '/marvin';
        FieldID                = [];
    end

    if isempty(BasePath)
        % assume LAST machine
        Result = sprintf('/%s/data', tools.os.get_computer);
    else
        Result = BasePath;
    end
    Result = sprintf('%s/references', Result);
    
    if ~isempty(FieldID)
        Result = sprintf('%s/%d', Result, FieldID);
    end
        
end
