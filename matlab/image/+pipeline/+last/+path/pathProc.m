function [Path] = pathProc(Mount, Cam, Date, Args)
    % Get path to LAST proc data in local storage
    % Input  : - Mount number
    %          - Camera number
    %          - Date [Day, Month, Year]
    %          * ...,key,val,... 
    %            'Visit' - If empty, do not add visit to path.
    %                   Default is [].
    %            'BasePath' - Base path. Default is '/lastdata'.
    %            'Node' - Node number. Default is 1.
    %            'ProjName' - Project name. Default is 'LAST'.
    % Output : - Path.
    % Author : Eran Ofek (2025 Sep) 
    % Example: Path=pipeline.last.path.pathProc(2,3,[1 1 2025])

    arguments
        Mount
        Cam
        Date
        Args.Visit      = [];
        Args.BasePath   = '/lastdata';
        Args.Node       = 1;
        Args.ProjName   = 'LAST';
    end

    ProjName = sprintf('%s.%02d.%02d.%02d',Args.ProjName, Args.Node, Mount, Cam);

    Path = fullfile(Args.BasePath, ProjName, sprintf('%04d',Date(3)), sprintf('%02d',Date(2)), sprintf('%02d',Date(1)), 'proc');
    if ~isempty(Args.Visit)
        Path = fullfile(Path, Args.Visit);
    end
    


end
