function mkdir(Path, Args)
    % mkdir for a list of directories (check if they exist before creation)
    % Input  : - String array or cell array of paths.
    %          * ...,key,val,... 
    %            'CheckUniuqe' - Check if paths are unique and execute only
    %                   for unique paths. Default is true.
    % Output : null
    % Author : Eran Ofek (2026 Jan) 
    % Example: io.files.mkdir(Path)

    arguments
        Path
        Args.CheckUnique   = true;
    end

    if Args.CheckUnique
        UnPath = unique(Path);
    else
        UnPath = Path;
    end

    Exist = isfolder(UnPath);
    N     = numel(Exist);
    for I=1:1:N
        if ~Exist(I)
            mkdir(UnPath{I});
        end
    end

end
