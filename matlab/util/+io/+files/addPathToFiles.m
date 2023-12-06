function Cell = addPathToFiles(Cell, Path)
    % Concat a path to a list of file names.
    % Input  : - A file name or a cell of file names
    %          - A path to add.
    % Output : - A cell of file names. This is a cell regardless of the
    %            input type.
    % Author : Eran Ofek (Jan 2022)
    % Example: io.files.addPathToFiles('aa','bnnb')
    
    if ischar(Cell)
        Cell = {Cell};
    end
    
    Ncell = numel(Cell);
    for Icell=1:1:Ncell
        Cell{Icell} = sprintf('%s%s%s',Path, filesep, Cell{Icell});
    end
end