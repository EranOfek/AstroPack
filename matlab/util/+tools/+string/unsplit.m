 function Str = unsplit(Cell, Delim)
    % unsplit a cell array of strings.
    % Input  : - A cell array of strings.
    %          - Delimiter. 
    % Output : - The unsplitted string.
    % Author : Eran Ofek (Jan 2022)
    % Example: Str = tools.string.unsplit({'1','2'}, '_')

    N = numel(Cell);
    Str  = Cell{1};
    for I=2:1:N
        Str = [Str, Delim, Cell{I}];
    end
 end
        