function Str = sprintf_concatCell(Seperator, varargin)
    % Concat a multiple cell arrays content into a string 
    % Input  : - A sepeartor between cell elements.
    %          * An arbitrary number of argumnts.
    %            Each argument is either a cell array of strings, or a
    %            char/string.
    % Output : - Concat all the strings in the cells/char array into a
    %            single long string. The elements are seperated by the
    %            seperator.
    % Author : Eran Ofek (May 2022)
    % Example: Str = tools.cell.sprintf_concatCell('.', 'a',{'b','c'})
    
    N = numel(varargin);
    Str = '';
    for I=1:1:N
        if iscell(varargin{I})
           
            Ncell = numel(varargin{I});
            for Icell=1:1:Ncell
                if isempty(Str)
                    Sep = '';
                else
                    Sep = Seperator;
                end
                Str = sprintf('%s%s%s', Str, Sep, varargin{I}{Icell});
            end
        else
            if isempty(Str)
                Sep = '';
            else
                Sep = Seperator;
            end
            Str = sprintf('%s%s%s', Str, Sep, varargin{I});
        end
    end    
end
