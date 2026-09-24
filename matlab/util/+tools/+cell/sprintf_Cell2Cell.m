function [Result] = sprintf_Cell2Cell(Format, varargin)
    % Build sprintf output from a mix of scalars and cell/string inputs.
    % Input  : - Format string.
    %          * Arbitrary number of arguments, either scalar or string
    %          arrays or cell array.
    % Output : - A string array with the output strings.
    % Author : Eran Ofek (2025 May) 
    % Example: R=tools.cell.sprintf_Cell2Cell("%s.%d.%s",'a',{2,3},{"b","s"})


    Narg = numel(varargin);
    Nel  = nan(Narg,1);
    for Iarg=1:1:Narg
        if iscell(varargin{Iarg}) || isstring(varargin{Iarg})
            Nel(Iarg) = numel(varargin{Iarg});
        else
            Nel(Iarg) = 1;
        end
    end
    MaxNel = max(Nel);
    for Iarg=1:1:Narg
        if ~(iscell(varargin{Iarg}) || isstring(varargin{Iarg}))
            varargin{Iarg} = {varargin{Iarg}};
        end
    end

    Result = strings(MaxNel,1);
    for Iel=1:1:MaxNel
        CellArgs = cell(1,Narg);
        for Iarg=1:1:Narg
            if numel(varargin{Iarg})==1
                CellArgs{Iarg} = varargin{Iarg}{1};
            else
                CellArgs{Iarg} = varargin{Iarg}{Iel};
            end
        end
        Result(Iel) = sprintf(Format, CellArgs{:});
    end

    
end
