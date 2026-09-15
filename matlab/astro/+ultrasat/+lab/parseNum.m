function [Val, IsNum] = parseNum(Str)
    % Parse a number written with a decimal comma (DESY lab files), if possible.
    % Input  : - A char array, string, or numeric scalar.
    % Output : - The numeric value, or the input unchanged if it is not a
    %            number (e.g., 'disable', 'TH02954', '0x21000101').
    %          - Logical, true if the output is numeric.
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: ultrasat.lab.parseNum('-50,0')      % -50
    %          ultrasat.lab.parseNum('1,0E-05')    % 1e-5
    %          ultrasat.lab.parseNum('disable')    % 'disable'

    if isnumeric(Str) || islogical(Str)
        Val   = Str;
        IsNum = true;
        return;
    end
    Str = strtrim(char(Str));
    Std = strrep(Str, ',', '.');
    if ~isempty(regexp(Std, '^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$', 'once'))
        Val   = str2double(Std);
        IsNum = true;
    else
        Val   = Str;
        IsNum = false;
    end
end
