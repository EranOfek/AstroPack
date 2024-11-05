function Result=convert2strings(Array)
    % Convert numeric or char array to strings array and remove trailing/leading spaces.
    % Input  : - Array.
    % Output : - String array.
    % Author : Eran Ofek (Nov 2024)
    % Example: tools.string.convert2strings('2')

    % convert to string
    if isnumeric(Array)
        Result = string(arrayfun(@num2str, Array, 'UniformOutput', false));
    elseif ischar(Array)
        Result = string(Array);
    else
        % do nothing
        Result = Array;
    end
    % trim spaces from FieldName
    Result = strtrim(Result);
end