function Result = readPTCConfig(FileName)
    % Read a DESY lab PTC_Config.xlsx file into a structure.
    %   The configuration is a two-column Key | Value sheet. Values written
    %   with a decimal comma are converted, and list values of the form
    %   [N]{v1;v2;...} are returned as numeric row vectors.
    % Input  : - File name of the xlsx file.
    % Output : - A structure whose field names are the (valid-name) keys,
    %            e.g. PTC_ExpTime, Dark_ExpTime, Bright_Intensity,
    %            ChuckTemperature, Dark_Frames, zVDDA, ...
    %            Keys that appear more than once keep the first value.
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: C = ultrasat.lab.readPTCConfig('PTC_Config.xlsx')

    Sheets = sheetnames(FileName);
    C = {};
    Is = 0;
    while isempty(C) && Is<numel(Sheets)
        Is = Is + 1;
        C  = readcell(FileName, 'Sheet',Sheets{Is});
    end
    if size(C,2)<2
        error('ultrasat:lab:readPTCConfig:format', 'No Key|Value sheet found in %s', FileName);
    end

    Result = struct;
    for Ir=1:1:size(C,1)
        Key = C{Ir,1};
        Val = C{Ir,2};
        if ~(ischar(Key) || isstring(Key)) || ismissing(string(Key))
            % not a key row
        else
            Key = matlab.lang.makeValidName(strtrim(char(Key)));
            if ~isfield(Result, Key)
                Result.(Key) = parseValue(Val);
            end
        end
    end
end

function Val = parseValue(Val)
    if isa(Val, 'missing')
        Val = [];
    elseif ischar(Val) || isstring(Val)
        Val = strtrim(char(Val));
        T = regexp(Val, '^\[(\d+)\]\{(.*)\}$', 'tokens', 'once');
        if ~isempty(T)
            % list: [N]{v1;v2;...}
            Items = strsplit(T{2}, ';');
            Val   = cellfun(@(s) ultrasat.lab.parseNum(s), Items);
            if numel(Val)~=str2double(T{1})
                warning('ultrasat:lab:readPTCConfig:count', 'List declares %s items but %d found', T{1}, numel(Val));
            end
        else
            Val = ultrasat.lab.parseNum(Val);
        end
    end
end
