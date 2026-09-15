function Result = readResult(FileName)
    % Read a DESY lab *_Result.txt file (tab separated, decimal comma).
    %   The file starts with Key<tab>Value lines (Product Name, Time, LOT ID,
    %   Wafer ID, Tester Temperature, ...), followed by a table of test
    %   results (ID, Name, Bin, Flag, Low, Value, High) and a trailer
    %   (Total Time, Soft Bin, Pass).
    % Input  : - File name.
    % Output : - A structure with fields:
    %            .Info  - structure of the key/value lines; field names are
    %                     the keys made valid (e.g., 'LOT ID' -> LOTID,
    %                     'Tester Temperature' -> TesterTemperature) and
    %                     numeric values parsed (decimal comma allowed).
    %            .Tests - table with columns ID, Name, Bin, Flag, Low,
    %                     Value, High; non-numeric limits ('disable') are NaN.
    %            .Trailer - structure of the trailing key/value lines.
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: R = ultrasat.lab.readResult('Ultrasat_BSI_L_TH02954_W08_D02_Result.txt')

    Lines = strsplit(fileread(FileName), {'\r\n', '\n'});
    Lines = Lines(~cellfun(@isempty, strtrim(Lines)));
    Nl    = numel(Lines);

    Result.Info    = struct;
    Result.Trailer = struct;
    TestRows = cell(0,7);
    InTable  = false;
    for Il=1:1:Nl
        F = strsplit(Lines{Il}, '\t', 'CollapseDelimiters',false);
        IsTestRow = ~isempty(regexp(F{1}, '^\d{4,}$', 'once')) && numel(F)>=7;
        if IsTestRow
            InTable = true;
            TestRows(end+1,:) = F(1:7);
        else
            Key = matlab.lang.makeValidName(strtrim(F{1}));
            if numel(F)>1
                Val = ultrasat.lab.parseNum(F{2});
            else
                Val = '';
            end
            if InTable
                Result.Trailer.(Key) = Val;
            else
                Result.Info.(Key) = Val;
            end
        end
    end

    Nt = size(TestRows,1);
    Num = @(C) cellfun(@(s) numOrNaN(s), C);
    Result.Tests = table(Num(TestRows(:,1)), TestRows(:,2), Num(TestRows(:,3)), Num(TestRows(:,4)), ...
                         Num(TestRows(:,5)), Num(TestRows(:,6)), Num(TestRows(:,7)), ...
                         'VariableNames', {'ID','Name','Bin','Flag','Low','Value','High'});
    if Nt==0
        Result.Tests = Result.Tests([],:);
    end
end

function V = numOrNaN(S)
    [V, IsNum] = ultrasat.lab.parseNum(S);
    if ~IsNum
        V = NaN;
    end
end
