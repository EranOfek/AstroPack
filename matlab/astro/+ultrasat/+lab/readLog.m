function Result = readLog(FileName)
    % Read a DESY lab test log (Time<tab>ID<tab>Text lines).
    % Input  : - File name of the *.log file.
    % Output : - A table with columns Time (datetime, lab local time), ID
    %            (numeric), Text (char).
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: L = ultrasat.lab.readLog('Ultrasat_BSI_L_TH02954_W08_D02.log')

    Lines = strsplit(fileread(FileName), {'\r\n', '\n'});
    Lines = Lines(~cellfun(@isempty, strtrim(Lines)));
    % skip the column-title line
    Lines = Lines(cellfun(@(s) ~isempty(regexp(s, '^\d{2}:\d{2}:\d{2} \d{2}\.\d{2}\.\d{4}\t', 'once')), Lines));
    Nl    = numel(Lines);

    Time = NaT(Nl,1);
    ID   = nan(Nl,1);
    Text = cell(Nl,1);
    for Il=1:1:Nl
        F = strsplit(Lines{Il}, '\t');
        Time(Il) = datetime(strtrim(F{1}), 'InputFormat','HH:mm:ss dd.MM.yyyy');
        ID(Il)   = str2double(F{2});
        Text{Il} = strtrim(F{3});
    end
    Result = table(Time, ID, Text);
end
