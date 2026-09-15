function Result = parseFrameName(FileName)
    % Parse a DESY lab TIFF frame name into its components.
    %   Frame names look like:
    %   Ultrasat_BSI_L_TH02954_W08_D02_PTC_int_hr_#01_B_0001.tif
    % Input  : - File name (with or without path), or a cell array of names.
    % Output : - A structure array (one element per name) with fields:
    %            Base (e.g., 'Ultrasat_BSI_L_TH02954_W08_D02'), Lot (char),
    %            Wafer, Device (numeric), Test (e.g., 'PTC_int_hr'),
    %            Step, FrameIndex (numeric), FrameType (e.g., 'B', 'D',
    %            'ZE'), and Match (false if the name did not parse; the
    %            other fields are then empty).
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: R = ultrasat.lab.parseFrameName('Ultrasat_BSI_L_TH02954_W08_D02_PTC_int_hr_#01_B_0001.tif')

    if ~iscell(FileName)
        FileName = {FileName};
    end
    Nf = numel(FileName);

    Pattern     = '^(?<Base>.+_L_[A-Za-z0-9]+_W\d+_D\d+)_(?<Test>.+?)_#(?<Step>\d+)_(?<FrameType>[A-Za-z]+)_(?<FrameIndex>\d+)$';
    BasePattern = '_L_(?<Lot>[A-Za-z0-9]+)_W(?<Wafer>\d+)_D(?<Device>\d+)$';

    Result = struct('Base',cell(Nf,1), 'Lot',[], 'Wafer',[], 'Device',[], 'Test',[], ...
                    'Step',[], 'FrameType',[], 'FrameIndex',[], 'Match',false);
    for If=1:1:Nf
        [~, Name] = fileparts(FileName{If});
        T = regexp(Name, Pattern, 'names', 'once');
        if ~isempty(T)
            B = regexp(T.Base, BasePattern, 'names', 'once');
            Result(If).Base       = T.Base;
            Result(If).Lot        = B.Lot;
            Result(If).Wafer      = str2double(B.Wafer);
            Result(If).Device     = str2double(B.Device);
            Result(If).Test       = T.Test;
            Result(If).Step       = str2double(T.Step);
            Result(If).FrameType  = T.FrameType;
            Result(If).FrameIndex = str2double(T.FrameIndex);
            Result(If).Match      = true;
        end
    end
end
