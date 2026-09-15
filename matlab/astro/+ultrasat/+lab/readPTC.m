function [AI, Frames, Sidecar] = readPTC(DeviceDir, Args)
    % Read DESY lab PTC test frames of one device into an AstroImage array.
    %   A device directory (e.g., LOT_TH02954_W08_D02) holds a *_Result.txt
    %   summary, a *.log time log, Calib/*.txt calibration files, and a test
    %   sub-directory (e.g., PTC_int_hr) with a PTC_Config.xlsx and the
    %   TIFF frames <Base>_<Test>_#<Step>_<Type>_<Index>.tif, where Type is
    %   B (bright), D (dark), or ZE (zero exposure).
    %   Each output AstroImage gets the TIFF header (see io.tiff.read1) plus
    %   keys from the sidecars: LOTID, WAFERID, DEVICE, RUNNO, OPERATOR,
    %   STATION, TESTSEQ, SEQREV, TESTTEMP, CHUCKTMP, TESTNAME, FRMTYPE,
    %   STEP, FRMINDEX, EXPTIME, INTENS, DATE-OBS (file time, UTC),
    %   TESTSTRT/TESTEND (lab local time), PASS, SOFTBIN, supply voltages
    %   and ADC registers from the configuration, and Calib best matches.
    %   EXPTIME and INTENS are copied as written in PTC_Config (units as
    %   there): B frames get PTC_ExpTime and Bright_Intensity(Step), D
    %   frames get Dark_ExpTime(Step) and INTENS=0, ZE frames get 0 and 0.
    % Input  : - Device directory.
    %          * ...,key,val,...
    %            'Test' - Test sub-directory / frame-name test tag.
    %                   Default is 'PTC_int_hr'.
    %            'FrameType' - Frame type(s) to read: 'all', or a char or
    %                   cell array of 'B', 'D', 'ZE'. Default is 'all'.
    %            'Step' - Step numbers to read. Empty for all. Default is [].
    %            'FrameIndex' - Frame indices to read. Empty for all.
    %                   Default is [].
    %            'ReadImage' - Read the pixel data. If false, only the
    %                   headers are populated (fast inventory).
    %                   Default is true.
    %            'CCDSEC' - [Xmin Xmax Ymin Ymax] section to read.
    %                   Default is [].
    %            'FlipUD' - Flip the frames vertically (FITS row order).
    %                   Default is false.
    %            'Verbosity' - 0 silent, 1 report progress. Default is 0.
    % Output : - An AstroImage column array, sorted by frame type (B, D,
    %            ZE), step, and frame index.
    %          - A table with one row per frame: FileName, FrameType, Step,
    %            FrameIndex, ExpTime, Intensity, DateObs.
    %          - A structure with the parsed sidecars: Result, Log, Config,
    %            Calib (see ultrasat.lab.readResult, readLog, readPTCConfig).
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: AI = ultrasat.lab.readPTC('LOT_TH02954_W08_D02', 'FrameType','D');
    %          [~, Frames] = ultrasat.lab.readPTC('LOT_TH02954_W08_D02', 'ReadImage',false);
    %          AI = ultrasat.lab.readPTC('LOT_TH02954_W08_D02', 'FrameType','B', 'Step',10, 'CCDSEC',[1 1000 1 1000]);

    arguments
        DeviceDir
        Args.Test                     = 'PTC_int_hr';
        Args.FrameType                = 'all';
        Args.Step                     = [];
        Args.FrameIndex               = [];
        Args.ReadImage(1,1) logical   = true;
        Args.CCDSEC                   = [];
        Args.FlipUD(1,1) logical      = false;
        Args.Verbosity(1,1) double    = 0;
    end

    TypeOrder = {'B', 'D', 'ZE'};

    %--- sidecars
    Sidecar = struct('Result',[], 'Log',[], 'Config',[], 'Calib',[]);
    ResFile = dir(fullfile(DeviceDir, '*_Result.txt'));
    if isempty(ResFile)
        error('ultrasat:lab:readPTC:noResult', 'No *_Result.txt found in %s', DeviceDir);
    end
    Base = regexprep(ResFile(1).name, '_Result\.txt$', '');
    Sidecar.Result = ultrasat.lab.readResult(fullfile(DeviceDir, ResFile(1).name));

    LogFile = fullfile(DeviceDir, [Base, '.log']);
    if isfile(LogFile)
        Sidecar.Log = ultrasat.lab.readLog(LogFile);
    end
    ConfigFile = fullfile(DeviceDir, Args.Test, 'PTC_Config.xlsx');
    if isfile(ConfigFile)
        Sidecar.Config = ultrasat.lab.readPTCConfig(ConfigFile);
    end
    CalibFiles = dir(fullfile(DeviceDir, 'Calib', [Base, '_Calib_*.txt']));
    Sidecar.Calib = struct;
    for Ic=1:1:numel(CalibFiles)
        Name = regexp(CalibFiles(Ic).name, '_Calib_(.+)\.txt$', 'tokens', 'once');
        Sidecar.Calib.(Name{1}) = readCalib(fullfile(CalibFiles(Ic).folder, CalibFiles(Ic).name));
    end

    %--- frame list
    TifFiles = dir(fullfile(DeviceDir, Args.Test, [Base, '_', Args.Test, '_#*.tif']));
    P = ultrasat.lab.parseFrameName({TifFiles.name});
    Flag = [P.Match];
    if ~strcmpi(Args.FrameType, 'all')
        Flag = Flag & ismember({P.FrameType}, cellstr(Args.FrameType));
    end
    if ~isempty(Args.Step)
        Flag = Flag & ismember([P.Step], Args.Step);
    end
    if ~isempty(Args.FrameIndex)
        Flag = Flag & ismember([P.FrameIndex], Args.FrameIndex);
    end
    TifFiles = TifFiles(Flag);
    P        = P(Flag);
    [~, TypeRank] = ismember({P.FrameType}, TypeOrder);
    TypeRank(TypeRank==0) = numel(TypeOrder) + 1;
    [~, SortInd] = sortrows([TypeRank(:), [P.Step].', [P.FrameIndex].']);
    TifFiles = TifFiles(SortInd);
    P        = P(SortInd);
    Nf       = numel(TifFiles);

    %--- common header keys
    CommonHeader = commonKeys(Sidecar, Args.Test);

    %--- read frames
    AI = AstroImage([Nf, 1]);
    ExpTime   = nan(Nf,1);
    Intensity = nan(Nf,1);
    DateObs   = cell(Nf,1);
    for If=1:1:Nf
        File = fullfile(TifFiles(If).folder, TifFiles(If).name);
        if Args.Verbosity>0
            fprintf('readPTC: %d/%d %s\n', If, Nf, TifFiles(If).name);
        end
        [ExpTime(If), Intensity(If)] = frameExposure(P(If), Sidecar.Config);
        try
            if Args.ReadImage
                [AI(If).Image, TiffHeader] = io.tiff.read1(File, 'CCDSEC',Args.CCDSEC, 'FlipUD',Args.FlipUD);
            else
                TiffHeader = io.tiff.readHeader1(File);
            end
            DateObs{If} = TiffHeader{strcmp(TiffHeader(:,1), 'FILEDATE'), 2};
            FrameHeader = {'DATE-OBS', DateObs{If},      'File modification time (UTC)';
                           'FRMTYPE',  P(If).FrameType,  'Frame type: B bright, D dark, ZE zero exposure';
                           'STEP',     P(If).Step,       'Step number within the frame type';
                           'FRMINDEX', P(If).FrameIndex, 'Frame index within the step';
                           'EXPTIME',  ExpTime(If),      'Exposure time as in PTC_Config';
                           'INTENS',   Intensity(If),    'Illumination intensity as in PTC_Config'};
            AI(If).HeaderData.Data = [TiffHeader; FrameHeader; CommonHeader];
            AI(If).ImageData.FileName = File;
        catch ME
            warning('ultrasat:lab:readPTC:read', 'Failed to read %s (%s)', File, ME.message);
        end
    end

    Frames = table({TifFiles.name}.', {P.FrameType}.', [P.Step].', [P.FrameIndex].', ExpTime, Intensity, DateObs, ...
                   'VariableNames', {'FileName','FrameType','Step','FrameIndex','ExpTime','Intensity','DateObs'});
end

function [ExpTime, Intensity] = frameExposure(P, Config)
    % exposure time and intensity of a frame from the PTC configuration
    ExpTime   = NaN;
    Intensity = NaN;
    if isempty(Config)
        return;
    end
    switch upper(P.FrameType)
        case 'B'
            ExpTime   = pick(Config, 'PTC_ExpTime', P.Step);
            Intensity = pick(Config, 'Bright_Intensity', P.Step);
        case 'D'
            ExpTime   = pick(Config, 'Dark_ExpTime', P.Step);
            Intensity = 0;
        case 'ZE'
            ExpTime   = 0;
            Intensity = 0;
    end
end

function Val = pick(Config, Field, Step)
    % element Step of a config list, or the scalar itself
    Val = NaN;
    if isfield(Config, Field) && isnumeric(Config.(Field))
        V = Config.(Field);
        if isscalar(V)
            Val = V;
        elseif Step>=1 && Step<=numel(V)
            Val = V(Step);
        else
            warning('ultrasat:lab:readPTC:step', 'Step %d is outside %s (%d values)', Step, Field, numel(V));
        end
    end
end

function Header = commonKeys(Sidecar, Test)
    % header keys shared by all frames of the device
    Header = cell(0,3);
    Info = Sidecar.Result.Info;
    Map  = {'LOTID',    'LOTID',             'Lot ID';
            'WAFERID',  'WaferID',           'Wafer ID';
            'DEVICE',   'DeviceNo',          'Device number';
            'RUNNO',    'RunNo',             'Test run number';
            'OPERATOR', 'OperatorName',      'Operator';
            'STATION',  'TestStation',       'Test station';
            'TESTSEQ',  'TestSequenceName',  'Test sequence';
            'SEQREV',   'SequenceRev',       'Test sequence revision';
            'TESTTEMP', 'TesterTemperature', 'Tester temperature [C]'};
    Header = [Header; mapFields(Info, Map)];
    Header(end+1,:) = {'TESTNAME', Test, 'Test name'};
    if ~isempty(Sidecar.Result.Trailer)
        Header = [Header; mapFields(Sidecar.Result.Trailer, {'PASS', 'Pass', 'Test passed'; 'SOFTBIN', 'SoftBin', 'Soft bin'})];
    end

    if ~isempty(Sidecar.Log)
        L = Sidecar.Log;
        Fmt = 'yyyy-MM-dd''T''HH:mm:ss';
        Is = find(strcmp(L.Text, [Test, ' started']), 1);
        Ie = find(strcmp(L.Text, [Test, ' finished']), 1, 'last');
        if ~isempty(Is)
            Header(end+1,:) = {'TESTSTRT', char(datetime(L.Time(Is), 'Format',Fmt)), 'Test start (lab local time)'};
        end
        if ~isempty(Ie)
            Header(end+1,:) = {'TESTEND', char(datetime(L.Time(Ie), 'Format',Fmt)), 'Test end (lab local time)'};
        end
    end

    if ~isempty(Sidecar.Config)
        Map = {'CHUCKTMP', 'ChuckTemperature', 'Chuck temperature [C]';
               'NBRIGHT',  'Bright_Frames',     'Bright frames per step';
               'NDARK',    'Dark_Frames',       'Dark frames per step';
               'NZEROEXP', 'ZeroExp_Frames',    'Zero-exposure frames';
               'EXPTOFFS', 'zDUT_ExpTimeOffset','Exposure time offset as in PTC_Config';
               'VDDA',     'zVDDA',             'VDDA [V]';
               'VDDD',     'zVDDD',             'VDDD [V]';
               'VDD_SEN',  'zVDD_SEN',          'VDD_SEN [V]';
               'VDD_SF',   'zVDD_SF',           'VDD_SF [V]';
               'VDD_TX',   'zVDD_TX',           'VDD_TX [V]';
               'VDDRST_L', 'zVDD_RST_L',        'VDD_RST_L [V]';
               'VDDRST_H', 'zVDD_RST_H',        'VDD_RST_H [V]';
               'VDDRSTSL', 'zVDD_RST_SEL',      'VDD_RST_SEL [V]';
               'ADC_CAP',  'zDUT_ADC_CAP',      'DUT ADC cap register';
               'ADC_ADJ',  'zDUT_ADC_ADJ',      'DUT ADC adjust register';
               'COL_ADJ',  'zDUT_COL_ADJ',      'DUT column adjust register';
               'CNTSETVL', 'zDUT_CntSetVal',    'DUT counter set value';
               'CPU_PRG',  'CPU_Prg_File',      'CPU program file'};
        Header = [Header; mapFields(Sidecar.Config, Map)];
    end

    Map = {'CAL_ADC',  'adc',     'Calib adc best match';
           'CAL_ADCC', 'adc_cap', 'Calib adc_cap best match';
           'CAL_COL',  'col',     'Calib col best match'};
    for Im=1:1:size(Map,1)
        if isfield(Sidecar.Calib, Map{Im,2})
            Header(end+1,:) = {Map{Im,1}, Sidecar.Calib.(Map{Im,2}).BestMatch, Map{Im,3}};
        end
    end
end

function Header = mapFields(S, Map)
    % {Key, Field, Comment} rows for the fields of S that exist
    Exist  = cellfun(@(f) isfield(S, f), Map(:,2));
    Header = [Map(Exist,1), cellfun(@(f) S.(f), Map(Exist,2), 'UniformOutput',false), Map(Exist,3)];
end

function Result = readCalib(FileName)
    % Calib file: Description, Target Value, Best Match, Measured Values list
    Lines  = strsplit(fileread(FileName), {'\r\n', '\n'});
    Lines  = Lines(~cellfun(@isempty, strtrim(Lines)));
    Result = struct('Description','', 'TargetValue',NaN, 'BestMatch',NaN, 'MeasuredValues',[]);
    Il = 1;
    while Il<=numel(Lines) && contains(Lines{Il}, ':')
        F = strsplit(Lines{Il}, ':', 'CollapseDelimiters',false);
        Key = strtrim(F{1});
        Val = strtrim(strjoin(F(2:end), ':'));
        switch Key
            case 'Description'
                Result.Description = Val;
            case 'Target Value'
                Result.TargetValue = ultrasat.lab.parseNum(Val);
            case 'Best Match'
                Result.BestMatch = ultrasat.lab.parseNum(Val);
        end
        Il = Il + 1;
    end
    Result.MeasuredValues = cellfun(@(s) ultrasat.lab.parseNum(s), Lines(Il:end));
end
