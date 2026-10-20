function Config = readUCConfig(RunDir, Args)
    % Read the metadata of one DESY uc_st_measurement run directory.
    %   A run directory is <DieDir>/<Lot>_<Wafer>_<Die>_<Type>_<Date>_<Time>
    %   and holds measurement/raw_images/*.fits plus measurement/metadata with
    %     mcfg_*.toml    the measurement configuration (what was stepped)
    %     scfg_*.toml    the sensor configuration (bias voltages, registers)
    %     telemetry_CameraSegmentA.csv   the camera state per measurement point,
    %                    including the MEASURED exposure, reset delay and
    %                    readout time
    %     camera_configuration_CameraSegmentA.csv  the commanded camera state
    %     temperature.csv, tvac.csv      thermometry
    %     bonn_shutter_log_<MODE>.csv    every shutter open/close timestamp
    %   The returned structure mixes the three sources on purpose: the fields
    %   named like the aSpect PTC_Config (zVDD_*, zDUT_*) come from the sensor
    %   configuration so that the same header map serves both readers, and the
    %   fields named RO_* come from the telemetry, i.e. they are measured on
    %   the instrument rather than assumed.
    %
    %   The readout time is not a constant of the instrument. It follows
    %     RO_time = 2 * (n_rows + n_row0_resets - 2) * (1.000 ms + PdResetDelay)
    %   which reproduces every value seen in this campaign to the microsecond
    %   (12.37860 s for the full die with 20 row-0 resets and a 300 us
    %   photodiode reset delay, 9.57884 s with 1 and 10 us, and the windowed
    %   750- and 500-row equivalents). The telemetry value is returned as
    %   measured; RO_Formula is the closed form, for cross-checking only.
    %
    % Input  : - Run directory.
    %          * ...,key,val,...
    %            'Verbosity' - 0 silent, 1 report. Default is 0.
    % Output : - Structure with the fields
    %            RunName, RunDir, MeasDir, Lot, Wafer, Die, MeasType, RunDate
    %            MeasTemp          [C] as DECLARED in mcfg (unreliable, see
    %                              Temperature for the measured value)
    %            Temperature       [C] median of temperature.csv Sensor2
    %            TempSpread        [C] half the 5-95 percentile range of it
    %            CameraExpList     [s] stepped sensor exposures (DC/PTCexpsen)
    %            ShutterExpList    [s] stepped shutter openings (PTCexpsh)
    %            CameraExpTime     [s] fixed sensor exposure (PTCexpsh)
    %            ShutterExpDelay   [s] shutter delay w.r.t. readout stop
    %            LedIntensity      [%] LED setting
    %            NZero, NDark, NBright, NPreDark, NPostDark  frames per point
    %            Nrows, Nrow0Resets, PdResetDelay, ExposureUs  from scfg
    %            zVDDA zVDDD zVDD_SEN zVDD_SF zVDD_TX zVDD_RST_L zVDD_RST_H
    %            zVDD_RST_SEL                      [V] commanded bias voltages
    %            zDUT_ADC_CAP zDUT_ADC_ADJ zDUT_COL_ADJ zDUT_CntSetVal
    %                                              sensor registers
    %            ScfgName, McfgName                the two configuration files
    %            Tele              table of the DISTINCT telemetry states, with
    %                              Nrows, Nrow0Resets, PdResetDelay, ExposureS,
    %                              ResetDelayS, ROTimeS, CycleS
    %            RO_Time           [s] readout time of the measurement points
    %                              (the most frequent telemetry value)
    %            RO_TimeZero       [s] readout time of the zero-exposure frames
    %                              when they used a different reset setting
    %            RO_Formula        [s] the closed form above, for RO_Time
    %            Shutter           structure per image mode with the measured
    %                              open and close times and the open interval
    % Author : A.M. Krassilchtchikov (Oct 2026)
    % Example: C = ultrasat.lab.readUCConfig('/Data1/uc_data-test/LT112181_W14_D1/LT112181_W14_D1_DC_2026-08-16_01-30-13');

    arguments
        RunDir
        Args.Verbosity(1,1) double = 0;
    end

    if ~isfolder(RunDir)
        error('ultrasat:lab:readUCConfig:noDir', 'No such run directory: %s', RunDir);
    end
    RunDir = regexprep(RunDir, '[/\\]+$', '');
    [~, RunName] = fileparts(RunDir);

    MeasDir = fullfile(RunDir, 'measurement');
    if ~isfolder(MeasDir)
        error('ultrasat:lab:readUCConfig:noMeas', 'No measurement/ in %s', RunDir);
    end
    MetaDir = fullfile(MeasDir, 'metadata');

    Config = struct('RunName',RunName, 'RunDir',RunDir, 'MeasDir',MeasDir);

    %--- identity from the run name: <LOT>_<WAFER>_<DIE>_<TYPE>_<DATE>_<TIME>
    Tok = regexp(RunName, '^(LT\d+)_(W\d+)_(D\d+)_([A-Za-z]+)_(\d{4}-\d\d-\d\d)_(\d\d-\d\d-\d\d)$', 'tokens', 'once');
    if isempty(Tok)
        error('ultrasat:lab:readUCConfig:name', ['Run directory name %s does not match ' ...
              '<LOT>_<WAFER>_<DIE>_<TYPE>_<DATE>_<TIME>'], RunName);
    end
    Config.Lot      = Tok{1};
    Config.Wafer    = Tok{2};
    Config.Die      = Tok{3};
    Config.MeasType = Tok{4};
    Config.RunDate  = [Tok{5}, 'T', strrep(Tok{6}, '-', ':')];

    %--- measurement configuration
    Mcfg = dir(fullfile(MetaDir, 'mcfg_*.toml'));
    M = struct;
    if ~isempty(Mcfg)
        M = readToml(fullfile(Mcfg(1).folder, Mcfg(1).name));
        Config.McfgName = Mcfg(1).name;
    else
        Config.McfgName = '';
    end
    Config.MeasTemp       = getNum(M, 'measurement_temperature', NaN);
    Config.CameraExpList  = getNum(M, 'camera_exposure_list',  []);
    Config.ShutterExpList = getNum(M, 'shutter_exposure_list', []);
    Config.CameraExpTime  = getNum(M, 'camera_exposure_time',  NaN);
    Config.ShutterExpDelay= getNum(M, 'shutter_exposure_delay',NaN);
    Config.LedIntensity   = getNum(M, 'led_intensity',         NaN);
    Config.NZero          = getNum(M, 'beginning_zero_frames', NaN);
    Config.NDark          = getNum(M, 'dark_frames',           NaN);
    Config.NBright        = getNum(M, 'bright_frames',         NaN);
    Config.NPreDark       = getNum(M, 'pre_dark_frames',       NaN);
    Config.NPostDark      = getNum(M, 'post_dark_frames',      NaN);

    %--- sensor configuration
    Scfg = dir(fullfile(MetaDir, 'scfg_*.toml'));
    S = struct;
    if ~isempty(Scfg)
        S = readToml(fullfile(Scfg(1).folder, Scfg(1).name));
        Config.ScfgName = Scfg(1).name;
    else
        Config.ScfgName = '';
    end
    Config.ExposureUs   = getNum(S, 'exposure_us',               NaN);
    Config.Nrows        = getNum(S, 'n_rows',                    NaN);
    Config.Nrow0Resets  = getNum(S, 'n_row0_resets',             NaN);
    Config.PdResetDelay = getNum(S, 'photodiode_reset_delay',    NaN);
    % bias voltages, named as in the aSpect PTC_Config so that one header map serves both
    Config.zVDDA        = getNum(S, 'sbb_dac_vdda',        NaN);
    Config.zVDDD        = getNum(S, 'sbb_dac_vddd',        NaN);
    Config.zVDD_SEN     = getNum(S, 'sbb_dac_vdd_sen',     NaN);
    Config.zVDD_SF      = getNum(S, 'sbb_dac_vdd_sf',      NaN);
    Config.zVDD_TX      = getNum(S, 'sbb_dac_vdd_tx',      NaN);
    Config.zVDD_RST_L   = getNum(S, 'sbb_dac_vdd_rst_l',   NaN);
    Config.zVDD_RST_H   = getNum(S, 'sbb_dac_vdd_rst_h',   NaN);
    Config.zVDD_RST_SEL = getNum(S, 'sbb_dac_vdd_sel_rst', NaN);
    Config.zVinj        = getNum(S, 'sbb_dac_vinj',        NaN);
    Config.zDUT_ADC_CAP = getNum(S, 'bs_adc_cap_adj',      NaN);
    Config.zDUT_ADC_ADJ = getNum(S, 'bs_adc_adj',          NaN);
    Config.zDUT_COL_ADJ = getNum(S, 'bs_col_adj',          NaN);
    Config.zDUT_CntSetVal = getNum(S, 'cnt_set_val',       NaN);
    Config.zDUT_RampAdj = getNum(S, 'bs_ramp_adj',         NaN);
    Config.zDUT_AmpGain = getNum(S, 'amp_gain',            NaN);
    Config.GainThreshold= getNum(S, 'gain_threshold',      NaN);

    %--- telemetry: the MEASURED exposure, reset delay and readout time
    Config.Tele        = table;
    Config.RO_Time     = NaN;
    Config.RO_TimeZero = NaN;
    Config.RO_Formula  = NaN;
    TeleFile = fullfile(MetaDir, 'telemetry_CameraSegmentA.csv');
    if isfile(TeleFile)
        Config.Tele = teleStates(TeleFile);
        if ~isempty(Config.Tele)
            % the measurement points are the states with a reset delay set by the
            % ladder; the zero frames run with the default (shortest) readout
            [RO, ~, Ir] = unique(Config.Tele.ROTimeS);
            Cnt = accumarray(Ir, Config.Tele.Npt, [numel(RO) 1]);
            [~, Imax]          = max(Cnt);
            Config.RO_Time     = RO(Imax);
            if numel(RO)>1
                Other = RO(RO~=Config.RO_Time);
                Config.RO_TimeZero = min(Other);
            end
            Row = find(Config.Tele.ROTimeS==Config.RO_Time, 1);
            Config.Nrows        = Config.Tele.Nrows(Row);
            Config.Nrow0Resets  = Config.Tele.Nrow0Resets(Row);
            Config.PdResetDelay = Config.Tele.PdResetDelay(Row);
            Config.RO_Formula   = roTimeFormula(Config.Nrows, Config.Nrow0Resets, Config.PdResetDelay);
        end
    end

    %--- thermometry (the declared measurement_temperature is not reliable)
    Config.Temperature = NaN;
    Config.TempSpread  = NaN;
    TempFile = fullfile(MetaDir, 'temperature.csv');
    if isfile(TempFile)
        Opt = csvOpts();
        T = readtable(TempFile, Opt{:});
        Col = find(contains(T.Properties.VariableNames, 'Sensor2[deg]'), 1);
        if ~isempty(Col)
            V = T{:,Col};
            V = V(isfinite(V));
            if ~isempty(V)
                Config.Temperature = median(V);
                Config.TempSpread  = 0.5*diff(prctile(V, [5 95]));
            end
        end
    end

    %--- the mechanical shutter: every open and close, as logged
    Config.Shutter = struct;
    Logs = dir(fullfile(MetaDir, 'bonn_shutter_log_*.csv'));
    for Il=1:1:numel(Logs)
        Mode = regexp(Logs(Il).name, '^bonn_shutter_log_(.+)\.csv$', 'tokens', 'once');
        % An INTERACTIVE run writes one log per step, named after the step
        % itself ('wl400_expsen15s_expsh0.001s'), which is not a valid field
        % name; keep the original in .Name so the step can still be matched.
        Fld = matlab.lang.makeValidName(Mode{1});
        Config.Shutter.(Fld) = shutterIntervals(fullfile(Logs(Il).folder, Logs(Il).name));
        Config.Shutter.(Fld).Name = Mode{1};
    end

    if Args.Verbosity>0
        fprintf('readUCConfig: %s  %s  %s  T=%.2f C  RO=%.5f s (formula %.5f)\n', ...
                RunName, Config.MeasType, Config.ScfgName, Config.Temperature, ...
                Config.RO_Time, Config.RO_Formula);
    end
end

function Opt = csvOpts()
    % readtable options for the instrument's CSV files: the delimiter has to be
    % stated, otherwise the timestamp columns defeat the automatic detection
    % and the whole file is read as two variables
    Opt = {'VariableNamingRule','preserve', 'Delimiter',',', ...
           'ReadVariableNames',true, 'TextType','string'};
end

function RO = roTimeFormula(Nrows, Nrow0Resets, PdResetDelayUs)
    % Closed form of the readout time of this instrument [s].
    %   RO = 2 * (n_rows + n_row0_resets - 2) * (1.000 ms + photodiode_reset_delay)
    %   The factor 2 is the odd/even column pair sharing one ADC block. The
    %   form reproduces all six readout times seen in the 2026 campaign to the
    %   microsecond; it is computed only to be compared with the telemetry.
    RO = 2 * (Nrows + Nrow0Resets - 2) * (1e-3 + PdResetDelayUs*1e-6);
end

function T = teleStates(File)
    % distinct camera states in a telemetry file, with the number of points in each
    Opt = csvOpts();
    D = readtable(File, Opt{:});
    Need = {'n_rows','n_row0_resets','photodiode_reset_delay_us','exposure_us', ...
            'cycle_duration_us','cycle_reset_delay_s','image_readout_time_us'};
    if ~all(ismember(Need, D.Properties.VariableNames))
        T = table;
        return;
    end
    V = D{:, Need};
    V = V(V(:,7)>0, :);                       % drop rows with no readout time recorded
    [U, ~, Ir] = unique(V, 'rows');
    Npt = accumarray(Ir, 1);
    T = table(U(:,1), U(:,2), U(:,3), U(:,4)/1e6, U(:,5)/1e6, U(:,6), U(:,7)/1e6, Npt, ...
              'VariableNames', {'Nrows','Nrow0Resets','PdResetDelay','ExposureS','CycleS','ResetDelayS','ROTimeS','Npt'});
    T = sortrows(T, 'ExposureS');
end

function S = shutterIntervals(File)
    % open/close pairs of a Bonn-shutter log
    S = struct('Open',[], 'Close',[], 'Interval',[], 'Name','');
    Opt = csvOpts();
    D = readtable(File, Opt{:});
    if isempty(D) || ~all(ismember({'time_stamp','operation'}, D.Properties.VariableNames))
        return;
    end
    Time = datetime(string(D.time_stamp), 'InputFormat','yyyy-MM-dd HH:mm:ss.SSSSSSXXX', ...
                    'TimeZone','UTC', 'Format','yyyy-MM-dd''T''HH:mm:ss.SSS');
    Op   = string(D.operation);
    IsOpen = Op=="open";
    Io = find(IsOpen);
    Open  = NaT(0,1, 'TimeZone','UTC');
    Close = NaT(0,1, 'TimeZone','UTC');
    for Ii=1:1:numel(Io)
        Ic = find(~IsOpen & (1:numel(Op)).'>Io(Ii), 1);
        if isempty(Ic)
            continue;
        end
        Open(end+1,1)  = Time(Io(Ii));    %#ok<AGROW>
        Close(end+1,1) = Time(Ic);        %#ok<AGROW>
    end
    S.Open     = Open;
    S.Close    = Close;
    S.Interval = seconds(Close - Open);
end

function S = readToml(File)
    % Minimal TOML reader for the flat key = value files of this instrument.
    %   Section headers are ignored (the keys are unique across sections);
    %   values are numbers, numeric lists, booleans, 0b... bit literals or
    %   strings. Inline '#' comments are stripped. Nothing else is supported,
    %   which is all these files contain.
    S = struct;
    Txt = fileread(File);
    Txt = regexprep(Txt, '"""(.|\n|\r)*?"""', '');          % drop triple-quoted blocks
    Lines = strsplit(Txt, {sprintf('\r\n'), sprintf('\n')});
    Ic = 0;
    for Il=1:1:numel(Lines)
        L = strtrim(Lines{Il});
        if isempty(L) || startsWith(L, '#') || startsWith(L, '[')
            continue;
        end
        % a list may be spread over several lines: join until the bracket closes
        if count(L, '[')>count(L, ']')
            Ic = Il;
            while Ic<numel(Lines) && count(L, '[')>count(L, ']')
                Ic = Ic + 1;
                L  = [L, ' ', strtrim(Lines{Ic})];           %#ok<AGROW>
            end
        end
        Eq = strfind(L, '=');
        if isempty(Eq)
            continue;
        end
        Key = strtrim(L(1:Eq(1)-1));
        Val = strtrim(L(Eq(1)+1:end));
        if ~isvarname(Key)
            continue;
        end
        Val = stripComment(Val);
        S.(Key) = parseTomlValue(Val);
    end
end

function V = stripComment(V)
    % remove a trailing # comment that is not inside quotes
    InQ = false;
    for Ii=1:1:numel(V)
        if V(Ii)=='"'
            InQ = ~InQ;
        elseif V(Ii)=='#' && ~InQ
            V = V(1:Ii-1);
            break;
        end
    end
    V = strtrim(V);
end

function Out = parseTomlValue(V)
    if isempty(V)
        Out = '';
    elseif startsWith(V, '[')
        Body = strtrim(V(2:end));
        Body = regexprep(Body, '\]\s*$', '');
        if isempty(strtrim(Body))
            Out = [];
        else
            Parts = strsplit(Body, ',');
            Parts = Parts(~cellfun(@(s) isempty(strtrim(s)), Parts));
            Num   = cellfun(@(s) str2double(strtrim(s)), Parts);
            if all(isfinite(Num))
                Out = Num(:).';
            else
                Out = cellfun(@(s) strtrim(strrep(s, '"', '')), Parts, 'UniformOutput',false);
            end
        end
    elseif startsWith(V, '"') || startsWith(V, '''')
        Out = V(2:end-1);
    elseif strcmpi(V, 'true')
        Out = true;
    elseif strcmpi(V, 'false')
        Out = false;
    elseif startsWith(V, '0b')
        Out = bin2dec(V(3:end));
    else
        Num = str2double(V);
        if isfinite(Num)
            Out = Num;
        else
            Out = V;
        end
    end
end

function V = getNum(S, Field, Default)
    % field of a parsed TOML structure, or a default
    if isfield(S, Field)
        V = S.(Field);
    else
        V = Default;
    end
end
