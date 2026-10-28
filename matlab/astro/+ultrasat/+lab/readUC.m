function [AI, Frames, Sidecar] = readUC(DieDir, Args)
    % Read DESY uc_st_measurement FITS frames of one die into an AstroImage array.
    %   This is the DESY-native counterpart of ultrasat.lab.readPTC, which reads
    %   the aSpect wafer-tester TIFFs. It returns the same three outputs with
    %   the same meaning, so that PTCAnalysis and the desy_die_* stages work on
    %   either source without knowing which one they are looking at.
    %
    %   A die directory holds one sub-directory per measurement run,
    %     <Lot>_<Wafer>_<Die>_<Type>_<Date>_<Time>/measurement/raw_images/*.fits
    %   and one run contains only one kind of ladder, so an analysis needs a
    %   PAIR of runs: a DC run for the dark ladder and a PTCexpsh (or PTCexpsen)
    %   run for the bright one. The pair is named in 'Test' as
    %       '<darkRun>+<brightRun>'
    %   with either the full run directory names or the part after the die tag.
    %   A single run name on its own is also accepted (that ladder only).
    %
    %   Frame types follow the aSpect convention:
    %     'D'  <- IMG-MODE 'DARK'  of the dark run   (ExpTime = sensor exposure)
    %     'B'  <- IMG-MODE 'LIGHT' of the bright run (ExpTime = sensor exposure,
    %                                                 Intensity = shutter opening)
    %     'ZE' <- IMG-MODE 'ZERO'  of whichever run(s) 'ZeroFrom' selects
    %   Step is the measurement-point index M + 1, FrameIndex the repetition
    %   index R + 1, both taken from the file name and checked against the
    %   MP-INDEX / RP-INDEX header cards.
    %
    %   Geometry. In these files NAXIS1 is the READOUT ROW index
    %   (STARTROW..ENDROW) and NAXIS2 the column index (STARTCOL..ENDCOL), so
    %   the array as stored already has the readout columns running along its
    %   rows - which is what the aSpect reader calls the DESY orientation and
    %   what rawColGeom and parityMap expect. No transposition is applied and
    %   ORIENT is set to 'ucfits' to say so. CCDSEC is [Xmin Xmax Ymin Ymax]
    %   with X along the readout rows and Y along the readout columns, as in
    %   readPTC. RAW=T frames include the dummy readout rows 1 and 2, which are
    %   not science pixels; they are not removed here, exclude them with CCDSEC.
    %
    %   Exposure. The commanded exposure of this instrument is
    %   t_exp = RO_time + Reset_delay and both terms are MEASURED per run in
    %   telemetry_CameraSegmentA.csv, so 'ExpTimeOffset' accepts, besides a
    %   number, the string 'ro': subtract that run's own readout time, i.e.
    %   work in charge-collecting time without assuming a value.
    %
    % Input  : - Die directory, e.g. /Data1/uc_data-test/LT112181_W14_D1.
    %          * ...,key,val,...
    %            'Test' - '<darkRun>+<brightRun>', or one run name. Required.
    %            'FrameType' - 'all', or a char or cell array of 'B','D','ZE'.
    %                   Default is 'all'.
    %            'Step' - Step numbers to read. Empty for all. Default is [].
    %            'FrameIndex' - Frame indices to read. Empty for all. Default [].
    %            'ReadImage' - Read the pixel data. Default is true.
    %            'Gain' - 'high' or 'low': which gain-channel file to read.
    %                   Default is 'high'.
    %            'Orient' - 'ucfits' (as stored). Default is 'ucfits'.
    %            'CCDSEC' - [Xmin Xmax Ymin Ymax], empty for the whole frame.
    %                   Default is [].
    %            'FlipUD' - Flip the returned frames vertically. Default false.
    %            'ExpTimeOffset' - [s] subtracted from the B and D exposure
    %                   times, or 'ro' for the measured readout time of the
    %                   run the frame belongs to. Default is 0.
    %            'DropFrames' - cell array of file-name fragments; any frame
    %                   whose name contains one of them is left out of the
    %                   inventory. For acquisition faults that make a frame
    %                   invalid rather than merely odd - a LIGHT frame read out
    %                   before the shutter opened, say - which is a judgement
    %                   about the instrument and so is named explicitly here
    %                   instead of being guessed from the pixel values.
    %            'ZeroFrom' - which run's ZERO frames become 'ZE': 'dark',
    %                   'bright' or 'both'. Default is 'both'.
    %            'ZeroMode' - INTERACTIVE runs only: which image mode is the
    %                   reference the ladder is measured against, 'DARK' or
    %                   'ZERO'. Those runs carry both, at the SAME 15 s
    %                   exposure: the ZERO frames use one row-0 reset and a
    %                   10 us photodiode delay, the DARK frames the same 20 and
    %                   300 us as the light frames. So the DARK frames are the
    %                   exact zero-light counterpart of the ladder and
    %                   subtracting them removes the bias, the dark signal and
    %                   the reset pedestal together - which is why they, and
    %                   not the ZERO frames, are the default reference.
    %                   Default is 'DARK'.
    %            'ShutterTime' - bright Intensity axis: 'measured' (the
    %                   open-to-close interval of bonn_shutter_log) or
    %                   'commanded' (shutter_exposure_list).
    %                   Default is 'measured'.
    %            'Verbosity' - 0 silent, 1 report progress. Default is 0.
    % Output : - An AstroImage column array sorted by frame type (B, D, ZE),
    %            step and frame index.
    %          - A table with one row per frame: FileName, FrameType, Step,
    %            FrameIndex, ExpTime, Intensity, DateObs.
    %          - A structure with Result, Log, Config, Calib, where Config is
    %            the merged configuration of the pair (see readUCConfig) with
    %            the aSpect-compatible fields Dark_ExpTime, PTC_ExpTime,
    %            Bright_Intensity added, and Result.Info carries the identity.
    % Author : A.M. Krassilchtchikov (Oct 2026)
    % Example: [AI,F,S] = ultrasat.lab.readUC('/Data1/uc_data-test/LT112181_W14_D1', ...
    %              'Test','DC_2026-08-16_01-30-13+PTCexpsh_2026-08-19_17-25-19', ...
    %              'FrameType','ZE', 'CCDSEC',[2046 2151 2446 2575]);

    arguments
        DieDir
        Args.Test                     = '';
        Args.FrameType                = 'all';
        Args.Step                     = [];
        Args.FrameIndex               = [];
        Args.ReadImage(1,1) logical   = true;
        Args.Gain                     = 'high';
        Args.Orient                   = 'ucfits';
        Args.CCDSEC                   = [];
        Args.FlipUD(1,1) logical      = false;
        Args.ExpTimeOffset            = 0;
        Args.DropFrames               = {};
        Args.ZeroFrom                 = 'both';
        Args.ZeroMode                 = 'DARK';
        Args.ShutterTime              = 'measured';
        Args.Verbosity(1,1) double    = 0;
    end

    TypeOrder = {'B', 'D', 'ZE'};
    DieDir = regexprep(DieDir, '[/\\]+$', '');
    [~, DieTag] = fileparts(DieDir);

    %--- the run pair
    [DarkRun, BrightRun] = parsePair(Args.Test, DieDir, DieTag);
    Runs = struct('Name',{}, 'Dir',{}, 'Config',{}, 'Role',{});
    if ~isempty(DarkRun)
        Runs(end+1) = struct('Name',DarkRun, 'Dir',fullfile(DieDir,DarkRun), ...
                             'Config',ultrasat.lab.readUCConfig(fullfile(DieDir,DarkRun)), 'Role','dark');
    end
    if ~isempty(BrightRun)
        Runs(end+1) = struct('Name',BrightRun, 'Dir',fullfile(DieDir,BrightRun), ...
                             'Config',ultrasat.lab.readUCConfig(fullfile(DieDir,BrightRun)), 'Role','bright');
    end
    if isempty(Runs)
        error('ultrasat:lab:readUC:noRun', 'No run selected by Test = "%s"', Args.Test);
    end

    %--- sidecar
    Sidecar = struct('Result',[], 'Log',[], 'Config',[], 'Calib',struct);
    Sidecar.Config = mergeConfig(Runs, Args);
    Sidecar.Result = resultInfo(Runs, Sidecar.Config, Args.Test);

    %--- frame inventory
    P = struct('File',{}, 'Name',{}, 'FrameType',{}, 'Step',{}, 'FrameIndex',{}, ...
               'Run',{}, 'Role',{}, 'XPosure',{}, 'Mode',{}, 'Inter',{});
    Dropped = {};
    for Ir=1:1:numel(Runs)
        [Pr, Dr] = runFrames(Runs(Ir), Args);
        P = [P, Pr];                                                         %#ok<AGROW>
        Dropped = [Dropped, Dr];                                             %#ok<AGROW>
    end
    Sidecar.Config.DroppedFrames = Dropped;
    if ~isempty(Dropped) && Args.Verbosity>0
        fprintf('readUC: %d frame(s) excluded by DropFrames\n', numel(Dropped));
    end
    if isempty(P)
        error('ultrasat:lab:readUC:noFrames', 'No FITS frames found for Test = "%s"', Args.Test);
    end
    % An INTERACTIVE run keeps its ladder in the file names, so the bright axis
    % and the sensor exposure are known only once the frames have been listed.
    if any([P.Inter])
        Sidecar.Config = interConfig(Sidecar.Config, P, Runs, Args);
    end

    Flag = true(1, numel(P));
    if ~strcmpi(Args.FrameType, 'all')
        Flag = Flag & ismember({P.FrameType}, cellstr(Args.FrameType));
    end
    if ~isempty(Args.Step)
        Flag = Flag & ismember([P.Step], Args.Step);
    end
    if ~isempty(Args.FrameIndex)
        Flag = Flag & ismember([P.FrameIndex], Args.FrameIndex);
    end
    P = P(Flag);
    if isempty(P)
        error('ultrasat:lab:readUC:noSelection', 'No frames left after the FrameType/Step/FrameIndex selection');
    end
    [~, TypeRank] = ismember({P.FrameType}, TypeOrder);
    TypeRank(TypeRank==0) = numel(TypeOrder) + 1;
    [~, SortInd] = sortrows([TypeRank(:), [P.Step].', [P.FrameIndex].']);
    P  = P(SortInd);
    Nf = numel(P);

    %--- common header keys
    CommonHeader = commonKeys(Sidecar, Args.Test, Runs);

    %--- read
    AI        = AstroImage([Nf, 1]);
    ExpTime   = nan(Nf,1);
    Intensity = nan(Nf,1);
    DateObs   = cell(Nf,1);
    for If=1:1:Nf
        if Args.Verbosity>0
            fprintf('readUC: %d/%d %s\n', If, Nf, P(If).Name);
        end
        [ExpTime(If), Intensity(If), OffApplied] = frameExposure(P(If), Sidecar.Config, Args);
        try
            [Img, FitsHeader, Geom] = readFrame(P(If).File, Args);
            AI(If).Image = Img;
            DateObs{If}  = Geom.DateObs;
            P(If).XPosure = getKey(FitsHeader, 'XPOSURE', NaN);
            if ~P(If).Inter
                checkFrame(P(If), FitsHeader);
            end
            FrameHeader = {'DATE-OBS', Geom.DateObs,       'Frame acquisition start (UTC)';
                           'FRMTYPE',  P(If).FrameType,    'Frame type: B bright, D dark, ZE zero exposure';
                           'STEP',     P(If).Step,         'Step number within the frame type';
                           'FRMINDEX', P(If).FrameIndex,   'Frame index within the step';
                           'EXPTIME',  ExpTime(If),        'Exposure time used by the fits [s]';
                           'EXPTCMD',  P(If).XPosure,      'Commanded sensor exposure XPOSURE [s]';
                           'EXPTOFFA', OffApplied,         'Exposure-time offset APPLIED [s]';
                           'INTENS',   Intensity(If),      'Shutter opening of a bright frame [s]';
                           'UCRUN',    P(If).Run,          'Run directory the frame comes from';
                           'UCROLE',   P(If).Role,         'Role of that run in the pair';
                           'GAINSEL',  lower(Args.Gain),   'Gain channel file: high or low';
                           'ORIENT',   'ucfits',           'ucfits: array as stored (readout rows along NAXIS1)';
                           'RAWSEC',   sprintf('[%d:%d,%d:%d]', Geom.RawSec), 'Section read [x1:x2,y1:y2]';
                           'RAWXOFF',  0,                  'Kept for compatibility: no gain-half offset here';
                           'METACOLS', 0,                  'No counter columns in these files'};
            AI(If).HeaderData.Data = [FitsHeader; FrameHeader; CommonHeader];
            AI(If).ImageData.FileName = P(If).File;
        catch ME
            warning('ultrasat:lab:readUC:read', 'Failed to read %s (%s)', P(If).File, ME.message);
        end
    end

    Frames = table({P.Name}.', {P.FrameType}.', [P.Step].', [P.FrameIndex].', ExpTime, Intensity, DateObs, ...
                   'VariableNames', {'FileName','FrameType','Step','FrameIndex','ExpTime','Intensity','DateObs'});
end

%----------------------------------------------------------------------------
function [DarkRun, BrightRun] = parsePair(Test, DieDir, DieTag)
    % run names of the pair; each may be given full or without the die tag
    DarkRun   = '';
    BrightRun = '';
    if isempty(Test)
        error('ultrasat:lab:readUC:noTest', 'Test must name the run(s): ''<darkRun>+<brightRun>''');
    end
    Parts = strsplit(char(Test), '+');
    Parts = strtrim(Parts(~cellfun(@isempty, strtrim(Parts))));
    for Ip=1:1:numel(Parts)
        Name = Parts{Ip};
        if ~isfolder(fullfile(DieDir, Name))
            Full = [DieTag, '_', Name];
            if isfolder(fullfile(DieDir, Full))
                Name = Full;
            else
                error('ultrasat:lab:readUC:noRunDir', 'No run directory %s (nor %s) in %s', Parts{Ip}, Full, DieDir);
            end
        end
        Type = regexp(Name, '_([A-Za-z]+)_\d{4}-\d\d-\d\d_', 'tokens', 'once');
        if isempty(Type)
            error('ultrasat:lab:readUC:runName', 'Cannot read the measurement type from %s', Name);
        end
        switch upper(Type{1})
            case {'DC', 'DS', 'ZES'}
                DarkRun = Name;
            case {'PTCEXPSH', 'PTCEXPSEN', 'PTCINT', 'QE', 'INTERACTIVE'}
                BrightRun = Name;
            otherwise
                error('ultrasat:lab:readUC:runType', 'Unknown measurement type %s in %s', Type{1}, Name);
        end
    end
end

function [P, Dropped] = runFrames(Run, Args)
    % frame records of one run, from the file names, checked against the headers
    P = struct('File',{}, 'Name',{}, 'FrameType',{}, 'Step',{}, 'FrameIndex',{}, ...
               'Run',{}, 'Role',{}, 'XPosure',{}, 'Mode',{}, 'Inter',{});
    Dropped = {};
    Dir0 = fullfile(Run.Dir, 'measurement', 'raw_images');
    Files = dir(fullfile(Dir0, sprintf('*_%s_*_DLRAW_I*_M*_R*.fits', upper(Args.Gain))));
    if isempty(Files)
        return;
    end
    IsInter = strcmpi(Run.Config.MeasType, 'INTERACTIVE');
    if IsInter
        % The INTERACTIVE layout puts the ladder in the FILE NAME rather than in
        % MP-INDEX, which is 0 for every frame of the run: the context token is
        % 'wl400_expsen15s_expsh0.05s' for a light frame and '..._DARK' or
        % '..._ZERO' for the two references. The step is therefore the rank of
        % the shutter opening among those present, and the repetition index has
        % to be renumbered within the step because R runs across the whole run.
        [Steps, ShutterOf] = interSteps(Files, Args);
    end
    Rep = containers.Map('KeyType','double', 'ValueType','double');
    for If=1:1:numel(Files)
        if IsInter
            [Mode, Sh] = interMode(Files(If).name);
            if isempty(Mode)
                continue;
            end
            if strcmp(Mode, 'LIGHT')
                St = find(abs(Steps - Sh) < 1e-9, 1);
                if isempty(St)
                    continue;
                end
            else
                St = 1;
            end
            % the reference the ladder is measured against becomes 'ZE'
            if strcmp(Mode, upper(Args.ZeroMode))
                Mode = 'ZERO';
            elseif any(strcmp(Mode, {'DARK','ZERO'}))
                continue;            % the other reference is not analysed here
            end
            Key = St + 1000*double(strcmp(Mode,'LIGHT'));
            if isKey(Rep, Key)
                Rep(Key) = Rep(Key) + 1;
            else
                Rep(Key) = 1;
            end
            Tok = {upper(Args.Gain), Mode, '0', num2str(St-1), num2str(Rep(Key)-1)};
        else
            Tok = regexp(Files(If).name, '_(HIGH|LOW)_(DARK|LIGHT|ZERO)_DLRAW_I(\d+)_M(\d+)_R(\d+)\.fits$', 'tokens', 'once');
        end
        if isempty(Tok)
            continue;
        end
        Mode = Tok{2};
        switch Mode
            case 'DARK',  Type = 'D';
            case 'LIGHT', Type = 'B';
            case 'ZERO',  Type = 'ZE';
            otherwise,    continue;
        end
        % a run contributes only the ladder its role defines, plus the zeros
        % that ZeroFrom selects
        Keep = (strcmp(Type,'D') && strcmp(Run.Role,'dark')) || ...
               (strcmp(Type,'B') && strcmp(Run.Role,'bright')) || ...
               (strcmp(Type,'ZE') && (strcmpi(Args.ZeroFrom,'both') || strcmpi(Args.ZeroFrom, Run.Role)));
        if ~Keep
            continue;
        end
        if ~isempty(Args.DropFrames) && any(contains(Files(If).name, cellstr(Args.DropFrames)))
            Dropped{end+1} = Files(If).name;                                 %#ok<AGROW>
            continue;
        end
        P(end+1) = struct('File',fullfile(Files(If).folder, Files(If).name), ...
                          'Name',Files(If).name, 'FrameType',Type, ...
                          'Step',str2double(Tok{4})+1, 'FrameIndex',str2double(Tok{5})+1, ...
                          'Run',Run.Name, 'Role',Run.Role, 'XPosure',NaN, 'Mode',Mode, ...
                          'Inter',IsInter);                                        %#ok<AGROW>
    end
    % ZERO frames of two runs would collide on (Step, FrameIndex): offset the
    % second run's repetition index so that every frame keeps a unique key
    if strcmpi(Args.ZeroFrom, 'both') && strcmp(Run.Role, 'bright')
        Iz = strcmp({P.FrameType}, 'ZE');
        for Ii=find(Iz)
            P(Ii).FrameIndex = P(Ii).FrameIndex + 1000;
        end
    end
end

function C = interConfig(C, P, Runs, Args)
    % Fill the bright axis of an INTERACTIVE run from its frame names: the
    % sensor exposure from the 'expsen' token and the ladder from 'expsh',
    % replaced where possible by the interval the shutter log actually
    % measured for that step.
    Ib = strcmp({P.FrameType}, 'B');
    if ~any(Ib)
        return;
    end
    Pb = P(Ib);
    [St, Iu] = unique([Pb.Step]);
    Cmd = nan(1, numel(St));
    for Ii=1:1:numel(St)
        [~, Sh] = interMode(Pb(Iu(Ii)).Name);
        Cmd(Ii) = Sh;
    end
    X = Cmd;
    Bright = [];
    for Ir=1:1:numel(Runs)
        if strcmp(Runs(Ir).Role, 'bright')
            Bright = Runs(Ir).Config;
        end
    end
    if ~strcmpi(Args.ShutterTime, 'commanded') && ~isempty(Bright) && isstruct(Bright.Shutter)
        Fn = fieldnames(Bright.Shutter);
        for Ii=1:1:numel(St)
            for If=1:1:numel(Fn)
                Nm = Bright.Shutter.(Fn{If}).Name;
                Sh = regexp(Nm, 'expsh([0-9.]+)s$', 'tokens', 'once');
                if ~isempty(Sh) && abs(str2double(Sh{1}) - Cmd(Ii)) < 1e-9
                    Iv = Bright.Shutter.(Fn{If}).Interval;
                    if ~isempty(Iv)
                        X(Ii) = median(Iv);
                    end
                    break;
                end
            end
        end
    end
    C.Bright_Intensity  = X;
    C.ShutterCommanded  = Cmd;
    Tok = regexp(Pb(1).Name, 'expsen([0-9.]+)s', 'tokens', 'once');
    if ~isempty(Tok)
        C.PTC_ExpTime = str2double(Tok{1});
    end
    C.Interactive = true;
    C.ZeroMode    = upper(Args.ZeroMode);
end

function [Mode, Shutter] = interMode(Name)
    % image mode and shutter opening of an INTERACTIVE frame, from its name
    Mode = '';
    Shutter = NaN;
    Tok = regexp(Name, '_(?:HIGH|LOW)_(.+?)_DLRAW_I\d+_M\d+_R\d+\.fits$', 'tokens', 'once');
    if isempty(Tok)
        return;
    end
    Ctx = Tok{1};
    Sh = regexp(Ctx, 'expsh([0-9.]+)s$', 'tokens', 'once');
    if ~isempty(Sh)
        Mode    = 'LIGHT';
        Shutter = str2double(Sh{1});
    elseif endsWith(Ctx, '_DARK')
        Mode = 'DARK';
    elseif endsWith(Ctx, '_ZERO')
        Mode = 'ZERO';
    end
end

function [Steps, ShutterOf] = interSteps(Files, Args)
    % the shutter openings present in an INTERACTIVE run, ascending
    ShutterOf = containers.Map('KeyType','char', 'ValueType','double');
    V = [];
    for If=1:1:numel(Files)
        [Mode, Sh] = interMode(Files(If).name);
        if strcmp(Mode, 'LIGHT')
            V(end+1) = Sh;                                                   %#ok<AGROW>
            ShutterOf(Files(If).name) = Sh;
        end
    end
    Steps = unique(V);
    if isempty(Steps)
        error('ultrasat:lab:readUC:interSteps', 'No INTERACTIVE light frames of gain %s', Args.Gain);
    end
end

function [Image, Header, Geom] = readFrame(File, Args)
    % one FITS frame (or its header), with the CCDSEC section and the keys we keep
    Info = fitsinfo(File);
    Sz   = Info.PrimaryData.Size;          % [NAXIS2 NAXIS1] as MATLAB reads it
    Ny   = Sz(1);                          % readout columns (image rows)
    Nx   = Sz(2);                          % readout rows    (image columns)
    CCDSEC = Args.CCDSEC;
    if isempty(CCDSEC)
        CCDSEC = [1 Nx 1 Ny];
    end
    if any(CCDSEC<1) || CCDSEC(2)>Nx || CCDSEC(4)>Ny || CCDSEC(1)>CCDSEC(2) || CCDSEC(3)>CCDSEC(4)
        error('ultrasat:lab:readUC:ccdsec', 'CCDSEC [%d %d %d %d] is outside the %dx%d image', CCDSEC, Nx, Ny);
    end
    Geom.RawSec = [CCDSEC(1) CCDSEC(2) CCDSEC(3) CCDSEC(4)];
    if Args.ReadImage
        Image = fitsread(File, 'primary', 'PixelRegion', {[CCDSEC(3) CCDSEC(4)], [CCDSEC(1) CCDSEC(2)]});
        Image = double(Image);
        if Args.FlipUD
            Image = flipud(Image);
        end
    else
        Image = [];
    end
    Header = keepKeys(Info.PrimaryData.Keywords, CCDSEC);
    Geom.DateObs = getKey(Info.PrimaryData.Keywords, 'DATE', '');
end

function checkFrame(P, Header)
    % the file name and the header must agree about which frame this is
    Mp = getKey(Header, 'MP-INDEX', NaN);
    Rp = getKey(Header, 'RP-INDEX', NaN);
    Md = getKey(Header, 'IMG-MODE', '');
    if isfinite(Mp) && Mp+1~=P.Step
        error('ultrasat:lab:readUC:mpindex', '%s: MP-INDEX %d but step %d from the name', P.Name, Mp, P.Step);
    end
    if isfinite(Rp) && mod(P.FrameIndex,1000)~=Rp+1
        error('ultrasat:lab:readUC:rpindex', '%s: RP-INDEX %d but frame index %d from the name', P.Name, Rp, P.FrameIndex);
    end
    if ~isempty(Md) && ~strcmp(strtrim(Md), P.Mode)
        error('ultrasat:lab:readUC:imgmode', '%s: IMG-MODE %s but %s from the name', P.Name, strtrim(Md), P.Mode);
    end
end

function Header = keepKeys(K, CCDSEC)
    % the FITS cards worth carrying into the AstroImage header
    Keep = {'BITPIX','NAXIS','NAXIS1','NAXIS2','INDEX','TIMECAM','DATE','XPOSURE','TELAPSE', ...
            'MJD-BEG','MJD-END','UUIDIMG','GCHANNEL','GTHRESH','RAW','BUNIT','PIXSIZE','SEGMENT', ...
            'PIXORIG','ROWDIR','STARTROW','ENDROW','STARTCOL','ENDCOL','MP-INDEX','RP-INDEX', ...
            'IMG-MODE','VDDD','VDDA','VDDRSTL','VDDRSTH','VDDRSTS','VDDSF','VDDTX','VDDSEN', ...
            'PROGRAM','CONTEXT','DLEVEL','TEMP-MON','RUN-BEG','RUN-NAME','UVL-TYPE','UVL-USED', ...
            'BS-USED','VER-GSS','VER-STME','ID-DIE','ID-LOT','ID-WAFER', ...
            'CIFRTIME','CILRTIME','NIFRTIME','NILRTIME'};
    Name = K(:,1);
    Sel  = ismember(Name, Keep);
    Header = K(Sel, 1:3);
    % NAXIS1/2 describe the section actually returned
    Header{strcmp(Header(:,1),'NAXIS1'), 2} = CCDSEC(2)-CCDSEC(1)+1;
    Header{strcmp(Header(:,1),'NAXIS2'), 2} = CCDSEC(4)-CCDSEC(3)+1;
end

function V = getKey(K, Name, Default)
    Ind = find(strcmp(K(:,1), Name), 1);
    if isempty(Ind)
        V = Default;
    else
        V = K{Ind,2};
    end
end

function [ExpTime, Intensity, Off] = frameExposure(P, Config, Args)
    % exposure time and intensity of one frame
    %   D: the sensor exposure of that step; B: the sensor exposure of the
    %   bright run; ZE: 0, as in the aSpect reader, although these frames do
    %   integrate the default exposure - the difference is reported, not hidden
    %   (see ZEXPTIME in the common header).
    Intensity = 0;
    Off = resolveOffset(Args.ExpTimeOffset, Config, P.Role);
    switch P.FrameType
        case 'D'
            ExpTime = pick(Config, 'Dark_ExpTime', P.Step) - Off;
        case 'B'
            ExpTime   = pick(Config, 'PTC_ExpTime', 1) - Off;
            Intensity = pick(Config, 'Bright_Intensity', P.Step);
        case 'ZE'
            ExpTime = 0;
        otherwise
            ExpTime = NaN;
    end
end

function Off = resolveOffset(Spec, Config, Role)
    % numeric exposure-time offset: a number, or the measured readout time
    if isnumeric(Spec)
        Off = Spec;
        return;
    end
    if ~(ischar(Spec) || isstring(Spec)) || ~any(strcmpi(Spec, {'ro','measured'}))
        error('ultrasat:lab:readUC:offset', 'ExpTimeOffset must be a number or ''ro''');
    end
    Field = 'RO_TimeDark';
    if strcmp(Role, 'bright')
        Field = 'RO_TimeBright';
    end
    if isfield(Config, Field) && isfinite(Config.(Field))
        Off = Config.(Field);
    else
        error('ultrasat:lab:readUC:noRO', 'No measured readout time for the %s run', Role);
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
            warning('ultrasat:lab:readUC:step', 'Step %d is outside %s (%d values)', Step, Field, numel(V));
        end
    end
end

function C = mergeConfig(Runs, Args)
    % one configuration structure for the pair, with aSpect-compatible fields
    Dark   = [];
    Bright = [];
    for Ir=1:1:numel(Runs)
        if strcmp(Runs(Ir).Role, 'dark')
            Dark = Runs(Ir).Config;
        else
            Bright = Runs(Ir).Config;
        end
    end
    Base = Dark;
    if isempty(Base)
        Base = Bright;
    end
    C = Base;
    C.DarkRun      = '';
    C.BrightRun    = '';
    C.RO_TimeDark  = NaN;
    C.RO_TimeBright= NaN;
    C.Dark_ExpTime = [];
    C.PTC_ExpTime  = NaN;
    C.Bright_Intensity = [];

    if ~isempty(Dark)
        C.DarkRun       = Dark.RunName;
        C.RO_TimeDark   = Dark.RO_Time;
        C.Dark_ExpTime  = Dark.CameraExpList(:).';
        C.DarkTemp      = Dark.Temperature;
        C.DarkZeroExp   = Dark.ExposureUs/1e6;
        C.Dark_Frames   = Dark.NDark;
        C.ZeroExp_Frames= Dark.NZero;
        C.DarkScfg      = Dark.ScfgName;
        C.DarkRampAdj   = Dark.zDUT_RampAdj;   % bs_ramp_adj: the ADC ramp field that
                                               % separates cfg_aSpect from cfg_aSpect2
    end
    if ~isempty(Bright)
        C.BrightRun      = Bright.RunName;
        C.RO_TimeBright  = Bright.RO_Time;
        C.PTC_ExpTime    = Bright.CameraExpTime;
        C.BrightTemp     = Bright.Temperature;
        C.BrightZeroExp  = Bright.ExposureUs/1e6;
        C.Bright_Frames  = Bright.NBright;
        C.BrightScfg     = Bright.ScfgName;
        C.BrightRampAdj  = Bright.zDUT_RampAdj;
        % Whether the bright ladder's zero reference already contains the dark
        % signal. An INTERACTIVE run referenced to its own DARK frames (same
        % sensor exposure, same reset setting) has the dark term removed
        % exactly by the subtraction, so a DC*ExpSen pedestal must NOT be added
        % to it afterwards - that would count the dark twice.
        C.BrightInter    = strcmpi(Bright.MeasType, 'INTERACTIVE');
        C.NrowsBright    = Bright.Nrows;   % the bright run's own readout window

        C.BrightZeroMode = 'ZERO';
        if C.BrightInter
            C.BrightZeroMode = upper(Args.ZeroMode);
        end
        C.Bright_Intensity = brightIntensity(Bright, Args);
        C.ShutterCommanded = Bright.ShutterExpList(:).';
    end
    C.ConfigPair = sprintf('%s | %s', C.DarkRun, C.BrightRun);
end

function X = brightIntensity(Bright, Args)
    % the bright ladder's X axis: the measured shutter opening per step, or the
    % commanded one. The shutter is mechanical, so the two differ.
    Cmd = Bright.ShutterExpList(:).';
    if strcmpi(Args.ShutterTime, 'commanded') || ~isfield(Bright, 'Shutter') || ...
       ~isfield(Bright.Shutter, 'LIGHT') || isempty(Bright.Shutter.LIGHT.Interval)
        X = Cmd;
        return;
    end
    Iv = Bright.Shutter.LIGHT.Interval(:);
    Nf = Bright.NBright;
    Ns = numel(Cmd);
    if ~isfinite(Nf) || Nf<1 || numel(Iv)~=Nf*Ns
        warning('ultrasat:lab:readUC:shutter', ...
                ['The shutter log has %d intervals, not %d steps x %d frames; ' ...
                 'falling back to the commanded shutter times'], numel(Iv), Ns, Nf);
        X = Cmd;
        return;
    end
    X = mean(reshape(Iv, Nf, Ns), 1);
end

function R = resultInfo(Runs, Config, Test)
    % the identity block the chain expects in Sidecar.Result
    %   The FITS cards ID-DIE / ID-LOT / ID-WAFER are rotated with respect to
    %   their names (ID-DIE holds the lot), so the identity is taken from the
    %   run directory name, whose order is documented in the measurement
    %   configuration, and the header cards are only checked against it.
    C = Runs(1).Config;
    Info = struct('LOTID',C.Lot, 'WaferID',C.Wafer, 'DeviceNo',C.Die, ...
                  'RunNo',C.RunDate, 'OperatorName','', 'TestStation','DESY', ...
                  'TestSequenceName',C.MeasType, 'SequenceRev','', ...
                  'TesterTemperature',C.Temperature, 'Base',Test);
    R = struct('Info',Info, 'Trailer',struct('Pass','N/A', 'SoftBin',NaN), 'Raw',{{}});
    if isfield(Config, 'ConfigPair')
        R.Info.ConfigPair = Config.ConfigPair;
    end
end

function Header = commonKeys(Sidecar, Test, Runs)
    % header keys shared by all frames of the pair
    C = Sidecar.Config;
    Header = cell(0,3);
    Header(end+1,:) = {'LOTID',    C.Lot,   'Lot ID (from the run directory name)'};
    Header(end+1,:) = {'WAFERID',  C.Wafer, 'Wafer ID'};
    Header(end+1,:) = {'DEVICE',   C.Die,   'Die ID'};
    Header(end+1,:) = {'TESTNAME', Test,    'Run pair'};
    Header(end+1,:) = {'STATION',  'DESY',  'Test station'};
    Map = {'DARKRUN',  'DarkRun',         'Dark-ladder run';
           'BRIGHTRN', 'BrightRun',       'Bright-ladder run';
           'ROTIMED',  'RO_TimeDark',     'Measured readout time, dark run [s]';
           'ROTIMEB',  'RO_TimeBright',   'Measured readout time, bright run [s]';
           'ZEXPTIME', 'DarkZeroExp',     'Sensor exposure of the ZERO frames [s]';
           'TESTTEMP', 'Temperature',     'Measured temperature [C]';
           'TEMPDARK', 'DarkTemp',        'Measured temperature, dark run [C]';
           'TEMPBRGT', 'BrightTemp',      'Measured temperature, bright run [C]';
           'NROWS',    'Nrows',           'Readout rows addressed';
           'NROW0RST', 'Nrow0Resets',     'Row-0 resets per frame';
           'PDRSTDLY', 'PdResetDelay',    'Photodiode reset delay [us]';
           'NBRIGHT',  'Bright_Frames',   'Bright frames per step';
           'NDARK',    'Dark_Frames',     'Dark frames per step';
           'NZEROEXP', 'ZeroExp_Frames',  'Zero-exposure frames';
           'SCFGDARK', 'DarkScfg',        'Sensor configuration, dark run';
           'SCFGBRGT', 'BrightScfg',      'Sensor configuration, bright run';
           'VDDA',     'zVDDA',           'VDDA [V]';
           'VDDD',     'zVDDD',           'VDDD [V]';
           'VDD_SEN',  'zVDD_SEN',        'VDD_SEN [V]';
           'VDD_SF',   'zVDD_SF',         'VDD_SF [V]';
           'VDD_TX',   'zVDD_TX',         'VDD_TX [V]';
           'VDDRST_L', 'zVDD_RST_L',      'VDD_RST_L [V]';
           'VDDRST_H', 'zVDD_RST_H',      'VDD_RST_H [V]';
           'VDDRSTSL', 'zVDD_RST_SEL',    'VDD_RST_SEL [V]';
           'ADC_CAP',  'zDUT_ADC_CAP',    'ADC cap register';
           'ADC_ADJ',  'zDUT_ADC_ADJ',    'ADC adjust register';
           'COL_ADJ',  'zDUT_COL_ADJ',    'Column adjust register';
           'CNTSETVL', 'zDUT_CntSetVal',  'Counter set value'};
    for Im=1:1:size(Map,1)
        if isfield(C, Map{Im,2})
            V = C.(Map{Im,2});
            if isnumeric(V) && ~isscalar(V)
                continue;
            end
            Header(end+1,:) = {Map{Im,1}, V, Map{Im,3}};                     %#ok<AGROW>
        end
    end
    for Ir=1:1:numel(Runs)
        RC = Runs(Ir).Config;
        if isfinite(RC.RO_Time) && isfinite(RC.RO_Formula) && abs(RC.RO_Time-RC.RO_Formula)>1e-5
            warning('ultrasat:lab:readUC:roTime', ...
                    'Run %s: telemetry readout time %.6f s differs from the formula %.6f s', ...
                    RC.RunName, RC.RO_Time, RC.RO_Formula);
        end
    end
end
