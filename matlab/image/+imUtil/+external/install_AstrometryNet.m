function [Result] = install_AstrometryNet(Args)
    % Install astrometry.net and its index (star) files on the current host.
    %
    % Two stages:
    %   1. Install the astrometry.net binaries (solve-field etc.) via the
    %      system package manager. Skipped if already on PATH or if
    %      'SkipPackageInstall' is true.
    %   2. Download index files from http://data.astrometry.net/ for each
    %      requested series into 'IndexDir', and write an astrometry.cfg
    %      that points to that directory.
    %
    % Note  : Index files are large. The default set (series 4100, 4200,
    %         5000, 5200) is on the order of 100 GB. Trim 'IndexSeries' to
    %         match your disk budget.
    %
    % Input  : * ...,key,val,...
    %            'PackageManager' - 'auto','apt','dnf','yum','brew'.
    %                   Default 'auto'.
    %            'SkipPackageInstall' - Do not run the package manager
    %                   (e.g. already installed). Default false.
    %            'IndexDir' - Directory to hold the downloaded index
    %                   files. Created if missing. If empty, uses
    %                   ~/astrometry/data. Default ''.
    %            'IndexBaseURL' - Root URL of the index file server.
    %                   Default 'http://data.astrometry.net'.
    %            'IndexSeries' - Cell array of series names to download.
    %                   Each becomes a subdirectory of the base URL.
    %                   Default {'4100','4200','5000','5200'} (all).
    %            'IndexFilePattern' - wget -A pattern of files to accept
    %                   inside each series directory.
    %                   Default 'index-*.fits'.
    %            'SkipIndex' - Skip the index-file download stage.
    %                   Default false.
    %            'UpdateConfig' - Write an astrometry.cfg into IndexDir
    %                   that contains 'add_path <IndexDir>' and
    %                   'autoindex'. Default true.
    %            'ConfigFile' - Where to write the config. If empty,
    %                   IndexDir/astrometry.cfg is used. Default ''.
    %            'DryRun' - Print commands and exit without executing.
    %                   Default false.
    % Output : - Result: struct with fields
    %            .Installed (logical), .Executable (path to solve-field),
    %            .IndexDir, .Downloaded (cellstr of index files now in
    %            IndexDir), .ConfigFile.
    % Example: imUtil.external.install_AstrometryNet();
    %          imUtil.external.install_AstrometryNet( ...
    %              'IndexSeries',{'5200'},'IndexDir','/data/astrometry');

    arguments
        Args.PackageManager     char    = 'auto'
        Args.SkipPackageInstall logical = false
        Args.IndexDir           char    = ''
        Args.IndexBaseURL       char    = 'http://data.astrometry.net'
        Args.IndexSeries        cell    = {'4100','4200','5000','5200'}
        Args.IndexFilePattern   char    = 'index-*.fits'
        Args.SkipIndex          logical = false
        Args.UpdateConfig       logical = true
        Args.ConfigFile         char    = ''
        Args.DryRun             logical = false
    end

    Result = struct('Installed',false,'Executable','', ...
                    'IndexDir','','Downloaded',{{}}, ...
                    'ConfigFile','');

    % --- Stage 1: package install -----------------------------------------
    Exe = locateSolveField();
    if isempty(Exe) && ~Args.SkipPackageInstall
        installPackage(Args.PackageManager, Args.DryRun);
        if ~Args.DryRun
            Exe = locateSolveField();
        end
    end
    if isempty(Exe) && ~Args.DryRun
        error(['solve-field not found on PATH. Either the package install ', ...
               'failed, or pass ''SkipPackageInstall'',false to run it.']);
    end
    Result.Executable = Exe;

    % --- Stage 2: index files ---------------------------------------------
    IndexDir = Args.IndexDir;
    if isempty(IndexDir)
        IndexDir = fullfile(getenv('HOME'), 'astrometry', 'data');
    end
    Result.IndexDir = IndexDir;
    if ~Args.DryRun && ~isfolder(IndexDir)
        [Ok, Msg] = mkdir(IndexDir);
        if ~Ok
            error('Cannot create IndexDir %s: %s', IndexDir, Msg);
        end
    end

    if ~Args.SkipIndex
        if system('command -v wget >/dev/null') ~= 0
            error('wget not found. Install it or set SkipIndex=true.');
        end
        for I = 1:numel(Args.IndexSeries)
            S   = Args.IndexSeries{I};
            URL = sprintf('%s/%s/', Args.IndexBaseURL, S);
            Cmd = sprintf(['wget --no-verbose -r -np -nH -nd -nc ', ...
                           '-P ''%s'' -A ''%s'' ''%s'''], ...
                          IndexDir, Args.IndexFilePattern, URL);
            fprintf('Downloading series %s into %s\n  %s\n', S, IndexDir, Cmd);
            if Args.DryRun
                continue
            end
            [St, ~] = system(Cmd, '-echo');
            if St ~= 0
                warning('wget exited with status %d for series %s', St, S);
            end
        end
        if ~Args.DryRun
            D = dir(fullfile(IndexDir, 'index-*.fits'));
            Result.Downloaded = fullfile(IndexDir, {D.name});
            fprintf('Index files in %s: %d\n', IndexDir, numel(D));
        end
    end

    % --- Stage 3: config --------------------------------------------------
    if Args.UpdateConfig
        CfgPath = Args.ConfigFile;
        if isempty(CfgPath)
            CfgPath = fullfile(IndexDir, 'astrometry.cfg');
        end
        if ~Args.DryRun
            writeConfig(CfgPath, IndexDir);
        end
        Result.ConfigFile = CfgPath;
        fprintf(['Config written: %s\n', ...
                 '  Use it with:  solve-field --config %s ...\n'], ...
                CfgPath, CfgPath);
    end

    Result.Installed = ~isempty(Exe);
end


function P = locateSolveField()
    [St, Out] = system('command -v solve-field');
    if St == 0
        P = strtrim(Out);
    else
        P = '';
    end
end


function installPackage(PMArg, DryRun)
    if strcmpi(PMArg, 'auto')
        if ismac
            PM = 'brew';
        elseif ispc
            error('Windows install not supported.');
        else
            PM = '';
            for N = {'apt','dnf','yum'}
                if system(['command -v ' N{1} ' >/dev/null']) == 0
                    PM = N{1};
                    break
                end
            end
            if isempty(PM)
                error('No supported package manager found (apt/dnf/yum).');
            end
        end
    else
        PM = lower(PMArg);
    end
    switch PM
        case 'apt'
            Cmd = 'sudo apt install -y astrometry.net';
        case {'dnf','yum'}
            Cmd = sprintf('sudo %s install -y astrometry', PM);
        case 'brew'
            Cmd = 'brew install astrometry-net';
        otherwise
            error('Unsupported package manager: %s', PM);
    end
    fprintf('Installing astrometry.net via %s ...\n  %s\n', PM, Cmd);
    if ~strcmp(PM, 'brew')
        fprintf('(you may be prompted for your sudo password)\n');
    end
    if DryRun
        return
    end
    [St, Out] = system(Cmd, '-echo');
    if St ~= 0
        error('Package install failed (status %d):\n%s', St, Out);
    end
end


function writeConfig(CfgPath, IndexDir)
    Parent = fileparts(CfgPath);
    if ~isempty(Parent) && ~isfolder(Parent)
        mkdir(Parent);
    end
    FID = fopen(CfgPath, 'w');
    if FID < 0
        error('Cannot write config: %s', CfgPath);
    end
    OC = onCleanup(@() fclose(FID)); %#ok<NASGU>
    fprintf(FID, '# astrometry.cfg generated by imUtil.external.install_AstrometryNet\n');
    fprintf(FID, 'autoindex\n');
    fprintf(FID, 'inparallel\n');
    fprintf(FID, 'add_path %s\n', IndexDir);
end
