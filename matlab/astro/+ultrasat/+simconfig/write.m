function [File, Status, Cfg] = write(Cfg, Dir, Args)
    % Save a simulation configuration with its run record, refusing a changed configuration under the same version
    %   The configuration is saved as <Dir>/<Cfg.Meta.Version>.mat (exact
    %   MATLAB types; read back with ultrasat.simconfig.read) and .json (to
    %   read or diff by eye, or from Python), with a field Record added:
    %     .Created    - UTC time of the save, 'yyyy-mm-ddTHH:MM:SS'
    %     .Host, .MatlabRelease
    %     .GitCommit  - the AstroPack commit (git rev-parse HEAD)
    %     .GitStatus  - git status --porcelain of the +ultrasat package
    %                   ('' when it has no local changes)
    %     .Defaults   - per function in 'Defaults': the default expressions
    %                   of its arguments (ultrasat.simconfig.funDefaults),
    %                   i.e. the values of everything the configuration does
    %                   not set. The field name is the function name with
    %                   '.' replaced by '_'.
    %   If the version is saved already, the two configurations are
    %   compared (without Record): equal - nothing is written; different -
    %   an error listing the differing fields, so that one version never
    %   holds products of two configurations. A changed configuration needs
    %   a new version (e.g. 'ConfigN' of ultrasat.dut.config).
    % Input  : - Cfg: the configuration, with Cfg.Meta.Version.
    %          - Dir: the directory (created if missing).
    %          * ...,key,val,...
    %            'Defaults' - functions whose defaults are recorded.
    %                   Default is ["ultrasat.usim", "ultrasat.simulateCatalogField",
    %                   "ultrasat.simulateRefSet"].
    % Output : - File: the .mat file.
    %          - Status: "written" or "same" (saved before, equal).
    %          - Cfg: the configuration as saved, with Record.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.dut.config;
    %          [File, Status] = ultrasat.simconfig.write(Cfg, Cfg.Paths.Config);

    arguments
        Cfg (1,1) struct
        Dir (1,:) char
        Args.Defaults string = ["ultrasat.usim", "ultrasat.simulateCatalogField", "ultrasat.simulateRefSet"]
    end

    Version = char(Cfg.Meta.Version);
    File    = fullfile(Dir, [Version '.mat']);
    if isfile(File)
        Old  = ultrasat.simconfig.read(File);
        Diff = ultrasat.simconfig.compare(Old, Cfg);
        if ~isempty(Diff)
            error('simconfig:write:Changed', ...
                  ['Version %s is saved in %s with a different configuration (%s).\n' ...
                   'Use a new version for a changed configuration.'], Version, File, strjoin(Diff, ', '));
        end
        Status = "same";
        Cfg    = Old;
    else
        Cfg.Record = record(Args.Defaults);
        if ~isfolder(Dir)
            mkdir(Dir);
        end
        save(File, 'Cfg');
        try
            Json = jsonencode(Cfg, 'PrettyPrint', true);
        catch
            Json = jsonencode(Cfg);    % before R2021a: no PrettyPrint
        end
        Fid = fopen(fullfile(Dir, [Version '.json']), 'w');
        fprintf(Fid, '%s\n', Json);
        fclose(Fid);
        Status = "written";
    end
end

function R = record(Funs)
    % when, where and with which code the configuration was saved
    R.Created       = char(datetime('now', 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd''T''HH:mm:ss'));
    [~, Host]       = system('hostname');
    R.Host          = strtrim(Host);
    R.MatlabRelease = version('-release');
    PkgDir          = fileparts(which('ultrasat.usim'));
    [St, Commit]    = system(sprintf('git -C "%s" rev-parse HEAD', PkgDir));
    R.GitCommit     = '';
    if St == 0
        R.GitCommit = strtrim(Commit);
    end
    [St, Changes]   = system(sprintf('git -C "%s" status --porcelain -- .', PkgDir));
    R.GitStatus     = '';
    if St == 0
        R.GitStatus = strtrim(Changes);
    end
    R.Defaults = struct();
    for I = 1:numel(Funs)
        R.Defaults.(strrep(char(Funs(I)), '.', '_')) = ultrasat.simconfig.funDefaults(char(Funs(I)));
    end
end
