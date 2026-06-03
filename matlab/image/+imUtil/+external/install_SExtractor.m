function [Result] = install_SExtractor(Args)
    % Install SExtractor on the current computer.
    % Uses the system package manager (apt/dnf/yum on Linux, brew on macOS).
    % Installing requires administrative privileges (sudo on Linux) -- if
    % run from MATLAB, the sudo password prompt is forwarded to the
    % command window when '-echo' is supported. If SExtractor is already
    % on PATH the function reports its location and exits without
    % reinstalling (override with 'Force').
    %
    % Input  : * ...,key,val,...
    %            'BinLink' - If non-empty, after a successful install a
    %                   symlink named after this path (typically '.../sex')
    %                   is created pointing to the installed executable.
    %                   Use empty to skip. Default '/home/eran/bin/sex'.
    %            'Force' - If true, run the install command even when an
    %                   existing executable is detected. Default false.
    %            'DryRun' - If true, print the command that would be
    %                   executed and exit. Default false.
    %            'PackageManager' - Override auto-detection. One of
    %                   'auto','apt','dnf','yum','brew'. Default 'auto'.
    % Output : - Result: struct with fields .Installed (logical),
    %            .Executable (path on PATH), .Command (string actually
    %            run, or attempted), .Status (system exit code), .Output
    %            (combined stdout/stderr).
    % Example: imUtil.external.install_SExtractor();
    %          imUtil.external.install_SExtractor('BinLink','');

    arguments
        Args.BinLink        char    = '/home/eran/bin/sex'
        Args.Force          logical = false
        Args.DryRun         logical = false
        Args.PackageManager char    = 'auto'
    end

    Result = struct('Installed',false,'Executable','','Command','', ...
                    'Status',NaN,'Output','');

    % Already installed?
    Existing = locateSExtractor();
    if ~isempty(Existing) && ~Args.Force
        Result.Installed  = true;
        Result.Executable = Existing;
        Result.Status     = 0;
        Result.Output     = sprintf('Already installed at %s', Existing);
        fprintf('%s\n', Result.Output);
        maybeCreateLink(Existing, Args.BinLink);
        return
    end

    % Pick package manager
    if strcmpi(Args.PackageManager, 'auto')
        PM = detectPackageManager();
    else
        PM = lower(Args.PackageManager);
    end

    switch PM
        case 'apt'
            Cmd = 'sudo apt update && sudo apt install -y source-extractor';
        case {'dnf','yum'}
            Cmd = sprintf('sudo %s install -y sextractor', PM);
        case 'brew'
            Cmd = 'brew install sextractor';
        otherwise
            error('Unsupported package manager: %s', PM);
    end

    Result.Command = Cmd;
    fprintf('Installing SExtractor via %s ...\n  %s\n', PM, Cmd);
    if ~strcmp(PM, 'brew')
        fprintf('(you may be prompted for your sudo password)\n');
    end

    if Args.DryRun
        fprintf('(dry run -- not executed)\n');
        return
    end

    [Status, Output] = system(Cmd, '-echo');
    Result.Status = Status;
    Result.Output = Output;
    if Status ~= 0
        error('Install failed (status %d).\nCommand: %s\nOutput:\n%s', ...
              Status, Cmd, Output);
    end

    Result.Executable = locateSExtractor();
    if isempty(Result.Executable)
        error(['Install command succeeded but executable not found on ', ...
               'PATH. Check your shell PATH or call source-extractor by ', ...
               'full path.']);
    end
    Result.Installed = true;
    fprintf('Installed: %s\n', Result.Executable);

    maybeCreateLink(Result.Executable, Args.BinLink);
end


function P = locateSExtractor()
    Candidates = {'source-extractor','sextractor','sex'};
    for I = 1:numel(Candidates)
        [St, Out] = system(['command -v ' Candidates{I}]);
        if St == 0
            P = strtrim(Out);
            return
        end
    end
    P = '';
end


function PM = detectPackageManager()
    if ismac
        PM = 'brew';
        return
    end
    if ispc
        error(['Automatic install on Windows is not supported. ', ...
               'Use WSL/Cygwin or download a binary manually.']);
    end
    for Name = {'apt','dnf','yum'}
        [St,~] = system(['command -v ' Name{1}]);
        if St == 0
            PM = Name{1};
            return
        end
    end
    error('No supported package manager found (apt/dnf/yum).');
end


function maybeCreateLink(Target, LinkPath)
    if isempty(LinkPath)
        return
    end
    LinkDir = fileparts(LinkPath);
    if ~isempty(LinkDir) && ~isfolder(LinkDir)
        warning('BinLink directory does not exist: %s -- skipping symlink.', LinkDir);
        return
    end
    [St, Existing] = system(sprintf('readlink ''%s''', LinkPath));
    if St == 0
        fprintf('Symlink already exists: %s -> %s\n', LinkPath, strtrim(Existing));
        return
    end
    if isfile(LinkPath)
        fprintf('File already exists at link path (not a symlink): %s -- skipping.\n', LinkPath);
        return
    end
    [St, Out] = system(sprintf('ln -s ''%s'' ''%s''', Target, LinkPath));
    if St == 0
        fprintf('Created symlink: %s -> %s\n', LinkPath, Target);
    else
        warning('Failed to create symlink: %s', strtrim(Out));
    end
end
