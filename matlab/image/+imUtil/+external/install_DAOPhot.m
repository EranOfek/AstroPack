function [Result] = install_DAOPhot(Args)
    % Install Stetson DAOPHOT II from a Fortran source archive.
    %
    % DAOPHOT II is not redistributable through standard package managers
    % -- you must supply either a local source archive ('SourceArchive')
    % or a URL to fetch it from ('SourceURL'). The function extracts the
    % archive, builds the Fortran sources, and copies the resulting
    % executables to InstallDir.
    %
    % Input  : * ...,key,val,...
    %            'SourceArchive' - Path to a local
    %                   .tar.gz / .tgz / .tar.bz2 / .tar.xz / .tar / .zip
    %                   archive with the DAOPHOT source tree. Takes
    %                   priority over SourceURL. Default ''.
    %            'SourceURL' - URL to download the source archive from.
    %                   Used only when SourceArchive is empty. Default ''.
    %            'InstallDir' - Directory where the resulting executables
    %                   are placed. Created if missing.
    %                   Default '/home/eran/bin'.
    %            'BuildDir' - Working directory for extraction and build.
    %                   Default fullfile(tempdir,'daophot_build').
    %            'BuildCommand' - Shell command run inside the source
    %                   directory to build the binaries. If empty, the
    %                   function uses 'make' when a Makefile is present
    %                   in the source dir; otherwise errors with a
    %                   message asking you to supply one.
    %                   Default ''.
    %            'Programs' - Cell array of expected binary names. After
    %                   the build, each existing file is copied from the
    %                   source dir into InstallDir; missing ones are
    %                   reported in Result.Skipped.
    %                   Default {'daophot','allstar','allframe',
    %                            'daomatch','daomaster'}.
    %            'Fortran' - Fortran compiler. Exported as F77 and FC in
    %                   the build environment.
    %                   Default 'gfortran'.
    %            'FortranFlags' - Compile flags exported as FFLAGS.
    %                   Default '-O2 -fno-automatic -std=legacy'.
    %            'Force' - Reinstall even if all expected executables are
    %                   already present in InstallDir. Default false.
    %            'CleanBuildDir' - Remove BuildDir after a successful
    %                   build. Default false.
    % Output : - Result: struct with fields
    %            .Installed (logical), .InstallDir, .BuildDir,
    %            .Binaries (cellstr of installed binary paths),
    %            .Skipped  (cellstr of programs the build did not produce).
    % Example: imUtil.external.install_DAOPhot( ...
    %              'SourceArchive','/data/daophot.tar.gz');
    %          imUtil.external.install_DAOPhot( ...
    %              'SourceURL','https://.../daophot.tar.gz', ...
    %              'BuildCommand','./compile_daophot.csh');

    arguments
        Args.SourceArchive char    = ''
        Args.SourceURL     char    = ''
        Args.InstallDir    char    = '/home/eran/bin'
        Args.BuildDir      char    = ''
        Args.BuildCommand  char    = ''
        Args.Programs      cell    = {'daophot','allstar','allframe', ...
                                      'daomatch','daomaster'}
        Args.Fortran       char    = 'gfortran'
        Args.FortranFlags  char    = '-O2 -fno-automatic -std=legacy'
        Args.Force         logical = false
        Args.CleanBuildDir logical = false
    end

    Result = struct('Installed',false,'InstallDir',Args.InstallDir, ...
                    'BuildDir','','Binaries',{{}},'Skipped',{{}});

    % Already installed?
    if ~Args.Force
        Existing = locateInDir(Args.InstallDir, Args.Programs);
        if all(~cellfun(@isempty, Existing))
            fprintf('DAOPHOT binaries already installed in %s\n', Args.InstallDir);
            Result.Installed = true;
            Result.Binaries  = Existing(:)';
            return
        end
    end

    if isempty(Args.SourceArchive) && isempty(Args.SourceURL)
        error(['Provide ''SourceArchive'' (a local tarball) or ', ...
               '''SourceURL'' to download. DAOPHOT II is not in any ', ...
               'package manager; obtain it from Peter Stetson.']);
    end

    % Fortran compiler must be present
    [St,~] = system(['command -v ' Args.Fortran]);
    if St ~= 0
        error('Fortran compiler not found: %s. Install gfortran first.', Args.Fortran);
    end

    % Build directory
    BuildDir = Args.BuildDir;
    if isempty(BuildDir)
        BuildDir = fullfile(tempdir, 'daophot_build');
    end
    if ~isfolder(BuildDir)
        mkdir(BuildDir);
    end
    Result.BuildDir = BuildDir;

    % Resolve archive (local or download)
    if ~isempty(Args.SourceArchive)
        Archive = Args.SourceArchive;
        if ~isfile(Archive)
            error('SourceArchive not found: %s', Archive);
        end
    else
        Archive = downloadArchive(Args.SourceURL, BuildDir);
    end

    % Extract
    fprintf('Extracting %s ...\n', Archive);
    SrcDir = extractArchive(Archive, BuildDir);
    fprintf('Source dir: %s\n', SrcDir);

    % Build command
    BuildCmd = Args.BuildCommand;
    if isempty(BuildCmd)
        if isfile(fullfile(SrcDir,'Makefile')) || isfile(fullfile(SrcDir,'makefile'))
            BuildCmd = 'make';
        else
            error(['No Makefile found in %s and no ''BuildCommand'' supplied. ', ...
                   'Pass e.g. ''BuildCommand'',''./compile_daophot.csh''.'], SrcDir);
        end
    end

    % Run build
    Env = sprintf('F77=%s FC=%s FFLAGS=''%s''', ...
                  Args.Fortran, Args.Fortran, Args.FortranFlags);
    FullCmd = sprintf('cd ''%s'' && %s %s', SrcDir, Env, BuildCmd);
    fprintf('Building: %s\n', FullCmd);
    [Status, Output] = system(FullCmd, '-echo');
    if Status ~= 0
        error('Build failed (status %d):\n%s', Status, Output);
    end

    % Install binaries
    if ~isfolder(Args.InstallDir)
        mkdir(Args.InstallDir);
    end
    Installed = {};
    Skipped   = {};
    for I = 1:numel(Args.Programs)
        Name = Args.Programs{I};
        Src  = fullfile(SrcDir, Name);
        if isfile(Src)
            Dst = fullfile(Args.InstallDir, Name);
            copyfile(Src, Dst);
            system(sprintf('chmod +x ''%s''', Dst));
            Installed{end+1} = Dst; %#ok<AGROW>
            fprintf('  installed %s -> %s\n', Name, Dst);
        else
            Skipped{end+1} = Name; %#ok<AGROW>
        end
    end

    Result.Binaries  = Installed;
    Result.Skipped   = Skipped;
    Result.Installed = ~isempty(Installed);

    if isempty(Installed)
        error('No DAOPHOT programs were produced by the build. Check %s.', SrcDir);
    end
    if ~isempty(Skipped)
        warning('Did not find expected binaries in %s: %s', ...
                SrcDir, strjoin(Skipped, ', '));
    end

    if Args.CleanBuildDir
        try
            rmdir(BuildDir, 's');
        catch ME
            warning('Failed to clean BuildDir %s: %s', BuildDir, ME.message);
        end
    end
end


function Found = locateInDir(Dir, Names)
    Found = cell(size(Names));
    for I = 1:numel(Names)
        P = fullfile(Dir, Names{I});
        if isfile(P)
            Found{I} = P;
        else
            Found{I} = '';
        end
    end
end


function Archive = downloadArchive(URL, DestDir)
    % Save to <DestDir>/<basename-from-URL>; the file's full name is
    % preserved so compound extensions (.tar.gz) survive for the
    % extraction step.
    URLNoQuery = regexprep(URL, '\?.*$', '');
    Parts = strsplit(URLNoQuery, '/');
    Name  = Parts{end};
    if isempty(Name)
        Name = 'daophot_src';
    end
    DlPath  = fullfile(DestDir, Name);
    fprintf('Downloading %s ...\n', URL);
    Archive = websave(DlPath, URL);
end


function SrcDir = extractArchive(Archive, DestDir)
    Before = listSubdirs(DestDir);
    Lower = lower(Archive);
    if endsWith(Lower, '.tar.gz') || endsWith(Lower, '.tgz')
        Cmd = sprintf('tar -xzf ''%s'' -C ''%s''', Archive, DestDir);
    elseif endsWith(Lower, '.tar.bz2') || endsWith(Lower, '.tbz2')
        Cmd = sprintf('tar -xjf ''%s'' -C ''%s''', Archive, DestDir);
    elseif endsWith(Lower, '.tar.xz') || endsWith(Lower, '.txz')
        Cmd = sprintf('tar -xJf ''%s'' -C ''%s''', Archive, DestDir);
    elseif endsWith(Lower, '.tar')
        Cmd = sprintf('tar -xf ''%s'' -C ''%s''', Archive, DestDir);
    elseif endsWith(Lower, '.zip')
        Cmd = sprintf('unzip -q -o ''%s'' -d ''%s''', Archive, DestDir);
    else
        error('Unknown archive type: %s', Archive);
    end
    [St, Out] = system(Cmd);
    if St ~= 0
        error('Archive extraction failed:\n%s', Out);
    end
    New = setdiff(listSubdirs(DestDir), Before);
    if numel(New) == 1
        SrcDir = fullfile(DestDir, New{1});
    else
        SrcDir = DestDir;
    end
end


function L = listSubdirs(Dir)
    D = dir(Dir);
    L = {D([D.isdir] & ~ismember({D.name},{'.','..'})).name};
end
