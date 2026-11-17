function Result = execListener(Args)
    % Watch a directory for MATLAB script files, run them, and archive them. An "execute scripts dropped into a folder" loop.
    %
    %   Turns a directory into a simple MATLAB execution channel: whenever a new
    %   *.m script file appears in the watched directory, the listener waits
    %   until it is fully written, RUNS it, captures everything it prints (and
    %   any error + stack) into a result log next to it, then moves the script
    %   into ./executed so it is not run again. It keeps listening until a stop
    %   file appears (or a timeout / idle / Ctrl-C).
    %
    %   Intended use: an external agent (or the user) writes a .m script into the
    %   watched directory; the running listener executes it and writes a readable
    %   <script>.out.txt result that the author can then read back.
    %
    %   Partial-write safe (size-stability guard) and portable (MATLAB `dir`).
    %
    % Input  : * ...,key,val,...   (defaults in parentheses)
    %            'Dir'          - Directory to watch for incoming *.m scripts.
    %                             Created if missing (fullfile(pwd,'mlexec')).
    %            'Template'     - File glob to match ('*.m').
    %            'ExecutedDir'  - Where executed scripts are moved. '' ->
    %                             fullfile(Dir,'executed') (i.e. ./executed).
    %            'ResultDir'    - Where <name>.out.txt logs are written. '' ->
    %                             ExecutedDir.
    %            'Workspace'    - Where scripts run:
    %                             'base'    - the base workspace, so variables
    %                                         PERSIST across successive scripts
    %                                         (a REPL-like session). Does not
    %                                         support script-local functions.
    %                             'sandbox' - a fresh private workspace per
    %                                         script (isolated; supports
    %                                         script-local functions; no
    %                                         persistence). ('base')
    %            'PollSec'      - Seconds between polls (1).
    %            'StableChecks' - Consecutive polls a script's size must be
    %                             unchanged before it is run (1). Guards against
    %                             executing a half-written file.
    %            'RunExisting'  - Also run *.m scripts already present at start
    %                             (true). Set false to ignore pre-existing ones.
    %            'StopFile'     - A file whose appearance in Dir stops the loop
    %                             gracefully ('STOP'). It is not executed.
    %            'MaxScripts'   - Stop after running this many scripts (Inf).
    %            'MaxEmptyPolls'- Stop after this many idle polls (Inf).
    %            'Timeout'      - Stop after this many seconds (Inf).
    %            'EchoOutput'   - Also echo each script's captured output to the
    %                             console as it finishes (true).
    %            'Verbose'      - Print listener progress (true).
    % Output : - Result: struct with fields:
    %            .Scripts    - struct array (Name, Success, OutFile, Error, When).
    %            .Nok        - number of scripts that ran without error.
    %            .Nfail      - number that errored.
    %            .StopReason - 'stopfile'|'maxscripts'|'maxemptypolls'|'timeout'|
    %                          'error'|'interrupt'.
    %            .ElapsedSec - wall time of the run.
    %
    % Author : Claude (Opus 4.8) for E. Ofek, Nov 2026.
    %
    % Example:
    %   % In MATLAB, start the listener (blocks, prints progress):
    %   tools.listener.execListener('Dir','/raid/eran/projects/telescopes/PAST/images/mlexec');
    %
    %   % Then drop a script into that dir, e.g. mlexec/a001_try.m containing:
    %   %     AI = pipelinePastI('....fits','BinFactor',[2 2]);  disp(AI(1).Nsrc)
    %   % The listener runs it, writes mlexec/executed/a001_try.out.txt, and
    %   % moves a001_try.m to mlexec/executed/. Read the .out.txt for the result.
    %
    %   % Stop it by creating the stop file:  touch mlexec/STOP

    arguments
        Args.Dir char                      = fullfile(pwd, 'mlexec');
        Args.Template char                 = '*.m';
        Args.ExecutedDir char              = '';
        Args.ResultDir char                = '';
        Args.Workspace char {mustBeMember(Args.Workspace,{'base','sandbox'})} = 'base';
        Args.PollSec (1,1) double          = 1;
        Args.StableChecks (1,1) double     = 1;
        Args.RunExisting logical           = true;
        Args.StopFile char                 = 'STOP';
        Args.MaxScripts (1,1) double       = Inf;
        Args.MaxEmptyPolls (1,1) double    = Inf;
        Args.Timeout (1,1) double          = Inf;
        Args.EchoOutput logical            = true;
        Args.Verbose logical               = true;
    end

    ensureDir(Args.Dir);
    if isempty(Args.ExecutedDir);  Args.ExecutedDir = fullfile(Args.Dir, 'executed');  end
    if isempty(Args.ResultDir);    Args.ResultDir   = Args.ExecutedDir;                end
    ensureDir(Args.ExecutedDir);
    ensureDir(Args.ResultDir);

    % state
    Processed  = containers.Map('KeyType','char','ValueType','logical');
    SizeMap    = containers.Map('KeyType','char','ValueType','any'); % name -> [lastBytes stableCount]
    Result     = struct('Scripts',struct('Name',{},'Success',{},'OutFile',{},'Error',{},'When',{}), ...
                        'Nok',0, 'Nfail',0, 'StopReason','', 'ElapsedSec',0);
    EmptyPolls = 0;
    T0         = tic;

    if ~Args.RunExisting
        Pre = listScripts(Args.Dir, Args.Template, Args.StopFile);
        for Ip = 1:numel(Pre);  Processed(Pre{Ip}) = true;  end
        vlog(Args.Verbose, 'ignoring %d pre-existing script(s)', numel(Pre));
    end

    vlog(Args.Verbose, 'listening on %s (%s); workspace=%s; executed-> %s', ...
         Args.Dir, Args.Template, Args.Workspace, Args.ExecutedDir);

    try
        while true
            % ---- stop conditions ----
            if ~isempty(Args.StopFile) && isfile(fullfile(Args.Dir, Args.StopFile))
                Result.StopReason = 'stopfile';
                vlog(Args.Verbose, 'stop file found (%s) - stopping', Args.StopFile);
                break;
            end
            if Result.Nok + Result.Nfail >= Args.MaxScripts;  Result.StopReason='maxscripts';     break;  end
            if toc(T0) >= Args.Timeout;                       Result.StopReason='timeout';        break;  end
            if EmptyPolls >= Args.MaxEmptyPolls;              Result.StopReason='maxemptypolls';  break;  end

            % ---- find ready scripts (size-stable, not yet processed) ----
            Cand  = listScripts(Args.Dir, Args.Template, Args.StopFile);
            Ready = {};
            for Ic = 1:numel(Cand)
                FN = Cand{Ic};
                if isKey(Processed, FN);  continue;  end
                D = dir(FN);
                if isempty(D);  continue;  end
                Bytes = D(1).bytes;
                if isKey(SizeMap, FN)
                    SC = SizeMap(FN);
                    if SC(1)==Bytes;  SC(2)=SC(2)+1;  else;  SC=[Bytes 0];  end
                else
                    SC = [Bytes 0];
                end
                SizeMap(FN) = SC;
                if SC(2) >= Args.StableChecks
                    Ready{end+1} = FN; %#ok<AGROW>
                end
            end

            if isempty(Ready)
                EmptyPolls = EmptyPolls + 1;
                pause(Args.PollSec);
                continue;
            end
            EmptyPolls = 0;
            Ready = sort(Ready);   % run in name order (e.g. a001, a002, ...)

            for Ir = 1:numel(Ready)
                runOneScript(Ready{Ir});
                if Result.Nok + Result.Nfail >= Args.MaxScripts;  break;  end
            end
        end

    catch ME
        Result.StopReason = 'error';
        vlog(true, 'listener loop error: %s (%s line %d)', ME.message, ME.stack(1).name, ME.stack(1).line);
    end

    if isempty(Result.StopReason);  Result.StopReason = 'interrupt';  end
    Result.ElapsedSec = toc(T0);
    vlog(Args.Verbose, 'done: %d ok, %d failed, reason=%s, %.1f s', ...
         Result.Nok, Result.Nfail, Result.StopReason, Result.ElapsedSec);

    % ===================== nested helper (shares state) =====================
    function runOneScript(FullName)
        [~, Name, Ext] = fileparts(FullName);
        Base   = [Name Ext];
        When   = datestr(now, 'yyyy-mm-dd HH:MM:SS');
        OutFile = fullfile(Args.ResultDir, [Name '.out.txt']);
        vlog(Args.Verbose, 'running %s', Base);

        [Output, Ok, Err] = i_execScript(FullName, Args.Workspace);

        % write a readable result log
        writeResult(OutFile, Base, When, Ok, Output, Err);
        if Args.EchoOutput
            fprintf('----- %s output -----\n%s\n----- end (%s) -----\n', Base, Output, ternary(Ok,'OK','ERROR'));
        end

        % archive the script (move out of the watch dir so it is not re-run)
        Processed(FullName) = true;   % guard the window until the move completes
        Dest = fullfile(Args.ExecutedDir, Base);
        try
            movefile(FullName, Dest, 'f');
            % moved away: forget it, so re-dropping the SAME name runs fresh
            remove(Processed, FullName);
            if isKey(SizeMap, FullName);  remove(SizeMap, FullName);  end
        catch MEm
            % move failed: keep it marked processed so we do not loop on it
            vlog(true, 'could not move %s to %s: %s', Base, Args.ExecutedDir, MEm.message);
        end

        % record
        Rec = struct('Name',Base, 'Success',Ok, 'OutFile',OutFile, 'Error',Err, 'When',When);
        if isempty(Result.Scripts);  Result.Scripts = Rec;  else;  Result.Scripts(end+1) = Rec;  end
        if Ok;  Result.Nok = Result.Nok + 1;  else;  Result.Nfail = Result.Nfail + 1;  end
        vlog(Args.Verbose, 'done %s -> %s (%s)', Base, OutFile, ternary(Ok,'OK','ERROR'));
    end
end

% ======================= local (file-scope) helpers =======================
function [Output, Ok, Err] = i_execScript(FullName, Workspace)
    % Run a script file, capturing all console output (and any error) via diary.
    DiaryFile = [tempname '.txt'];
    Ok  = true;
    Err = '';
    diary(DiaryFile);
    try
        switch Workspace
            case 'base'
                % run the script text in the base workspace -> variables persist
                % across scripts. (No script-local functions in this mode.)
                evalin('base', fileread(FullName));
            case 'sandbox'
                % fresh private workspace (this function's), supports local funcs
                run(FullName);
        end
    catch ME
        Ok  = false;
        Err = i_formatError(ME);
        fprintf('<<execListener caught error: %s>>\n', ME.message); % short note into the diary
    end
    diary off;
    if isfile(DiaryFile)
        Output = fileread(DiaryFile);
        delete(DiaryFile);
    else
        Output = '';
    end
end

function Str = i_formatError(ME)
    % A concise, script-focused error report: identifier+message, plus only the
    % stack frames outside this listener (so the user's script line is shown,
    % not the execListener/run/evalin machinery).
    if isempty(ME.identifier)
        Lines = {ME.message};
    else
        Lines = {sprintf('%s: %s', ME.identifier, ME.message)};
    end
    for K = 1:numel(ME.stack)
        F = ME.stack(K);
        if isempty(F.file) || endsWith(F.file, 'execListener.m')
            continue;
        end
        [~, N, E] = fileparts(F.file);
        Lines{end+1} = sprintf('  at %s%s line %d (%s)', N, E, F.line, F.name); %#ok<AGROW>
    end
    Str = strjoin(Lines, newline);
end

function writeResult(OutFile, Base, When, Ok, Output, Err)
    % Write a human/agent-readable result log for one executed script.
    FID = fopen(OutFile, 'w');
    if FID < 0
        warning('execListener:resultWrite', 'could not open %s for writing', OutFile);
        return;
    end
    C = onCleanup(@() fclose(FID));
    fprintf(FID, '# script : %s\n', Base);
    fprintf(FID, '# when   : %s\n', When);
    fprintf(FID, '# status : %s\n', ternary(Ok, 'OK', 'ERROR'));
    fprintf(FID, '# ================= output =================\n');
    fprintf(FID, '%s', Output);
    if ~isempty(Output) && Output(end)~=newline;  fprintf(FID, '\n');  end
    if ~Ok
        fprintf(FID, '# ================= error  =================\n');
        fprintf(FID, '%s\n', Err);
    end
end

function ensureDir(D)
    if ~isempty(D) && ~isfolder(D)
        mkdir(D);
    end
end

function Files = listScripts(Dir, Template, StopFile)
    % Non-recursive list of matching script files in Dir (full paths), sorted.
    Files = {};
    D = dir(fullfile(Dir, Template));
    for Id = 1:numel(D)
        if D(Id).isdir;  continue;  end
        Name = D(Id).name;
        if ~isempty(Name) && Name(1)=='.';           continue;  end  % hidden
        if ~isempty(StopFile) && strcmp(Name,StopFile);  continue;  end
        Files{end+1} = fullfile(D(Id).folder, Name); %#ok<AGROW>
    end
end

function Out = ternary(Cond, A, B)
    if Cond;  Out = A;  else;  Out = B;  end
end

function vlog(DoIt, Fmt, varargin)
    if DoIt
        fprintf('[%s] execListener: %s\n', datestr(now,'HH:MM:SS'), sprintf(Fmt, varargin{:}));
    end
end
