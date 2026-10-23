function F = saveAsync(filename, varargin)
    % Asynchronous save to MAT-file (returns prompt immediately).
    %   Good for saving large matrices.
    % SYNTAX
    %   F = saveAsync(FileName, VarName1, VarName2, ...)
    %   F = saveAsync(FileName, S)
    %   F = saveAsync(FileName, 'VarName1', Var1, 'VarName2', Var2, ...)
    %   F = saveAsync(..., 'MatVersion', VERSION)
    %
    % DESCRIPTION
    %   Fires a background task that writes variables to a MAT-file while MATLAB
    %   prompt is returned immediately. Variables are snapshotted at call time.
    %   The task runs on backgroundPool when it exists (R2021b+), otherwise on
    %   the current parallel pool (gcp('nocreate'), process workers - the
    %   variables are copied to the worker), otherwise the file is saved
    %   synchronously and F is empty. The file is written by io.files.saveMat,
    %   which falls back to '-v7.3' for a variable too large for the
    %   requested version. '-v7.3' cannot be saved on threads, so with
    %   backgroundPool a '-v7.3' file (requested, or a variable over 2 GB,
    %   see io.files.chooseMatVersion) is saved synchronously and F is empty.
    %
    % Input  : - FileName    : char|string. Output MAT-file path.
    %          * Either:              
    %                 char(s). Variable names to capture from the *caller*
    %                 workspace (e.g., 'A','B','C').
    %
    %                 scalar struct. Fields become variables in the file
    %                 (e.g., S.A, S.B -> 'A','B' in MAT-file).
    %
    %                 explicit name/value pairs to save (e.g., 'R', R).
    %
    %           NAME-VALUE
    %                 'MatVersion': char|string. MAT-file version flag for SAVE.
    %                 Default: '-v7.3'. Examples: '-v7.3', '-v7', '-v6'.
    %
    % Output : - F : parallel.FevalFuture, or [] if the file was saved
    %            synchronously (no pool, or '-v7.3' with backgroundPool).
    %            Use F.State, wait(F), F.Error, or fetchOutputs(F)
    %            (returns the MAT version written). An error in the save is
    %            reported only through the future.
    % Authors: ChatGPT + Eran Ofek (Oct 2025)
    % Example:
    %   % Capture by names from caller:
    %   A = rand(1e3); B = magic(256);
    %   F = saveAsync('out.mat','A','B');
    %
    %   % Struct input:
    %   S = struct('A',A,'B',B);
    %   F = saveAsync('out.mat', S);
    %
    %   % Name/value pairs (your style):
    %   R = rand(1726,1726);
    %   F = saveAsync('my.mat','R',R);
    %
    %   % Choose MAT version:
    %   F = saveAsync('out.mat','A','MatVersion','-v7');
    %
    % NOTES
    %   - Without a pool the save is synchronous (blocking).
    %   - Uses parfeval(backgroundPool|gcp, ...) and io.files.saveMat under the hood.
    %   - Data is copied to the worker at call time (no live linkage).
    %   - Use '-v7.3' for very large arrays (>2 GB). '-v7' is smaller/faster for
    %     modest variables (no objects/tall arrays; 2 GB limit).
    %
    % ERRORS
    %   - Invalid variable name(s).
    %   - Mixed struct input with other arguments.
    %   - Odd number of name/value inputs.
    

    % ---- Defaults
    matVersion = '-v7.3';

    % ---- Parse trailing 'MatVersion' if present
    if numel(varargin) >= 2 && ischar(varargin{end-1}) && strcmpi(varargin{end-1}, 'MatVersion')
        matVersion = varargin{end};
        varargin(end-1:end) = [];
    end

    % ---- Build struct S with data to save
    S = struct();

    if ~isempty(varargin) && isscalar(varargin{1}) && isstruct(varargin{1})
        % Case 2: struct provided directly
        if numel(varargin) ~= 1
            error('saveAsync:TooManyInputs', ...
                  'When passing a struct, do not mix with other variable arguments.');
        end
        S = varargin{1};

    elseif ~isempty(varargin) && all(cellfun(@ischar, varargin))
        % Case 1: list of variable names (capture from caller)
        names = string(varargin);
        for k = 1:numel(names)
            namek = char(names(k));
            if ~isvarname(namek)
                error('saveAsync:BadName','"%s" is not a valid variable name.', namek);
            end
            S.(namek) = evalin('caller', namek);
        end

    else
        % Case 3: name/value pairs
        if mod(numel(varargin),2) ~= 0
            error('saveAsync:Pairs', 'Name/value inputs must come in pairs.');
        end
        for i = 1:2:numel(varargin)
            name = varargin{i};
            val  = varargin{i+1};
            if ~ischar(name) || ~isvarname(name)
                error('saveAsync:BadName','"%s" is not a valid variable name.', string(name));
            end
            S.(name) = val;
        end
    end

    % ---- Pool: backgroundPool (R2021b+), else the current parallel pool
    Pool = [];
    if exist('backgroundPool') > 0 %#ok<EXIST>
        Pool = backgroundPool;
    elseif license('test', 'Distrib_Computing_Toolbox')
        Pool = gcp('nocreate');
    end

    % ---- Kick off the background save (returns immediately)
    % '-v7.3' cannot be saved on threads (backgroundPool), so a file that
    % needs it (requested, or a variable over 2 GB) is saved synchronously.
    matVersion = io.files.chooseMatVersion(S, matVersion);
    IsV73 = any(strcmp(matVersion, '-v7.3'));
    if isempty(Pool) || (IsV73 && (isa(Pool, 'parallel.BackgroundPool') || isa(Pool, 'parallel.ThreadPool')))
        io.files.saveMat(filename, S, matVersion, Inf);
        F = [];
    else
        F = parfeval(Pool, @io.files.saveMat, 1, filename, S, matVersion, Inf);
    end

    % If you want a console ping on completion, uncomment:
    % afterAll(F, @(~) fprintf('Async save complete: %s\n', filename), 0);
end
