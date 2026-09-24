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
    % Output : - F : parallel.FevalFuture. Use F.State, wait(F), fetchOutputs(F)
    %            (no outputs expected), or afterAll(F,@cb,0) for a callback.
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
    %   - Requires Parallel Computing Toolbox with backgroundPool (R2021a+).
    %   - Uses parfeval(backgroundPool, ...) and SAVE -struct under the hood.
    %   - Data is copied to the worker at call time (no live linkage).
    %   - Use '-v7.3' for very large arrays (>2 GB). '-v7' is smaller/faster for
    %     modest variables (no objects/tall arrays; 2 GB limit).
    %
    % ERRORS
    %   - Invalid variable name(s).
    %   - Mixed struct input with other arguments.
    %   - Odd number of name/value inputs.
    %   - Missing backgroundPool / unsupported MATLAB version.
    

    % ---- Defaults
    matVersion = '-v7.3';

    % ---- Parse trailing 'MatVersion' if present
    if numel(varargin) >= 2 && ischar(varargin{end-1}) && strcmpi(varargin{end-1}, 'MatVersion')
        matVersion = varargin{end};
        varargin(end-1:end) = [];
    end

    % ---- Build struct S with data to save
    S = struct();

    if ~isempty(varargin) && isscalar(varargin{1}) && isstruct(varargin{1}) && ~isscalar(filename)
        % Case 2: struct provided directly (and not mistaken for a name)
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

    % ---- Ensure background pool exists (Parallel Computing Toolbox, R2021a+)
    try
        bg = backgroundPool; %#ok<NASGU>
    catch
        error(['saveAsync requires Parallel Computing Toolbox with backgroundPool ', ...
               '(MATLAB R2021a or newer).']);
    end

    % ---- Kick off the background save (returns immediately)
    % Use '-struct' with the struct variable itself (no name string needed).
    F = parfeval(backgroundPool, @(fn,SS,opt) save(fn, '-struct', SS, opt), ...
                 0, filename, S, matVersion);

    % If you want a console ping on completion, uncomment:
    % afterAll(F, @(~) fprintf('Async save complete: %s\n', filename), 0);
end
