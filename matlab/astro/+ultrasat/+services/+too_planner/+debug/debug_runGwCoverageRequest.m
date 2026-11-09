%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/+debug/debug_runGwCoverageRequest.m
% Author      : Chen Tishler
% Created     : 09/11/2026
% Description : Run TooPlannerRunner on GW coverage S250119cv request.json
%               (Windows: \\socsrv\rwshare_data\... ; Linux: /data/rwshare_data/...).
%               Intended for Chen laptop (socsrv share mounted) or socsrv itself.
%
% Run by      : ultrasat.services.too_planner.debug.debug_runGwCoverageRequest()
%==========================================================================

function debug_runGwCoverageRequest()
    % Run TooPlannerRunner using the GW coverage experiment request on socsrv share.

    fprintf('\n========== DEBUG GW COVERAGE TOO REQUEST ==========\n');

    ensureDebugEnvironment();

    relRequest = fullfile( ...
        'gw_coverage_experiment_2026_08', '2025', '01', '19', ...
        'S250119cv', '01_preliminary', 'too_planner', 'request.json');
    requestJson = fullfile(getRwshareRoot(), relRequest);

    if ~isfile(requestJson)
        error('debug_runGwCoverageRequest:RequestNotFound', ...
            'request.json not found: %s', requestJson);
    end

    fprintf('Source request: %s\n', requestJson);

    try
        txt = fileread(requestJson);
        cfg = jsondecode(txt);
    catch Ex
        error('debug_runGwCoverageRequest:ReadJson', ...
            'Failed reading %s: %s', requestJson, Ex.message);
    end

    cfg = remapRequestSharePaths(cfg);

    tempRequestJson = fullfile(tempdir, ...
        sprintf('debug_gw_coverage_request_%s.json', datestr(now, 'yyyymmdd_HHMMSS_FFF')));
    writeJsonFile(tempRequestJson, cfg);
    fprintf('Temp request (paths remapped for this OS): %s\n', tempRequestJson);

    runner = ultrasat.planner.TooPlannerRunner();
    summaryFile = runner.runFromJson(tempRequestJson);

    if isempty(summaryFile)
        fprintf('TooPlannerRunner finished with no summary (check logs).\n');
    else
        fprintf('summary.json: %s\n', summaryFile);
    end

    fprintf('========== DEBUG GW COVERAGE TOO REQUEST DONE ==========\n\n');
end

% -------------------------------------------------------------------------

function ensureDebugEnvironment()
    % Set SOC_PATH and ASTROPACK_DATA_PATH fallbacks for local / IDE runs.

    if isempty(getenv('SOC_PATH'))
        fprintf('SOC_PATH not set. Setting fallback for local testing...\n');
        if ispc
            setenv('SOC_PATH', 'c:\soc');
        else
            setenv('SOC_PATH', '~/soc');
        end
    end

    if isempty(getenv('ASTROPACK_DATA_PATH'))
        fprintf('ASTROPACK_DATA_PATH not set. Using fallback for local testing...\n');
        if ispc
            setenv('ASTROPACK_DATA_PATH', 'C:\AstroPack\matlab\data');
        else
            setenv('ASTROPACK_DATA_PATH', '~/matlab/data');
        end
    end
end

% -------------------------------------------------------------------------

function root = getRwshareRoot()
    % rwshare root on Windows (UNC) vs Linux (socsrv mount).

    if ispc
        root = '\\socsrv\rwshare_data';
    else
        root = '/data/rwshare_data';
    end
end

% -------------------------------------------------------------------------

function cfg = remapRequestSharePaths(cfg)
    % Remap csv_filename and output_folder to the current OS rwshare root.

    if isfield(cfg, 'csv_filename') && ~isempty(cfg.csv_filename)
        cfg.csv_filename = remapSharePath(cfg.csv_filename);
    end
    if isfield(cfg, 'output_folder') && ~isempty(cfg.output_folder)
        cfg.output_folder = remapSharePath(cfg.output_folder);
    end
end

% -------------------------------------------------------------------------

function out = remapSharePath(pathIn)
    % Map any socsrv/rwshare_data or /data/rwshare_data prefix to getRwshareRoot().

    if isempty(pathIn)
        out = pathIn;
        return;
    end

    p = char(string(pathIn));
    pNorm = strrep(p, '\', '/');

    tok = regexp(pNorm, 'rwshare_data[/\\]?(.*)$', 'tokens', 'once');
    if isempty(tok) || isempty(tok{1})
        out = p;
        return;
    end

    rel = tok{1};
    relParts = strsplit(rel, '/');
    relParts = relParts(~cellfun(@isempty, relParts));
    if isempty(relParts)
        out = getRwshareRoot();
    else
        out = fullfile(getRwshareRoot(), relParts{:});
    end
end

% -------------------------------------------------------------------------

function writeJsonFile(filename, cfg)
    % Write struct as pretty JSON.

    txt = jsonencode(cfg, 'PrettyPrint', true);
    fid = fopen(filename, 'w');
    if fid < 0
        error('debug_runGwCoverageRequest:WriteJson', ...
            'Failed opening %s for write', filename);
    end
    cleaner = onCleanup(@() fclose(fid)); %#ok<NASGU>
    fwrite(fid, txt, 'char');
end
