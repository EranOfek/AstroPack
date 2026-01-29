classdef TooPlannerRunner < Component
    % TooPlannerRunner
    % Runs ULTRASAT TOO planner multiple times from one JSON config.
    %
    % Input: one JSON file + CSV probability map file referenced by JSON
    % Output: N plans -> N JSON files + N MAT files in output_folder
    %
    % The class is designed to be "safe": failures in one plan do not stop others.

    properties
        PlannerName = "AK";          % 'AK' as requested
    end

    methods
        function Obj = TooPlannerRunner()
        end


        function summaryFileName = runFromJson(Obj, jsonFilename)
            % runFromJson
            %
            % :param jsonFilename: Path to JSON config file.
            summaryFileName = [];

            try
                cfg = Obj.loadJson(jsonFilename);
            catch Ex
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Failed reading JSON %s: %s', jsonFilename, Ex.message);
                return;
            end

            % Apply optional overrides from JSON
            if isfield(cfg, 'planner_name') && ~isempty(cfg.planner_name)
                Obj.PlannerName = string(cfg.planner_name);
            end

            % Validate required fields
            if ~isfield(cfg, 'csv_filename') || isempty(cfg.csv_filename)
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Missing cfg.csv_filename in %s', jsonFilename);
                return;
            end
            if ~isfield(cfg, 'output_folder') || isempty(cfg.output_folder)
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Missing cfg.output_folder in %s', jsonFilename);
                return;
            end
            if ~isfield(cfg, 'plans') || isempty(cfg.plans)
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Missing cfg.plans array in %s', jsonFilename);
                return;
            end

            % Ensure output folder exists
            outFolder = string(cfg.output_folder);
            Obj.ensureFolder(outFolder);

            % Ensure probability map file exists
            csvFile = string(cfg.csv_filename);
            if ~isfile(csvFile)
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: CSV file not found: %s', csvFile);
                return;
            end

            % Load probability map once (reused for all plan runs)
            try
                probMapTable = readtable(csvFile);
            catch Ex
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Failed reading CSV %s: %s', csvFile, Ex.message);
                return;
            end

            % Run each plan independently
            n = numel(cfg.plans);
            Obj.msgLog(LogLevel.Info, 'TooPlannerRunner: Running %d TOO plans from %s', n, jsonFilename);

            % Track all successfully created plan files
            createdPlans = struct('run_id', {}, 'json_file', {}, 'mat_file', {}, 'plan_index', {}, 'status', {}, 'exposures_scheduled', {});

            for i = 1:n
                planCfg = cfg.plans(i);
                planInfo = Obj.runOnePlanSafe(planCfg, probMapTable, outFolder, jsonFilename, i);
                if ~isempty(planInfo)
                    createdPlans(end+1) = planInfo; %#ok<AGROW>
                end
            end

            % Create summary JSON file with all created plans
            summaryFileName = Obj.createSummaryJson(outFolder, jsonFilename, createdPlans, n);            
        end
    end


    methods (Access = private)

        function planInfo = runOnePlanSafe(Obj, planCfg, probMapTable, outFolder, jsonFilename, planIndex)
            % Run one plan with try/catch so pipeline continues on failure.

            % runOnePlanSafe
            %
            % :param planCfg: Plan configuration struct.
            % :param probMapTable: Probability map table.
            % :param outFolder: Path to output folder.
            % :param jsonFilename: Path to JSON file.
            % :param planIndex: Plan index.
            % :return: Struct with plan file information, or empty if failed.

            % Initialize return value
            planInfo = [];

            % Make run ID
            runId = Obj.makeRunId(planIndex, planCfg);

            try
                % Build planner object
                % Build planner
                upTOO = ultrasat.planner.uplanner('AstPlanner', Obj.PlannerName, 'Type', 'TOO');

                % Get planner parameters from config (with defaults)
                tooMaxTargets     = Obj.getField(planCfg, 'TOOMaxTargets', 4);
                tooMinCoveredProb = Obj.getField(planCfg, 'TOOMinCoveredProb', 0.3);
                tooWindowHours    = Obj.getField(planCfg, 'TOOWindowDurationHours', 3);

                % Set planner parameters
                upTOO.TOOMaxTargets     = tooMaxTargets;
                upTOO.TOOMinCoveredProb = tooMinCoveredProb;
                upTOO.TOOWindowDuration = hours(tooWindowHours);
                upTOO.TOOAlertProbMap   = probMapTable;

                % Optional planner args
                verbosity = Obj.getField(planCfg, 'Verbosity', 0);
                drawMaps  = Obj.getField(planCfg, 'DrawMaps', 0);

                Obj.msgLog(LogLevel.Info, ...
                    'TOO run %s: MaxTargets=%d MinCoveredProb=%.3f Window=%g[h]', ...
                    runId, upTOO.TOOMaxTargets, upTOO.TOOMinCoveredProb, tooWindowHours);

                % Run planner
                upTOO.buildTOO('Verbosity', verbosity, 'DrawMaps', drawMaps, ...
                    'SaveMaps', true, ...
                    'MapOutputDir', outFolder, ...
                    'MapBaseName', runId, ...
                    'MapFormats', {'png','fig'}, ...
                    'CloseFigures', true);

                % Extract results
                planTable = upTOO.Plan;  % MATLAB table

                % Write outputs
                meta = Obj.makeMetadata(runId, jsonFilename, planCfg, upTOO, planTable);

                targets = Obj.planTableToTargets(planTable);

                outJson = fullfile(outFolder, runId + ".json");
                outMat  = fullfile(outFolder, runId + ".mat");

                Obj.savePlanJson(outJson, meta, targets);
                Obj.savePlannerMat(outMat, upTOO, meta, planCfg);

                Obj.msgLog(LogLevel.Info, 'TOO run %s: done. exposures=%d json=%s', runId, height(planTable), outJson);

                % Return file information with absolute paths
                planInfo = struct();
                planInfo.run_id = char(runId);
                planInfo.json_file = char(Obj.getAbsolutePath(outJson));
                planInfo.mat_file = char(Obj.getAbsolutePath(outMat));
                planInfo.plan_index = planIndex;
                planInfo.status = 'success';
                planInfo.exposures_scheduled = height(planTable);

            catch Ex
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Plan %d failed (runId=%s): %s', planIndex, runId, Ex.message);
                % Return empty to indicate failure
            end
        end


        function cfg = loadJson(~, filename)
            % loadJson
            %
            % :param filename: Path to JSON file.
            % :return: Struct.

            % Read JSON file
            txt = fileread(filename);
            cfg = jsondecode(txt);

            % jsondecode can return struct or struct array. Normalize to struct.
            if ~isstruct(cfg)
                error('JSON decode did not produce struct');
            end

            % If cfg.plans comes as cell, normalize to struct array
            if isfield(cfg, 'plans') && iscell(cfg.plans)
                cfg.plans = [cfg.plans{:}];
            end
        end


        function ensureFolder(Obj, folder)
            % ensureFolder
            %
            % :param folder: Path to folder.
            if ~isfolder(folder)
                try
                    mkdir(folder);
                catch Ex
                    Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Failed creating folder %s: %s', folder, Ex.message);
                    rethrow(Ex);
                end
            end
        end


        function val = getField(~, s, fieldName, defaultVal)
            % Get field from struct with default value
            %
            % :param s: Struct.
            % :param fieldName: Field name.
            % :param defaultVal: Default value.
            % :return: Value of field or default value.

            if isfield(s, fieldName) && ~isempty(s.(fieldName))
                val = s.(fieldName);
            else
                val = defaultVal;
            end
        end


        function runId = makeRunId(~, planIndex, planCfg)
            % makeRunId
            %
            % :param planIndex: Plan index.
            % :param planCfg: Plan configuration struct.
            % :return: Run ID string.

            % A stable filename-safe run id
            ts = datetime("now","TimeZone","UTC");
            tsStr = datestr(ts, 'yyyymmdd_HHMMSS_FFF');

            % Optional label in JSON
            label = "";
            if isfield(planCfg, 'label') && ~isempty(planCfg.label)
                label = "_" + string(planCfg.label);
                label = regexprep(label, '[^A-Za-z0-9_\-]', '_');
            end

            runId = sprintf("too_%02d%s_%s", planIndex, label, tsStr);
            runId = string(runId);
        end


        function meta = makeMetadata(Obj, runId, jsonFilename, planCfg, upTOO, planTable)
            % makeMetadata
            %
            % :param runId: Run ID string.
            % :param jsonFilename: Path to JSON file.
            % :param planCfg: Plan configuration struct.
            % :param upTOO: Planner object.
            % :param planTable: Plan table.
            % :return: Metadata struct.

            meta = struct();
            meta.run_id = char(runId);
            meta.created_time_utc = char(Obj.isoFormat(datetime("now","TimeZone","UTC")));
            meta.input_json = char(string(jsonFilename));

            meta.planner = struct();
            meta.planner.name = char(Obj.PlannerName);

            % Also include the planCfg raw fields for traceability
            meta.plan_cfg = planCfg;

            meta.result = struct();
            meta.result.exposures_scheduled = height(planTable);

            % If available, include overall time bounds
            meta.result.tstart_utc = '';
            meta.result.tend_utc = '';
            if height(planTable) > 0 && ismember('Tstart', planTable.Properties.VariableNames)
                t0 = planTable.Tstart(1);
                t1 = planTable.Tend(end);
                try
                    meta.result.tstart_utc = char(Obj.isoFormat(t0));
                    meta.result.tend_utc   = char(Obj.isoFormat(t1));
                catch
                    % keep empty
                end
            end
        end


        function targets = planTableToTargets(Obj, T)
            % planTableToTargets
            %
            % :param T: Plan table.
            % :return: Targets struct array.

            % Convert the planner table into a JSON-friendly struct array.
            % We pick the main columns you likely care about in SOC.

            targets = struct('Name', {}, 'RA', {}, 'Dec', {}, 'Roll', {}, ...
                             'Tiles', {}, 'Tstart_utc', {}, 'Tend_utc', {}, ...
                             'ExpTime_s', {}, 'Nexposures', {}, 'TotalDuration_s', {});

            if isempty(T) || height(T) == 0
                return;
            end

            for k = 1:height(T)
                row = struct();

                row.Name = Obj.safeGetTableValue(T, k, 'Name', "");
                row.RA   = Obj.safeGetTableValue(T, k, 'RA', NaN);
                row.Dec  = Obj.safeGetTableValue(T, k, 'Dec', NaN);

                % ExpectedRoll column name based on your table
                rollVal = Obj.safeGetTableValue(T, k, 'ExpectedRoll', NaN);
                row.Roll = rollVal;

                row.Tiles = Obj.safeGetTableValue(T, k, 'Tiles', "");

                tstart = Obj.safeGetTableValue(T, k, 'Tstart', []);
                tend   = Obj.safeGetTableValue(T, k, 'Tend', []);

                row.Tstart_utc = "";
                row.Tend_utc   = "";
                if ~isempty(tstart)
                    row.Tstart_utc = Obj.isoFormat(tstart);
                end
                if ~isempty(tend)
                    row.Tend_utc = Obj.isoFormat(tend);
                end

                % ExpTime in your table is duration-like ("00:05:00")
                expTime = Obj.safeGetTableValue(T, k, 'ExpTime', []);
                row.ExpTime_s = Obj.durationToSeconds(expTime);

                row.Nexposures = Obj.safeGetTableValue(T, k, 'Nexposures', NaN);

                totalDur = Obj.safeGetTableValue(T, k, 'TotalDuration', []);
                row.TotalDuration_s = Obj.durationToSeconds(totalDur);

                targets(end+1) = row; %#ok<AGROW>
            end
        end


        function v = safeGetTableValue(~, T, idx, varName, defaultVal)
            % safeGetTableValue
            %
            % :param T: Plan table.
            % :param idx: Index.
            % :param varName: Variable name.
            % :param defaultVal: Default value.
            % :return: Value of variable or default value.

            if ismember(varName, T.Properties.VariableNames)
                v = T.(varName)(idx,:);
                % Convert string/categorical to plain
                if iscell(v) && numel(v) == 1
                    v = v{1};
                end
            else
                v = defaultVal;
            end
        end


        function s = durationToSeconds(~, x)
            % durationToSeconds
            %
            % :param x: Duration, datetime-like, or char time "HH:MM:SS".
            % :return: Duration in seconds.

            % Convert duration, datetime-like, or char time "HH:MM:SS" to seconds.
            if isempty(x)
                s = NaN;
                return;
            end

            if isduration(x)
                s = seconds(x);
                return;
            end

            if ischar(x) || isstring(x)
                % attempt to parse "HH:MM:SS"
                try
                    d = duration(string(x));
                    s = seconds(d);
                    return;
                catch
                    s = NaN;
                    return;
                end
            end

            s = NaN;
        end


        function savePlanJson(Obj, outJson, meta, targets)
            % savePlanJson
            %
            % :param outJson: Path to JSON file.
            % :param meta: Metadata struct.
            % :param targets: Targets struct array.

            % Create payload struct with metadata and targets
            payload = struct();
            payload.metadata = meta;
            payload.targets = targets;

            try
                % Write JSON file
                txt = jsonencode(payload);
                txt = Obj.prettyJson(txt);
                fid = fopen(outJson, 'w');
                fwrite(fid, txt);
                fclose(fid);
            catch Ex
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Failed writing JSON %s: %s', outJson, Ex.message);
                rethrow(Ex);
            end
        end


        function savePlannerMat(Obj, outMat, plannerObj, meta, planCfg)
            % savePlannerMat
            %
            % :param outMat: Path to MAT file.
            % :param plannerObj: Planner object.
            % :param meta: Metadata struct.
            % :param planCfg: Plan configuration struct.

            % Save planner object, metadata, and plan configuration to MAT file
            try
                save(outMat, 'plannerObj', 'meta', 'planCfg', '-v7.3');
            catch Ex
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Failed writing MAT %s: %s', outMat, Ex.message);
                rethrow(Ex);
            end
        end

        
        function txt = prettyJson(~, txt)
            % Minimal pretty printer: adds newlines after commas/braces.
            % Keeps it simple and robust; SOC side can reformat if needed.
            txt = string(txt);
            txt = replace(txt, ',"', sprintf(',\n  "'));
            txt = replace(txt, '{', sprintf('{\n  '));
            txt = replace(txt, '}', sprintf('\n}'));
            txt = char(txt);
        end

        function summaryFileName = createSummaryJson(Obj, outFolder, jsonFilename, createdPlans, totalPlans)
            % createSummaryJson
            %
            % Creates a summary JSON file listing all successfully created plan files.
            %
            % :param outFolder: Path to output folder.
            % :param jsonFilename: Path to input JSON config file.
            % :param createdPlans: Struct array with information about created plans.
            % :param totalPlans: Total number of plans that were attempted.

            summaryFileName = [];
            summary = struct();
            summary.created_time_utc = char(Obj.isoFormat(datetime("now","TimeZone","UTC")));
            summary.input_json = char(Obj.getAbsolutePath(string(jsonFilename)));
            summary.output_folder = char(Obj.getAbsolutePath(string(outFolder)));
            summary.total_plans_attempted = totalPlans;
            summary.total_plans_succeeded = numel(createdPlans);
            summary.total_plans_failed = totalPlans - numel(createdPlans);

            % Convert createdPlans struct array to cell array for JSON encoding
            if numel(createdPlans) > 0
                plansList = cell(numel(createdPlans), 1);
                
                for i = 1:numel(createdPlans)
                    p = createdPlans(i);
                
                    runId = string(p.run_id);
                
                    % expected image files
                    skyPng      = fullfile(outFolder, runId + "_sky.png");
                    coveragePng = fullfile(outFolder, runId + "_coverage.png");
                
                    images = struct();
                    if isfile(skyPng)
                        images.sky_png = char(skyPng);
                    end
                    if isfile(coveragePng)
                        images.coverage_png = char(coveragePng);
                    end
                
                    if ~isempty(fieldnames(images))
                        p.images = images;
                    end
                
                    plansList{i} = p;
                end

                summary.plans = plansList;
            else
                summary.plans = {};
            end

            % Write summary JSON file
            summaryFile = fullfile(outFolder, "summary.json");
            try
                txt = jsonencode(summary);
                txt = Obj.prettyJson(txt);
                fid = fopen(summaryFile, 'w');
                fwrite(fid, txt);
                fclose(fid);
                Obj.msgLog(LogLevel.Info, 'TooPlannerRunner: Summary file created: %s', summaryFile);
                summaryFileName = summaryFile;
            catch Ex
                Obj.msgLog(LogLevel.Error, 'TooPlannerRunner: Failed writing summary JSON %s: %s', summaryFile, Ex.message);
            end
        end

        function absPath = getAbsolutePath(~, path)
            % getAbsolutePath
            %
            % Converts a relative or absolute path to an absolute path.
            %
            % :param path: Path string (relative or absolute).
            % :return: Absolute path string.

            if isempty(path)
                absPath = "";
                return;
            end

            pathStr = string(path);
            if isempty(pathStr) || pathStr == ""
                absPath = "";
                return;
            end

            % Check if already absolute (Windows: starts with drive letter, Unix: starts with /)
            pathChar = char(pathStr);
            isAbsolute = startsWith(pathChar, filesep) || (ispc && length(pathChar) >= 2 && pathChar(2) == ':');
            
            if isAbsolute
                absPath = pathChar;
            else
                % Convert to absolute path
                absPath = char(fullfile(pwd, pathChar));
            end
        end
    end


    methods (Static)
        function result = isoFormat(dt)
            % Converts a MATLAB datetime object to ISO 8601 format
            %
            % :param dt: A MATLAB datetime object.
            % :return: A string in the format 'YYYY-MM-DDTHH:MM:SS.FFFZ'.

            if isempty(dt)
                result = "";
                return;
            end

            % Ensure UTC timezone
            if isempty(dt.TimeZone)
                dt.TimeZone = 'UTC';
            elseif dt.TimeZone ~= "UTC"
                dt = datetime(dt, 'ConvertFrom', dt.TimeZone, 'TimeZone', 'UTC');
            end

            result = string(datestr(dt, 'yyyy-mm-ddTHH:MM:SS.FFFZ'));
        end

    end
end
