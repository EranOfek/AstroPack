%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.MissionApiSim.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 11/11/2025
% Description : Simulator implementation of the MissionApiBase interface.
%==========================================================================
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95

classdef MissionApiSim < ultrasat.api.core.Loggable
    %

    properties
        ApiUrl          % Base URL of the mission control API
        PlanData        % Instance of ultrasat.api.PlanData containing current plan information
        LogFileName     % Path to the log file for storing client operations
        ApprovedTargetsStartTime        % Updated by getApprovedTargets()
        ApprovedTargetsEndTime           % Updated by getApprovedTargets()

        LocalDbPath     % Path to simulator data files
        Validator       % instance of ultrasat.api.ValidatorSim()
        ApiSimProvider  % instance of ultrasat.api.ApiSimProvider()
    end


    methods
        function obj = MissionApiSim(Args)
            arguments
                Args.SubUrl         = '/mission';  % planner_backend
                Args.LogFileName    = []
            end

            obj.LogPrefix = 'MissionApiSim';
            if isempty(Args.LogFileName)
                srcFile = mfilename('fullpath');
                srcFolder = fileparts(srcFile);
                obj.LogFileName = fullfile(srcFolder, [mfilename, '.log']);
            else
                obj.LogFileName = Args.LogFileName;
            end
            obj.msglog('MissionClientSim constructor started');

            % Initialize the ApiSimProvider
            obj.ApiSimProvider = ultrasat.api.clients.ApiSimProvider(Args.SubUrl);

            % SOC_PATH must be defined in env
            soc_path = getenv('SOC_PATH');
            if soc_path == ""
                obj.msglog('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
                error('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
            end

            % -------------------------- NOT USED - Files are stored only on the server
            % Master files path from the git repo: use sim/ subfolder under current folder, there should be a .gitignore file
            % currentFile = mfilename('fullpath');
            % currentFolder = fileparts(currentFile);
            % masterPath = fullfile(currentFolder, 'sim_master');
            %
            % Copy master files if first run % @TODO
            %if ~exist(obj.DbPath, 'dir') || isempty(dir(fullfile(obj.DbPath, '*.json')))
            %    obj.msglog('DbPath does not exist, creating it from master files: %s', masterPath);
            %    obj.msglog('First run: copying default simulator files to:\n%s\n', obj.DbPath);
            %    copyfile(masterPath, obj.DbPath);
            %end
            % -------------------------- 

            % Target writable data path, on Linux it is ~/soc/sim/backend/planner, on Windows it is c:\soc\sim\backend\planner
            obj.LocalDbPath = fullfile(soc_path, 'temp', 'planner_sim');
            obj.msglog('LocalDbPath: %s', obj.LocalDbPath);
            if ~exist(obj.LocalDbPath, 'dir')
                mkdir(obj.LocalDbPath);
            end

            % Create an instance of ValidatorSim
            obj.Validator = ultrasat.api.clients.ValidatorSim(fullfile(obj.LocalDbPath, 'validator.json'), obj.LogFileName);
            obj.msglog('MissionClientSim constructor done');
        end

        % -------------------------------------------------------------------

        function response = newResponse(obj)
            response = struct('status', '', 'message', '');
        end

        function Result = getPlannerBasePath(obj)
            % Returns the base path for a given namespace's planner directory
            if isempty(ultrasat.api.utils.PathUtils.NamespaceId)
                error('NamespaceId must be set in the object to get the base path.');
            end

            Result = ultrasat.api.utils.PathUtils.getNamespaceDataFolder( ...
                'planner', ...                  % module name
                '', ...                         % subfolder (empty, top-level)
                'NamespaceId', ultrasat.api.utils.PathUtils.NamespaceId);% pass current namespace

            obj.msglog('getPlannerBasePath: %s', Result);
        end

        % -----------------------------------------------------------------

        function response = getApprovedTargets(obj, start_time, end_time)
            % Retrieves the list of approved targets within the given time range from a JSON file.
            try
                if ~isdatetime(start_time)
                    start_time = datetime(start_time, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC');
                end
                if ~isdatetime(end_time)
                    end_time = datetime(end_time, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC');
                end
                start_time.TimeZone = 'UTC';
                end_time.TimeZone = 'UTC';

                % Store the times, will be displayed to the user in GUI
                obj.ApprovedTargetsStartTime = start_time;
                obj.ApprovedTargetsEndTime = end_time;

                obj.msglog('getApprovedTargets: start_time=%s, end_time=%s', datestr(start_time), datestr(end_time));

                targetsFile = ultrasat.api.utils.PathUtils.getNamespaceDataFilename( ...
                    'mission', ...                  % module name
                    '', ...                         % subfolder (empty, top-level)
                    'scheduled_targets.json', ...   % Filename
                    'NamespaceId', ultrasat.api.utils.PathUtils.NamespaceId);% pass current namespace

                %targetsFile = fullfile(obj.getPlannerBasePath(), 'scheduled_targets.json');


                response = struct();

                targets = obj.ApiSimProvider.readJsonFile(targetsFile);
                if isempty(targets)
                    obj.msglog('No approved targets found in the specified time range.');
                    response.status = 'ok';
                    response.targets = [];
                    response.ok = true;
                    return;
                end

                % Get the 'targets' array from the file
                targets = targets.targets;

                % Filter targets by start_time and end_time
                filteredTargets = [];
                for i = 1:numel(targets)
                    tStart = obj.parseIsoDatetime(targets(i).start_time);
                    tEnd = obj.parseIsoDatetime(targets(i).end_time);

                    if tStart <= end_time && tEnd >= start_time
                        filteredTargets = [filteredTargets; targets(i)];
                    end
                end

                if isempty(filteredTargets)
                    obj.msglog('No approved targets found in the specified time range.');
                    response.status = 'ok';
                    response.targets = [];
                    response.ok = true;
                else
                    obj.msglog('Found %d approved targets.', numel(filteredTargets));
                    response.status = 'ok';
                    response.targets = filteredTargets;
                    response.ok = true;
                end
            catch ME
                obj.msglog('Error getting approved targets: %s', ME.message);
                response.status = 'error';
                response.message = 'Error getting approved targets.';
                response.targets = [];
                response.ok = false;
            end
        end

        % -----------------------------------------------------------------

        function updateApprovedTargets(obj, targets, replace)
            % Update the approved_targets.json file by either replacing or merging targets.
            obj.msglog('updateApprovedTargets: targets: %d, replace=%d', numel(targets), replace);
            try
                approvedTargetsFile = fullfile(obj.getPlannerBasePath(), 'approved_targets.json');

                % Read existing file
                existingTargets = obj.ApiSimProvider.readJsonFile(approvedTargetsFile);

                if replace
                    % Replace all existing targets
                    updatedTargets = targets;
                else
                    % Merge new targets with existing ones
                    updatedTargets = existingTargets;

                    % Add new targets while preventing duplicates (based on `pk`)
                    existingPKs = [existingTargets.pk];
                    for i = 1:numel(targets)
                        if ~ismember(targets(i).pk, existingPKs)
                            updatedTargets(end+1) = targets(i);
                        end
                    end
                end

                % Add 'pk' field if not exist
                for i = 1:numel(updatedTargets)
                    if ~isfield(updatedTargets(i), 'pk')
                        updatedTargets(i).pk = 0;
                    end

                    if ~isfield(updatedTargets(i), 'target_id')
                        updatedTargets(i).target_id = 'TRG';
                    end
                end

                % Sort targets by start_time
                % [~, sortIdx] = sort(datetime({updatedTargets.start_time}, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC'));
                % updatedTargets = updatedTargets(sortIdx);

                % Save back to JSON
                obj.ApiSimProvider.writeJsonFile(approvedTargetsFile, updatedTargets);

                obj.msglog('Updated successfully: %s', approvedTargetsFile);
            catch ME
                obj.msglog('Error updating approved targets: %s', ME.message);
            end
        end

        % -----------------------------------------------------------------

        function response = validatePlan(obj, Plan)
            % Validates the observation plan using the ValidatorSim class.
            obj.msglog('validatePlan: Validating plan with pk=%d', obj.PlanData.pk);
            try
                Plan = ultrasat.api.utils.PlanDataUtils.convertPlanTimesToUtc(Plan);

                % Call validateTargets with the provided Plan (array of structs)
                try
                    response = obj.Validator.validateTargets(Plan);
                catch ME
                    obj.msglog('ValidatorSim.validateTargets error: %s', ME.message);
                    response = obj.newResponse();
                    response.status = 'error';
                    response.message = 'Validation failed due to an exception.';
                end

                % Ensure metadata.ValidationResponse exists as a cell array
                if ~isfield(obj.PlanData.metadata, 'ValidationResponse') || isempty(obj.PlanData.metadata.ValidationResponse)
                    obj.PlanData.metadata.ValidationResponse = {}; % Initialize as empty cell array
                elseif ~iscell(obj.PlanData.metadata.ValidationResponse)
                    obj.PlanData.metadata.ValidationResponse = {obj.PlanData.metadata.ValidationResponse}; % Convert to cell if needed
                end

                % Insert the latest response at the beginning of the array (most recent first)
                obj.PlanData.metadata.ValidationResponse = [{response}, obj.PlanData.metadata.ValidationResponse];

                if isfield(response, 'task') && isfield(response, 'status')
                    obj.msglog('Validation status: %s', response.status);
                    response.ok = true;
                else
                    obj.msglog('Validation failed or returned unexpected output.');
                    response.ok = false;
                    response.status = 'error';
                    response.message = 'Validation failed or unexpected output.';
                end
            catch ME
                obj.msglog('Error validating plan: %s', ME.message);
                response.status = 'error';
                response.message = 'Error validating plan.';
                response.ok = false;
            end
        end

        % -----------------------------------------------------------------

        function response = submitPlan(obj, Plan)
            % Submits the observation plan by updating targets, status, and history.
            % Plan: array of struct, created by uplanner.planTable2struct()

            obj.msglog('submitPlan: Submitting plan with pk=%d', obj.PlanData.pk);
            try
                % Allow submit only if not submitted yet, if cannot submit
                if ~isempty(obj.PlanData.planner.Status) && ~strcmp(obj.PlanData.planner.Status, 'draft') && ~strcmp(obj.PlanData.planner.Status, 'submitted')
                    response.status = 'error';
                    response.message = sprintf('Submit ignored for non-draft plan: %d.', obj.PlanData.planner.Pk);
                    response.ok = false;
                    obj.msglog('Submit ignored for non-draft plan: %d', obj.PlanData.planner.Pk);
					return;
                end

                plansFolder = fullfile(obj.getPlannerBasePath(), 'plans');
                response = struct();

                % Set targets from provided Plan array of structs
                obj.PlanData.targets = Plan;  % Direct assignment, no conversion needed

                % UGLY but currently required: @TODO - Fix or clarify !!
				% Update status here to allow calling savePlan() below to save it
				% with status 'submitted', otherwise it will save it as 'draft'
                obj.PlanData.status = 'submitted';

                % Add entry to history
                obj.PlanData.addHistory(sprintf('plan submitted by %s', obj.PlanData.created_by));

                % Save updated plan data to JSON

                % Change uplanner status to submitted otherwrite it will not
                % be saved - @TODO --- Bad workaround but for now (19/10/2025)
                %SavePlannerStatus = obj.PlanData.planner.Status;
                %obj.PlanData.planner.Status = 'submitted';
                obj.savePlan('forceSave', true);

                % Restore status, it will be set again to submitted in uplanner.submit()
                %obj.PlanData.planner.Status = SavePlannerStatus;

                response.status = 'ok';
                response.message = sprintf('Plan %d submitted successfully.', obj.PlanData.pk);
                response.ok = true;
                obj.msglog('Plan %d submitted successfully and updated in JSON file.', obj.PlanData.pk);

                % Simulator version: replace exsiting Approved Targets by the targets of this plan
                % obj.updateApprovedTargets(Plan, true);
            catch ME
                obj.msglog('Error submitting plan: %s', ME.message);
                response.status = 'error';
                response.message = 'Error submitting plan.';
                response.ok = false;
            end
        end

        % -------------------------------------------------------------------

        function response = retractPlan(obj, Plan)
            % Called from uplanner - @Future - need to define and implement
            obj.msglog('retractPlan: Retracting plan with pk=%d', obj.PlanData.pk);
            try
            catch ME
                obj.msglog('Error retracting plan: %s', ME.message);
                response.status = 'error';
                response.message = 'Error retracting plan.';
                response.ok = false;
            end
        end

        % -------------------------------------------------------------------

        function response = getExposure(obj, table_name, healpix_indices, start_timestamp, end_timestamp, select_all)
            % Retrieves exposure data from the specified JSON file (mapped from table_name).
            obj.msglog('getExposure: table=%s, healpix_indices=%s, start=%s, end=%s, select_all=%d', ...
                       table_name, mat2str(healpix_indices), datestr(start_timestamp), datestr(end_timestamp), select_all);

            try
                dbFile = fullfile(obj.getPlannerBasePath(), sprintf('%s.json', table_name));
                response = struct();

                fid = fopen(dbFile, 'r');
                raw = fread(fid, inf, 'char');
                fclose(fid);
                data = jsondecode(char(raw'));

                filteredData = [];
                startNum = datenum(start_timestamp);
                endNum = datenum(end_timestamp);

                for i = 1:numel(data)
                    row = data(i);
                    tNums = arrayfun(@(x) datenum(x{1}, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), row.timestamps);

                    if select_all || ...
                    ((isempty(healpix_indices) || ismember(row.healpix_index, healpix_indices)) && ...
                        any(tNums >= startNum & tNums <= endNum))
                        filteredData = [filteredData; row];
                    end
                end

                if isempty(filteredData)
                    obj.msglog('No exposure data found for the given filters.');
                    response.status = 'ok';
                    response.data = [];
                    response.ok = true;
                else
                    obj.msglog('Found %d exposure records.', numel(filteredData));
                    response.status = 'ok';
                    response.data = filteredData;
                    response.ok = true;
                end
            catch ME
                obj.msglog('Error getting exposure: %s', ME.message);
                response.status = 'error';
                response.message = 'Error getting exposure.';
                response.data = [];
                response.ok = false;
            end
        end

        % -------------------------------------------------------------------

        function response = getPlansList(obj, start_timestamp, end_timestamp, title_subtext)
            % Returns a list of existing plans from JSON files
            obj.msglog('getPlansList: Scanning for plans in %s', obj.getPlannerBasePath());
            try
                plansFolder = fullfile(obj.getPlannerBasePath(), 'plans');
                response = struct();
                plansList = [];

                % Ensure timestamps are datetime objects
                if ~isempty(start_timestamp) && ~isdatetime(start_timestamp)
                    start_timestamp = datetime(start_timestamp, 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z');
                end
                if ~isempty(end_timestamp) && ~isdatetime(end_timestamp)
                    end_timestamp = datetime(end_timestamp, 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z');
                end

                % Find all JSON files in the plans folder
                jsonFiles = obj.ApiSimProvider.listFilesInFolder(plansFolder, '*.json');
                for i = 1:numel(jsonFiles)
                    try
                        % Decode the number from the file name (e.g., 00001.json)
                        [~, name, ext] = fileparts(jsonFiles(i));  % Removed 25/09/2025 - .name);
                        if length(name) == 5 && all(isstrprop(name, 'digit'))
                            planNum = str2double(name);
                            if planNum > 9999

                                % Skip if plan number exceeds 9999,
                                % NOTE: values above it are for Maintenance Plans (In the SIM version, to be modified in the Database version)
                                continue;
                            end
                        else
                            continue; % Skip files that do not match the pattern
                        end

                        % Load the JSON file
                        fileName = fullfile(plansFolder, jsonFiles(i));  % .name);
                        planData = obj.ApiSimProvider.readJsonFile(fileName);

                        % Apply time filter if specified
                        if (~isempty(start_timestamp) && planData.end_time < start_timestamp) || ...
                        (~isempty(end_timestamp) && planData.start_time > end_timestamp)
                            continue;
                        end

                        % Apply title search if specified
                        if ~isempty(title_subtext) && ~contains(lower(planData.title), lower(title_subtext))
                            continue;
                        end

                        plansList = [plansList; {planData}];
                    catch ME
                        obj.msglog('Error processing file %s: getting plans list: %s', jsonFiles(i), ME.message);
                    end
                end

                response.status = 'ok';
                response.plans = plansList;
                response.ok = true;
            catch ME
                obj.msglog('Error getting plans list: %s', ME.message);
                response.status = 'error';
                response.message = 'Error getting plans list.';
                response.plans = [];
                response.ok = false;
            end
        end


        function response = loadPlan(obj, plan_pk)
            % Loads a specific plan by its primary key (pk) into obj.PlanData.
            obj.msglog('loadPlan: Loading plan with pk=%d', plan_pk);
            try
                plansFolder = fullfile(obj.getPlannerBasePath(), 'plans');
                response = struct();

                jsonFile = fullfile(plansFolder, sprintf('%05d.json', plan_pk));
                matFile = fullfile(plansFolder, sprintf('%05d.mat', plan_pk));

                % Load JSON data
                text = obj.ApiSimProvider.readFile(jsonFile);
                if isempty(text)
                    obj.msglog('Plan files not found for pk=%d', plan_pk);
                    response.status = 'error';
                    response.message = 'Plan files not found.';
                    response.ok = false;
                    return;
                end

                % Load MATLAB object (planner) from .mat file
                loadedMat = obj.ApiSimProvider.loadMatObject(matFile, 'planner');

                % Populate obj.PlanData
                obj.PlanData = ultrasat.api.models.PlanData.fromJson(text);
                obj.PlanData.planner = loadedMat;  %.planner;

                response.status = 'ok';
                response.message = sprintf('Plan %d loaded successfully.', plan_pk);
                response.ok = true;
                response.plan = obj.PlanData.toStruct();  % Return as struct if needed
                obj.msglog('Plan %d loaded successfully.', plan_pk);
            catch ME
                obj.msglog('Error loading plan: %s', ME.message);
                response.status = 'error';
                response.message = 'Error loading plan.';
                response.ok = false;
            end
        end


        function response = savePlan(obj, Args)
            % Saves the current PlanData instance as JSON and MAT files.
            arguments
                obj
                Args.forceSave (1,1) logical = false
            end            
            obj.msglog('savePlan: Saving plan with pk=%d', obj.PlanData.pk);
            try

                % Allow save only if allowed
                if ~Args.forceSave && ~obj.PlanData.planner.isEditable()
                    response.status = 'error';
                    response.message = sprintf('Save ignored for non-draft plan: %d - Status: %s', obj.PlanData.planner.Pk, obj.PlanData.planner.Status);
                    response.ok = false;
                    obj.msglog('Error: savePlan ignored for non-draft plan: %d - Status: %s', obj.PlanData.planner.Pk, obj.PlanData.planner.Status);
					return;
                end

                % Prepare to save the plan
                plansFolder = fullfile(obj.getPlannerBasePath(), 'plans');
                response = struct();

                ultrasat.api.utils.PlanDataUtils.syncFromPlanner(obj.PlanData, obj.PlanData.planner);

                % Generate pk if not provided, as next file number (i.e '00003')
                if isempty(obj.PlanData.pk)
                    obj.msglog('savePlan: Pk is empty, obtaining new pk');
                    NextAvailableFile = obj.ApiSimProvider.nextAvailableFile(plansFolder, '*.json', 5, 0, 9999);
                    if ~isempty(NextAvailableFile)
                        obj.PlanData.pk = NextAvailableFile.index;
                        obj.PlanData.planner.Pk = obj.PlanData.pk;
                        obj.msglog('Generated new pk=%d for the plan.', obj.PlanData.pk);
                    end
                end

                % Write JSON file without 'PlanData.planner' field, it will be
                % stored separetly in .mat file (see below)
                jsonFile = fullfile(plansFolder, sprintf('%05d.json', obj.PlanData.pk));
                planStruct = obj.PlanData.toStruct();
                planStruct = rmfield(planStruct, 'planner');  % Remove planner for JSON

                % Convert datetime objects to iso format
                planStruct = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(planStruct);

                % Convert datetime objects to iso format
                if ~isempty(planStruct.targets)
                    planStruct.targets = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(planStruct.targets);
                end

                % Save JSON file
                obj.msglog('savePlan: writing json file: %s', jsonFile);
                obj.ApiSimProvider.writeJsonFile(jsonFile, planStruct);

                % Write MATLAB object (planner) to .mat file
                matFile = fullfile(plansFolder, sprintf('%05d.mat', obj.PlanData.pk));
                planner = obj.PlanData.planner;  % Instance of ultrasat.uplanner
                obj.msglog('savePlan: writing mat file: %s', matFile);                
                obj.ApiSimProvider.saveMatObject(matFile, planner, 'planner');

                response.status = 'ok';
                response.message = sprintf('Plan %d saved successfully.', obj.PlanData.pk);
                response.ok = true;
                obj.msglog(sprintf('Plan saved successfully, Pk=%d', obj.PlanData.pk));
            catch ME
                obj.msglog('Error saving plan: %s', ME.message);
                response.status = 'error';
                response.message = 'Error saving plan.';
                response.ok = false;
            end
        end


        function response = deletePlan(obj, plan_pk)
            % Deletes a specific plan by its primary key (pk).
            obj.msglog('deletePlan: Deleting plan with pk=%d', plan_pk);
            try
                plansFolder = fullfile(obj.getPlannerBasePath(), 'plans');
                response = struct();

                jsonFile = fullfile(plansFolder, sprintf('%05d.json', plan_pk));
                matFile = fullfile(plansFolder, sprintf('%05d.mat', plan_pk));

                obj.ApiSimProvider.deleteFile(jsonFile);
                obj.ApiSimProvider.deleteFile(matFile);

                obj.msglog('Plan %d deleted successfully.', plan_pk);
                response.status = 'ok';
                response.message = sprintf('Plan %d deleted successfully.', plan_pk);
                response.ok = true;
            catch ME
                obj.msglog('Error deleting plan: %s', ME.message);
                response.status = 'error';
                response.message = 'Error deleting plan.';
                response.ok = false;
            end
        end

        % -------------------------------------------------------------------

        function response = getPlanStatus(obj, plan_pk)
            % Retrieves the status, update_time, metadata, and history of a plan from its JSON file.
            obj.msglog('getPlanStatus: Fetching status for plan with pk=%d', plan_pk);
            try
                plansFolder = fullfile(obj.getPlannerBasePath(), 'plans');
                response = struct();

                jsonFile = fullfile(plansFolder, sprintf('%05d.json', plan_pk));

                % Load the JSON plan file
                planData = obj.ApiSimProvider.readJsonFile(jsonFile);

                % Extract relevant fields
                response.status = 'ok';
                response.data = struct(...
                    'status', planData.status, ...
                    'update_time', planData.update_time, ...
                    'metadata', planData.metadata, ...
                    'history', planData.history ...
                );
                response.ok = true;
                obj.msglog('Plan status fetched successfully for pk=%d', plan_pk);
            catch ME
                obj.msglog('Error getting plan status: %s', ME.message);
                response.status = 'error';
                response.message = 'Error getting plan status.';
                response.ok = false;
                response.data = [];
            end
        end


        function dt = parseIsoDatetime(obj, str)
            % Parse ISO 8601 datetime strings with 'Z' or timezone offsets.
            dt = ultrasat.api.utils.DateTimeUtils.parseIsoDateTime(str);
        end

    end
end
