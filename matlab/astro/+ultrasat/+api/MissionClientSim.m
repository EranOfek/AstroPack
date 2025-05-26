%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.MissionClientSim.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 08/04/2025
%
%==========================================================================
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95
%

classdef MissionClientSim < ultrasat.api.MissionClientBase
    % 
    
    properties
        DbPath          % Path to simulator data files
        Validator       % instance of ultrasat.api.ValidatorSim()
    end


    methods
        function obj = MissionClientSim(Args)
            arguments          
                Args.SubUrl         = '/mission';  % planner_backend  
                Args.LogFileName
            end
            ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.MissionClientBase(ArgsCell{:});  % Args);  % , 'SubUrl', '/mission');

            % Use sim/ subfolder under current folder, there should be a .gitignore file
            currentFile = mfilename('fullpath');
            currentFolder = fileparts(currentFile);
            obj.DbPath = fullfile(currentFolder, 'sim');
            if ~exist(obj.DbPath, 'dir')
                mkdir(obj.DbPath);
                mkdir(fullfile(obj.DbPath, 'plans'));
            end

            % Create an instance of ValidatorSim
            obj.Validator = ultrasat.api.ValidatorSim(fullfile(obj.DbPath, 'validator.json'), obj.LogFileName);
        end                

        % -------------------------------------------------------------------

        function response = login(obj, Params)
            % Simulate login by checking credentials from users.json and updating current_user.json
            %
            % Parameters:
            %   obj - Instance of MissionClientSim
            %   Params - Instance of MissionApiModels.LoginParams containing:
            %     .userName (string) - Name of the user to login
            %     .password (string) - Password of the user
            %
            % Returns:
            arguments
                obj
                Params  
            end
            
            obj.msglog('login: user=%s, password=%s', Params.userName, Params.password);
          
            usersFile = fullfile(obj.DbPath, 'users.json');
            currentUserFile = fullfile(obj.DbPath, 'current_user.json');
            response = struct();
        
            if ~isfile(usersFile)
                obj.msglog('login: Users file not found at %s', usersFile);
                response.status = 'error';
                response.message = 'Users database not found.';
                response.ok = false;
                return;
            end
        
            % Load users from JSON
            users = obj.readJsonFile(usersFile);
        
            % Find user and verify password
            user = [];
            for i = 1:numel(users)
                if strcmp(users(i).UserName, Params.userName) && strcmp(users(i).Password, Params.password)
                    user = users(i);
                    break;
                end
            end
        
            if isempty(user)
                obj.msglog('login: Invalid username or password for user=%s', Params.userName);
                response.status = 'error';
                response.message = 'Invalid username or password.';
                response.ok = false;
            else
                obj.msglog('login: User %s logged in successfully.', Params.userName);
                response.status = 'ok';
                response.user = user;
        
                % Update current_user.json
                currentUser = struct('UserName', Params.userName, 'Role', user.Role);
                obj.writeJsonFile(currentUserFile, currentUser);
        
                response.ok = true;
            end
        end


        function response = logout(obj, Params)
            % Simulate logout by clearing current_user.json
            %
            % Parameters:
            %   obj - Instance of MissionClientSim
            %   Params - Instance of MissionApiModels.LogoutParams containing:
            %     .userName (string) - Name of the user to logout
            %
            % Returns:
            arguments
                obj
                Params
            end
        
            currentUserFile = fullfile(obj.DbPath, 'current_user.json');
            response = struct();
        
            if ~isfile(currentUserFile)
                obj.msglog('logout: Current user file not found at %s', currentUserFile);
                response.status = 'error';
                response.message = 'No user currently logged in.';
                response.ok = false;
                return;
            end
        
            % Load current user and verify
            currentUser = obj.readJsonFile(currentUserFile);
        
            if ~strcmp(currentUser.UserName, Params.userName)
                obj.msglog('logout: User %s is not currently logged in.', Params.userName);
                response.status = 'error';
                response.message = 'User not logged in.';
                response.ok = false;
                return;
            end
        
            % Clear current user
            obj.writeJsonFile(currentUserFile, struct('UserName', '', 'Role', ''));
        
            obj.msglog('logout: User %s logged out successfully.', Params.userName);
            response.status = 'ok';
            response.ok = true;
        end        

        % -------------------------------------------------------------------

        function response = getKeyValue(obj, Params)
            % Retrieves a value from the key-value database JSON file.
            %
            % Parameters:
            %   obj - Instance of MissionClientSim
            %   Params - Instance of MissionApiModels.GetKeyValueParams containing:
            %     .store (string) - Name of the store to access
            %     .key (string) - Key to retrieve
            %     .default (string) - Default value to return if key not found
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .value - Retrieved value or default value if not found
            %     .ok - Boolean indicating success (true) or failure (false)
            arguments
                obj
                Params
            end
            
            obj.msglog('getKeyValue: store=%s, key=%s', Params.store, Params.key);
            dbFile = fullfile(obj.DbPath, 'key_value_db.json');
            response = struct();
        
            if ~isfile(dbFile)
                obj.msglog('Database file not found, returning default value.');
                response.value = Params.defaultValue;
                response.status = 'ok';
                response.ok = true;
                return;
            end
        
            % Read and parse the JSON file
            db = obj.readJsonFile(dbFile);
        
            if isfield(db, Params.store) && isfield(db.(Params.store), Params.key)
                response.value = db.(Params.store).(Params.key);
                response.status = 'ok';
                response.ok = true;
            else
                obj.msglog('Key not found, returning default value.');
                response.value = Params.default;
                response.status = 'ok';
                response.ok = true;
            end
        end
        
        
        function response = setKeyValue(obj, Params)
            % Sets a value in the key-value database JSON file.
            %
            % Parameters:
            %   obj - Instance of MissionClientSim
            %   Params - Instance of MissionApiModels.SetKeyValueParams containing:
            %     .store (string) - Name of the store to access
            %     .key (string) - Key to set
           
            obj.msglog('setKeyValue: store=%s, key=%s, value=%s', Params.store, Params.key, Params.value);
            dbFile = fullfile(obj.DbPath, 'key_value_db.json');
            response = struct();
        
            db = struct();
            if isfile(dbFile)
                % Load existing data
                db = obj.readJsonFile(dbFile);
            end
        
            if ~isfield(db, Params.store)   
                db.(Params.store) = struct();
            end
            db.(Params.store).(Params.key) = Params.value;
        
            % Write updated data to the JSON file
            obj.writeJsonFile(dbFile, db);
        
            response.status = 'ok';
            response.ok = true;
            obj.msglog('Key-value pair saved successfully.');
        end
    
        % -----------------------------------------------------------------

        function response = getApprovedTargets(obj, Params)
            % Retrieves the list of approved targets within the given time range from a JSON file.
            %
            % Parameters:
            %   obj - Instance of MissionClientSim
            %   Params - Instance of MissionApiModels.GetApprovedTargetsParams containing:
            %     .start_time (datetime) - Start time of the time range
            %     .end_time (datetime) - End time of the time range
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .targets - Array of target structures (if successful)
            %     .ok - Boolean indicating success (true) or failure (false)
            arguments
                obj
                Params
            end

            if ~isdatetime(Params.start_time)
                start_time = datetime(Params.start_time, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC');                
            end
            if ~isdatetime(Params.end_time)
                end_time = datetime(end_time, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC');                
            end
            start_time.TimeZone = 'UTC';
            end_time.TimeZone = 'UTC';

            obj.msglog('getApprovedTargets: start_time=%s, end_time=%s', datestr(start_time), datestr(end_time));
            targetsFile = fullfile(obj.DbPath, 'approved_targets.json');
            response = struct();
        
            if ~isfile(targetsFile)
                obj.msglog('Approved targets file not found at %s', targetsFile);
                response.targets = [];
                response.status = 'error';
                response.message = 'Approved targets database not found.';
                response.ok = false;
                return;
            end
        
            % Load approved targets from JSON
            targets = obj.readJsonFile(targetsFile);
        
            % Filter targets by start_time and end_time
            filteredTargets = [];
            for i = 1:numel(targets)
                tStart = datetime(targets(i).start_time, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC');
                tEnd = datetime(targets(i).end_time, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z', 'TimeZone', 'UTC');
                
                if tStart >= Params.start_time && tEnd <= Params.end_time
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
        end

        % -----------------------------------------------------------------        

        function internal_updateApprovedTargetsFile(obj, targets, replace)
            % Update the approved_targets.json file by either replacing or merging targets.
            obj.msglog('internal_updateApprovedTargetsFile: targets: %d, replace=%d', numel(targets), replace);
            approvedTargetsFile = fullfile(obj.DbPath, 'approved_targets.json');
            
            % Read existing file
            if isfile(approvedTargetsFile)
                existingTargets = obj.readJsonFile(approvedTargetsFile);
            else
                existingTargets = [];
            end
            
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
            obj.writeJsonFile(approvedTargetsFile, updatedTargets);
            
            obj.msglog('Updated successfully: %s', approvedTargetsFile);
        end
        
        % -----------------------------------------------------------------        

        function response = validatePlan(obj, Params)
            % Validates the observation plan using the ValidatorSim class.
            obj.msglog('validatePlan: Validating plan with pk=%d', obj.PlanData.pk);
               
            Plan = obj.convertPlanTimesToUtc(Plan);

            % Call validateTargets with the provided Plan (array of structs)
            try
                response = obj.Validator.validateTargets(Plan);
            catch ME
                obj.msglog('Validation error: %s', ME.message);
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
        end

        % -----------------------------------------------------------------

        function response = submitPlan(obj, Plan)
            % Submits the observation plan by updating targets, status, and history.
            % Plan: array of struct, created by uplanner.planTable2struct()

            obj.msglog('submitPlan: Submitting plan with pk=%d', obj.PlanData.pk);
            plansFolder = fullfile(obj.DbPath, 'plans');
            response = struct();
        
            jsonFile = fullfile(plansFolder, sprintf('%03d.json', obj.PlanData.pk));
            if ~isfile(jsonFile)
                obj.msglog('Plan file not found for pk=%d', obj.PlanData.pk);
                response.status = 'error';
                response.message = 'Plan file not found.';
                response.ok = false;
                return;
            end
              
            % Set targets from provided Plan array of structs
            obj.PlanData.targets = Plan;  % Direct assignment, no conversion needed        
        
            % Update status
            obj.PlanData.status = 'submitted';
        
            % Add entry to history
            obj.PlanData.addHistory(sprintf('plan submitted by %s', obj.PlanData.created_by));
        
            % Save updated plan data to JSON
            obj.savePlan();
        
            response.status = 'ok';
            response.message = sprintf('Plan %d submitted successfully.', obj.PlanData.pk);
            response.ok = true;
            obj.msglog('Plan %d submitted successfully and updated in JSON file.', obj.PlanData.pk);

            % Simulator version: replace exsiting Approved Targets by the targets of this plan
            obj.internal_updateApprovedTargetsFile(Plan, true);
        end

        % -------------------------------------------------------------------

        function response = retractPlan(obj, Plan)
            % Called from uplanner - @Future - need to define and implement
        end                        
        
        % -------------------------------------------------------------------

        function response = getExposure(obj, Params)
            % Retrieves exposure data from the specified JSON file (mapped from table_name).
            arguments
                obj
                Params
            end

            obj.msglog('getExposure: table=%s, healpix_indices=%s, start=%s, end=%s, select_all=%d', ...
                       Params.table_name, mat2str(Params.healpix_indices), ...
                       datestr(Params.start_timestamp), datestr(Params.end_timestamp), ...
                       Params.select_all);
        
            dbFile = fullfile(obj.DbPath, sprintf('%s.json', Params.table_name));
            response = struct();
        
            if ~isfile(dbFile)
                obj.msglog('Exposure data file not found at %s', dbFile);
                response.status = 'error';
                response.message = 'Exposure data file not found.';
                response.ok = false;
                return;
            end
        
            data = obj.readJsonFile(dbFile);
        
        
            filteredData = [];
            startNum = datenum(Params.start_timestamp);
            endNum = datenum(Params.end_timestamp);
        
            for i = 1:numel(data)
                row = data(i);
                tNums = arrayfun(@(x) datenum(x{1}, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), row.timestamps);
        
                if Params.select_all || ...
                   ((isempty(Params.healpix_indices) || ismember(row.healpix_index, Params.healpix_indices)) && ...
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
        end

        % -------------------------------------------------------------------

        function response = getPlansList(obj, Params)
            % Retrieves a list of all observation plans from the server.
            %
            % Parameters:
            %   Params - Instance of MissionApiModels.GetPlansListParams containing:
            %     .user_name (string) - Optional filter by user name
            %     .title (string) - Optional filter by plan status
            %
            % Returns:
            %   response - Structure containing result with fields:
            %     .status - Status of the operation ('ok' or 'error')
            %     .message - Description message (if error)
            %     .plans - Array of plan summary structures (if successful)
            %     .ok - Boolean indicating success (true) or failure (false)
            arguments
                obj
                Params
            end

            obj.msglog('getPlansList: Scanning for plans in %s', obj.DbPath);
            plansFolder = fullfile(obj.DbPath, 'plans');
            response = struct();
            plansList = [];
        
            if ~exist(plansFolder, 'dir')
                obj.msglog('Plans folder not found.');
                response.status = 'error';
                response.message = 'Plans folder not found.';
                response.ok = false;
                return;
            end

            % Ensure timestamps are datetime objects
            if ~isempty(Params.start_timestamp) && ~isdatetime(Params.start_timestamp)
                Params.start_timestamp = datetime(Params.start_timestamp, 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z');
            end
            if ~isempty(Params.end_timestamp) && ~isdatetime(Params.end_timestamp)
                end_timestamp = datetime(end_timestamp, 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z');
            end            
        
            % Find all JSON files in the plans folder
            jsonFiles = dir(fullfile(plansFolder, '*.json'));
            for i = 1:numel(jsonFiles)
                filePath = fullfile(plansFolder, jsonFiles(i).name);
                fid = fopen(filePath, 'r');
                raw = fread(fid, inf, 'char');
                fclose(fid);
                planData = api.ModelBase.json2struct(char(raw'));  %ultrasat.api.PlanData.fromJson(char(raw'));
       
                % Apply time filter if specified
                if (~isempty(Params.start_timestamp) && planData.end_time < Params.start_timestamp) || ...
                   (~isempty(Params.end_timestamp) && planData.start_time > Params.end_timestamp)
                    continue;
                end
        
                % Apply title search if specified
                if ~isempty(Params.title) && ~contains(lower(planData.title), lower(Params.title))
                    continue;
                end

                plansList = [plansList; planData];
            end
        
            response.status = 'ok';
            response.plans = plansList;
            response.ok = true;
        end
        
                
        function response = loadPlan(obj, plan_pk)
            % Loads a specific plan by its primary key (pk) into obj.PlanData.
            obj.msglog('loadPlan: Loading plan with pk=%d', plan_pk);
            plansFolder = fullfile(obj.DbPath, 'plans');
            response = struct();
        
            jsonFile = fullfile(plansFolder, sprintf('%03d.json', plan_pk));
            matFile = fullfile(plansFolder, sprintf('%03d.mat', plan_pk));
        
            if ~isfile(jsonFile) || ~isfile(matFile)
                obj.msglog('Plan files not found for pk=%d', plan_pk);
                response.status = 'error';
                response.message = 'Plan files not found.';
                response.ok = false;
                return;
            end
        
            % Load JSON data
            text = obj.readTextFile(jsonFile);
        
            % Load MATLAB object (planner) from .mat file
            loadedMat = load(matFile, 'planner');
        
            % Populate obj.PlanData
            obj.PlanData = ultrasat.api.PlanData.fromJson(text);
            obj.PlanData.planner = loadedMat.planner;
        
            response.status = 'ok';
            response.message = sprintf('Plan %d loaded successfully.', plan_pk);
            response.ok = true;
            response.plan = obj.PlanData.toStruct();  % Return as struct if needed
            obj.msglog('Plan %d loaded successfully.', plan_pk);
        end
        
        
        function response = savePlan(obj)
            % Saves the current PlanData instance to the DbPath folder as JSON and MAT files.
            obj.msglog('savePlan: Saving plan with pk=%d', obj.PlanData.pk);
            plansFolder = fullfile(obj.DbPath, 'plans');
            response = struct();

            obj.updateFromPlanner();
            if ~exist(plansFolder, 'dir')
                mkdir(plansFolder);
            end
        
            % Generate pk if not provided, as next file number (i.e '003')
            if isempty(obj.PlanData.pk)
                existingFiles = dir(fullfile(plansFolder, '*.json'));
                pks = [];
                for i = 1:numel(existingFiles)
                    [~, pk, ~] = fileparts(existingFiles(i).name);
                    pks(end+1) = str2double(pk);
                end
                obj.PlanData.pk = max([pks, 0]) + 1;
                obj.msglog('Generated new pk=%d for the plan.', obj.PlanData.pk);
            end
        
            % Write JSON file without 'PlanData.planner' field, it will be
            % stored separetly in .mat file (see below)
            jsonFile = fullfile(plansFolder, sprintf('%03d.json', obj.PlanData.pk));
            planStruct = obj.PlanData.toStruct();
            planStruct = rmfield(planStruct, 'planner');  % Remove planner for JSON

            % Convert datetime objects to iso format
            planStruct = api.ModelBase.convertDatetimeToString(planStruct);

            fid = fopen(jsonFile, 'w');
            fwrite(fid, jsonencode(planStruct, 'PrettyPrint', true), 'char');
            fclose(fid);
        
            % Write MATLAB object (planner) to .mat file
            matFile = fullfile(plansFolder, sprintf('%03d.mat', obj.PlanData.pk));
            planner = obj.PlanData.planner;  % Instance of ultrasat.uplanner
            save(matFile, 'planner');
        
            response.status = 'ok';
            response.message = sprintf('Plan %d saved successfully.', obj.PlanData.pk);
            response.ok = true;
            obj.msglog('Plan %d saved successfully.', obj.PlanData.pk);
        end


        function updateFromPlanner(obj)
            % Update obj.PlanData with data from uplanner, including targets list (converted from table to array of struct)
            if ~isempty(obj.PlanData.planner)
                obj.PlanData.plan_type = obj.PlanData.planner.Type;
                obj.PlanData.ast_planner = obj.PlanData.planner.AstPlanner;
                obj.PlanData.title = obj.PlanData.planner.Title;
                obj.PlanData.start_time = obj.PlanData.planner.StartTime;
                obj.PlanData.end_time = obj.PlanData.planner.EndTime;
                obj.PlanData.targets = obj.PlanData.planner.planTable2struct();

                % MATLAB cannot have array with single struct item, the
                % only solution is to convert the array to cellarray
                if numel(obj.PlanData.targets) == 1
                    obj.PlanData.targets = {obj.PlanData.targets};
                end                
            end
                    
        end

        function response = deletePlan(obj, plan_pk)
            % Deletes a specific plan by its primary key (pk).
            obj.msglog('deletePlan: Deleting plan with pk=%d', plan_pk);
            plansFolder = fullfile(obj.DbPath, 'plans');
            response = struct();
        
            jsonFile = fullfile(plansFolder, sprintf('%03d.json', plan_pk));
            matFile = fullfile(plansFolder, sprintf('%03d.mat', plan_pk));
        
            if isfile(jsonFile)
                delete(jsonFile);
            end
            if isfile(matFile)
                delete(matFile);
            end
        
            obj.msglog('Plan %d deleted successfully.', plan_pk);
            response.status = 'ok';
            response.message = sprintf('Plan %d deleted successfully.', plan_pk);
            response.ok = true;
        end

        % -------------------------------------------------------------------

        function response = getPlanStatus(obj, plan_pk)
            % Retrieves the status, update_time, metadata, and history of a plan from its JSON file.
            obj.msglog('getPlanStatus: Fetching status for plan with pk=%d', plan_pk);
            plansFolder = fullfile(obj.DbPath, 'plans');
            response = struct();
        
            jsonFile = fullfile(plansFolder, sprintf('%03d.json', plan_pk));
            if ~isfile(jsonFile)
                obj.msglog('Plan file not found for pk=%d', plan_pk);
                response.status = 'error';
                response.message = 'Plan file not found.';
                response.ok = false;
                return;
            end
        
            % Load the JSON plan file
            planData = obj.readJsonFile(jsonFile);
        
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
        end

        % -------------------------------------------------------------------

        function text = readTextFile(obj, filename)
            % Reads file as text
            %
            % Parameters:
            %   filename - Full path to the JSON file to read
            %
            % Returns:
            %   data - Parsed JSON data as MATLAB structure
            %
            % Notes:
            %   - Returns empty struct if file does not exist
            %   - Logs errors to console
            if ~isfile(filename)
                obj.msglog('readTextFile: File not found at %s', filename);
                text = '';
                return;
            end

            try
                fid = fopen(filename, 'r');
                raw = fread(fid, inf, 'char');
                fclose(fid);
                text = char(raw');  % ' added to avoid Cursor problem with comments after single quote
            catch ME
                obj.msglog('readTextFile: Error reading file %s: %s', filename, ME.message);
                text = '';
            end
        end


        function data = readJsonFile(obj, filename)
            % Reads and parses a JSON file.
            %
            % Parameters:
            %   filename - Full path to the JSON file to read
            %
            % Returns:
            %   data - Parsed JSON data as MATLAB structure
            %
            % Notes:
            %   - Returns empty struct if file does not exist
            %   - Logs errors to console
            if ~isfile(filename)
                obj.msglog('readFile: File not found at %s', filename);
                data = struct();
                return;
            end
            
            try
                fid = fopen(filename, 'r');
                raw = fread(fid, inf, 'char');
                fclose(fid);
                data = jsondecode(char(raw'));  % ' added to avoid Cursor problem with comments after single quote
            catch ME
                obj.msglog('readFile: Error reading file %s: %s', filename, ME.message);
                data = struct();
            end
        end


        function writeJsonFile(obj, filename, data)
            % Writes struct to JSON file.
            %
            % Parameters:
            %   filename - Full path to the JSON file to write
            %   data - Data to write to the file
            %
            % Notes:
            %   - Converts datetime objects to ISO format  @Todo 
            %   - Writes the JSON data to the file
            %   - Logs errors to console

            fid = fopen(filename, 'w');
            fwrite(fid, jsonencode(data, 'PrettyPrint', true), 'char');
            fclose(fid);            
        end

    end
end

