%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.UserManagerSim.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Simulator implementation of the UserManagerBase interface.
%==========================================================================
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95

classdef UserManagerSim < ultrasat.api.UserManagerBase
    % Simulator implementation of the UserManagerBase interface.
    % This class provides methods to interact with the UserManagerBase interface.
    % It is a subclass of ultrasat.api.UserManagerBase.
    %
    % Typical Usage:
    %   userManager = ultrasat.api.UserManagerSim();
    %   response = userManager.login('chen', '123', 'OPER');
    %   response = userManager.IsAllowed('MissionControl.Planner.Run', 'any_plan', true);
    %   response = userManager.logout('chen');
    
    properties
        DbPath          % Path to simulator data files
        Validator       % instance of ultrasat.api.ValidatorSim()
        ApiSimProvider  % instance of ultrasat.api.ApiSimProvider()
    end


    methods
        function obj = UserManagerSim()
            % Call the base class constructor with the Args
            % ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.UserManagerBase();
            obj.msglog('UserManagerSim constructor started');           

            % Initialize the logger
            obj.LogPrefix = 'UserManagerSim';

            % Initialize the ApiSimProvider
            obj.ApiSimProvider = ultrasat.api.ApiSimProvider('', '');  %Args.SubUrl);

            % SOC_PATH must be defined in env
            soc_path = getenv('SOC_PATH');
            if soc_path == ""
                obj.msglog('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
                error('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
            end

            % Target writable data path, on Linux it is ~/soc/sim/backend/planner, on Windows it is c:\soc\sim\backend\planner
            %obj.DbPath = fullfile(soc_path, 'sim', 'backend');  % @TODO
            obj.DbPath = ultrasat.api.PathUtils.getGlobalDataFolder('users', '');
            obj.msglog('DbPath: %s', obj.DbPath);

            
            if ~exist(obj.DbPath, 'dir')
                mkdir(obj.DbPath);
                mkdir(fullfile(obj.DbPath, 'plans'));
            end

            % Master files path from the git repo: use sim/ subfolder under current folder, there should be a .gitignore file
            currentFile = mfilename('fullpath');
            currentFolder = fileparts(currentFile);
            masterPath = fullfile(currentFolder, 'sim_master');

            % Copy master files if first run % @TODO
            if ~exist(obj.DbPath, 'dir') || isempty(dir(fullfile(obj.DbPath, '*.json')))
                obj.msglog('DbPath does not exist, creating it from master files: %s', masterPath);
                obj.msglog('First run: copying default simulator files to:\n%s\n', obj.DbPath);
                copyfile(masterPath, obj.DbPath);
            end

            % Create an instance of ValidatorSim
            obj.Validator = ultrasat.api.ValidatorSim(fullfile(obj.DbPath, 'validator.json'), obj.LogFileName);
            obj.msglog('MissionClientSim constructor done');
        end                

        % -------------------------------------------------------------------
        
        function response = getNamespaceList(obj)
            % Returns the list of namespace_id values from namespaces.json
            obj.msglog('getNamespaceList: Getting list of namespaces');

       
            try
                % Use PathUtils to get the global data filename for namespaces.json
                dbFile = ultrasat.api.PathUtils.getGlobalDataFilename('', '', 'namespaces.json');

                % Use ApiSimProvider to read the JSON file
                data = obj.ApiSimProvider.readJsonFile(dbFile);

                % Extract namespace_id values
                if isfield(data, 'namespaces') && isstruct(data.namespaces)
                    entries = data.namespaces;
                    list = {entries.namespace_id};
                    displayList = strcat({entries.namespace_id}, ':', {entries.name});
                else
                    list = {};
                    displayList = {};
                end

                response.status = 'ok';
                response.ok = true;
                response.namespaces = list;
                response.display_list = displayList;       
            catch ME
                obj.msglog('Error reading namespaces: %s', ME.message);
                response.status = 'error';
                response.message = 'Failed to read or parse namespaces.';
                response.ok = false;
                response.namespaces = {};
                response.display_list = {};
            end
        end

        % -------------------------------------------------------------------

        function response = login(obj, UserName, Password, Namespace)
            % Login using username, password and device ID
            % Loads users, roles, permissions and updates session
               
            response = struct();
            obj.msglog('login: user=%s', UserName);
        
            % Set DB paths
            usersFile       = fullfile(obj.DbPath, 'users.json');
            rolesFile       = fullfile(obj.DbPath, 'roles.json');
            permissionsFile = fullfile(obj.DbPath, 'permissions.json');
            sessionsFile    = fullfile(obj.DbPath, 'sessions.json');
            %currentUserFile = fullfile(obj.DbPath, 'current_user.json');
        
            % Load users
            %if ~isfile(usersFile)
            %    obj.msglog('login: users.json not found');
            %    response.ok = false;
            %    response.status = 'error';
            %    response.message = 'Users file not found';
            %    return;
            %end
            users = obj.load_json(usersFile);
            if isfield(users, 'users')
                users = users.users;
            end
        
            % Find user
            User = [];
            UserKeys = fieldnames(users);
            for i = 1:numel(UserKeys)
                u = users.(UserKeys{i});
                if strcmp(UserKeys{i}, UserName) && strcmp(u.password, Password)
                    User = u;
                    break;
                end
            end
        
            if isempty(User)
                obj.msglog('login: invalid credentials for %s', UserName);
                response.ok = false;
                response.status = 'error';
                response.message = 'Invalid username or password';
                return;
            end
        
            % Check is_active
            if isfield(User, 'is_active') && ~User.is_active
                response.ok = false;
                response.status = 'error';
                response.message = 'User is not active';
                return;
            end
        
            % Load roles and permissions
            roles = obj.load_json(rolesFile);
            if isfield(roles, 'roles')
                roles = roles.roles;
            end
        
            permissions = obj.load_json(permissionsFile);
            if isfield(permissions, 'permissions')
                permissions = permissions.permissions;
            end
        
            % Create session ID
            loginTime = datetime('now', 'TimeZone', 'UTC');
            sessionId = sprintf('%s_%s_%s', obj.DeviceId, UserName, datestr(loginTime, 'yyyymmddTHHMMSS'));
        
            % Load existing sessions
            if isfile(sessionsFile)
                sessionsAll = load_json(sessionsFile);
            else
                sessionsAll = struct();
            end
            if ~isfield(sessionsAll, 'sessions')
                sessionsAll.sessions = struct();
            end
            sessions = sessionsAll.sessions;
        
            % Add current session
            ThisSession = struct();
            ThisSession.device_id = obj.DeviceId;
            ThisSession.user_id = UserName;
            ThisSession.login_time = datestr(loginTime, 'yyyy-mm-ddTHH:MM:SSZ');
            ThisSession.expire_time = datestr(loginTime + days(1), 'yyyy-mm-ddTHH:MM:SSZ');
            %sessions.(sessionId) = ThisSession;
            %sessionsAll.sessions = sessions;

            % DO NOT update sessions (???) @TODO - Think 
            % obj.save_json(sessionsFile, sessionsAll);
        
            % Store in current_user.json
            % currentUser = struct( ...
            %     'user_id', UserName, ...
            %     'roles', {User.roles}, ...
            %     'session_id', sessionId, ...
            %     'namespace', Namespace, ...
            %     'display_name', user.display_name, ...
            %     'login_time', datestr(loginTime, 'yyyy-mm-ddTHH:MM:SSZ') ...
            % );

            % Save to local computer (not server)
            % obj.save_json(currentUserFile, currentUser);
        
            % Store in object
            obj.User = UserName;
            obj.SessionId = sessionId;
            obj.NamespaceId = Namespace;
            obj.Roles = User.roles;
            obj.Permissions = permissions;
            obj.RolesData = roles;
            %obj.Sessions = sessionsAll;
            obj.IsLoggedIn = true;
        
            response.ok = true;
            response.status = 'ok';
            response.message = 'Login successful';
            response.session_id = sessionId;
            response.user = UserName;
        end

        % -------------------------------------------------------------------

        function response = login0(obj, UserName, Password, Namespace)
            % Simulate login by checking credentials from users.json and updating current_user.json
            obj.msglog('login: user=%s, password=%s', UserName, Password);
          
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
            fid = fopen(usersFile, 'r');
            raw = fread(fid, inf, 'char');
            fclose(fid);
            users = jsondecode(char(raw'));
        
            % Find user and verify password
            user = [];
            for i = 1:numel(users)
                if strcmp(users(i).UserName, UserName) && strcmp(users(i).Password, Password)
                    user = users(i);
                    break;
                end
            end
        
            if isempty(user)
                obj.msglog('login: Invalid username or password for user=%s', UserName);
                response.status = 'error';
                response.message = 'Invalid username or password.';
                response.ok = false;
            else
                obj.msglog('login: User %s logged in successfully.', UserName);
                response.status = 'ok';
                response.user = user;
        
                % Update current_user.json
                currentUser = struct('UserName', UserName, 'Role', user.Role, 'Namespace', Namespace);
                fid = fopen(currentUserFile, 'w');
                fwrite(fid, jsonencode(currentUser, 'PrettyPrint', true), 'char');
                fclose(fid);
        
                response.ok = true;
            end
        end


        function response = logout(obj, UserName)
            % Simulate logout by clearing current_user.json
        
            % currentUserFile = fullfile(obj.DbPath, 'current_user.json');
            % response = struct();        
            % if ~isfile(currentUserFile)
            %     obj.msglog('logout: Current user file not found at %s', currentUserFile);
            %     response.status = 'error';
            %     response.message = 'No user currently logged in.';
            %     response.ok = false;
            %     return;
            % end
        
            % Load current user and verify
            % fid = fopen(currentUserFile, 'r');
            % raw = fread(fid, inf, 'char');
            % fclose(fid);
            % currentUser = jsondecode(char(raw'));
            % 
            % if ~strcmp(currentUser.UserName, UserName)
            %     obj.msglog('logout: User %s is not currently logged in.', UserName);
            %     response.status = 'error';
            %     response.message = 'User not logged in.';
            %     response.ok = false;
            %     return;
            % end
            % 
            % % Clear current user
            % fid = fopen(currentUserFile, 'w');
            % fwrite(fid, jsonencode(struct('UserName', '', 'Role', '', 'Namespace', ''), 'PrettyPrint', true), 'char');
            % fclose(fid);
            % 
            obj.msglog('logout: User %s logged out successfully.', UserName);
            response.status = 'ok';
            response.ok = true;
        end        

        % -------------------------------------------------------------------

        function isAllowed = IsAllowed(obj, Action, Item, Params)
            %ISALLOWED Checks if the current user is permitted to perform a given action on an item.
            %   This is a MATLAB conversion of the Delphi TUserManagerSim.IsAllowed function.
            %
            %   Parameters:
            %       Action (char): The action to be performed (e.g., 'read', 'write').
            %       Item (char): The item on which the action is performed (e.g., a filename, a dataset ID).
            %       Params (struct): Optional. A struct of additional parameters for context.
            %
            %   Returns:
            %       isAllowed (logical): True if the action is permitted, false otherwise.
            %       message (char): A string explaining the reason for the decision.

            % Handle optional 'Params' argument
            if nargin < 4
                Params = struct();
            end

            isAllowed = false;
            obj.Message = '';

            % --- Initial Validation ---
            if ~obj.IsLoggedIn || isempty(obj.User)
                obj.Message = 'User not logged in';
                isAllowed = false;
                return;
            end

            if ~isfield(obj.Users, obj.User)
                obj.Message = 'User not found in user database';
                isAllowed = false;
                return;
            end

            % --- Main Permission Check Logic ---
            userStruct = obj.Users.(obj.User);
            userRoles = userStruct.roles; % This should be a cell array of role IDs

            % 1. Iterate through all roles assigned to the user
            for i = 1:numel(userRoles)
                roleID = userRoles{i};
                if ~isfield(obj.Roles, roleID)
                    continue; % Skip if role ID from user does not exist in roles db
                end
                
                roleStruct = obj.Roles.(roleID);
                permissions = roleStruct.permissions; % Cell array of permission IDs

                % 2. For each role, iterate through its permissions
                for j = 1:numel(permissions)
                    permID = permissions{j};

                    % Handle wildcard permission: if a role has '*', it grants all permissions.
                    if strcmp(permID, '*')
                        obj.Message = sprintf('Permission granted for role ''%s'' via wildcard (*).', roleID);
                        isAllowed = true;
                        return;
                    end

                    if ~isfield(obj.Permissions, permID)
                        continue; % Skip if perm ID does not exist in permissions db
                    end
                    
                    permStruct = obj.Permissions.(permID);
                    if ~isfield(permStruct, 'actions') || ~isfield(permStruct.actions, Action)
                        continue; % Skip if this permission doesn't grant the requested Action
                    end
                    
                    actionStruct = permStruct.actions.(Action);

                    % 3. Check parameter match (if required by the permission)
                    % Note: This logic was commented out in the Delphi source.
                    % It has been implemented here based on the helper functions.
                    if isfield(actionStruct, 'params')
                        requiredParams = actionStruct.params;
                        effectiveParams = obj.MergeParams(struct(), Params); % Assuming some base params might exist
                        
                        if ~obj.MatchParams(requiredParams, effectiveParams)
                            continue; % Parameters do not match, so this rule does not apply.
                        end
                    end

                    if ~isfield(actionStruct, 'items')
                        continue; % This action has no items, so it cannot match.
                    end
                    
                    items = actionStruct.items; % Cell array of item masks

                    % 4. Check if the target Item matches any of the allowed item masks
                    for k = 1:numel(items)
                        pattern = items{k};
                        % Grant permission if pattern is wildcard, or if Item is empty,
                        % or if the Item matches the mask.
                        if strcmp(pattern, '*') || isempty(Item) || obj.matchMask(Item, pattern)
                            obj.Message = sprintf('Permission granted for role ''%s'' on action ''%s'' for item ''%s''.', roleID, Action, Item);
                            isAllowed = true;
                            return;
                        end
                    end
                end
            end
            
            % If loops complete, no permission was found
            obj.Message = sprintf('Permission denied for action ''%s'' on item ''%s''.', Action, Item);
            isAllowed = false;
        end

        % -------------------------------------------------------------------

        function response = getKeyValue(obj, Store, Key, Default)
            % Retrieves a value from the key-value database JSON file.
            
            obj.msglog('getKeyValue: store=%s, key=%s', Store, Key);
            dbFile = fullfile(obj.DbPath, 'key_value_db.json');
            response = struct();
        
            if ~isfile(dbFile)
                obj.msglog('Database file not found, returning default value.');
                response.value = Default;
                response.status = 'ok';
                response.ok = true;
                return;
            end
        
            % Read and parse the JSON file
            db = obj.load_json(dbFile);
        
            if isfield(db, Store) && isfield(db.(Store), Key)
                response.value = db.(Store).(Key);
                response.status = 'ok';
                response.ok = true;
            else
                obj.msglog('Key not found, returning default value.');
                response.value = Default;
                response.status = 'ok';
                response.ok = true;
            end
        end
        
        
        function response = setKeyValue(obj, Store, Key, Value)
            % Sets a value in the key-value database JSON file.
            
            obj.msglog('setKeyValue: store=%s, key=%s, value=%s', Store, Key, Value);
            dbFile = fullfile(obj.DbPath, 'key_value_db.json');
            response = struct();
        
            db = struct();
            db = obj.load_json(dbFile);
        
            if ~isfield(db, Store)
                db.(Store) = struct();
            end
            db.(Store).(Key) = Value;
        
            % Write updated data to the JSON file
            obj.save_json(dbFile, db);
        
            response.status = 'ok';
            response.ok = true;
            obj.msglog('Key-value pair saved successfully.');
        end
    
        % -------------------------------------------------------------------

        function data = load_json(obj, path)
            data = obj.ApiSimProvider.readJsonFile(path);
        end
        
        function save_json(obj, path, data)
            obj.ApiSimProvider.writeJsonFile(path, data);
        end

        % -------------------------------------------------------------------        

    end
end
