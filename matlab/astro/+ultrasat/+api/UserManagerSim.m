%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.MissionClientSim.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 16/03/2025
% Description : 
%==========================================================================
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95

classdef UserManagerSim < ultrasat.api.UserManagerBase
    % 
    
    properties
        DbPath          % Path to simulator data files
        Validator       % instance of ultrasat.api.ValidatorSim()
        ApiSimProvider  % instance of ultrasat.api.ApiSimProvider()
    end


    methods
        function obj = MissionClientSim(Args)
            arguments          
                Args.SubUrl         = '/mission';  % planner_backend  
                Args.LogFileName
            end

            % Initialize the logger
            obj.LogPrefix = 'UserManagerSim';

            % Initialize the ApiSimProvider
            obj.ApiSimProvider = ultrasat.api.ApiSimProvider(Args.SubUrl);

            % Call the base class constructor with the Args
            ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.UserManagerBase(ArgsCell{:});  % Args);  % , 'SubUrl', '/mission');
            obj.msglog('UserManagerSim constructor started');
            
            % SOC_PATH must be defined in env
            soc_path = getenv('SOC_PATH');
            if soc_path == ""
                obj.msglog('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
                error('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
            end

            % Target writable data path, on Linux it is ~/soc/sim/backend/planner, on Windows it is c:\soc\sim\backend\planner
            obj.DbPath = fullfile(soc_path, 'sim', 'backend');  % @TODO
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
        
            response = struct();        
            dbFile = fullfile(obj.DbPath, 'namespaces.json');
        
            if ~isfile(dbFile)
                obj.msglog('Namespaces file not found at %s', dbFile);
                response.status = 'error';
                response.message = 'Namespaces database not found.';
                response.ok = false;
                response.namespaces = {};
                response.display_list = {};
                return;
            end
        
            try
                % Read and decode JSON
                fid = fopen(dbFile, 'r');
                cleaner = onCleanup(@() fclose(fid));  % Ensure file is closed on exit
                raw = fread(fid, inf, 'char');
                data = jsondecode(char(raw'));
        
                % Extract namespace_id values
                if isfield(data, 'namespaces') && isstruct(data.namespaces)
                    entries = data.namespaces;
                    list = {entries.namespace_id};
                    displayList = strcat({entries.namespace_id}, ' - ', {entries.name});
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
            usersFile       = fullfile(obj.DbPath, 'users', 'users.json');
            rolesFile       = fullfile(obj.DbPath, 'users', 'roles.json');
            permissionsFile = fullfile(obj.DbPath, 'users', 'permissions.json');
            sessionsFile    = fullfile(obj.DbPath, 'users', 'sessions.json');
            currentUserFile = fullfile(obj.DbPath, 'users', 'current_user.json');
        
            % Load users
            if ~isfile(usersFile)
                obj.msglog('login: users.json not found');
                response.ok = false;
                response.status = 'error';
                response.message = 'Users file not found';
                return;
            end
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
                obj.msglog('login: invalid credentials for %s', username);
                response.ok = false;
                response.status = 'error';
                response.message = 'Invalid username or password';
                return;
            end
        
            % Check is_active
            if isfield(User, 'is_active') && ~user.is_active
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
            sessionId = sprintf('%s_%s_%s', deviceId, username, datestr(loginTime, 'yyyymmddTHHMMSS'));
        
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
            s = struct();
            s.device_id = deviceId;
            s.user_id = username;
            s.login_time = datestr(loginTime, 'yyyy-mm-ddTHH:MM:SSZ');
            s.expire_time = datestr(loginTime + days(1), 'yyyy-mm-ddTHH:MM:SSZ');
            sessions.(sessionId) = s;
            sessionsAll.sessions = sessions;
            obj.save_json(sessionsFile, sessionsAll);
        
            % Store in current_user.json
            currentUser = struct( ...
                'user_id', username, ...
                'roles', {user.roles}, ...
                'session_id', sessionId, ...
                'namespace', namespace, ...
                'display_name', user.display_name, ...
                'login_time', datestr(loginTime, 'yyyy-mm-ddTHH:MM:SSZ') ...
            );
            obj.save_json(currentUserFile, currentUser);
        
            % Store in object
            obj.User = username;
            obj.SessionId = sessionId;
            obj.NamespaceId = namespace;
            obj.Roles = user.roles;
            obj.Permissions = permissions;
            obj.RolesData = roles;
            obj.Sessions = sessionsAll;
            obj.IsLoggedIn = true;
        
            response.ok = true;
            response.status = 'ok';
            response.message = 'Login successful';
            response.session_id = sessionId;
            response.user = user;
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
            fid = fopen(currentUserFile, 'r');
            raw = fread(fid, inf, 'char');
            fclose(fid);
            currentUser = jsondecode(char(raw'));
        
            if ~strcmp(currentUser.UserName, UserName)
                obj.msglog('logout: User %s is not currently logged in.', UserName);
                response.status = 'error';
                response.message = 'User not logged in.';
                response.ok = false;
                return;
            end
        
            % Clear current user
            fid = fopen(currentUserFile, 'w');
            fwrite(fid, jsonencode(struct('UserName', '', 'Role', '', 'Namespace', ''), 'PrettyPrint', true), 'char');
            fclose(fid);
        
            obj.msglog('logout: User %s logged out successfully.', UserName);
            response.status = 'ok';
            response.ok = true;
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
            data = obj.ApiSimProvider.ReadJsonFile(path);
        end
        
        function save_json(obj, path, data)
            obj.ApiSimProvider.WriteJsonFile(path, data);
        end

        % -------------------------------------------------------------------        

    end
end
