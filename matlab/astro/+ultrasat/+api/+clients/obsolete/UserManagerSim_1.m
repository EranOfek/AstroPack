%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.UserManagerSim.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 18/02/2026
% Description : Simulator implementation of the UserManagerBase interface.
%==========================================================================
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95

classdef UserManagerSim < ultrasat.api.clients.UserManagerBase
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
            obj@ultrasat.api.clients.UserManagerBase();
            obj.msglog('UserManagerSim constructor started');

            % Initialize the logger
            obj.LogPrefix = 'UserManagerSim';

            % Initialize the ApiSimProvider
            obj.ApiSimProvider = ultrasat.api.clients.ApiSimProvider('', '');  %Args.SubUrl);

            % SOC_PATH must be defined in env
            soc_path = getenv('SOC_PATH');
            if soc_path == ""
                obj.msglog('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
                error('SOC_PATH environment variable is not defined, on Linux set it to ~/soc, on Windows set it to c:\\soc');
            end

            % Target writable data path, on Linux it is ~/soc/sim/backend/planner, on Windows it is c:\soc\sim\backend\planner
            %obj.DbPath = fullfile(soc_path, 'sim', 'backend');  % @TODO
            obj.DbPath = ultrasat.api.utils.PathUtils.getGlobalDataFolder('users', '');
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
            obj.Validator = ultrasat.api.clients.ValidatorSim(fullfile(obj.DbPath, 'validator.json'), obj.LogFileName);
            obj.msglog('MissionClientSim constructor done');
        end

        % -------------------------------------------------------------------

        function response = getNamespaceList(obj)
            % Returns the list of namespace_id values from namespaces.json
            obj.msglog('getNamespaceList: Getting list of namespaces');


            try
                % Use PathUtils to get the global data filename for namespaces.json
                dbFile = ultrasat.api.utils.PathUtils.getGlobalDataFilename('', '', 'namespaces.json');

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
            usersFile = fullfile(obj.DbPath, 'users.json');
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

            response.ok = true;
            response.status = 'ok';
            response.message = 'Login successful';
            response.session_id = sessionId;
            response.user = UserName;
        end

        % -------------------------------------------------------------------

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

        function data = load_json(obj, path)
            data = obj.ApiSimProvider.readJsonFile(path);
        end

        function save_json(obj, path, data)
            obj.ApiSimProvider.writeJsonFile(path, data);
        end

        % -------------------------------------------------------------------

    end
end
