%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.MissionClientBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 16/02/2025
%
%==========================================================================

classdef SimpleFileLocal < ultrasat.api.Loggable
    %SIMPLEFILELOCAL A local file system interface with the same API as SimpleFileClient.
    %   This class provides methods to list, read, and write files directly
    %   on the local disk. It is designed to be a drop-in replacement for
    %   SimpleFileClient for local development and testing, allowing code to
    %   be written once and run against either a local directory or a remote server.
    %
    %   To use:
    %   % Create a client that operates in a subfolder named 'local_data'
    %   local_client = SimpleFileLocal("c:\temp\local_data");
    %   local_client.writeFile("greetings.txt", "Hello, local world!");
    %   files = local_client.listFiles("", "*.txt");
    %   disp(files);

    properties
        % The base path on the local file system where all operations will occur.
        % This acts as the 'root' directory for this class instance.
        BasePath char
    end


    methods (Access = public)
        function obj = SimpleFileLocal(basePath)
            % Construct an instance of this class.
            %   obj = SimpleFileLocal(basePath) creates an object that will
            %   perform all file operations within the specified local base path.
            %   The base path directory will be created if it does not exist.
            arguments
                basePath char
            end

            % Initialize the logger
            obj.LogPrefix = 'SimpleFileLocal';

            % If no base path is provided, use the default.
            if isempty(basePath)
                basePath = obj.resolveDefaultBasePath();
            end
            obj.BasePath = basePath;

            % Ensure the base directory exists.
            if ~isfolder(obj.BasePath)
                try
                    mkdir(obj.BasePath);
                    obj.msglog(sprintf('Created base directory at: %s', obj.BasePath));
                catch ME
                    error('Could not create base directory: %s\n%s', obj.BasePath, ME.message);
                end
            end
        end


        function basePath = resolveDefaultBasePath(obj)
            % Resolves the default base path for simulation data.
            % This function replicates the logic from the Python project to find the
            % correct directory for backend simulation files.
            %
            %   The logic is as follows:
            %   1. Check for the SOC_PATH environment variable. If it exists, use
            %      [SOC_PATH]/sim/backend.
            %   2. If not, check the operating system:
            %      - On Windows, default to "C:/soc/sim/backend".
            %      - On Linux/macOS, default to "~/soc/sim/backend" (home directory).
            %
            %   Returns:
            %       basePath (char): The resolved, absolute path.
        
            soc_env = getenv('SOC_PATH');
        
            if ~isempty(soc_env)
                % Use the path from the environment variable
                basePath = fullfile(soc_env, 'sim', 'backend');
            else
                % Fallback to OS-specific defaults
                if ispc
                    % Windows default
                    basePath = fullfile('C:', 'soc', 'sim', 'backend');
                else
                    % Linux, macOS, and other Unix-like systems default
                    home_dir = getenv('HOME');
                    if isempty(home_dir)
                        % A fallback just in case HOME is not set
                        home_dir = char(java.lang.System.getProperty('user.home'));
                    end
                    basePath = fullfile(home_dir, 'soc', 'sim', 'backend');
                end
            end
        end


        function fileList = listFiles(obj, folderPath, masks)
            % List files in a local folder.
            %   fileList = obj.listFiles(folderPath) lists all files in the folder.
            %   fileList = obj.listFiles(folderPath, masks) lists files matching
            %   the specified masks (e.g., '*.txt,*.json').
            arguments
                obj
                folderPath char
                masks char = ''
            end

            fullFolderPath = fullfile(obj.BasePath, folderPath);
            if ~isfolder(fullFolderPath)
                obj.msglog(sprintf('Warning: Folder does not exist for listing: %s', fullFolderPath));
                fileList = {};
                return;
            end

            if isempty(masks)
                masks = '*'; % Default to all files
            end
            
            % Split masks by comma or semicolon
            mask_patterns = split(masks, [',', ';']);
            
            allFileNames = {};
            for i = 1:numel(mask_patterns)
                pattern = strtrim(mask_patterns{i});
                if isempty(pattern)
                    continue;
                end
                
                dir_struct = dir(fullfile(fullFolderPath, pattern));
                
                % Filter out directories
                is_file = ~[dir_struct.isdir];
                filenames = {dir_struct(is_file).name};
                allFileNames = [allFileNames, filenames]; %#ok<AGROW>
            end

            if isempty(allFileNames)
                fileList = {}
            else
                % Return unique sorted list
                fileList = unique(allFileNames)';
            end
        end


        function content = readFile(obj, filePath)
            % Read file content as text from the local disk.
            %   content = obj.readFile(filePath)
            %   Returns the file content as a string or an empty string on error.
            arguments
                obj
                filePath char
            end

            fullLocalPath = fullfile(obj.BasePath, filePath);
            if ~isfile(fullLocalPath)
                obj.msglog(sprintf('Error reading file: File not found at %s', fullLocalPath));
                content = '';
                return;
            end

            try
                content = fileread(fullLocalPath);
            catch ME
                obj.msglog(sprintf('Error reading file %s: %s', filePath, ME.message));
                content = '';
            end
        end


        function data = readJson(obj, filePath)
            % Read and parse a local JSON file.
            %   data = obj.readJson(filePath)
            %   Returns the parsed JSON as a MATLAB struct or an empty struct on error.
            arguments
                obj
                filePath char
            end

            content = obj.readFile(filePath);
            if ~isempty(content)
                try
                    data = jsondecode(content);
                catch ME
                    obj.msglog(sprintf('Error decoding JSON from file %s": %s', filePath, ME.message));
                    data = struct();
                end
            else
                data = struct();
            end
        end


        function success = writeFile(obj, filePath, data, append)
            % Write text to a local file.
            %   success = obj.writeFile(filePath, data) writes/overwrites a file.
            %   success = obj.writeFile(filePath, data, true) appends to the file.
            %   Returns true on success, false on error.
            arguments
                obj
                filePath char
                data char
                append logical = false
            end
            
            fullLocalPath = fullfile(obj.BasePath, filePath);

            % Ensure the target directory exists
            [targetDir, ~, ~] = fileparts(fullLocalPath);
            if ~isfolder(targetDir)
                mkdir(targetDir);
            end

            if append
                fileMode = 'a'; % Append
            else
                fileMode = 'w'; % Write (overwrite)
            end

            try
                fileID = fopen(fullLocalPath, fileMode, 'n', 'UTF-8');
                if fileID == -1
                    error('Could not open file for writing.');
                end
                fprintf(fileID, '%s', data);
                fclose(fileID);
                success = true;
            catch ME
                obj.msglog(sprintf('Error writing to file %s: %s', fullLocalPath, ME.message));
                success = false;
            end
        end


        function success = writeJson(obj, filePath, data)
            % Write a MATLAB struct/array to a local JSON file.
            %   success = obj.writeJson(filePath, data)
            %   Returns true on success, false on error.
            arguments
                obj
                filePath char
                data
            end

            try
                jsonStr = jsonencode(data);
                success = obj.writeFile(filePath, jsonStr);
            catch ME
                obj.msglog(sprintf('Error encoding data for JSON file %s: %s', filePath, ME.message));
                success = false;
            end
        end


        function result = nextAvailableFile(obj, folderPath, mask, zeroPad, minIndex, maxIndex)
            % Get the next available file in a local folder.
            %   result = obj.nextAvailableFile(folderPath, mask, zeroPad, minIndex, maxIndex)
            %   Finds the next sequential filename based on a numeric prefix.
            %   Returns a struct with details or an empty struct on error.
            arguments
                obj
                folderPath char
                mask char
                zeroPad double {mustBeInteger, mustBeNonnegative}
                minIndex double {mustBeInteger, mustBeNonnegative}
                maxIndex double {mustBeInteger, mustBePositive}
            end

            result = struct();
            try
                existingFiles = obj.listFiles(folderPath, mask);
                
                nextIndex = -1;
                for i = minIndex:maxIndex
                    numStr = sprintf(['%0' num2str(zeroPad) 'd'], i);
                    expectedFile = replace(mask, '*', numStr);
                    
                    if ~ismember(expectedFile, existingFiles)
                        nextIndex = i;
                        break;
                    end
                end

                if nextIndex == -1
                     obj.msglog(sprintf('No available file index found in range %d-%d for mask %s', minIndex, maxIndex, mask));
                     return;
                end
                
                finalNumStr = sprintf(['%0' num2str(zeroPad) 'd'], nextIndex);
                finalFilename = replace(mask, '*', finalNumStr);
                
                result.index = nextIndex;
                result.filename = finalFilename;
                result.full_path = fullfile(obj.BasePath, folderPath, finalFilename);

            catch ME
                obj.msglog(sprintf('Error getting next available file in %s: %s', folderPath, ME.message));
                result = struct();
            end
        end

    end
end
