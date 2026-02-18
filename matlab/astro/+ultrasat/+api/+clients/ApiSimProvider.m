%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.ApiSimProvider.m
% Author      : Chen Tishler
% Created     : 14/09/2025
% Updated     : 06/10/2025
% Description : Provides a unified interface for file operations, simulating an API backend.
%==========================================================================

classdef ApiSimProvider < ultrasat.api.core.Loggable
    % Provides a unified interface for file operations, simulating an API backend.
    % It can operate on a local filesystem or a remote server.
    %
    % This class acts as a wrapper around SimpleFileClient and SimpleFileLocal.
    % The constructor intelligently determines which client to use based on the provided 'backendTarget' string.
    %
    %   Usage (Remote Server):
    %     provider = ApiSimProvider('http://localhost:5000', 'sim_data/');
    %     jsonData = provider.ReadJsonFile('config.json');
    %
    %   Usage (Local Filesystem):
    %     provider = ApiSimProvider('C:\SOC\sim\backend');
    %     jsonData = provider.ReadJsonFile('config.json');

    properties (Access = private)
        % This property holds the instance of either SimpleFileClient or
        % SimpleFileLocal, determined at construction time.
        Mode char = 'remote';   % 'remote' | 'local' - Loaded from Config/UltrasatPlanner.yaml
        FileClient              % ultrasat.api.SimpleFileClient | ultrasat.api.SimpleFileLocal
        BasePath char
    end


    methods (Access = public)
        function obj = ApiSimProvider(backendTarget, basePath)
            % Construct an instance of this class.
            %   obj = ApiSimProvider(backendTarget)
            %   obj = ApiSimProvider(backendTarget, basePath)
            %
            %   backendTarget: A string that is either a URL (e.g.,
            %                  "http://localhost:5000") or a local file
            %                  path (e.g., "C:\temp\sim_data").
            %
            %   basePath: (Optional) For remote servers, this is a path
            %             prefix on the server (e.g., "data/"). It is
            %             ignored for local filesystem targets.
            arguments
                backendTarget char
                basePath char = ''
            end

            % Initialize the logger
            obj.LogPrefix = 'ApiSimProvider';
            obj.BasePath = basePath;

            % Get configuration
            config = ultrasat.api.core.Config.getApiConfig();
            obj.Mode = config.mode;

            % Client mode - use API to access server
            % Local mode - access local files directly
            if strcmp(obj.Mode, 'client')
                backendTarget = config.server_url;
                obj.FileClient = ultrasat.api.clients.SimpleFileClient(backendTarget);
            else
                backendTarget = getenv('SOC_PATH');
                obj.FileClient = ultrasat.api.clients.SimpleFileLocal(backendTarget);
            end

            return;

            % Target is a URL, so use the remote client.
            if startsWith(backendTarget, 'http://', 'IgnoreCase', true) || ...
               startsWith(backendTarget, 'https://', 'IgnoreCase', true)
                obj.msglog(sprintf('Initializing with remote backend at %s', backendTarget));
                obj.FileClient = ultrasat.api.clients.SimpleFileClient(backendTarget, basePath);

            % Target is a local path, so use the local client.
            else
                obj.msglog(sprintf('Initializing with local backend at %s', backendTarget));

                useLocal = true;
                if useLocal
                    obj.FileClient = ultrasat.api.clients.SimpleFileLocal(backendTarget);
                else
                    obj.FileClient = ultrasat.api.clients.SimpleFileClient(backendTarget);
                end
            end
        end


        function result = healthCheck(obj)
            % Check the health of the server.
            %   healthCheck = obj.healthCheck()
            %   Returns true on success, false on error.
            arguments
                obj
            end
            result = obj.FileClient.healthCheck();
        end


        function content = readFile(obj, filePath)
            % Reads a file from the server.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                filePath char
            end
            path = [obj.BasePath, filePath];
            content = obj.FileClient.readFile(path);
        end


        function data = readJsonFile(obj, fileName)
            % Reads and parses a JSON file.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                fileName char
            end
            path = [obj.BasePath, fileName];
            data = obj.FileClient.readJson(path);
        end


        function success = writeFile(obj, filePath, data, append)
            % Writes a file to the server.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                filePath char
                data
                append (1,1) logical
            end
            path = [obj.BasePath, filePath];
            success = obj.FileClient.writeFile(path, data, append);
        end


        function success = writeJsonFile(obj, fileName, data)
            % Writes a MATLAB struct/array to a JSON file.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                fileName char
                data
            end
            path = [obj.BasePath, fileName];
            success = obj.FileClient.writeJson(path, data);
        end


        function data = readBinaryFile(obj, fileName)
            % Reads and parses a binary file.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                fileName char
            end
            path = [obj.BasePath, fileName];
            data = obj.FileClient.readBinaryFile(path);
        end


        function success = writeBinaryFile(obj, filePath, data)
            % Writes a binary file to the server.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                filePath char
                data
            end
            path = [obj.BasePath, filePath];
            success = obj.FileClient.writeBinaryFile(path, data);
        end


        function success = saveMatObject(obj, relativeFilePath, dataObject, variableName)
            % Saves a MATLAB object/variable to a .mat file via the provider.
            %   This is a high-level wrapper that handles the process of:
            %   1. Saving the object to a temporary local .mat file.
            %   2. Reading the raw binary bytes of that temp file.
            %   3. Sending those bytes using the WriteBinaryFile method.
            arguments
                obj
                relativeFilePath char
                dataObject
                variableName char
            end

            success = false; % Default to failure

            % Create a temporary file path.
            tempMatFile = [tempname, '.mat'];

            % CRITICAL: Use onCleanup to guarantee the temp file is deleted,
            % even if an error occurs during the process.
            cleanupObj = onCleanup(@() delete(tempMatFile));

            try
                % The 'save' command saves variables, not direct objects. The
                % standard, robust way to handle this is to put the object
                % into a struct and use the '-struct' flag with 'save'.
                tempStruct.(variableName) = dataObject;
                save(tempMatFile, '-struct', 'tempStruct', variableName);

                % Read the raw bytes from the newly created temporary file
                fid = fopen(tempMatFile, 'rb');
                matBytes = fread(fid, inf, '*uint8')';
                fclose(fid);

                % Now, use the existing binary write method to send the data.
                success = obj.writeBinaryFile(relativeFilePath, matBytes);

            catch ME
                obj.msglog('Failed to save MAT object to "%s": %s', relativeFilePath, ME.message);
            end
        end


        function [loadedObject, success] = loadMatObject(obj, relativeFilePath, variableName)
            % Loads a MATLAB object/variable from a .mat file via the provider.
            %   This is a high-level wrapper that handles the process of:
            %   1. Reading the raw binary bytes using the ReadBinaryFile method.
            %   2. Writing those bytes to a temporary local .mat file.
            %   3. Loading the object from that temporary file.
            arguments
                obj
                relativeFilePath char
                variableName char
            end

            loadedObject = [];
            success = false;

            % Read the binary data from the provider (local or remote)
            matBytes = obj.readBinaryFile(relativeFilePath);

            if isempty(matBytes)
                obj.msglog('Failed to load MAT object: received no binary data from "%s".', relativeFilePath);
                return;
            end

            % Create a temporary file path and guarantee its deletion.
            tempMatFile = [tempname, '.mat'];
            cleanupObj = onCleanup(@() delete(tempMatFile));

            try
                % Write the received bytes to the temporary file
                fid = fopen(tempMatFile, 'wb');
                fwrite(fid, matBytes, 'uint8');
                fclose(fid);

                % Load the variable from the temporary .mat file
                loadedStruct = load(tempMatFile, variableName);

                % Extract the object from the loaded struct
                loadedObject = loadedStruct.(variableName);
                success = true;

            catch ME
                obj.msglog('Failed to load MAT object from "%s": %s', relativeFilePath, ME.message);
            end
        end


        function fileList = listFilesInFolder(obj, folderName, masks)
            % Lists files in a given folder.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                folderName char
                masks char = ''
            end
            path = [obj.BasePath, folderName];
            fileList = obj.FileClient.listFiles(path, masks);
        end


        function result = nextAvailableFile(obj, folderPath, mask, zeroPad, minIndex, maxIndex)
            % Finds the next available sequential filename.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                folderPath char
                mask char
                zeroPad (1,1) double {mustBeInteger, mustBeNonnegative}
                minIndex (1,1) double {mustBeInteger, mustBeNonnegative}
                maxIndex (1,1) double {mustBeInteger, mustBePositive}
            end
            path = [obj.BasePath, folderPath];
            result = obj.FileClient.nextAvailableFile(path, mask, zeroPad, minIndex, maxIndex);
        end


        function result = deleteFile(obj, filePath)
            % Deletes a file from the server.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                filePath char
            end
            path = [obj.BasePath, filePath];
            result = obj.FileClient.deleteFile(path);
        end

    end
end
