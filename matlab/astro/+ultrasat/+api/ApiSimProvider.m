%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.ApiSimProvider.m
% Author: Chen Tishler
% Created: 14/09/2025
% Updated: 14/09/2025
%
%==========================================================================

classdef ApiSimProvider < ultrasat.api.Loggable
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
        FileClient
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

            if startsWith(backendTarget, 'http://', 'IgnoreCase', true) || ...
               startsWith(backendTarget, 'https://', 'IgnoreCase', true)
                % Target is a URL, so use the remote client.
                obj.msglog(sprintf('Initializing with remote backend at %s', backendTarget));
                obj.FileClient = ultrasat.api.SimpleFileClient(backendTarget, basePath);
            else
                % Target is a local path, so use the local client.
                obj.msglog(sprintf('Initializing with local backend at %s', backendTarget));
                obj.FileClient = ultrasat.api.SimpleFileLocal(backendTarget);
            end
        end


        function data = ReadJsonFile(obj, fileName)
            % Reads and parses a JSON file.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                fileName (1,1) string
            end
            data = obj.FileClient.readJson(fileName);
        end


        function success = WriteJsonFile(obj, fileName, data)
            % Writes a MATLAB struct/array to a JSON file.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                fileName (1,1) string
                data
            end
            success = obj.FileClient.writeJson(fileName, data);
        end


        function fileList = ListFilesInFolder(obj, folderName, masks)
            % Lists files in a given folder.
            %   Delegates the call to the underlying file client.
            arguments
                obj
                folderName char
                masks char = ''
            end
            fileList = obj.FileClient.listFiles(folderName, masks);
        end

        function result = NextAvailableFile(obj, folderPath, mask, zeroPad, minIndex, maxIndex)
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
            result = obj.FileClient.nextAvailableFile(folderPath, mask, zeroPad, minIndex, maxIndex);
        end
    
    end
end
