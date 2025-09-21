% ***************************************************************************
% Project     : CT-Library for MATLAB
% Filename    : PathUtils.m
% Author      : Chen Tishler
% Created     : 16/09/2025
% Modified    : 17/09/2025
% Description : Utility functions for path operations
% ***************************************************************************

% PathUtils - Utility functions for system-wide path operations.
% VERSION: char-based for compatibility with older MATLAB versions or specific requirements.
classdef PathUtils
    methods (Static)
        function val = BasePath(newVal)
            persistent basePath
            if isempty(basePath)
                basePath = ''; % initialize
            end
            if nargin > 0
                basePath = newVal; % setter
            end
            val = basePath;
        end
    
        function val = NamespaceId(newVal)
            persistent nsId
            if isempty(nsId)
                nsId = '';
            end
            if nargin > 0
                nsId = newVal;
            end
            val = nsId;
        end
    end


    methods (Static)
        %% Folder Generation Methods

        function folderPath = getGlobalDataFolder(moduleName, subFolder)
            % Get the global data folder.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
            end
            % CHANGE: Concatenation with [] instead of +
            folderPath = [fullfile(ultrasat.api.PathUtils.BasePath, 'data', 'global', moduleName, subFolder), filesep];
        end

        function folderPath = getNamespaceDataFolder(moduleName, subFolder, options)
            % Get the namespace data folder.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
                options.NamespaceId (1,:) char = ''
            end
            nsId = ultrasat.api.PathUtils.resolveNamespaceId(options.NamespaceId);
            folderPath = [fullfile(ultrasat.api.PathUtils.BasePath, 'data', 'namespaces', nsId, moduleName, subFolder), filesep];
        end

        function folderPath = getGlobalDailyDataFolder(moduleName, subFolder, options)
            % Get the global daily data folder.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
                options.DT datetime = datetime.empty
            end
            dt = ultrasat.api.PathUtils.resolveDateTime(options.DT);
            % CHANGE: datestr instead of string()
            dateSubFolder = datestr(dt, 'yyyy-mm-dd');

            % CHANGE: isempty() check instead of == ""
            if isempty(subFolder)
                folderPath = [fullfile(ultrasat.api.PathUtils.BasePath, 'data', 'global', moduleName, dateSubFolder), filesep];
            else
                folderPath = [fullfile(ultrasat.api.PathUtils.BasePath, 'data', 'global', moduleName, subFolder, dateSubFolder), filesep];
            end
        end

        function folderPath = getNamespaceDailyDataFolder(moduleName, subFolder, options)
            % Get the namespace daily data folder.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
                options.DT datetime = datetime.empty
                options.NamespaceId (1,:) char = ''
            end
            dt = ultrasat.api.PathUtils.resolveDateTime(options.DT);
            nsId = ultrasat.api.PathUtils.resolveNamespaceId(options.NamespaceId);
            dateSubFolder = datestr(dt, 'yyyy-mm-dd');
            
            if isempty(subFolder)
                folderPath = [fullfile(ultrasat.api.PathUtils.BasePath, 'data', 'namespaces', nsId, moduleName, dateSubFolder), filesep];
            else
                folderPath = [fullfile(ultrasat.api.PathUtils.BasePath, 'data', 'namespaces', nsId, moduleName, subFolder, dateSubFolder), filesep];
            end
        end

        %% Filename Generation Methods

        function filePath = getGlobalDataFilename(moduleName, subFolder, fileName)
            % Get the global data filename.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
                fileName (1,:) char
            end
            folder = ultrasat.api.PathUtils.getGlobalDataFolder(moduleName, subFolder);
            filePath = fullfile(folder, fileName);
        end

        function filePath = getNamespaceDataFilename(moduleName, subFolder, fileName, options)
            % Get the namespace data filename.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
                fileName (1,:) char
                options.NamespaceId (1,:) char = ''
            end
            folder = ultrasat.api.PathUtils.getNamespaceDataFolder(moduleName, subFolder, 'NamespaceId', options.NamespaceId);
            filePath = fullfile(folder, fileName);
        end

        function filePath = getGlobalDailyDataFilename(moduleName, subFolder, fileName, options)
            % Get the global daily data filename.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
                fileName (1,:) char
                options.DT datetime = datetime.empty
                options.IncludeTimestampInFilename (1,1) logical = true
            end
            dt = ultrasat.api.PathUtils.resolveDateTime(options.DT);
            folder = ultrasat.api.PathUtils.getGlobalDailyDataFolder(moduleName, subFolder, 'DT', dt);
            if options.IncludeTimestampInFilename
                % CHANGE: datestr instead of string(), different format specifiers
                timestamp = datestr(dt, 'yyyy-mm-dd-HH-MM-SS');
                % CHANGE: Concatenation with []
                fileName = [timestamp, '-', fileName];
            end
            filePath = fullfile(folder, fileName);
        end

        function filePath = getNamespaceDailyDataFilename(moduleName, subFolder, fileName, options)
            % Get the namespace daily data filename.
            arguments
                moduleName (1,:) char
                subFolder (1,:) char
                fileName (1,:) char
                options.DT datetime = datetime.empty
                options.NamespaceId (1,:) char = ''
                options.IncludeTimestampInFilename (1,1) logical = true
            end
            dt = ultrasat.api.PathUtils.resolveDateTime(options.DT);
            folder = ultrasat.api.PathUtils.getNamespaceDailyDataFolder(moduleName, subFolder, 'DT', dt, 'NamespaceId', options.NamespaceId);
            if options.IncludeTimestampInFilename
                timestamp = datestr(dt, 'yyyy-mm-dd-HH-MM-SS');
                fileName = [timestamp, '-', fileName];
            end
            filePath = fullfile(folder, fileName);
        end
        
        %% Log Filename Generation Methods

        function filePath = getGlobalLogFilename(moduleName, fileName, options)
            % Get the global log filename.
            arguments
                moduleName (1,:) char
                fileName (1,:) char
                options.DT datetime = datetime.empty
            end
            dt = ultrasat.api.PathUtils.resolveDateTime(options.DT);
            logSubPath = ultrasat.api.PathUtils.getLogSubfolderFilename(fileName, dt);
            filePath = fullfile(ultrasat.api.PathUtils.BasePath, 'log', 'global', moduleName, logSubPath);
        end

        function filePath = getNamespaceLogFilename(moduleName, fileName, options)
            % Get the namespace log filename.
            arguments
                moduleName (1,:) char
                fileName (1,:) char
                options.DT datetime = datetime.empty
                options.NamespaceId (1,:) char = ''
            end
            dt = ultrasat.api.PathUtils.resolveDateTime(options.DT);
            nsId = ultrasat.api.PathUtils.resolveNamespaceId(options.NamespaceId);
            logSubPath = ultrasat.api.PathUtils.getLogSubfolderFilename(fileName, dt);
            filePath = fullfile(ultrasat.api.PathUtils.BasePath, 'log', 'namespaces', nsId, moduleName, logSubPath);
        end
        
        %% Configuration Method

        function setBasePath(basePath)
            % Set the base path, normalizing path separators.
            arguments
                basePath (1,:) char
            end
            path = strrep(basePath, '\', '/'); % strrep is common for char
            if ~endsWith(path, '/')
                path = [path, '/'];
            end
            ultrasat.api.PathUtils.BasePath(path);
        end

        function setNamespaceId(Id)
            % Set the base path, normalizing path separators.
            arguments
                Id (1,:) char
            end
            ultrasat.api.PathUtils.NamespaceId = Id;
        end        
    end

    methods (Static, Access = private)
        % Internal helper methods
        
        function subPath = getLogSubfolderFilename(fileName, dt)
            % Internal: Get the log subfolder filename.
            yearStr = datestr(dt, 'yyyy');
            monthStr = datestr(dt, 'mm');
            
            baseName = [yearStr, '-', monthStr, '-', fileName];
            if ~endsWith(baseName, '.log', 'IgnoreCase', true)
                baseName = [baseName, '.log'];
            end
            subPath = fullfile(yearStr, monthStr, baseName);
        end
        
        function nsId = resolveNamespaceId(providedNsId)
            % Resolves the namespace ID based on provided, static, and default values.
            if ~isempty(providedNsId)
                nsId = providedNsId;
            elseif ~isempty(ultrasat.api.PathUtils.NamespaceId)
                nsId = ultrasat.api.PathUtils.NamespaceId;
            else
                nsId = 'OPER'; % Default fallback
            end
        end

        function dt = resolveDateTime(providedDT)
            % Resolves the datetime object, using current UTC time if not provided.
            if isempty(providedDT)
                dt = datetime('now', 'TimeZone', 'UTC');
            else
                dt = providedDT;
            end
        end
    end
end
