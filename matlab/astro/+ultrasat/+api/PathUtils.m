% ***************************************************************************
% Project     : CT-Library for MATLAB
% Filename    : PathUtils.m
% Author      : Chen Tishler
% Created     : 16/09/2025
% Modified    : 16/09/2025
% Description : Utility functions for path operations
% ***************************************************************************

classdef PathUtils
    properties (Constant, Access = private)
        DEFAULT_NAMESPACE = 'OPER';   % default namespace when not set
    end
    
    properties (Access = private, Constant)
        % Static storage for BasePath and NamespaceId
        % (MATLAB does not support "class var" directly, so we use persistent)
    end
    
    methods (Static)
        
        % --- setters/getters for BasePath and NamespaceId ---
        function setBasePath(pathStr)
            persistent BasePath
            BasePath = char(pathStr);
        end
        
        function bp = getBasePath()
            persistent BasePath
            if isempty(BasePath)
                bp = '';
            else
                bp = BasePath;
            end
        end
        
        function setNamespaceId(nsId)
            persistent NamespaceId
            NamespaceId = char(nsId);
        end
        
        function ns = getNamespaceId()
            persistent NamespaceId
            if isempty(NamespaceId)
                ns = '';
            else
                ns = NamespaceId;
            end
        end
        
        % --- GetGlobalDataPath ---
        function result = getGlobalDataPath(moduleName, relativePath)
            if nargin < 1, moduleName = ''; end
            if nargin < 2, relativePath = ''; end
            
            bp = ultrasat.api.PathUtils.getBasePath();
            if isempty(moduleName)
                result = fullfile(bp, 'data', 'global', relativePath);
            else
                result = fullfile(bp, 'data', 'global', moduleName, relativePath);
            end
        end
        
        % --- GetNamespaceDataPath ---
        function result = getNamespaceDataPath(moduleName, relativePath)
            if nargin < 1, moduleName = ''; end
            if nargin < 2, relativePath = ''; end
            
            bp = ultrasat.api.PathUtils.getBasePath();
            ns = ultrasat.api.PathUtils.getNamespaceId();
            if isempty(ns)
                ns = ultrasat.api.PathUtils.DEFAULT_NAMESPACE;
            end
            
            nsPath = fullfile(bp, 'data', 'namespaces', ns);
            
            if isempty(moduleName)
                result = fullfile(nsPath, relativePath);
            else
                result = fullfile(nsPath, moduleName, relativePath);
            end
        end
        
        % --- GetGlobalLogPath ---
        function result = getGlobalLogPath(moduleName, relativePath)
            if nargin < 1, moduleName = ''; end
            if nargin < 2, relativePath = ''; end
            
            bp = ultrasat.api.PathUtils.getBasePath();
            if isempty(moduleName)
                result = fullfile(bp, 'log', 'global', relativePath);
            else
                result = fullfile(bp, 'log', 'global', moduleName, relativePath);
            end
        end
        
        % --- GetNamespaceLogPath ---
        function result = getNamespaceLogPath(moduleName, fileName)
            if nargin < 1, moduleName = ''; end
            if nargin < 2, fileName = ''; end
            
            bp = ultrasat.api.PathUtils.getBasePath();
            ns = ultrasat.api.PathUtils.getNamespaceId();
            if isempty(ns)
                ns = ultrasat.api.PathUtils.DEFAULT_NAMESPACE;
            end
            
            nsPath = fullfile(bp, 'log', 'namespaces', ns);
            
            if isempty(moduleName)
                result = fullfile(nsPath, fileName);
            else
                result = fullfile(nsPath, moduleName, fileName);
            end
        end
    end
end
