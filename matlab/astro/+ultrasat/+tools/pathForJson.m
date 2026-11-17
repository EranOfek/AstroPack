%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+tools/pathForJson.m
% Author      : Chen Tishler
% Created     : 16/11/2026
% Description : Normalize filesystem paths for JSON (forward slashes).
%==========================================================================

function out = pathForJson(pathIn)
    % pathForJson Replace backslashes with slashes for JSON serialization.
    %
    % Empty or missing input returns ''. UNC paths keep a leading // prefix
    % when the original used \\host\share form. Does not change paths used
    % for local fopen/isfile — only use when writing JSON or IPC payloads.

    if nargin < 1 || isempty(pathIn)
        out = '';
        return;
    end

    s = char(string(pathIn));
    if isempty(s)
        out = '';
        return;
    end

    out = strrep(s, '\', '/');
end