%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+tools/localPathForIo.m
% Author      : Chen Tishler
% Created     : 16/11/2026
% Description : Convert JSON-style paths to native paths for local I/O.
%==========================================================================

function out = localPathForIo(pathIn)
    % localPathForIo Map forward-slash UNC paths to OS-native form for I/O.
    %
    % On Windows, JSON may store //socsrv/rwshare_data/... ; fopen and isfile
    % need \\socsrv\rwshare_data\... . Linux paths are returned unchanged.

    if nargin < 1 || isempty(pathIn)
        out = '';
        return;
    end

    out = char(string(pathIn));
    if isempty(out)
        return;
    end

    if ispc && length(out) >= 2 && out(1) == '/' && out(2) == '/'
        out = strrep(out, '/', '\');
    end
end