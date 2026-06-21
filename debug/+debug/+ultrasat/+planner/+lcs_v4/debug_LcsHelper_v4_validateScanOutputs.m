%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v4/debug_LcsHelper_v4_validateScanOutputs.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 11/06/2026
% Description : Thin wrapper — scan-output validator moved to
%               ultrasat.planner.LcsHelper_v4_validateScanOutputs.
%
% Run by      : debug.ultrasat.planner.lcs_v4.debug_LcsHelper_v4_validateScanOutputs()
%               debug.ultrasat.planner.lcs_v4.debug_LcsHelper_v4_validateScanOutputs('Year', 2029, 'ScanDir', '<path>')
%==========================================================================

function debug_LcsHelper_v4_validateScanOutputs(varargin)
    P = inputParser;
    addParameter(P, 'Year', 2029);
    addParameter(P, 'ScanDir', '');
    parse(P, varargin{:});

    ScanDir = P.Results.ScanDir;
    if isempty(ScanDir)
        ThisDir = fileparts(mfilename('fullpath'));
        ScanDir = fullfile(ThisDir, 'output', 'scans', num2str(P.Results.Year));
    end

    ultrasat.planner.LcsHelper_v4_validateScanOutputs( ...
        'Year', P.Results.Year, 'ScanDir', ScanDir);
end
