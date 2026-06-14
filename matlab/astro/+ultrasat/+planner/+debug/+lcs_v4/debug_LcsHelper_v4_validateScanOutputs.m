%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.debug.lcs_v4.debug_LcsHelper_v4_validateScanOutputs.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 10/06/2026
% Description : Thin wrapper — scan-output validator moved to
%               ultrasat.planner.LcsHelper_v4_validateScanOutputs.
%
% Run by:
%   ultrasat.planner.debug.lcs_v4.debug_LcsHelper_v4_validateScanOutputs()
%   ultrasat.planner.debug.lcs_v4.debug_LcsHelper_v4_validateScanOutputs('Year', 2029, 'ScanDir', '<path>')
%==========================================================================

function debug_LcsHelper_v4_validateScanOutputs(varargin)
    ultrasat.planner.LcsHelper_v4_validateScanOutputs(varargin{:});
end
