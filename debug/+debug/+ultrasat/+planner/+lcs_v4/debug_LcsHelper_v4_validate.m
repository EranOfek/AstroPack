%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v4/debug_LcsHelper_v4_validate.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 11/06/2026
% Description : Thin wrapper — validator moved to ultrasat.planner.LcsHelper_v4_validate.
%               Runs the standalone validation (builds Jan 5, 2029 plan
%               internally and runs all 10 rule checks).
%
% Run by      : debug.ultrasat.planner.lcs_v4.debug_LcsHelper_v4_validate()
%==========================================================================

function debug_LcsHelper_v4_validate()
    ultrasat.planner.LcsHelper_v4_validate();
end
