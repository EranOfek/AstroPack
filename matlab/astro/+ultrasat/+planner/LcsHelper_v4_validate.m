%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.LcsHelper_v4_validate.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 21/06/2026
% Description : Validate LcsHelper_v4 schedule output against formal LCS
%               rules (Sets A/B/C/D, 45-day windows, slot budget, etc.).
%               Thin facade over LcsHelper_v4Validator; returns a single
%               LcsValidationResult. Check logic lives in
%               LcsHelper_v4_runValidationChecks.m.
%
% Usage:
%   R = ultrasat.planner.LcsHelper_v4_validate()           % standalone
%   R = ultrasat.planner.LcsHelper_v4_validate(Obj)
%   R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false)
%   R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Capture', true)
%
% Plan start date (standalone mode): January 5, 2029.
%==========================================================================

function Result = LcsHelper_v4_validate(Obj, Args)
    % Run all LCS v4 validation checks and return a LcsValidationResult.
    arguments
        Obj = []
        Args.Verbose logical = true
        Args.DumpCsv logical = false
        Args.Capture logical = false
        Args.PrintToConsole logical = true
    end

    % Create a validator object and run it
    V = ultrasat.planner.LcsHelper_v4Validator(Obj, Args);
    V.run();
    Result = V.Result;
end
