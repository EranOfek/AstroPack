%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.LcsHelper_v4Validator.m
% Author      : Chen Tishler
% Created     : 21/06/2026
% Description : Orchestrates LcsHelper_v4 validation and captures results.
%==========================================================================

classdef LcsHelper_v4Validator < handle

    properties
        SourceObj  % LcsHelper_v4 or []
        Args       % validation options struct
        Result     % LcsValidationResult (empty until run)
    end

    methods
        function obj = LcsHelper_v4Validator(Obj, varargin)
            % Create a validator for an LcsHelper_v4 object (or standalone build).
            %
            % Accepts either name-value options or a single options struct.
            if nargin < 1
                Obj = [];
            end

            if numel(varargin) == 1 && isstruct(varargin{1})
                Args = varargin{1};
            else
                P = inputParser;
                addParameter(P, 'Verbose', true);
                addParameter(P, 'DumpCsv', false);
                addParameter(P, 'Capture', false);
                addParameter(P, 'PrintToConsole', true);
                parse(P, varargin{:});
                Args = P.Results;
            end

            Defaults = struct( ...
                'Verbose', true, ...
                'DumpCsv', false, ...
                'Capture', false, ...
                'PrintToConsole', true);
            for k = 1:numel(fieldnames(Defaults))
                Name = fieldnames(Defaults);
                Name = Name{k};
                if ~isfield(Args, Name)
                    Args.(Name) = Defaults.(Name);
                end
            end

            obj.SourceObj = Obj;
            obj.Args = Args;
            obj.Result = ultrasat.planner.LcsValidationResult.empty();
        end

        function run(obj)
            % Run validation checks and store the outcome in obj.Result.
            Args = obj.Args;
            nWarn = 0;
            Report = '';
            FailReport = '';
            WarnReport = '';

            if Args.Capture
                Report = evalc( ...
                    '[nFail, nPass, nWarn] = ultrasat.planner.LcsHelper_v4_runValidationChecks(obj.SourceObj, obj.Args);');
                if Args.PrintToConsole
                    fprintf('%s', Report);
                end
                Parts = ultrasat.planner.LcsValidationResult.splitReport(Report);
                FailReport = Parts.FailReport;
                WarnReport = Parts.WarnReport;
            else
                [nFail, nPass, nWarn] = ultrasat.planner.LcsHelper_v4_runValidationChecks( ...
                    obj.SourceObj, obj.Args);
            end

            obj.Result = ultrasat.planner.LcsValidationResult.fromCounts( ...
                nFail, nPass, nWarn, Report, FailReport, WarnReport);
        end
    end
end
