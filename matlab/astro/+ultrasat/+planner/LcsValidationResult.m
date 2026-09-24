%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.LcsValidationResult.m
% Author      : Chen Tishler
% Created     : 21/06/2026
% Description : Immutable outcome bundle from LcsHelper_v4 validation.
%==========================================================================

classdef LcsValidationResult

    properties (SetAccess = private)
        nFail      double
        nPass      double
        nWarn      double
        Report     char
        FailReport char
        WarnReport char
        Status     char   % 'failed' | 'passed_with_warnings' | 'passed'
    end

    methods
        function obj = LcsValidationResult(nFail, nPass, nWarn, Report, FailReport, WarnReport, Status)
            obj.nFail      = nFail;
            obj.nPass      = nPass;
            obj.nWarn      = nWarn;
            obj.Report     = Report;
            obj.FailReport = FailReport;
            obj.WarnReport = WarnReport;
            obj.Status     = Status;
        end

        function tf = passed(obj)
            tf = obj.nFail == 0;
        end

        function tf = failed(obj)
            tf = obj.nFail > 0;
        end

        function tf = hasWarnings(obj)
            tf = obj.nWarn > 0;
        end
    end

    methods (Static)
        function obj = fromCounts(nFail, nPass, nWarn, Report, FailReport, WarnReport)
            % Build a result object from check counts and captured report text.
            if nargin < 4 || isempty(Report)
                Report = '';
            end
            if nargin < 5 || isempty(FailReport)
                FailReport = '';
            end
            if nargin < 6 || isempty(WarnReport)
                WarnReport = '';
            end

            if nFail > 0
                Status = 'failed';
            elseif nWarn > 0
                Status = 'passed_with_warnings';
            else
                Status = 'passed';
            end

            obj = ultrasat.planner.LcsValidationResult( ...
                nFail, nPass, nWarn, Report, FailReport, WarnReport, Status);
        end

        function Parts = splitReport(Report)
            % Split captured validation log into fail and warning line groups.
            %
            % [FAIL] lines -> FailReport; [WARN] but not [WARN-OK] -> WarnReport.
            Parts.FailReport = '';
            Parts.WarnReport = '';
            if isempty(Report)
                return
            end

            Lines = splitlines(string(Report));
            Trimmed = strtrim(Lines);
            FailLines = Lines(startsWith(Trimmed, '[FAIL]'));
            WarnLines = Lines(startsWith(Trimmed, '[WARN]') & ~startsWith(Trimmed, '[WARN-OK]'));

            if ~isempty(FailLines)
                Parts.FailReport = char(strjoin(FailLines, newline));
            end
            if ~isempty(WarnLines)
                Parts.WarnReport = char(strjoin(WarnLines, newline));
            end
        end
    end
end
