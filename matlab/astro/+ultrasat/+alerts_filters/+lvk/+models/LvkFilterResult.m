%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvk/+models/LvkFilterResult.m
% Author      : Chen Tishler
% Created     : 12/05/2026
% Updated     : 15/11/2026
% Description : Class to hold the result of an Incoming Alerts Filter.
%==========================================================================

classdef LvkFilterResult
    % Outcome of lvk_filter_*: score, TOO decision, prob snapshot, flags, and reasons.
    properties
        score           double = 0.0;   % Higher => more interesting for TOO (filter-specific)
        is_too          logical = false;  % Final mission decision from filter rules
        decision        string = "reject"   % accept | reject
        priority        string = "none"     % none | low | medium | high
        decision_reasons = {}            % Human-readable accept/reject trace
        class_probs     struct = struct();  % bns/nsbh/bbh/terrestrial from alert hydration
        flags           struct = struct();  % e.g. has_skymap, rejected_far
        reasons                = {};       % Human-readable scoring steps
    end


    methods
        function obj = LvkFilterResult()
            % Default constructor; is_too false until decision block runs.
        end


        function data = toStruct(obj)
            % JSON-safe struct for service output (NaN -> null via jsonencode).
            data = struct();
            data.score = obj.score;
            data.is_too = logical(obj.is_too);
            data.decision = char(obj.decision);
            data.priority = char(obj.priority);
            data.decision_reasons = obj.decision_reasons;
            data.class_probs = sanitizeClassProbs(obj.class_probs);
            data.flags = obj.flags;
            data.reasons = obj.reasons;
        end


        function result = toJsonString(obj)
            % Pretty-printed JSON string for debugging.
            result = jsonencode(obj.toStruct(), 'PrettyPrint', true);
        end
    end

end


function probs = sanitizeClassProbs(class_probs)
    % Replace non-finite class_probs values with [] so jsonencode emits null.

    probs = class_probs;
    if ~isstruct(probs)
        return;
    end
  for name = fieldnames(probs)'
        v = probs.(name{1});
        if isnumeric(v) && (isempty(v) || ~isfinite(v))
            probs.(name{1}) = [];
        end
    end
end