%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvc/+models/LvcFilterResult.m
% Author      : Chen Tishler
% Created     : 12/05/2026
% Updated     : 12/05/2026
% Description : Class to hold the result of an Incoming Alerts Filter.
%==========================================================================

classdef LvcFilterResult
    properties
        score           double = 0.0;
        class_probs     struct = struct();
        flags           struct = struct();
        reasons                = {};
    end


    methods
        function obj = LvcFilterResult()
            % Constructor
        end


        function data = toStruct(obj)
            % Convert the result into a JSON-safe struct for service output.
            data = struct();
            data.score = obj.score;
            data.class_probs = obj.class_probs;
            data.flags = obj.flags;
            data.reasons = obj.reasons;
        end


        function result = toJsonString(obj)
            % Convert the result into a pretty-printed JSON string.
            result = jsonencode(obj.toStruct(), 'PrettyPrint', true);
        end
    end

end
