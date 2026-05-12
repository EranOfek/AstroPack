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
        reasons         string = '';
    end


    methods
        function obj = LvcFilterResult()
            % Constructor
        end
    end

end
