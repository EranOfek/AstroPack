% ***************************************************************************
% Project     : ULTRASAT Observation Planner
% Filename    : PathUtils.m
% Author      : Chen Tishler
% Created     : 16/09/2025
% Modified    : 21/09/2025
% Description : Utility functions for path operations
% ***************************************************************************

classdef JsonUtils
    methods (Static)

        function s = json2struct(js)
            s = jsondecode(js);
            s = ultrasat.api.utils.DateTimeUtils.convertStringToDatetime(s);
        end

        function js = struct2json(s)
            s = ultrasat.api.utils.DateTimeUtils.convertDatetimeToString(s);
            js = jsonencode(s);
        end

    end
end


