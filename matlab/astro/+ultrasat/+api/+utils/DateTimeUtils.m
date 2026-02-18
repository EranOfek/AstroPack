% ***************************************************************************
% Project     : ULTRASAT Observation Planner
% Filename    : PathUtils.m
% Author      : Chen Tishler
% Created     : 16/09/2025
% Modified    : 21/09/2025
% Description : Utility functions for path operations
% ***************************************************************************

classdef DateTimeUtils
    methods (Static)

        function dt = toUtc(dt)
            if isdatetime(dt)
                if isempty(dt.TimeZone) || ~strcmp(dt.TimeZone, 'UTC')
                    dt.TimeZone = 'UTC';
                end
            elseif ischar(dt) || isstring(dt)
                dt = datetime(dt, ...
                    'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''', ...
                    'TimeZone', 'UTC');
            else
                error('Input must be a datetime object or a date-time string.');
            end
        end

    end
end


