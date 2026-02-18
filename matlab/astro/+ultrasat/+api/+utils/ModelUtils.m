% ***************************************************************************
% Project     : ULTRASAT Observation Planner
% Filename    : PathUtils.m
% Author      : Chen Tishler
% Created     : 16/09/2025
% Modified    : 21/09/2025
% Description : Utility functions for path operations
% ***************************************************************************

classdef ModelUtils
    methods (Static)

        function obj = struct2class(s, className)
            obj = feval(className);   % call default constructor
            props = properties(obj);

            for i = 1:numel(props)
                if isfield(s, props{i})
                    obj.(props{i}) = s.(props{i});
                end
            end
        end

        function s = class2struct(obj)
            props = properties(obj);
            s = struct();
            for i = 1:numel(props)
                s.(props{i}) = obj.(props{i});
            end
        end

    end
end

