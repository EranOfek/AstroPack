% ***************************************************************************
% Project     : AstroPack
% Filename    : Algo.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Search algorithm enum — mirrors Python Algo(str, Enum).
% ***************************************************************************
classdef Algo
    % Algo  Cone-search algorithm selection (mirrors Python Algo enum).
    %
    %   NEIGHBOR — central pixel + 8 neighbours at NSideSearch, then expand
    %              to NSideCat.  Always returns <= 9 ranges.  Fast, conservative.
    %   CONE     — query_disc / cone_search at NSideSearch, then expand to
    %              NSideCat.  Returns fewer, tighter ranges.

    enumeration
        NEIGHBOR   % central + 8 neighbours, always <= 9 ranges
        CONE       % query_disc, fewer ranges, more accurate
    end

    methods
        function S = char(Obj)
            % Return the Python-compatible string value.
            switch Obj
                case Algo.NEIGHBOR
                    S = 'neighbor';
                case Algo.CONE
                    S = 'cone';
            end
        end
    end
end
