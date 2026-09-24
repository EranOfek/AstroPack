% ***************************************************************************
% Project     : AstroPack
% Filename    : PixelRanges.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Pixel-range result container — mirrors Python @dataclass PixelRanges.
% ***************************************************************************
classdef PixelRanges
    % PixelRanges  List of [lo, hi] inclusive pixel-id ranges at NSide = NSIDE_CAT.
    %
    %   Mirrors Python PixelRanges dataclass.

    properties
        Ranges          % Nx2 int64 — [lo, hi] inclusive ranges at NSIDE_CAT
        NSideSearch = 0
        Algo = Algo.CONE
        NSearchPixels = 0   % how many low-NSide pixels were found
    end

    properties (Dependent)
        NRanges
    end

    methods
        function Obj = PixelRanges(Ranges, NSideSearch, AlgoVal, NSearchPixels)
            % Construct a PixelRanges object.
            if nargin >= 1, Obj.Ranges = Ranges; end
            if nargin >= 2, Obj.NSideSearch = NSideSearch; end
            if nargin >= 3, Obj.Algo = AlgoVal; end
            if nargin >= 4, Obj.NSearchPixels = NSearchPixels; end
        end

        function N = get.NRanges(Obj)
            N = size(Obj.Ranges, 1);
        end

        function S = char(Obj)
            % Reproduce Python __repr__ formatting.
            Lines = {sprintf('PixelRanges(algo=%s, nside_search=%d, search_pixels=%d, ranges=%d):', ...
                char(Obj.Algo), Obj.NSideSearch, Obj.NSearchPixels, Obj.NRanges)};
            for I = 1:Obj.NRanges
                Lo = Obj.Ranges(I, 1);
                Hi = Obj.Ranges(I, 2);
                Lines{end+1} = sprintf('  [%14d, %14d]  (width %d)', Lo, Hi, Hi - Lo + 1); %#ok<AGROW>
            end
            S = strjoin(Lines, newline);
        end

        function disp(Obj)
            fprintf('%s\n', char(Obj));
        end
    end
end
