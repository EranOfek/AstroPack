function [Result] = overlapRectangles(Xmin, Xmax, Ymin, Ymax)
    % determine the presence of overlaping neighbors for a set of rectangles 
    %     the set of rectangles is given by vectors of their Xmin, Xmax, Ymin, Ymax
    % Input  : - min X (vector)
    %          - max X (vector)
    %          - min Y (vector)
    %          - max Y (vector)
    % Output : - a struct showing the presence of overlaps from each side
    % Author : A.M. Krassilchtchikov (2026 Feb) 
    % Example: R = tools.math.geometry.overlapRectangles(Xmin, Xmax, Ymin, Ymax);
    %
    arguments
        Xmin
        Xmax
        Ymin
        Ymax
    end
    %
    N = numel(Xmin);
    overlapX = (Xmin < Xmax.') & (Xmax > Xmin.');
    overlapY = (Ymin < Ymax.') & (Ymax > Ymin.');
    overlap = overlapX & overlapY;
    overlap(1:N+1:end) = false;
    fromLeft  = overlap & (Xmax.' > Xmin) & (Xmin.' < Xmin);
    fromRight = overlap & (Xmin.' < Xmax) & (Xmax.' > Xmax);
    fromBottom= overlap & (Ymax.' > Ymin) & (Ymin.' < Ymin);
    fromTop   = overlap & (Ymin.' < Ymax) & (Ymax.' > Ymax);
    Result.hasLeft   = any(fromLeft,   2);
    Result.hasRight  = any(fromRight,  2);
    Result.hasBottom = any(fromBottom, 2);
    Result.hasTop    = any(fromTop,    2);
end
