function [Xfull, Yfull] = xy_crop2full(X, Y, CCDSEC)
    % Convert XY coordinates in the cropped image to XY in the full image
    % Input  : - An array of X coordinates in the cropped image.
    %          - An array of Y coordinates in the cropped image.
    %          - A 4 element CCDSEC vector [xmin, xmax, ymin, ymax].
    %            This represents the CCDSEC of the cropped image in respect
    %            to the full image.
    % Output : - X coordinates in the full image.
    %          - Y coordinates in the full image.
    % Author : Eran Ofek (2026 Feb) 
    % Example: [Xfull, Yfull] = imUtil.ccdsec.xy_crop2full(1, 2, [101 200 101 200])

    Xfull = X + CCDSEC(1) - 1;
    Yfull = Y + CCDSEC(3) - 1;


end
