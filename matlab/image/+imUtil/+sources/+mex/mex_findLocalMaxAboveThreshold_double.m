% Fast 2D local maxima finder - find local max above threshold value.
    %       This function was designed to be a faster version of
    %       imregionalmax for some specific cases.
    %       Its gives an order of magnitude speed improvment over
    %       imregionalmax.
    %       Unlike imregionalmax, local maxima are searched only in pixels
    %       which value is larer than some threshold.
    % Input  : - A 2D image. NaN are ignored.
    %          - Threshold above to search local maxima.
    %            Default is 5.
    %          - Local maxima connectivity. Either 4, or 8.
    %            Default is 8.
    %          - Allocate memory parameter. Default is 0.1 of matrix size.
    % Output : - A linear index of the local maxima.
    %          - I (rows) index of the local maxima.
    %          - J (columns) index of the local maximx.
    %          - A logical image indicating the pixels with local maxima.
    % Author : Eran Ofek (Nov 2021)
    % Example: Image=randn(1600,1600);
    %          [Ind,I,J]=imUtil.sources.findLocalMaxAboveThreshold(Image,2,8)
    % Input type: double. Output type: double
    % translated to mex code by Uri I. (March 2022)