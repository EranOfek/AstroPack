function CCDSEC = trim_ccdsec(CCDSEC, TrimXY)
    % trim edges from a CCDSEC vector
    % Input  : - Either a CCDSEC [Xmin Xmax Ymin Ymax]
    %            or size(Image) sizeIJ.
    %          - Number of pixels to trim in [X, Y].
    % Output : - An updated CCDSEC.
    % Author : Eran Ofek (Jul 2021)
    % Example: CCDSEC = imUtil.ccdsec.trim_ccdsec([100 100],[2 3])
    
    if numel(CCDSEC)==2
        % assume CCDSEC is sizeIJ
        CCDSEC = [1 CCDSEC(2) 1 CCDSEC(1)];
    end
    
    if numel(TrimXY)==1
        TrimXY = [TrimXY, TrimXY];
    end
    
    CCDSEC([1 3]) = CCDSEC([1 3]) + TrimXY - 1;
    CCDSEC([2 4]) = CCDSEC([2 4]) - TrimXY;
    
end
