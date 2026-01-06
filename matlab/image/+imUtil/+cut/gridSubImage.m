function [CCDSEC, NSub, NoOverlapCCDSEC, NewNoOverlapCCDSEC, CentersXY] = gridSubImage(ImageSize, SubSize)
    % Compute CCDSEC tiles (with overlap) + non-overlap cores in full-image coordinates and in each subimage's local frame.
    %   Given an image size and a subimage size, generate a list of CCDSEC that
    %   covers the full image and have the subimage size.
    %   See also: (obsolete) imUtil.cut.subimage_grid
    % Input  : - ImageSize: [SizeImageX, SizeImageY]
    %          - SubSize: [SizeSubX,   SizeSubY]
    % Output : - CCDSEC: (NSubX*NSubY) x 4 : [Xmin Xmax Ymin Ymax] of the
    %            selected sub images as measured in the full-image frame.
    %          - NSub: Number of X /Y sub images [NSubX, NSubY]
    %          - NoOverlapCCDSEC: The CCDSEC [Xmin Xmax Ymin Ymax] of the
    %            non-overlapping cores of the subimages as measured in the full
    %            image frame.
    %          - NewNoOverlapCCDSEC:  The CCDSEC [Xmin Xmax Ymin Ymax] of the
    %            non-overlapping cores of the subimages as measured in the sub image
    %            frame.
    %          - Centers [X Y] of the CCDSEC in the full image frame.
    % Notes: All indices are 1-based and inclusive.
    %        CCDSEC sizes are exactly SubSize.
    %        Cores are split symmetrically (up to +/-1 pixel) between adjacent tiles.
    % Author : ChatGPT + Eran Ofek (Jan 2026)
    % Example: [CCDSEC, NSub, NoOverlapCCDSEC, NewNoOverlapCCDSEC, CentersXY] = imUtil.cut.gridSubImage([1000 1000], [300 301])
    
    arguments
        ImageSize (1,2) double {mustBeFinite, mustBePositive}
        SubSize   (1,2) double {mustBeFinite, mustBePositive}
    end
    
    SizeImageX = int64(round(ImageSize(1)));
    SizeImageY = int64(round(ImageSize(2)));
    SizeSubX   = int64(round(SubSize(1)));
    SizeSubY   = int64(round(SubSize(2)));
    
    if SizeSubX > SizeImageX || SizeSubY > SizeImageY
        error('SubSize must be <= ImageSize in both dimensions.');
    end
    
    % Number of tiles in each axis (minimum to cover)
    NSubX = double(ceil(double(SizeImageX) / double(SizeSubX)));
    NSubY = double(ceil(double(SizeImageY) / double(SizeSubY)));
    NSub  = [NSubX, NSubY];
    
    % Start positions (integer, uniformly distributed, strictly increasing)
    XStart = localStarts(SizeImageX, SizeSubX, NSubX);
    YStart = localStarts(SizeImageY, SizeSubY, NSubY);
    
    % Build CCDSEC list (row-major: X changes fastest)
    NBox  = NSubX * NSubY;
    CCDSEC = zeros(NBox, 4, 'int64');
    
    K = 0;
    for Iy = 1:NSubY
        Ymin = YStart(Iy);
        Ymax = Ymin + SizeSubY - 1;
        for Ix = 1:NSubX
            Xmin = XStart(Ix);
            Xmax = Xmin + SizeSubX - 1;
    
            K = K + 1;
            CCDSEC(K,:) = [Xmin, Xmax, Ymin, Ymax];
        end
    end
    
    % 1D non-overlap "ownership" intervals along each axis (full-image frame)
    [XCoreMin, XCoreMax] = localNoOverlapCores(XStart, SizeSubX, SizeImageX);
    [YCoreMin, YCoreMax] = localNoOverlapCores(YStart, SizeSubY, SizeImageY);
    
    % Build NoOverlapCCDSEC + NewNoOverlapCCDSEC (same ordering as CCDSEC)
    NoOverlapCCDSEC     = zeros(NBox, 4, 'int64');
    NewNoOverlapCCDSEC  = zeros(NBox, 4, 'int64');
    
    K = 0;
    for Iy = 1:NSubY
        for Ix = 1:NSubX
            K = K + 1;
    
            % Full-image frame core
            XminF = XCoreMin(Ix);  XmaxF = XCoreMax(Ix);
            YminF = YCoreMin(Iy);  YmaxF = YCoreMax(Iy);
            NoOverlapCCDSEC(K,:) = [XminF, XmaxF, YminF, YmaxF];
    
            % Subimage-local frame core:
            % localX = fullX - CCDSEC_Xmin + 1
            XminTile = CCDSEC(K,1);
            YminTile = CCDSEC(K,3);
    
            XminL = XminF - XminTile + 1;
            XmaxL = XmaxF - XminTile + 1;
            YminL = YminF - YminTile + 1;
            YmaxL = YmaxF - YminTile + 1;
    
            NewNoOverlapCCDSEC(K,:) = [XminL, XmaxL, YminL, YmaxL];
        end
    end
    
    % Return as doubles (MATLAB convention)
    CCDSEC             = double(CCDSEC);
    NoOverlapCCDSEC    = double(NoOverlapCCDSEC);
    NewNoOverlapCCDSEC = double(NewNoOverlapCCDSEC);
    
    CentersXY           = [CCDSEC(:,1) + CCDSEC(:,2), CCDSEC(:,3) + CCDSEC(:,4)]./2;
end
    
    
function Start = localStarts(SizeImage, SizeSub, NSub)
    % Integer start positions, uniform and monotone, with fixed endpoints.
    
    SizeImage = int64(SizeImage);
    SizeSub   = int64(SizeSub);
    
    if NSub == 1
        Start = int64(1);
        return
    end
    
    First = int64(1);
    Last  = SizeImage - SizeSub + 1;
    
    Start = int64(round(linspace(double(First), double(Last), NSub)));
    Start(1)   = First;
    Start(end) = Last;
    
    for I = 2:NSub
        MinAllowed = Start(I-1) + 1;
        if Start(I) < MinAllowed
            Start(I) = MinAllowed;
        end
    end
    
    for I = (NSub-1):-1:1
        MaxAllowed = Start(I+1) - 1;
        if Start(I) > MaxAllowed
            Start(I) = MaxAllowed;
        end
    end
    
    Start(1)   = First;
    Start(end) = Last;
    
end
    
    
function [CoreMin, CoreMax] = localNoOverlapCores(Start, SizeSub, SizeImage)
    % Split overlaps symmetrically by midpoints to define non-overlapping cores.
    
    Start     = int64(Start);
    SizeSub   = int64(SizeSub);
    SizeImage = int64(SizeImage);
    
    N = numel(Start);
    EndPos = Start + SizeSub - 1;
    
    CoreMin = zeros(1, N, 'int64');
    CoreMax = zeros(1, N, 'int64');
    
    for I = 1:N
        if I == 1
            CoreMin(I) = int64(1);
        else
            Boundary = idivide(EndPos(I-1) + Start(I), int64(2), 'floor');
            CoreMin(I) = Boundary + 1;
        end
    
        if I == N
            CoreMax(I) = SizeImage;
        else
            Boundary = idivide(EndPos(I) + Start(I+1), int64(2), 'floor');
            CoreMax(I) = Boundary;
        end
    
        % Clamp to this tile's extent
        CoreMin(I) = max(CoreMin(I), Start(I));
        CoreMax(I) = min(CoreMax(I), EndPos(I));
    
        if CoreMax(I) < CoreMin(I)
            CoreMin(I) = min(max(Start(I), int64(1)), SizeImage);
            CoreMax(I) = CoreMin(I);
        end
    end
    


end
