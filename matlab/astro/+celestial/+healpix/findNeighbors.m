function NeighPix = findNeighbors(NSide, Pix, Args)
    % Return the 8 (or 9 incl. self) neighbors of HEALPix NESTED pixels.
    %   In cases that there are 7 neighboors one of the neighboors will appear
    %   twice.
    % Input  : - NSide.
    %          - Pix index (nested).
    %          * ...,key,val,...
    %            'IncludeSelf' - Include self pixel in the 9th row.
    %                   Default is false.
    % Output : - An array of [8 X numel(Pix)] or [9 X numel(Pix)]
    %            of all the neighboors.
    % Author : ChatGPT + Eran Ofek (Feb 2026)
    % Example: NeighPix = celestial.healpix.findNeighbors(NSide, Pix)

    arguments
        NSide
        Pix
        Args.IncludeSelf   = false; %(1,1) logical = false
    end

    PixCol = int64(Pix(:)).';   % 1 x N
    N      = numel(PixCol);

    [X, Y, F] = celestial.healpix.nest2xyf(NSide, PixCol);
    X = int64(X); Y = int64(Y); F = int64(F);

    % Order: [SW, W, NW, N, NE, E, SE, S]
    Dx = int64([-1, -1,  0,  1,  1,  1,  0, -1]).';
    Dy = int64([ 0,  1,  1,  1,  0, -1, -1, -1]).';

    Xn = X + Dx;              % 8 x N
    Yn = Y + Dy;              % 8 x N
    Fn = repmat(F, 8, 1);     % 8 x N

    Within = (Xn >= 0) & (Xn < NSide) & (Yn >= 0) & (Yn < NSide);

    NeighPix = -ones(8, N, 'int64');

    % Same-face
    if any(Within(:))
        Idx = find(Within);
        NeighPix(Idx) = int64(celestial.healpix.xyf2nest(NSide, ...
            double(Xn(Idx)), double(Yn(Idx)), double(Fn(Idx))));
    end

    % Boundary handling (same tables/logic as before)
    Boundary = ~Within;
    if any(Boundary(:))
        FaceArray = int64([ ...
             8,  9, 10, 11, -1, -1, -1, -1, 10, 11,  8,  9;  % S
             5,  6,  7,  4,  8,  9, 10, 11,  9, 10, 11,  8;  % SE
            -1, -1, -1, -1,  5,  6,  7,  4, -1, -1, -1, -1;  % E
             4,  5,  6,  7, 11,  8,  9, 10, 11,  8,  9, 10;  % SW
             0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11;  % center
             1,  2,  3,  0,  0,  1,  2,  3,  5,  6,  7,  4;  % NE
            -1, -1, -1, -1,  7,  4,  5,  6, -1, -1, -1, -1;  % W
             3,  0,  1,  2,  3,  0,  1,  2,  4,  5,  6,  7;  % NW
             2,  3,  0,  1, -1, -1, -1, -1,  0,  1,  2,  3   % N
        ]);

        SwapArray = int64([ ...
            0, 0, 3;  % S
            0, 0, 6;  % SE
            0, 0, 0;  % E
            0, 0, 5;  % SW
            0, 0, 0;  % center
            5, 0, 0;  % NE
            0, 0, 0;  % W
            6, 0, 0;  % NW
            3, 0, 0   % N
        ]);

        Xlow  = Xn < 0;
        Xhigh = Xn >= NSide;
        Ylow  = Yn < 0;
        Yhigh = Yn >= NSide;

        Cx = Xn; Cy = Yn;
        Cx(Xlow)  = Cx(Xlow)  + NSide;
        Cx(Xhigh) = Cx(Xhigh) - NSide;
        Cy(Ylow)  = Cy(Ylow)  + NSide;
        Cy(Yhigh) = Cy(Yhigh) - NSide;

        NbNum0 = int64(4) ...
              - int64(Xlow) + int64(Xhigh) ...
              - int64(3).*int64(Ylow) + int64(3).*int64(Yhigh);
        NbNum  = NbNum0 + 1;          % 1..9

        OrigFace = Fn;                % 8 x N
        FaceIdx  = OrigFace + 1;      % 1..12

        LinFA   = sub2ind(size(FaceArray), double(NbNum), double(FaceIdx));
        NewFace = FaceArray(LinFA);

        ValidCross = Boundary & (NewFace >= 0) & (NewFace < 12);

        FaceGrp = floor(OrigFace./4) + 1;  % 1..3
        LinSA   = sub2ind(size(SwapArray), double(NbNum), double(FaceGrp));
        SB      = SwapArray(LinSA);

        FlipX = (bitand(SB, 1) ~= 0) & ValidCross;
        FlipY = (bitand(SB, 2) ~= 0) & ValidCross;
        Swap  = (bitand(SB, 4) ~= 0) & ValidCross;

        Cx(FlipX) = NSide - Cx(FlipX) - 1;
        Cy(FlipY) = NSide - Cy(FlipY) - 1;

        Tx = Cx; Ty = Cy;
        Tx(Swap) = Cy(Swap);
        Ty(Swap) = Cx(Swap);
        Cx = Tx; Cy = Ty;

        Cf = OrigFace;
        Cf(ValidCross) = NewFace(ValidCross);

        if any(ValidCross(:))
            Idx = find(ValidCross);
            NeighPix(Idx) = int64(celestial.healpix.xyf2nest(NSide, ...
                double(Cx(Idx)), double(Cy(Idx)), double(Cf(Idx))));
        end
    end

    % ---- FIXED BLOCK: replace any remaining -1 by duplicating a valid neighbor ----
    Bad = (NeighPix < 0);
    if any(Bad(:))
        % For each column, pick the first valid neighbor (fallback to 0 if somehow none valid)
        FirstValid = NeighPix;
        FirstValid(~Bad) = FirstValid(~Bad); %#ok<NASGU>
        % Find first non-negative per column:
        Rep = zeros(1, N, 'int64');
        for j = 1:N
            Col = NeighPix(:, j);
            k = find(Col >= 0, 1, 'first');
            if isempty(k)
                Rep(j) = PixCol(j);   % extremely defensive fallback
            else
                Rep(j) = Col(k);
            end
        end
        RepMat = repmat(Rep, 8, 1);
        NeighPix(Bad) = RepMat(Bad);
    end

    if Args.IncludeSelf
        NeighPix = [NeighPix; PixCol];
    end
end