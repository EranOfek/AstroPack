function [BinImage, BinVar, BinCount] = binimage(Image, BinSizeXY, Back, Var, Args)
    % Bin an image by integer factors using the plain mean, while rejecting
    %       isolated outlier pixels (bad pixels / cosmic rays) without
    %       affecting real (PSF-correlated) stars.
    % Description: Shrink an image by the integer factors given in BinSizeXY,
    %              replacing each bin box by the plain mean of the pixels in
    %              the box that survive outlier rejection. Rejection is done
    %              on the full-resolution image, where a star is spatially
    %              correlated (its bright pixels have bright neighbours)
    %              while a defect is an isolated spike (its neighbours sit at
    %              the background). A positive pixel is rejected only if it
    %              is both significant and isolated; negative pixels (dead/
    %              cold) are rejected unconditionally. Assumes the image is
    %              Nyquist sampled before and after binning.
    % Input  : - An image matrix [M x N].
    %          - Bin size [X(columns), Y(rows)], or a scalar applied to both
    %            dimensions.
    %          - A background map [M x N], a scalar, or [] for a global
    %            median fallback (testing only). Default is [].
    %          - A variance map [M x N] (ADU^2), a scalar, or [] for a global
    %            robust fallback (testing only). Default is [].
    %          * ...,key,val,...
    %            'PosThresh' - Significance [sigma] above which a pixel is a
    %                   positive-outlier candidate. Default is 5.
    %            'IsoFrac' - A positive candidate is rejected as isolated only
    %                   if its brightest 8-neighbour significance is below
    %                   IsoFrac times the candidate significance. In the
    %                   range (0,1). Default is 0.25.
    %            'NegThresh' - Significance [sigma] below -NegThresh at which a
    %                   pixel is rejected as a negative outlier (no isolation
    %                   test). If empty, PosThresh is used. Default is [].
    %            'FullBoxFill' - Value assigned to a bin box in which every
    %                   pixel was rejected. Default is NaN.
    % Output : - The binned image [M/Y x N/X]: the plain mean of the
    %            surviving pixels in each box.
    %          - The propagated variance of that mean per box
    %            (sum of contributing variances / n_good^2).
    %          - The number of surviving pixels contributing to each box.
    % Author : Eran Ofek (Jun 2026)
    % Example: Image          = 100 + randn(256).*5;
    %          Image(128,128) = 5000;        % an isolated hot pixel
    %          [BinImage, BinVar, BinCount] = binimage(Image, [2 2], 100, 25);

    arguments
        Image
        BinSizeXY
        Back                 = [];
        Var                  = [];
        Args.PosThresh       = 5;
        Args.IsoFrac         = 0.25;
        Args.NegThresh       = [];
        Args.FullBoxFill     = NaN;
    end

    Image  = double(Image);
    [M, N] = size(Image);

    k    = Args.PosThresh;
    f    = Args.IsoFrac;
    negk = Args.NegThresh;
    if isempty(negk)
        negk = k;
    end
    fill = Args.FullBoxFill;

    % bin factors:  BinSizeXY = [X(columns), Y(rows)]
    if isscalar(BinSizeXY)
        bx = BinSizeXY;
        by = BinSizeXY;
    else
        bx = BinSizeXY(1);   % columns (X)
        by = BinSizeXY(2);   % rows    (Y)
    end

    % Back / Var: global fallbacks if empty (testing only), then expand scalars
    if isempty(Back)
        Back = median(Image(:), 'omitnan');
        warning('binimage:backFallback','No Back supplied; using global median.');
    end
    if isempty(Var)
        Var = (1.4826.*mad(Image(:),1)).^2;   % robust global sigma^2
        warning('binimage:varFallback','No Var supplied; using global robust variance.');
    end
    if isscalar(Back)
        Back = Back + zeros(M, N);
    end
    if isscalar(Var)
        Var = Var + zeros(M, N);
    end

    % --- trim (bottom/right) to an integer multiple of the bin size ---
    Mt = floor(M./by).*by;
    Nt = floor(N./bx).*bx;
    if Mt~=M || Nt~=N
        warning('binimage:trim','Trimming image from [%d %d] to [%d %d] to fit bins.', M, N, Mt, Nt);
        Image = Image(1:Mt, 1:Nt);
        Back  = Back(1:Mt, 1:Nt);
        Var   = Var(1:Mt, 1:Nt);
        M     = Mt;
        N     = Nt;
    end

    % --- significance map e (in sigma units) ---
    Sd = sqrt(max(Var,0));            % avoid complex; Sd==0 -> Inf in e -> bad
    e  = (Image - Back)./Sd;

    % --- brightest 8-neighbour significance (3x3 max filter, no toolbox) ---
    % Pad with -Inf so off-edge neighbours never count as bright.
    ePad           = -inf(M+2, N+2);
    ePad(2:M+1, 2:N+1) = e;
    NeighMax       = -inf(M, N);
    for Di = -1:1
        for Dj = -1:1
            if Di==0 && Dj==0
                continue;
            end
            NeighMax = max(NeighMax, ePad(2+Di:M+1+Di, 2+Dj:N+1+Dj));
        end
    end

    % --- rejection ---
    IsPosCand  = e > k;                  % significant enough to be a candidate
    IsIsolated = NeighMax < f.*e;        % no bright neighbour -> a spike
    RejPos     = IsPosCand & IsIsolated; % reject: positive isolated spike
    RejNeg     = e < -negk;              % reject: dead/cold pixel
    NotFinite  = ~isfinite(e) | ~isfinite(Image);

    Bad  = RejPos | RejNeg | NotFinite;
    Good = ~Bad;

    % --- masked block mean ---
    % Zero out bad pixels so they contribute nothing to the block sums.
    Ig = Image;  Ig(Bad) = 0;
    Vg = Var;    Vg(Bad) = 0;
    G  = double(Good);

    SumImg = blocksum(Ig, by, bx);
    SumVar = blocksum(Vg, by, bx);
    Count  = blocksum(G,  by, bx);

    BinCount = Count;
    BinImage = SumImg./Count;            % plain mean of the survivors
    BinVar   = SumVar./(Count.^2);       % variance of that mean

    % boxes with no surviving pixel
    Empty           = Count==0;
    BinImage(Empty) = fill;
    BinVar(Empty)   = fill;

end


function S = blocksum(A, by, bx)
    % Sum A over non-overlapping by-by-bx blocks (size(A) an exact multiple).
    % Input  : - A matrix whose size is an integer multiple of [by bx].
    %          - Block size along the rows (by).
    %          - Block size along the columns (bx).
    % Output : - The block-summed matrix [M/by x N/bx].
    % Author : Eran Ofek (Jun 2026)
    [M, N] = size(A);
    A = reshape(A, by, M./by, bx, N./bx);   % column-major: group rows then cols
    S = sum(A, 1);
    S = sum(S, 3);
    S = reshape(S, M./by, N./bx);
end