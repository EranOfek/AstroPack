function Result = matchPatternPairs(RefCat, ImageCat, Args)
    % Match two catalogs related by unknown shift+rotation(+flip) using pair voting.
    %   Solves the star-pattern matching problem for a known-scale
    %   transformation of the form:  Ref = R(Theta)*P*Image + Shift,
    %   where P is an optional flip (parity) matrix.
    %   The algorithm matches pairs of sources between the catalogs using
    %   the rotation-invariant pair separation (and optional magnitude
    %   difference), and each matched pair votes for a rotation angle in a
    %   histogram. Position-angle differences vote for the no-flip
    %   solution, while position-angle sums vote for the flipped solution.
    %   The translation is then found from the peak of a 2D histogram of
    %   coordinate differences, and the final solution is refined by
    %   nearest-neighbor matching and sigma-clipped least squares.
    %   The method is robust to sources that appear in only one catalog.
    % Input  : - RefCat: A 2 or 3 column matrix of [X, Y, Mag] of the
    %            reference catalog. Mag column is optional.
    %          - ImageCat: A 2 or 3 column matrix of [X, Y, Mag] of the
    %            catalog to be matched. Mag column is optional.
    %          * ...,key,val,... The following keywords are available:
    %            'MaxStars'    - Max. number of brightest stars (per catalog)
    %                            to use in the voting stage. If no magnitude
    %                            is available, the first MaxStars rows are
    %                            used. Default is 100.
    %            'UseMag'      - Use magnitude information (selection,
    %                            pair orientation, dMag pruning), if
    %                            available in both catalogs. Default is true.
    %            'MagTol'      - Tolerance [mag] on pair magnitude-difference
    %                            for candidate pair matching. Default is 1.
    %            'MinSep'      - Min. pair separation to use [pix]. Default is 10.
    %            'MaxSep'      - Max. pair separation to use [pix]. Default is Inf.
    %            'SepTol'      - Tolerance [pix] on pair separation matching.
    %                            Default is 2.
    %            'ThetaRange'  - [Min Max] allowed rotation angle [deg],
    %                            in the range [-180 180]. Default is [-180 180].
    %            'ThetaBin'    - Rotation histogram bin size [deg]. Default is 0.5.
    %            'TreatFlips'  - Also search for a flipped (negative parity)
    %                            solution. Default is false.
    %            'Flip'        - Flip matrix to use when a flipped solution is
    %                            searched/applied. Default is [-1 0; 0 1].
    %            'MaxShift'    - Max. absolute shift [pix] to search in each
    %                            axis. Default is Inf (search full range).
    %            'ShiftBin'    - Shift histogram bin size [pix]. Default is 3.
    %            'MinSN'       - Min. S/N of both the rotation and shift
    %                            histogram peaks for a solution to be declared
    %                            found. Default is 5.
    %            'SearchRadius'- Matching radius [pix] for the final
    %                            nearest-neighbor matching/refinement.
    %                            Default is 3.
    %            'Niter'       - Number of match/fit/clip refinement
    %                            iterations. Default is 3.
    %            'SigmaClip'   - Sigma clipping level for the refinement fit.
    %                            Default is 3.
    % Output : - Result: A structure with the following fields:
    %            .Found     - Logical flag indicating if a solution was found.
    %            .Flip      - Logical. True if the solution includes a flip.
    %            .Theta     - Rotation angle [deg].
    %            .ShiftX    - X shift [pix] (applied after rotation/flip).
    %            .ShiftY    - Y shift [pix].
    %            .Scale     - Fitted scale (sanity check; should be ~1).
    %            .Tran      - [2x3] transformation, Ref = Tran(:,1:2)*Img + Tran(:,3).
    %            .SN_Theta  - S/N of the rotation histogram peak.
    %            .SN_Shift  - S/N of the shift histogram peak.
    %            .Nmatch    - Number of matched stars in the final solution.
    %            .RMS       - RMS [pix] of the matched stars residuals.
    %            .MatchedRefInd - Indices in RefCat of matched stars.
    %            .MatchedImgInd - Corresponding indices in ImageCat.
    %            .ResidMag  - Std of magnitude differences of matched stars
    %                         (NaN if no magnitudes). Useful as a sanity check.
    %            .Candidates- Table of all tested (Flip,Theta) candidates
    %                         and their S/N.
    %            .ThetaVec  - Rotation histogram bin centers [deg].
    %            .ThetaHistNoFlip - Rotation votes histogram (no flip).
    %            .ThetaHistFlip   - Rotation votes histogram (flip), or [].
    % Author : Claude (Jul 2026)
    % Example: Ref = [rand(200,2).*2048, 15+3.*rand(200,1)];
    %          T = 33; R = [cosd(T) -sind(T); sind(T) cosd(T)];
    %          Img = [(R*Ref(1:150,1:2)')' + [120 -60], Ref(1:150,3)];
    %          Res = matchPatternPairs(Ref, Img, 'TreatFlips',true);

    arguments
        RefCat   (:,:) double
        ImageCat (:,:) double
        Args.MaxStars     (1,1) double = 100
        Args.UseMag       (1,1) logical = true
        Args.MagTol       (1,1) double = 1
        Args.MinSep       (1,1) double = 10
        Args.MaxSep       (1,1) double = Inf
        Args.SepTol       (1,1) double = 2
        Args.ThetaRange   (1,2) double = [-180 180]
        Args.ThetaBin     (1,1) double = 0.5
        Args.TreatFlips   (1,1) logical = false
        Args.Flip         (2,2) double = [-1 0; 0 1]
        Args.MaxShift     (1,1) double = Inf
        Args.ShiftBin     (1,1) double = 3
        Args.MinSN        (1,1) double = 5
        Args.SearchRadius (1,1) double = 3
        Args.Niter        (1,1) double = 3
        Args.SigmaClip    (1,1) double = 3
    end

    % --- init output ---
    Result = struct('Found',false, 'Flip',false, 'Theta',NaN, ...
                    'ShiftX',NaN, 'ShiftY',NaN, 'Scale',NaN, 'Tran',nan(2,3), ...
                    'SN_Theta',NaN, 'SN_Shift',NaN, 'Nmatch',0, 'RMS',NaN, ...
                    'MatchedRefInd',[], 'MatchedImgInd',[], 'ResidMag',NaN, ...
                    'Candidates',[], 'ThetaVec',[], ...
                    'ThetaHistNoFlip',[], 'ThetaHistFlip',[]);

    HasMag = size(RefCat,2)>2 && size(ImageCat,2)>2;
    UseMag = Args.UseMag && HasMag;

    % --- select brightest MaxStars ---
    SubRef = selectStars(RefCat,   Args.MaxStars, UseMag);
    SubImg = selectStars(ImageCat, Args.MaxStars, UseMag);

    % --- build pair tables ---
    PR = buildPairs(SubRef, UseMag, Args.MinSep, Args.MaxSep);
    PI = buildPairs(SubImg, UseMag, Args.MinSep, Args.MaxSep);
    if isempty(PR.D) || isempty(PI.D)
        return;
    end

    % --- match pairs on invariants (separation, dMag) and collect PA votes ---
    [PAr, PAi] = matchPairInvariants(PR, PI, Args.SepTol, UseMag, Args.MagTol);
    if isempty(PAr)
        return;
    end

    % votes for rotation: no-flip uses PA difference; flip uses PA sum
    % Model: Ref = R(Theta)*P*Img + Shift, with P=Args.Flip for flipped case
    FlipPA    = atan2d(Args.Flip(2,1), Args.Flip(1,1));  % PA of P*[1;0]
    ThNoFlip  = wrap180(PAr - PAi);
    ThFlip    = wrap180(PAr + PAi - 2.*FlipPA - 180.*(Args.Flip(1,1)<0)); % generic
    ThFlip    = wrap180(PAr + PAi - 180);   % for default P=diag([-1 1])
    if ~UseMag
        % no magnitude: pair orientation is arbitrary -> also vote +180
        ThNoFlip = [ThNoFlip; wrap180(ThNoFlip+180)];
        ThFlip   = [ThFlip;   wrap180(ThFlip+180)];
    end

    % --- rotation histograms and peak candidates ---
    Edges = Args.ThetaRange(1) : Args.ThetaBin : Args.ThetaRange(2);
    Result.ThetaVec = 0.5.*(Edges(1:end-1)+Edges(2:end));

    [Cand(1).Theta, Cand(1).SN, Result.ThetaHistNoFlip] = histPeak1(ThNoFlip, Edges);
    Cand(1).Flip = false;
    Ncand = 1;
    if ~UseMag && ~isnan(Cand(1).Theta)
        Ncand = Ncand+1;      % 180-deg ambiguous counterpart
        Cand(Ncand) = struct('Theta',wrap180(Cand(1).Theta+180), 'SN',Cand(1).SN, 'Flip',false);
    end
    if Args.TreatFlips
        Ncand = Ncand+1;
        [Cand(Ncand).Theta, Cand(Ncand).SN, Result.ThetaHistFlip] = histPeak1(ThFlip, Edges);
        Cand(Ncand).Flip = true;
        if ~UseMag && ~isnan(Cand(Ncand).Theta)
            Ncand = Ncand+1;
            Cand(Ncand) = struct('Theta',wrap180(Cand(Ncand-1).Theta+180), 'SN',Cand(Ncand-1).SN, 'Flip',true);
        end
    end

    % keep candidates inside allowed range and with sufficient S/N
    Cand = Cand(~isnan([Cand.Theta]));
    Cand = Cand([Cand.Theta]>=Args.ThetaRange(1) & [Cand.Theta]<=Args.ThetaRange(2));
    Cand = Cand([Cand.SN]>=Args.MinSN);
    if isempty(Cand)
        return;
    end

    % --- translation search for each candidate ---
    Best = struct('SN',-Inf);
    for Ic=1:numel(Cand)
        P = eye(2);
        if Cand(Ic).Flip
            P = Args.Flip;
        end
        M   = rotMat(Cand(Ic).Theta)*P;
        XYt = (M*SubImg(:,1:2).').';
        DX  = SubRef(:,1) - XYt(:,1).';        % Nref x Nimg
        DY  = SubRef(:,2) - XYt(:,2).';
        Ok  = abs(DX)<=Args.MaxShift & abs(DY)<=Args.MaxShift;
        [Sx, Sy, SN2] = histPeak2(DX(Ok), DY(Ok), Args.ShiftBin);
        Cand(Ic).SN_Shift = SN2;
        Cand(Ic).ShiftX = Sx;
        Cand(Ic).ShiftY = Sy;
        if SN2 > Best.SN
            Best = struct('SN',SN2, 'Ind',Ic, 'M',M, 'P',P, 'Sx',Sx, 'Sy',Sy);
        end
    end
    Result.Candidates = struct2table(Cand, 'AsArray',true);

    if Best.SN < Args.MinSN || isnan(Best.Sx)
        return;
    end

    % --- refinement: NN matching + sigma-clipped LSQ (Ref = [c -s;s c]*P*Img + T) ---
    XYref = RefCat(:,1:2);
    XYflp = (Best.P*ImageCat(:,1:2).').';   % flip applied once; fit rotation+shift
    C = cosd(Cand(Best.Ind).Theta);  S = sind(Cand(Best.Ind).Theta);
    Par = [C; S; Best.Sx; Best.Sy];         % [c s tx ty]

    IndRef = []; IndImg = []; Resid = [];
    for Iter=1:Args.Niter
        XYt = [Par(1).*XYflp(:,1)-Par(2).*XYflp(:,2)+Par(3), ...
               Par(2).*XYflp(:,1)+Par(1).*XYflp(:,2)+Par(4)];
        [IndNN, Dist] = knnsearch(XYref, XYt);
        Sel = find(Dist <= Args.SearchRadius);
        % enforce one-to-one: keep nearest image star per reference star
        [~,Is]  = sort(Dist(Sel));
        Sel     = Sel(Is);
        [~,Iu]  = unique(IndNN(Sel), 'first');
        IndImg  = Sel(Iu);
        IndRef  = IndNN(IndImg);
        if numel(IndImg) < 3
            break;
        end
        % LSQ fit: [Xr;Yr] = [xf -yf 1 0; yf xf 0 1]*[c;s;tx;ty]
        Xf = XYflp(IndImg,1); Yf = XYflp(IndImg,2);
        Nm = numel(IndImg);
        A  = [Xf, -Yf, ones(Nm,1), zeros(Nm,1); ...
              Yf,  Xf, zeros(Nm,1), ones(Nm,1)];
        B  = [XYref(IndRef,1); XYref(IndRef,2)];
        Par = A\B;
        Rx = XYref(IndRef,1) - (Par(1).*Xf - Par(2).*Yf + Par(3));
        Ry = XYref(IndRef,2) - (Par(2).*Xf + Par(1).*Yf + Par(4));
        Resid = hypot(Rx,Ry);
        % sigma clip for next iteration
        Std  = max(std(Resid), eps);
        Keep = Resid <= Args.SigmaClip.*Std;
        XYflpKeep = true(size(XYflp,1),1);   %#ok<NASGU>
        if Iter < Args.Niter
            % restrict matching in next iteration implicitly via updated Par;
            % explicit clip of current matched set:
            IndImg = IndImg(Keep); IndRef = IndRef(Keep); Resid = Resid(Keep);
        end
    end

    if numel(IndImg) < 3
        return;
    end

    % --- populate result ---
    Result.Found    = true;
    Result.Flip     = Cand(Best.Ind).Flip;
    Result.Theta    = atan2d(Par(2), Par(1));
    Result.Scale    = hypot(Par(1), Par(2));
    Result.ShiftX   = Par(3);
    Result.ShiftY   = Par(4);
    Result.Tran     = [[Par(1) -Par(2); Par(2) Par(1)]*Best.P, [Par(3); Par(4)]];
    Result.SN_Theta = Cand(Best.Ind).SN;
    Result.SN_Shift = Best.SN;
    Result.Nmatch   = numel(IndImg);
    Result.RMS      = sqrt(mean(Resid.^2));
    Result.MatchedRefInd = IndRef;
    Result.MatchedImgInd = IndImg;
    if HasMag
        Result.ResidMag = std(RefCat(IndRef,3) - ImageCat(IndImg,3));
    end
end

% ======================= local functions =======================

function Sub = selectStars(Cat, MaxStars, UseMag)
    % Select the MaxStars brightest stars (or first rows if no mag)
    N = size(Cat,1);
    if UseMag
        [~,Is] = sort(Cat(:,3), 'ascend');
        Sub = Cat(Is(1:min(N,MaxStars)), :);
    else
        Sub = Cat(1:min(N,MaxStars), 1:2);
    end
end

function P = buildPairs(Sub, UseMag, MinSep, MaxSep)
    % Build table of pairs: separation, position angle, dMag
    % Pairs are oriented bright -> faint when magnitudes are available.
    N = size(Sub,1);
    [I,J] = find(triu(true(N),1));
    DX = Sub(J,1)-Sub(I,1);
    DY = Sub(J,2)-Sub(I,2);
    D  = hypot(DX,DY);
    F  = D>=MinSep & D<=MaxSep;
    I=I(F); J=J(F); DX=DX(F); DY=DY(F); D=D(F);
    if UseMag
        Swap = Sub(J,3) < Sub(I,3);          % make I the brighter star
        [I(Swap),J(Swap)] = deal(J(Swap),I(Swap));
        DX(Swap) = -DX(Swap);  DY(Swap) = -DY(Swap);
        P.DMag = Sub(J,3)-Sub(I,3);
    else
        P.DMag = zeros(size(D));
    end
    P.I = I;  P.J = J;  P.D = D;  P.PA = atan2d(DY,DX);
    % sort by separation for fast interval search
    [P.D,Is] = sort(P.D);
    P.I=P.I(Is); P.J=P.J(Is); P.PA=P.PA(Is); P.DMag=P.DMag(Is);
end

function [PAr, PAi] = matchPairInvariants(PR, PI, SepTol, UseMag, MagTol)
    % All candidate pair matches with |dD|<=SepTol (and |dDMag|<=MagTol)
    % Chunked broadcasting to bound memory.
    Chunk = 500;
    Np    = numel(PI.D);
    Ncell = ceil(Np/Chunk);
    CellR = cell(Ncell,1);
    CellI = cell(Ncell,1);
    for Ic=1:Ncell
        K  = (Ic-1)*Chunk+1 : min(Ic*Chunk, Np);
        Mt = abs(PR.D(:) - PI.D(K).') <= SepTol;              % Nref x |K|
        if UseMag
            Mt = Mt & abs(PR.DMag(:) - PI.DMag(K).') <= MagTol;
        end
        [Ir, Ii] = find(Mt);
        Ir = Ir(:);                                           % force column
        Ii = Ii(:);
        CellR{Ic} = reshape(PR.PA(Ir),    [], 1);
        CellI{Ic} = reshape(PI.PA(K(Ii)), [], 1);
    end
    PAr = cat(1, CellR{:});
    PAi = cat(1, CellI{:});
end


function [Peak, SN, Counts] = histPeak1(Votes, Edges)
    % 1D histogram peak position (refined) and robust S/N
    Counts = histcounts(Votes, Edges).';
    [Cmax, Imax] = max(Counts);
    if isempty(Counts) || Cmax==0
        Peak=NaN; SN=NaN; return;
    end
    Med = median(Counts);
    Sig = max(1.4826.*mad(Counts,1), sqrt(max(Med,1)));   % floor at Poisson
    SN  = (Cmax - Med)./Sig;
    % refine: weighted circular-safe mean of votes near the peak bin
    Cen = 0.5.*(Edges(Imax)+Edges(Imax+1));
    W   = abs(wrap180(Votes - Cen)) <= 1.5.*(Edges(2)-Edges(1));
    Peak = wrap180(Cen + mean(wrap180(Votes(W)-Cen)));
end

function [Sx, Sy, SN] = histPeak2(DX, DY, Bin)
    % 2D histogram peak (refined) and robust S/N
    Sx=NaN; Sy=NaN; SN=NaN;
    if isempty(DX)
        return;
    end
    Ex = floor(min(DX)):Bin:ceil(max(DX))+Bin;
    Ey = floor(min(DY)):Bin:ceil(max(DY))+Bin;
    if numel(Ex)<2 || numel(Ey)<2
        return;
    end
    C = histcounts2(DX(:), DY(:), Ex, Ey);
    [Cmax, Ind] = max(C(:));
    [Ix,Iy] = ind2sub(size(C), Ind);
    Med = median(C(:));
    Sig = max(1.4826.*mad(C(:),1), sqrt(max(Med,1)));
    SN  = (Cmax - Med)./Sig;
    Cx  = 0.5.*(Ex(Ix)+Ex(Ix+1));
    Cy  = 0.5.*(Ey(Iy)+Ey(Iy+1));
    W   = abs(DX-Cx)<=1.5.*Bin & abs(DY-Cy)<=1.5.*Bin;
    Sx  = mean(DX(W));
    Sy  = mean(DY(W));
end

function A = wrap180(A)
    % wrap angle to [-180,180)
    A = mod(A+180, 360) - 180;
end

function R = rotMat(Theta)
    % 2D rotation matrix for angle Theta [deg]
    R = [cosd(Theta), -sind(Theta); ...
         sind(Theta),  cosd(Theta)];
end