function S = stepFixedPattern(Obj, Type, Args)
    % Fixed-pattern non-uniformity of every ladder step, temporal noise removed.
    %   For each step the spread of the per-pixel mean map over the pixels is
    %       sigma_obs^2 = sigma_fixed^2 + V_k / Nrep_k
    %   because the map is the average of Nrep_k frames, each carrying the
    %   temporal noise V_k of that step. Subtracting the measured V_k (the
    %   median per-pixel temporal variance, corrected for the chi2 median
    %   bias) leaves the fixed pattern: relative to the signal that is the
    %   PRNU on a bright step and the DSNU integrated over the exposure on
    %   a dark one.
    %   This is the direct route to the non-uniformity and it is far better
    %   than the spread of a per-pixel response SLOPE: the published bright
    %   window holds only three closely spaced intensity steps, which fixes
    %   a pixel's slope to ~3% -- five times coarser than the ~0.5% pattern
    %   being measured -- so the slope spread is almost all fit noise,
    %   whereas one step measures the pattern from 10^4 pixels at ~1.4%.
    %   Both modes are supported: region mode reads the cached per-pixel
    %   ladder (combineSteps first), full mode streams one step at a time and
    %   measures every mask while that step's maps are in memory, so the
    %   whole die costs one read per frame and no combineSteps.
    % Input  : - 'D' or 'B'.
    %          * ...,key,val,...
    %            'Mask'  - pixels to use ([] = all).
    %            'Steps' - step numbers ([] = every step of the ladder).
    %            'Robust'- robust (MAD-based) observed spread, default true.
    %            'FitMin', 'FitMax' - [ADU] signal range of the additive /
    %                      multiplicative decomposition (default 0 and
    %                      0.75*SatLevel, which keeps the saturation knee
    %                      out: the relative pattern rises again there).
    %   The profile of the pattern against signal separates the two kinds
    %   of non-uniformity, so each subset is also fitted with
    %       sigma_fixed^2 = Additive^2 + (Multiplicative * S)^2
    %   (weighted, in the variables S^2 vs sigma^2): Additive is the offset
    %   fixed pattern in ADU (the DSNU of the bias / threshold, which shows
    %   up as the rise of the relative pattern at low signal -- 4.3% at 139
    %   ADU for run 31) and Multiplicative is the PRNU (its floor, ~0.4%).
    %   Multiplicative is a better PRNU than any single step, and Additive
    %   is the offset term the noise budget needs -- the light-method
    %   threshold spread cannot supply it, being almost all fit noise.
    % Output : - Structure with Step, X, Nframes and the All / Even / Odd
    %            sub-structures, each holding the per-step arrays Median,
    %            StdObs, StdNoise, StdFixed, RelFixed, RelUL95, Sigma and SE
    %            (of StdFixed^2), plus the scalars Additive, AdditiveErr,
    %            Multiplicative, MultiplicativeErr and PatternNsteps.
    % Example: P.run;  F = P.stepFixedPattern('B');  F.All.RelFixed
    arguments
        Obj
        Type
        Args.Mask = [];
        Args.Steps = [];
        Args.Robust logical = true;
        Args.FitMin = 0;
        Args.FitMax = [];
    end
    FitMax = Args.FitMax;
    if isempty(FitMax)
        FitMax = 0.75.*Obj.SatLevel;                  % stay clear of the saturation knee
    end
    IsRegion = strcmp(Obj.Mode, 'region');
    L = Obj.ladderOf(Type);
    if IsRegion
        if ~isfield(L, 'Mean') || isempty(L.Mean) || ~isfield(L, 'VarTemporal')
            error('ultrasat:lab:PTCAnalysis:order', 'Run combineSteps before stepFixedPattern');
        end
    elseif ~isfield(L, 'X') || isempty(L.X)
        L = Obj.stepInventory(Type);          % streamed: no combineSteps needed
    end
    Idx = 1:1:numel(L.X);
    if ~isempty(Args.Steps)
        Idx = find(ismember(L.Step, Args.Steps));
    end
    Nj = numel(Idx);
    S = struct('Type',Type, 'Mode',Obj.Mode, 'Step',L.Step(Idx), 'X',L.X(Idx), ...
               'Nframes',L.Nframes(Idx), 'Robust',Args.Robust);
    Mask = Args.Mask;
    if isempty(Mask)
        Mask = true(size(Obj.Zero));
    end
    Names = {'All'};
    Masks = {Mask};
    if ~isempty(Obj.ParityMap)
        Names = [Names, {'Even'}, {'Odd'}];
        Masks = [Masks, {Mask & ~Obj.ParityMap}, {Mask & Obj.ParityMap}];
    end
    Q = cell(1, numel(Names));
    for Im = 1:1:numel(Names)
        Q{Im} = local_init(nnz(Masks{Im}));
    end

    % One pass over the steps, all masks measured while the maps of that step
    % are in memory: in region mode they come from the cached ladder, in full
    % mode each step is streamed from the frames (one read per frame).
    for J = 1:1:Nj
        if IsRegion
            Mi = double(L.Mean(:,:,Idx(J)));
            Vi = double(L.VarTemporal(:,:,Idx(J)));
            Nf = L.Nframes(Idx(J));
        else
            [Mi, Vi, Nf] = Obj.stepMaps(Type, L.Step(Idx(J)));
            Mi = double(Mi);
            Vi = double(Vi);
            S.Nframes(J) = Nf;
            if Obj.Verbosity>0
                fprintf('stepFixedPattern: %s step %d/%d\n', Type, J, Nj);
            end
        end
        Fin = isfinite(Mi) & isfinite(Vi);
        for Im = 1:1:numel(Names)
            Q{Im} = local_step(Q{Im}, J, Mi, Vi, Masks{Im} & Fin, Nf);
        end
    end
    for Im = 1:1:numel(Names)
        S.(Names{Im}) = local_fit(Q{Im});
    end

    function Q = local_init(N)
        Q = struct('Npix',N, 'Median',NaN(1,Nj), 'StdObs',NaN(1,Nj), ...
                   'StdNoise',NaN(1,Nj), 'StdFixed',NaN(1,Nj), ...
                   'RelFixed',NaN(1,Nj), 'RelUL95',NaN(1,Nj), ...
                   'Sigma',NaN(1,Nj), 'SE',NaN(1,Nj), ...
                   'Additive',NaN, 'AdditiveErr',NaN, 'Multiplicative',NaN, ...
                   'MultiplicativeErr',NaN, 'PatternNsteps',0);
    end

    function Q = local_step(Q, J, Mi, Vi, Ok, Nf)
        % fixed pattern of one step over one mask
        if Q.Npix<3 || nnz(Ok)<3
            return
        end
        Mv  = Mi(Ok);  Vv = Vi(Ok);
        Nk  = nnz(Ok);
        Med = median(Mv);
        if Args.Robust
            Obs = 1.4826.*median(abs(Mv - Med));
        else
            Obs = std(Mv);
        end
        Dof   = max(Nf-1, 1);
        Vstep = median(Vv).*Dof./(2.*gammaincinv(0.5, Dof./2));   % variance of ONE frame
        Vnois = Vstep./Nf;                                        % of the averaged map
        VarF  = Obs.^2 - Vnois;
        % sampling errors of the two terms, in quadrature
        SE = sqrt((Obs.^2.*sqrt(2./(Nk-1))).^2 + (1.44.*Vnois./sqrt(Nk)).^2);
        Q.Median(J)   = Med;
        Q.StdObs(J)   = Obs;
        Q.StdNoise(J) = sqrt(Vnois);
        Q.StdFixed(J) = sqrt(max(VarF, 0));
        Q.RelFixed(J) = Q.StdFixed(J)./abs(Med);
        Q.RelUL95(J)  = sqrt(max(VarF + 1.645.*SE, 0))./abs(Med);
        Q.Sigma(J)    = VarF./SE;
        Q.SE(J)       = SE;
    end

    function Q = local_fit(Q)
        % additive (+) multiplicative decomposition
        Use = isfinite(Q.StdFixed) & isfinite(Q.Median) & isfinite(Q.SE) & ...
              Q.Median>Args.FitMin & Q.Median<FitMax;
        if nnz(Use)>=3
            Xv = (Q.Median(Use).').^2;
            Yv = (Q.StdFixed(Use).').^2;
            Wv = 1./max(Q.SE(Use).', eps).^2;
            A  = [ones(numel(Xv),1), Xv];
            AW = Wv.*A;
            Cf = (A.'*AW)\(A.'*(Wv.*Yv));
            Cv = inv(A.'*AW);
            Q.Additive          = sqrt(max(Cf(1), 0));
            Q.Multiplicative    = sqrt(max(Cf(2), 0));
            Q.AdditiveErr       = 0.5.*sqrt(Cv(1,1))./max(Q.Additive, eps);
            Q.MultiplicativeErr = 0.5.*sqrt(Cv(2,2))./max(Q.Multiplicative, eps);
            Q.PatternNsteps     = nnz(Use);
        end
    end
end
