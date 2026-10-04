function Info = aliasIdentifier(Time, MagErr, Freq, Power, Args)
% Diagnose frequency (alias) ambiguities among periodogram peaks.
%   Given the observation times, their uncertainties, and either a list of
%   candidate frequencies or a full power spectrum, this function:
%   (1) builds the whitened sinusoidal design matrix of the candidates,
%       projects out the nuisance model (a constant by default), and
%       orthonormalizes the two columns of each frequency;
%   (2) computes the SVD of the result (singular values are inverse
%       standard errors of combinations of sinusoids), the participation of
%       each candidate in the poorly constrained subspace, and the
%       principal angles between all pairs of candidates;
%   (3) groups the candidates into families that may be produced by the
%       same signal (cos(theta_1)>=CosLink, or co-participation>=CLink);
%   (4) if the measurements are given (Args.Mag), fits sine models: one
%       candidate per family jointly (families that are not significant
%       are removed by backward elimination); then, within each family,
%       compares the best member with every alternative, estimates the
%       probability that the ranking is wrong, and tests whether additional
%       members are significant. Each family receives a decision:
%       'unique', 'ambiguous', or 'not significant'.
%   Everything in steps (1)-(3) depends only on the times, uncertainties,
%   and frequencies, so the function can also be used to plan observations.
%   Reference: Ofek (2026), Diagnosing frequency ambiguities in
%   irregularly sampled astronomical time series.
%
% Input  : - Vector of observation times (N elements).
%          - Vector of uncertainties (N elements). If empty, equal weights
%            are used, and, if Mag is given, the errors are scaled such that
%            the reduced chi^2 of the best model is 1 (see 'RescaleErr').
%          - Vector of frequencies. If Power is empty, these are the
%            candidate frequencies. Otherwise, the frequency grid of the
%            power spectrum. Units are 1/(units of Time).
%          - Vector of power (same size as Freq), or empty (default).
%            If given, the candidates are the 'Npeak' highest local maxima,
%            or all local maxima above 'MinPeak', separated by 'MinSep'.
%          * ...,key,val,...
%            'Mag' - Vector of measurements (N elements). Required for the
%                   sine fits and the decisions. Default is [].
%            'Npeak' - Number of highest peaks to select. Default is 10.
%            'MinPeak' - If not empty, select all peaks with Power>=MinPeak
%                   (overrides 'Npeak'). Default is [].
%            'MinSep' - Minimal separation between selected peaks.
%                   Default is [], i.e., 2/range(Time).
%            'Nuisance' - Nuisance model: 'const', 'linear', 'quad', or an
%                   N x q matrix of regressors (a constant is always added).
%                   Default is 'const'.
%            'RefTime' - Reference time. Default is [], i.e., the weighted
%                   mean time. Does not affect the results, but avoids loss
%                   of precision with, e.g., Julian dates.
%            'Threshold' - Requested relative singular value threshold for
%                   the poorly constrained subspace. It is moved to the
%                   largest gap within a factor of 2. Default is 0.1.
%            'CosLink' - Link two candidates if cos(theta_1)>=CosLink.
%                   Default is 0.5.
%            'CLink' - Link two candidates if their co-participation in the
%                   poorly constrained subspace is >=CLink. Default is 0.1.
%            'Rho' - Vector of candidate strengths (sqrt of Delta chi^2).
%                   Used for the worst-phase misidentification probability
%                   when Mag is not given. Default is [].
%            'PowerIsChi2' - If true and Mag is not given, Power is
%                   Delta chi^2, and Rho=sqrt(Power-2) at the peaks.
%                   Default is false.
%            'Kappa' - Decision level [sigma]. An alternative is rejected
%                   if its misidentification probability is below
%                   Phi(-Kappa). Default is 3.
%            'SignifPvalue' - P-value below which a family (or an
%                   additional member, after a Bonferroni correction) is
%                   considered significant. Default is 1e-3.
%            'RescaleErr' - Scale the errors such that the reduced chi^2
%                   of the best model is 1. Default is [], i.e., true if
%                   MagErr is empty and false otherwise.
%            'RefineFreq' - Refine each candidate to the local chi^2
%                   minimum before the analysis (requires Mag).
%                   Default is true.
%            'MaxModels' - Maximal number of one-candidate-per-family
%                   models to fit exhaustively. Above this, a coordinate
%                   search is used. Default is 1e4.
%            'Plot' - Make diagnostic plots. Default is true.
%            'Verbose' - Print a summary. Default is false.
% Output : - A structure with the following fields:
%            .Freq - Candidate frequencies used (after selection,
%                   refinement, and removal of degenerate candidates).
%            .PeakPower - Power at the selected peaks (NaN if no Power).
%            .RefTime, .N, .TimeSpan
%            .BlockIndex - Candidate index of each column of Q.
%            .SingularValues, .RelativeSingularValues, .ConditionNumber
%            .RightSingularVectors - V.
%            .Participation - p_kj, candidate k in mode j.
%            .Threshold, .GapRatio - Threshold used and its gap ratio.
%            .NullModes - Indices of the poorly constrained modes.
%            .NullParticipation - D_k.  .CoParticipation - C_kl.
%            .NumIndependent - n_ind.
%            .WithinFrequencyRatio - r^in_k.
%            .PairCos1, .PairCos2, .PairSin1, .PairRatio - Principal-angle
%                   quantities for all pairs (K x K).
%            .WindowCos1, .WindowCos2 - Leading-order window approximation.
%            .Families - Family label of each candidate.
%            .Rho - Candidate strengths (from the fits, Args.Rho, or Power).
%            .MisidentificationWorst - K x K; (k,l) is the worst-phase
%                   probability of preferring l when the signal is at k.
%            .Fit - (only if Mag is given) Single-sinusoid fits
%                   (DeltaChi2, Amp, Phase), the best joint model (one
%                   member of each significant family), and the error scale.
%            .Family - (only if Mag is given) Struct array, one element per
%                   family, with the best member, the comparison with each
%                   alternative (observed and predicted Delta chi^2,
%                   misidentification probabilities, relative likelihood),
%                   tests for additional signals, and the Decision.
%                   Note: Pvalue (significance of the family) is a
%                   fixed-frequency p-value; it does not account for the
%                   search over frequencies that produced the candidates
%                   (look-elsewhere effect).
%            .Figures - Handles of the four figures (empty if Plot=false):
%                   candidates and families, pairwise cos(theta_1),
%                   sin(theta_1) to the best member, singular values.
% Author : Eran Ofek (Oct 2026)
% Example: T   = sort(rand(400,1))*300; T = round(T) + 0.05*randn(size(T));
%          Err = 0.1*ones(size(T));
%          Y   = 0.05*sin(2*pi*0.3713*T) + Err.*randn(size(T));
%          F   = (0.01:1e-4:2.5)';
%          Info = aliasIdentifier(T, Err, [0.3713; 0.6287; 1.3713], [], 'Mag',Y);
%          % or let the function pick the peaks of a periodogram, Pow:
%          % Info = aliasIdentifier(T, Err, F, Pow, 'Mag',Y, 'Npeak',8);

arguments
    Time                 {mustBeNumeric, mustBeReal}
    MagErr               {mustBeNumeric, mustBeReal}
    Freq                 {mustBeNumeric, mustBeReal}
    Power                {mustBeNumeric, mustBeReal}          = []
    Args.Mag             {mustBeNumeric, mustBeReal}          = []
    Args.Npeak           (1,1) {mustBeInteger, mustBePositive} = 10
    Args.MinPeak                                              = []
    Args.MinSep                                               = []
    Args.Nuisance                                             = 'const'
    Args.RefTime                                              = []
    Args.Threshold       (1,1) {mustBePositive}               = 0.1
    Args.CosLink         (1,1) {mustBeNumeric}                = 0.5
    Args.CLink           (1,1) {mustBeNumeric}                = 0.1
    Args.Rho                                                  = []
    Args.PowerIsChi2     (1,1) logical                        = false
    Args.Kappa           (1,1) {mustBePositive}               = 3
    Args.SignifPvalue    (1,1) {mustBePositive}               = 1e-3
    Args.RescaleErr                                           = []
    Args.RefineFreq      (1,1) logical                        = true
    Args.MaxModels       (1,1) {mustBePositive}               = 1e4
    Args.Plot            (1,1) logical                        = true
    Args.Verbose         (1,1) logical                        = false
end

TolRank = 1e-8;      % relative tolerance for numerical rank

% ---------------------------------------------------------------------
% Data, weights, nuisance model
% ---------------------------------------------------------------------
Time   = Time(:);
N      = numel(Time);
Mag    = Args.Mag(:);
HasMag = ~isempty(Mag);
if HasMag && numel(Mag)~=N
    error('aliasIdentifier:size', 'Mag must have the same number of elements as Time');
end
if isempty(MagErr)
    W            = ones(N,1);
    EqualWeights = true;
else
    MagErr = MagErr(:);
    if numel(MagErr)~=N
        error('aliasIdentifier:size', 'MagErr must be empty or have the same number of elements as Time');
    end
    W            = 1./MagErr.^2;
    EqualWeights = false;
end
if isempty(Args.RescaleErr)
    RescaleErr = EqualWeights && HasMag;
else
    RescaleErr = logical(Args.RescaleErr) && HasMag;
end

if isempty(Args.RefTime)
    RefTime = sum(W.*Time)./sum(W);
else
    RefTime = Args.RefTime;
end
t  = Time - RefTime;
TimeSpan = max(Time) - min(Time);
SW = sqrt(W);

G  = buildNuisance(t, Args.Nuisance);
[Ug, Sg] = svd(G.*SW, 'econ');
Sg = diag(Sg);
Qg = Ug(:, Sg > TolRank*Sg(1));
Nnuis = size(Qg,2);
Proj  = @(M) M - Qg*(Qg'*M);          % project out the (whitened) nuisance model

% ---------------------------------------------------------------------
% Candidate frequencies
% ---------------------------------------------------------------------
Freq = Freq(:);
if isempty(Power)
    CandFreq  = Freq;
    PeakPower = nan(size(CandFreq));
    GridStep  = 0;
else
    Power = Power(:);
    if numel(Power)~=numel(Freq)
        error('aliasIdentifier:size', 'Freq and Power must have the same number of elements');
    end
    [Freq, Is] = sort(Freq);
    Power      = Power(Is);
    if isempty(Args.MinSep)
        MinSep = 2./TimeSpan;
    else
        MinSep = Args.MinSep;
    end
    Ip        = selectPeaks(Freq, Power, Args.Npeak, Args.MinPeak, MinSep);
    CandFreq  = Freq(Ip);
    PeakPower = Power(Ip);
    GridStep  = median(diff(Freq));
end
if isempty(CandFreq)
    error('aliasIdentifier:noPeaks', 'No candidate frequencies');
end
InputFreq = CandFreq;

if HasMag
    Yt0 = Proj(Mag.*SW);              % whitened, projected data (unscaled)
    if Args.RefineFreq
        Df = max(0.25./TimeSpan, GridStep);
        Opt = optimset('TolX', 1e-4./TimeSpan, 'Display', 'off');
        for K = 1:numel(CandFreq)
            Fun = @(F) -singleDeltaChi2(F, t, SW, Proj, Yt0);
            CandFreq(K) = fminbnd(Fun, CandFreq(K)-Df, CandFreq(K)+Df, Opt);
        end
        % remove candidates that converged to the same peak
        [CandFreq, Iu] = uniqueWithin(CandFreq, 0.1./TimeSpan);
        PeakPower = PeakPower(Iu);
        InputFreq = InputFreq(Iu);
    end
end

% ---------------------------------------------------------------------
% Per-frequency blocks: project, orthonormalize, within-frequency ratio
% ---------------------------------------------------------------------
Kc    = numel(CandFreq);
Qk    = cell(Kc,1);
Xk    = cell(Kc,1);
Rin   = nan(Kc,1);
Keep  = true(Kc,1);
for K = 1:Kc
    Xw  = sinCos(t, CandFreq(K)).*SW;
    Xt  = Proj(Xw);
    [U, S] = svd(Xt, 'econ');
    S   = diag(S);
    S0  = svd(Xw);
    Rin(K) = S(end)./S0(1);
    if S(1) < TolRank*S0(1)
        Keep(K) = false;                  % fully degenerate with the nuisance model
    elseif S(2) < TolRank*S(1)
        Qk{K} = U(:,1);                   % effectively one-dimensional block
    else
        Qk{K} = U(:,1:2);
    end
    Xk{K} = Xt;
end
if any(~Keep)
    warning('aliasIdentifier:dropped', ...
            'Dropped %d candidate(s) degenerate with the nuisance model: %s', ...
            sum(~Keep), mat2str(CandFreq(~Keep).', 6));
end
CandFreq  = CandFreq(Keep);
PeakPower = PeakPower(Keep);
InputFreq = InputFreq(Keep);
Qk  = Qk(Keep);
Xk  = Xk(Keep);
Rin = Rin(Keep);
Kc  = numel(CandFreq);
if Kc==0
    error('aliasIdentifier:noPeaks', 'All candidates are degenerate with the nuisance model');
end

BlockCols  = cell(Kc,1);
BlockIndex = zeros(0,1);
for K = 1:Kc
    BlockCols{K} = numel(BlockIndex) + (1:size(Qk{K},2));
    BlockIndex   = [BlockIndex; K.*ones(size(Qk{K},2),1)]; %#ok<AGROW>
end
Q    = [Qk{:}];
Ncol = size(Q,2);

% ---------------------------------------------------------------------
% SVD of Q (full V when N < Ncol, so that exact null modes are kept)
% ---------------------------------------------------------------------
if N >= Ncol
    [~, S, V] = svd(Q, 'econ');
else
    [~, S, V] = svd(Q);
end
Mu = zeros(Ncol,1);
Ds = diag(S);
Mu(1:numel(Ds)) = Ds;
R  = Mu./Mu(1);

[Thr, GapRatio] = snapThreshold(R, Args.Threshold);
NullModes = find(R < Thr);
Vn = V(:, NullModes);
Pn = Vn*Vn';

Participation = zeros(Kc, Ncol);
Dk = zeros(Kc,1);
Ckl = zeros(Kc,Kc);
for K = 1:Kc
    Participation(K,:) = sum(V(BlockCols{K},:).^2, 1);
    Dk(K) = sum(diag(Pn(BlockCols{K}, BlockCols{K})));
    for L = 1:Kc
        Ckl(K,L) = norm(Pn(BlockCols{K}, BlockCols{L}), 'fro');
    end
end

% ---------------------------------------------------------------------
% Pairs: principal angles and the window approximation
% ---------------------------------------------------------------------
Cos1 = eye(Kc);
Cos2 = eye(Kc);
WCos1 = nan(Kc);
WCos2 = nan(Kc);
for K = 1:Kc
    for L = K+1:Kc
        Sv = svd(Qk{K}.'*Qk{L});
        Cos1(K,L) = min(Sv(1), 1);
        if numel(Sv)==2
            Cos2(K,L) = min(Sv(2), 1);
        else
            Cos2(K,L) = NaN;
        end
        A = abs(windowNorm(t, W, CandFreq(K)-CandFreq(L)));
        B = abs(windowNorm(t, W, CandFreq(K)+CandFreq(L)));
        WCos1(K,L) = A + B;
        WCos2(K,L) = abs(A - B);
    end
end
Cos1  = triu(Cos1,1) + triu(Cos1,1).' + eye(Kc);
Cos2  = triu(Cos2,1) + triu(Cos2,1).' + eye(Kc);
WCos1 = fillSym(WCos1);
WCos2 = fillSym(WCos2);
Sin1  = sqrt(max(1 - Cos1.^2, 0));
PairRatio = sqrt((1 - Cos1)./(1 + Cos1));

% ---------------------------------------------------------------------
% Families: connected components of the link graph
% ---------------------------------------------------------------------
Link = (Cos1 >= Args.CosLink | Ckl >= Args.CLink);
Link(1:Kc+1:end) = false;
Families = connectedComponents(Link);
Nfam = max(Families);

% ---------------------------------------------------------------------
% Info: times-only part
% ---------------------------------------------------------------------
Info.Freq                   = CandFreq;
Info.InputFreq              = InputFreq;
Info.PeakPower              = PeakPower;
Info.RefTime                = RefTime;
Info.N                      = N;
Info.TimeSpan               = TimeSpan;
Info.NumNuisance            = Nnuis;
Info.BlockIndex             = BlockIndex;
Info.SingularValues         = Mu;
Info.RelativeSingularValues = R;
Info.ConditionNumber        = 1./R(end);
Info.RightSingularVectors   = V;
Info.Participation          = Participation;
Info.Threshold              = Thr;
Info.RequestedThreshold     = Args.Threshold;
Info.GapRatio               = GapRatio;
Info.NullModes              = NullModes;
Info.NullParticipation      = Dk;
Info.CoParticipation        = Ckl;
Info.NumIndependent         = (Ncol - numel(NullModes))./2;
Info.WithinFrequencyRatio   = Rin;
Info.PairCos1               = Cos1;
Info.PairCos2               = Cos2;
Info.PairSin1               = Sin1;
Info.PairRatio              = PairRatio;
Info.WindowCos1             = WCos1;
Info.WindowCos2             = WCos2;
Info.Families               = Families;
Info.NumFamilies            = Nfam;
Info.Rho                    = [];
Info.MisidentificationWorst = [];
Info.Fit                    = [];
Info.Family                 = [];
Info.Figures                = [];

% ---------------------------------------------------------------------
% Sine fits and decisions (requires the measurements)
% ---------------------------------------------------------------------
if HasMag
    Gq    = Q.'*Q;                                % Gram matrix of Q
    Bq0   = Q.'*Yt0;
    Chi0u = Yt0.'*Yt0;
    Members = cell(Nfam,1);
    for F = 1:Nfam
        Members{F} = find(Families==F);
    end
    DChi0 = cellfun(@(C) sum(Bq0(C).^2), BlockCols);   % single-sinusoid Delta chi^2 (unscaled)

    % Best model with one candidate per family. Families that are not
    % significant are removed one at a time (backward elimination).
    Active = true(Nfam,1);
    while true
        Fa = find(Active);
        if isempty(Fa)
            Sel = zeros(1,0);
            break
        end
        Sel  = searchBestModel(Members(Fa), BlockCols, Gq, Bq0, DChi0, Args.MaxModels);
        Cols = [BlockCols{Sel}];
        [Red, Rk] = reduction(Gq, Bq0, Cols);
        S2 = 1;
        if RescaleErr
            S2 = (Chi0u - Red)./(N - Nnuis - Rk);
        end
        Pf = zeros(numel(Fa),1);
        for I = 1:numel(Fa)
            Cols0 = [BlockCols{Sel([1:I-1, I+1:end])}];
            Pf(I) = chi2Sf((Red - reduction(Gq, Bq0, Cols0))./S2, numel(BlockCols{Sel(I)}));
        end
        [Pmax, Imax] = max(Pf);
        if Pmax <= Args.SignifPvalue
            break
        end
        Active(Fa(Imax)) = false;
    end
    BestSel = zeros(1,Nfam);
    BestSel(Active) = Sel;

    % error scale from the final model
    ColsBest = [BlockCols{BestSel(Active)}];
    [Red0, RankBest] = reduction(Gq, Bq0, ColsBest);
    DofRes      = N - Nnuis - RankBest;
    RedChi2Best = (Chi0u - Red0)./DofRes;
    if RescaleErr
        ErrScale = sqrt(RedChi2Best);
    else
        ErrScale = 1;
        if RedChi2Best > 2
            warning('aliasIdentifier:chi2', ['Reduced chi^2 of the best model is %.2f; ', ...
                    'the errors may be underestimated (consider ''RescaleErr'',true)'], RedChi2Best);
        end
    end
    Yt   = Yt0./ErrScale;
    Bq   = Q.'*Yt;
    Chi0 = Yt.'*Yt;

    % single-sinusoid fits
    DChi  = cellfun(@(C) sum(Bq(C).^2), BlockCols);
    Amp   = zeros(Kc,1);
    Phase = zeros(Kc,1);
    for K = 1:Kc
        Coef     = pinv(Xk{K})*Yt0;          % FWL: same as the joint fit with the nuisance terms
        Amp(K)   = hypot(Coef(1), Coef(2));
        Phase(K) = atan2(Coef(2), Coef(1));  % model: Amp*cos(2*pi*f*(t-RefTime) - Phase)
    end
    Rho = sqrt(max(DChi - 2, 0));

    Fit.Chi2Null        = Chi0;
    Fit.DeltaChi2       = DChi;
    Fit.Amp             = Amp;
    Fit.Phase           = Phase;
    Fit.ErrorScale      = ErrScale;
    Fit.RescaledErrors  = RescaleErr;
    Fit.ReducedChi2Best = RedChi2Best;
    Fit.BestModel       = BestSel(Active).';
    Fit.BestModelFreq   = CandFreq(BestSel(Active));
    Fit.Chi2Best        = Chi0 - reduction(Gq, Bq, ColsBest);
    Info.Fit = Fit;

    % per-family comparisons and decisions
    PrejKappa = normCdf(-Args.Kappa);
    Fam = struct([]);
    for F = 1:Nfam
        M      = Members{F};
        Others = BestSel(Active & ((1:Nfam).' ~= F));     % best members of the other significant families
        Others = Others(:).';
        if Active(F)
            Best = BestSel(F);
        else
            % best member when added to the final model
            Gains = zeros(numel(M),1);
            for I = 1:numel(M)
                Gains(I) = reduction(Gq, Bq, [BlockCols{[Others, M(I)]}]);
            end
            [~, I] = max(Gains);
            Best = M(I);
        end
        ColsA = [BlockCols{[Others, Best]}];
        [RedA, RankA] = reduction(Gq, Bq, ColsA);
        Alt = M(M~=Best);
        Na  = numel(Alt);

        % significance of the family (drop its best member)
        Pfam = chi2Sf(RedA - reduction(Gq, Bq, [BlockCols{Others}]), numel(BlockCols{Best}));

        % fitted (debiased) signal of model A and its basis
        SA  = Q(:,ColsA)*(pinvTol(Gq(ColsA,ColsA))*Bq(ColsA));
        NSA = SA.'*SA;
        if NSA > 0
            SA = SA.*sqrt(max(NSA - RankA, 0)./NSA);          % |s|^2 ~ chi2 reduction - dof
        end
        UA = orthBasis(Q(:,ColsA));

        Obs = zeros(Na,1);  Pred = zeros(Na,1);  PredStd = zeros(Na,1);
        Pw  = zeros(Na,1);  PwWorst = zeros(Na,1);
        Pex = zeros(Na,1);  GainEx = zeros(Na,1);
        for I = 1:Na
            ColsB  = [BlockCols{[Others, Alt(I)]}];
            Obs(I) = RedA - reduction(Gq, Bq, ColsB);        % chi2_B - chi2_A
            UB = orthBasis(Q(:,ColsB));
            D2 = SA.'*SA - sum((UB.'*SA).^2);
            Cs = svd(UA.'*UB);
            Pa = size(UA,2);
            Pb = size(UB,2);
            Pred(I)    = D2 + (Pa - Pb);
            PredStd(I) = sqrt(max(4*D2 + 2*(Pa + Pb - 2*sum(Cs.^2)), 0));
            if PredStd(I) > 0
                Pw(I) = normCdf(-Pred(I)./PredStd(I));
            else
                Pw(I) = 0.5;                                  % identical model spaces: no information
            end
            PwWorst(I) = normCdf(-0.5*Rho(Best)*Sin1(Best,Alt(I)));
            % additional signal: add the alternative to model A
            [RedC, RankC] = reduction(Gq, Bq, [ColsA, BlockCols{Alt(I)}]);
            GainEx(I) = RedC - RedA;
            Pex(I)    = chi2Sf(GainEx(I), max(RankC - RankA, 1));
        end
        RelLik = exp(-0.5*[0; Obs]);
        RelLik = RelLik./sum(RelLik);

        % decision
        if Pfam > Args.SignifPvalue
            Decision = 'not significant';
            Viable   = M(:).';
        else
            Viable = [Best, Alt(Pw > PrejKappa).'];
            if numel(Viable)==1
                Decision = 'unique';
            else
                Decision = ['ambiguous: ', strjoin(arrayfun(@(X) sprintf('%.6g', X), ...
                            CandFreq(Viable).', 'UniformOutput', false), ' or ')];
            end
            Extra = Alt(Pex < Args.SignifPvalue./max(Na,1));  % Bonferroni over the members
            if ~isempty(Extra)
                Decision = [Decision, '; additional signal(s) at ', ...
                            strjoin(arrayfun(@(X) sprintf('%.6g', X), CandFreq(Extra).', ...
                            'UniformOutput', false), ', ')];
            end
        end

        Fam(F).Members          = M(:).';
        Fam(F).Freq             = CandFreq(M).';
        Fam(F).Best             = Best;
        Fam(F).BestFreq         = CandFreq(Best);
        Fam(F).BestRho          = Rho(Best);
        Fam(F).Pvalue           = Pfam;
        Fam(F).InBestModel      = Active(F);
        Fam(F).Alt              = Alt(:).';
        Fam(F).AltFreq          = CandFreq(Alt).';
        Fam(F).DeltaChi2Obs     = Obs.';
        Fam(F).DeltaChi2Pred    = Pred.';
        Fam(F).DeltaChi2PredStd = PredStd.';
        Fam(F).Pwrong           = Pw.';
        Fam(F).PwrongWorst      = PwWorst.';
        Fam(F).RelLikelihood    = RelLik.';           % [best, Alt...]
        Fam(F).ExtraDeltaChi2   = GainEx.';
        Fam(F).ExtraPvalue      = Pex.';
        Fam(F).Viable           = Viable;
        Fam(F).Decision         = Decision;
    end
    Info.Family = Fam;
end

% strengths and worst-phase misidentification matrix
if HasMag
    Info.Rho = Rho;
elseif ~isempty(Args.Rho)
    if numel(Args.Rho)==numel(Keep)
        Info.Rho = Args.Rho(Keep);
        Info.Rho = Info.Rho(:);
    else
        Info.Rho = Args.Rho(:);
    end
elseif Args.PowerIsChi2 && ~isempty(Power)
    Info.Rho = sqrt(max(PeakPower - 2, 0));
end
if ~isempty(Info.Rho)
    Info.MisidentificationWorst = normCdf(-0.5*Info.Rho(:).*Sin1);
    Info.MisidentificationWorst(1:Kc+1:end) = NaN;
end

if Args.Verbose
    printSummary(Info);
end
if Args.Plot
    Info.Figures = makePlots(Info, Freq, Power, Args.Kappa);
end

end   % aliasIdentifier


% =====================================================================
% Local functions
% =====================================================================
function X = sinCos(T, F)
    % N x 2 matrix [cos, sin] at frequency F
    A = 2*pi*F*T;
    X = [cos(A), sin(A)];
end

function G = buildNuisance(T, Model)
    % Nuisance regressors (always including a constant)
    N = numel(T);
    if ischar(Model) || isstring(Model)
        Tn = T./max(max(abs(T)), eps);
        switch lower(char(Model))
            case 'const'
                G = ones(N,1);
            case 'linear'
                G = [ones(N,1), Tn];
            case 'quad'
                G = [ones(N,1), Tn, Tn.^2];
            otherwise
                error('aliasIdentifier:nuisance', 'Unknown Nuisance option: %s', char(Model));
        end
    else
        if size(Model,1)~=N
            error('aliasIdentifier:nuisance', 'Nuisance matrix must have N rows');
        end
        G = [ones(N,1), Model];
    end
end

function DC = singleDeltaChi2(F, T, SW, Proj, Yt)
    % Delta chi^2 of a single sinusoid at F (after the nuisance projection)
    Xt = Proj(sinCos(T, F).*SW);
    [U, S] = svd(Xt, 'econ');
    S = diag(S);
    U = U(:, S > 1e-8*S(1));
    DC = sum((U.'*Yt).^2);
end

function Ip = selectPeaks(F, P, Npeak, MinPeak, MinSep)
    % Local maxima of P, highest first, separated by more than MinSep
    N  = numel(P);
    Im = find([false; P(2:N-1) > P(1:N-2) & P(2:N-1) >= P(3:N); false]);
    if ~isempty(MinPeak)
        Im = Im(P(Im) >= MinPeak);
    end
    [~, Is] = sort(P(Im), 'descend');
    Im = Im(Is);
    Ip = zeros(0,1);
    for I = 1:numel(Im)
        if all(abs(F(Im(I)) - F(Ip)) > MinSep)
            Ip(end+1,1) = Im(I); %#ok<AGROW>
            if isempty(MinPeak) && numel(Ip) >= Npeak
                break
            end
        end
    end
end

function [F, Iu] = uniqueWithin(F, Tol)
    % Remove frequencies closer than Tol to an earlier one
    Iu = zeros(0,1);
    for I = 1:numel(F)
        if all(abs(F(I) - F(Iu)) > Tol)
            Iu(end+1,1) = I; %#ok<AGROW>
        end
    end
    F = F(Iu);
end

function W = windowNorm(T, Wt, F)
    % Normalized weighted spectral window
    W = sum(Wt.*exp(-2i*pi*F*T))./sum(Wt);
end

function A = fillSym(A)
    % Fill the lower triangle from the upper one, NaN on the diagonal
    U = triu(A,1);
    U(isnan(U)) = 0;
    A = U + U.';
    A(1:size(A,1)+1:end) = NaN;
end

function [Thr, Gap] = snapThreshold(R, Req)
    % Move the threshold to the largest gap within a factor of 2 of Req
    Lo = Req/2;  Hi = 2*Req;
    Thr = Req;   Gap = NaN;   Best = 0;
    for J = 1:numel(R)-1
        A = R(J);  B = R(J+1);
        if A >= Lo && B <= Hi
            if B <= 1e-12
                G = Inf;  T = min(Req, A/2);
            else
                G = A/B;  T = sqrt(A*B);
            end
            if G > Best
                Best = G;  Thr = T;  Gap = G;
            end
        end
    end
end

function Lab = connectedComponents(Adj)
    % Labels of the connected components of a symmetric adjacency matrix
    N   = size(Adj,1);
    Lab = zeros(N,1);
    C   = 0;
    for K = 1:N
        if Lab(K)==0
            C = C + 1;
            Stack = K;
            Lab(K) = C;
            while ~isempty(Stack)
                I = Stack(end);
                Stack(end) = [];
                Nb = find(Adj(I,:) & (Lab.'==0));
                Lab(Nb) = C;
                Stack = [Stack, Nb]; %#ok<AGROW>
            end
        end
    end
end

function P = pinvTol(A)
    % Pseudo-inverse of a small symmetric positive semi-definite matrix
    [U, S] = eig((A + A.')/2);
    S = diag(S);
    Keep = S > 1e-12*max(S);
    P = U(:,Keep)*diag(1./S(Keep))*U(:,Keep).';
end

function [Red, Rank] = reduction(Gq, Bq, Cols)
    % chi^2 reduction of the model spanned by the columns Cols of Q
    if isempty(Cols)
        Red = 0;  Rank = 0;
        return
    end
    A = Gq(Cols,Cols);
    [U, S] = eig((A + A.')/2);
    S = diag(S);
    Keep = S > 1e-12*max(S);
    B = U(:,Keep).'*Bq(Cols);
    Red  = sum(B.^2./S(Keep));
    Rank = sum(Keep);
end

function U = orthBasis(A)
    % Orthonormal basis of the column space of A
    [U, S] = svd(A, 'econ');
    S = diag(S);
    U = U(:, S > 1e-8*S(1));
end

function Sel = searchBestModel(Members, BlockCols, Gq, Bq, DChi, MaxModels)
    % One candidate per family minimizing chi^2 (exhaustive or coordinate search)
    Nf    = numel(Members);
    Sizes = cellfun(@numel, Members);
    Sel   = zeros(1,Nf);
    for F = 1:Nf
        [~, I] = max(DChi(Members{F}));
        Sel(F) = Members{F}(I);
    end
    if prod(Sizes) <= MaxModels
        BestRed = -Inf;
        Idx = ones(1,Nf);
        for Imod = 1:prod(Sizes)
            S = zeros(1,Nf);
            for F = 1:Nf
                S(F) = Members{F}(Idx(F));
            end
            Red = reduction(Gq, Bq, [BlockCols{S}]);
            if Red > BestRed
                BestRed = Red;  Sel = S;
            end
            % next combination (mixed radix)
            for F = 1:Nf
                Idx(F) = Idx(F) + 1;
                if Idx(F) <= Sizes(F)
                    break
                end
                Idx(F) = 1;
            end
        end
    else
        Changed = true;
        while Changed
            Changed = false;
            for F = 1:Nf
                BestRed = -Inf;  BestM = Sel(F);
                for M = Members{F}(:).'
                    S = Sel;  S(F) = M;
                    Red = reduction(Gq, Bq, [BlockCols{S}]);
                    if Red > BestRed
                        BestRed = Red;  BestM = M;
                    end
                end
                if BestM ~= Sel(F)
                    Sel(F) = BestM;  Changed = true;
                end
            end
        end
    end
end

function P = normCdf(X)
    % Standard normal CDF (no toolbox required)
    P = 0.5*erfc(-X./sqrt(2));
end

function P = chi2Sf(X, Dof)
    % Survival function of the chi^2 distribution (no toolbox required)
    P = gammainc(max(X,0)/2, Dof/2, 'upper');
end

function printSummary(Info)
    % Print the families and the decisions
    fprintf('aliasIdentifier: %d candidates, %d families, threshold %.3g (gap %.3g), n_ind = %.1f\n', ...
            numel(Info.Freq), Info.NumFamilies, Info.Threshold, Info.GapRatio, Info.NumIndependent);
    for F = 1:Info.NumFamilies
        M = find(Info.Families==F);
        fprintf('  Family %d: %s\n', F, mat2str(Info.Freq(M).', 6));
        if ~isempty(Info.Family)
            Fm = Info.Family(F);
            fprintf('    best %.6g (rho = %.1f, p = %.2g): %s\n', Fm.BestFreq, Fm.BestRho, Fm.Pvalue, Fm.Decision);
            for I = 1:numel(Fm.Alt)
                fprintf('      vs %-10.6g dchi2 obs %7.1f  pred %7.1f +- %5.1f  Pwrong %.2g (worst %.2g)\n', ...
                        Fm.AltFreq(I), Fm.DeltaChi2Obs(I), Fm.DeltaChi2Pred(I), ...
                        Fm.DeltaChi2PredStd(I), Fm.Pwrong(I), Fm.PwrongWorst(I));
            end
            for I = 1:numel(Fm.Alt)
                fprintf('      add %-9.6g dchi2 gain %6.1f  p %.2g\n', Fm.AltFreq(I), Fm.ExtraDeltaChi2(I), Fm.ExtraPvalue(I));
            end
        end
    end
end

function S = plural(N, Many, One)
    % Suffix for singular/plural
    if N==1
        S = One;
    else
        S = Many;
    end
end

function C = familyColors(Families)
    % One color per family; families with a single member are gray
    Nf   = max(Families);
    Nmem = accumarray(Families(:), 1, [Nf 1]);
    Multi = find(Nmem > 1);
    Base = lines(max(numel(Multi), 1));
    if numel(Multi) > size(Base,1) || numel(Multi) > 7
        Base = hsv(numel(Multi))*0.85;
    end
    C = repmat([0.6 0.6 0.6], Nf, 1);
    C(Multi,:) = Base(1:numel(Multi),:);
end

function Fig = makePlots(Info, Freq, Power, Kappa)
    % Diagnostic plots, one figure each: spectrum, pairwise coupling,
    % angles to the best member, singular values. Returns the figure handles.
    Kc   = numel(Info.Freq);
    Fams = Info.Families;
    Col  = familyColors(Fams);
    HasFit = ~isempty(Info.Family);
    if HasFit
        Best = [Info.Family.Best];
    else
        Best = zeros(1, Info.NumFamilies);
        for F = 1:Info.NumFamilies
            M = find(Fams==F);
            if all(isnan(Info.PeakPower(M)))
                Best(F) = M(1);
            else
                [~, I] = max(Info.PeakPower(M));
                Best(F) = M(I);
            end
        end
    end

    % (1) power spectrum or Delta chi^2 at the candidates
    Fig(1) = figure('Color', 'w', 'Name', 'aliasIdentifier: candidates and families');
    hold on
    if ~isempty(Power)
        plot(Freq, Power, '-', 'Color', [0.65 0.65 0.65]);
        Y = Info.PeakPower;
        Ylab = 'Power';
    elseif HasFit
        Y = Info.Fit.DeltaChi2;
        Ylab = '\Delta\chi^2';
    else
        Y = ones(Kc,1);
        Ylab = '';
    end
    for K = 1:Kc
        if isempty(Power)
            plot([1 1]*Info.Freq(K), [0 Y(K)], '-', 'Color', Col(Fams(K),:));
        end
        if any(Best==K)
            plot(Info.Freq(K), Y(K), 'p', 'MarkerSize', 12, 'MarkerFaceColor', Col(Fams(K),:), 'MarkerEdgeColor', 'k');
        else
            plot(Info.Freq(K), Y(K), 'o', 'MarkerSize', 6, 'MarkerFaceColor', Col(Fams(K),:), 'MarkerEdgeColor', 'k');
        end
    end
    if HasFit
        % decisions, in the color of each family
        Ytxt = 0.97;
        for F = 1:Info.NumFamilies
            if Ytxt < 0.4
                break
            end
            if sum(Fams==F) > 1 || ~strcmp(Info.Family(F).Decision, 'not significant')
                Txt = sprintf('%.5g: %s', Info.Family(F).BestFreq, Info.Family(F).Decision);
                if numel(Txt) > 70
                    Txt = [Txt(1:67), '...'];
                end
                text(0.98, Ytxt, Txt, 'Units', 'normalized', 'HorizontalAlignment', 'right', ...
                     'VerticalAlignment', 'top', 'Color', Col(F,:)*0.8, 'FontSize', 8, 'Interpreter', 'none');
                Ytxt = Ytxt - 0.07;
            end
        end
    end
    hold off
    box on
    xlabel('Frequency');
    ylabel(Ylab);
    title(sprintf('%d candidate%s, %d famil%s (star: best member)', Kc, plural(Kc, 's', ''), ...
          Info.NumFamilies, plural(Info.NumFamilies, 'ies', 'y')));

    % (2) cos(theta_1) matrix ordered by family
    Fig(2) = figure('Color', 'w', 'Name', 'aliasIdentifier: pairwise coupling');
    [~, Ord] = sortrows([Fams(:), Info.Freq(:)]);
    imagesc(Info.PairCos1(Ord,Ord), [0 1]);
    colormap(gca, flipud(gray(256)));
    colorbar;
    hold on
    Edges = find(diff(Fams(Ord)) ~= 0) + 0.5;
    for E = Edges(:).'
        plot([E E], [0.5 Kc+0.5], '-', 'Color', [0.85 0.2 0.2]);
        plot([0.5 Kc+0.5], [E E], '-', 'Color', [0.85 0.2 0.2]);
    end
    hold off
    Lab = arrayfun(@(X) sprintf('%.4g', X), Info.Freq(Ord), 'UniformOutput', false);
    set(gca, 'XTick', 1:Kc, 'XTickLabel', Lab, 'YTick', 1:Kc, 'YTickLabel', Lab, 'FontSize', 7);
    if Kc > 1
        set(gca, 'XTickLabelRotation', 90);
    end
    axis square
    title('cos\theta_1 between candidates (ordered by family)');

    % (3) sin(theta_1) of each member relative to the best member of its family
    Fig(3) = figure('Color', 'w', 'Name', 'aliasIdentifier: coupling to the best member');
    hold on
    for F = 1:Info.NumFamilies
        M = find(Fams==F);
        M = M(M~=Best(F));
        for K = M(:).'
            S = Info.PairSin1(Best(F), K);
            plot([1 1]*Info.Freq(K), [0 S], '-', 'Color', Col(F,:), 'LineWidth', 1.2);
            plot(Info.Freq(K), S, 'o', 'MarkerFaceColor', Col(F,:), 'MarkerEdgeColor', 'k');
        end
        if ~isempty(M) && ~isempty(Info.Rho) && Info.Rho(Best(F)) > 0
            Lim = 2*Kappa./Info.Rho(Best(F));
            if Lim <= 1
                Xl = get(gca, 'XLim');
                plot([min(Info.Freq) max(Info.Freq)], [Lim Lim], '--', 'Color', Col(F,:));
                set(gca, 'XLim', Xl);
            end
        end
    end
    hold off
    box on
    ylim([0 1]);
    xlabel('Frequency');
    ylabel('sin\theta_1 to best member');
    if isempty(Info.Rho)
        title('Coupling to the best member of each family');
    else
        title(sprintf('Dashed: %g\\sigma limit, sin\\theta_1 = 2\\kappa/\\rho', Kappa));
    end

    % (4) relative singular values
    Fig(4) = figure('Color', 'w', 'Name', 'aliasIdentifier: singular values');
    R = Info.RelativeSingularValues;
    Rp = max(R, 1e-16);
    semilogy(1:numel(R), Rp, 'ko', 'MarkerFaceColor', 'k', 'MarkerSize', 4);
    hold on
    Xl = [0.5, numel(R)+0.5];
    plot(Xl, [1 1]*Info.RequestedThreshold, '--', 'Color', [0.5 0.5 0.5]);
    plot(Xl, [1 1]*Info.Threshold, '-', 'Color', [0.85 0.2 0.2]);
    hold off
    xlim(Xl);
    ylim([max(min(Rp)/3, 1e-16), 2]);
    xlabel('Mode index j');
    ylabel('r_j');
    title(sprintf('Relative singular values; n_{ind} = %.1f', Info.NumIndependent));
end
