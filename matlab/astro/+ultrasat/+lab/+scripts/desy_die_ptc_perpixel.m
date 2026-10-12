% Stage 9: a PHOTON TRANSFER CURVE FITTED TO EVERY PIXEL, on each ladder apart.
%   Stage 5 fits the bright ladder per pixel. This fits the dark ladder the same
%   way and keeps the two separate, because the ensemble curves do not agree:
%   the dark ladder carries about 10 % less variance than the bright-ladder line
%   at the same measured signal. An ensemble comparison cannot say whether that
%   is something every pixel does or something a subset carries; the difference
%   of the two per-pixel gains can.
%   For each pixel and each ladder, x is that pixel's own mean signal at a step
%   and y its own TOTAL temporal variance there, so the fitted intercept is
%   RN^2 + g*T. Subtracting each pixel's own RN^2 first -- which the ensemble fits
%   of stage 10 do, and should -- is wrong HERE: RN^2_i is itself measured from
%   five frames, carries 50 % error with a long tail, and is subtracted once and
%   so lands entirely in the intercept. Doing it inflated the dark intercept's
%   width to 1.30 times its null, with the excess living wholly in the noisy
%   pixels: the width rises from 3.95 ADU^2 in the lowest read-noise decile,
%   which matches the null exactly, to 7.31 in the highest, tracking RN^2/sqrt(2),
%   the sampling error of the subtracted quantity. Fitting the total variance
%   instead brings the ratio to 1.08. An ensemble fit subtracts an average and
%   suffers none of this, fitted by weighted least squares with
%   the ensemble model as the weight (never the pixel's own variance: with 2
%   degrees of freedom that would weight a point by its own fluctuation).
%   Every parameter is read against a NULL in which all pixels are identical,
%   simulated through the whole measurement -- integer frames, the mean and the
%   variance taken from them, the same fit and the same weights -- because the
%   estimator is wide and skewed on its own: with 3 frames a single pixel's gain
%   is good to tens of per cent and its median sits about 10 % below the truth.
%   Output in DieOut: ptc_perpixel.json (histograms and statistics), and the
%   maps gainD.bin, interD.bin, gainB.bin, interB.bin (single, [Ny Nx]).
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 9: fitting a PTC to every pixel, each ladder apart\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol', 'ExpTimeOffset',DieExpOffset);
P.read;
P.subtractZero;
G    = P.rawColGeom;
Siz  = [G.Ny G.Nx];
RN2  = double(readBin(fullfile(DieStage1,'rn.bin'), Siz, 'stage 1')).^2;
Mask = [];
if isfile(fullfile(DieOut, 'mask.bin'))
    Fid  = fopen(fullfile(DieOut, 'mask.bin'), 'r');
    Mask = logical(fread(Fid, Siz, 'uint8'));
    fclose(Fid);
end
Nsim = 4e5;
RN2med = median(RN2(:), 'omitnan');
DofZ   = max(P.NZero-1, 1);
rng(41);
% The null keeps the read noise REAL. Each synthetic pixel is given a read-noise
% variance drawn from the measured map, so the null carries the long tail of noisy
% pixels that the die actually has; what the null asserts is only that every pixel
% shares one GAIN, which is the thing being tested. Giving every synthetic pixel
% the median read noise instead leaves the measured distribution looking far more
% tailed than the null for a reason that has nothing to do with the gain.
RN2sub = RN2(randperm(numel(RN2), Nsim));

% The dark ladder is capped at the linearity limit for the same reason the dark
% response fit is: on the high-dark-current setup its longest exposures reach
% 3829 ADU, where the INL and the start of the PTC variance dip would bend the
% per-pixel line this stage fits. On the low-dark-current setup the whole ladder
% is far below the limit and nothing is dropped.
Lad = struct('Name',{'Dark','Bright'}, 'Type',{'D','B'}, ...
             'Range',{[-Inf DieLinLimit], DieGainRange});
Out = struct();
Keep = struct();
for Il = 1:1:numel(Lad)
    Ty   = Lad(Il).Type;
    Flag = strcmp(P.Frames.FrameType, Ty);
    Steps = unique(P.Frames.Step(Flag)).';
    Mm = {};  Vv = {};  Sid = [];  Med = [];  Ven = [];  Vtt = [];  Nrp = [];
    for Is = 1:1:numel(Steps)
        A = ultrasat.lab.readPTC(DieDev, 'Test',P.Test, 'FrameType',Ty, 'Step',Steps(Is), 'Gain',DieGain, 'ExpTimeOffset',DieExpOffset);
        C = zeros([size(A(1).Image), numel(A)], 'single');
        for Ii = 1:1:numel(A)
            C(:,:,Ii) = single(A(Ii).Image);
        end
        Nf = size(C,3);
        M  = single(double(mean(C,3)) - double(P.Zero));
        Mk = mean(double(M(:)), 'omitnan');
        % 'signal' mode: the step list is the one the window stage chose, so that
        % this stage and the ensemble stages fit the very same points
        if exist('DieStepsExplicit','var') && DieStepsExplicit
            Want = DieFitStepsB;
            if strcmp(Ty,'D'), Want = DieFitStepsD; end
            if ~ismember(Steps(Is), Want)
                clear C M
                continue
            end
        elseif Mk < Lad(Il).Range(1) || Mk > Lad(Il).Range(2)
            clear C M
            continue
        end
        Vt  = var(double(C), 0, 3);              % total: what is fitted per pixel
        V   = Vt;
        Dof = max(Nf-1, 1);
        % one common mask for both axes: the pixels outside the top 0.1 % of the
        % variance, which is where the cosmic rays are
        Kp = isfinite(M) & isfinite(Vt) & Vt <= quantile(Vt(:), 1-1e-3);
        Mm{end+1} = M;                                                       %#ok<SAGROW>
        Vv{end+1} = single(V);                                               %#ok<SAGROW>
        Sid(end+1) = Steps(Is);                                              %#ok<SAGROW>
        Med(end+1) = mean(double(M(Kp)));                                    %#ok<SAGROW>
        Ven(end+1) = mean(Vt(Kp)) - mean(RN2(Kp));   % ensemble line: excess     %#ok<SAGROW>
        Vtt(end+1) = mean(Vt(Kp));         % TOTAL variance: what sets the noise %#ok<SAGROW>
        Nrp(end+1) = Nf;                                                     %#ok<SAGROW>
        clear C M V Vt
    end
    if numel(Sid)<3
        error('ultrasat:lab:scripts:ptc9', '%s ladder has only %d steps in range', Ty, numel(Sid));
    end
    % Ensemble line of this ladder. Unweighted, with means on both axes, which is
    % the unbiased estimator of the ensemble relation and the same convention the
    % stage 10 table uses -- an earlier version fitted weighted to per-step medians
    % and reported a gain 1 to 3 % different from the table for the same ladder.
    Am   = [ones(numel(Med),1), Med(:)];
    Cf   = Am\Ven(:);
    Ce   = Cf(1);  Ge = Cf(2);
    Dofs = max(Nrp-1,1);
    fprintf('  %s ladder: %d steps %s, ensemble Var-RN^2 = %.4f*S + %.2f (unweighted, means)\n', ...
        Lad(Il).Name, numel(Sid), mat2str(Sid), Ge, Ce);

    Sums = [];
    for Ii = 1:1:numel(Sid)
        Xi = double(Mm{Ii});
        Yi = double(Vv{Ii});
        % Var[y] = 2*Vtot^2/Dof, and Vtot is the TOTAL variance, read noise
        % included: subtracting RN^2 changes the mean of y, not how much it
        % fluctuates. Using the excess here instead would over-weight the lowest
        % dark steps several-fold, where the read noise is most of the total.
        Wi = Dofs(Ii)./(2.*Vtt(Ii).^2) + 0.*Xi;
        Wi(~isfinite(Xi) | ~isfinite(Yi)) = 0;
        Sums = ultrasat.lab.PTCAnalysis.accumulateFit(Sums, Yi, Xi, Wi, [-Inf Inf]);
    end
    F = ultrasat.lab.PTCAnalysis.solveFit(Sums);
    clear Sums
    % Weight sensitivity. Var[y] = 2*Vtot^2/Dof depends on the STEP, not on the
    % pixel, so the right weight is one scalar per step -- that is what the fit
    % above uses. An earlier version weighted per pixel by the ensemble model
    % evaluated at that pixel's own signal, 1/(g*x+c)^2, which is a different
    % estimator: it lets a pixel's own brightness decide how much each of its
    % points counts. Refitting that way here costs nothing (the maps are already
    % in memory) and says how much the choice moves the answer.
    S2 = [];
    for Ii = 1:1:numel(Sid)
        Xi = double(Mm{Ii});
        Yi = double(Vv{Ii});
        Wm = Dofs(Ii)./(2.*max(Ge.*Xi + Ce, 1).^2);
        Wm(~isfinite(Xi) | ~isfinite(Yi)) = 0;
        S2 = ultrasat.lab.PTCAnalysis.accumulateFit(S2, Yi, Xi, Wm, [-Inf Inf]);
    end
    F2 = ultrasat.lab.PTCAnalysis.solveFit(S2);
    clear S2 Mm Vv
    writeBin(fullfile(DieOut, ['gain' Ty '.bin']),  F.Slope);
    writeBin(fullfile(DieOut, ['inter' Ty '.bin']), F.Intercept);
    Keep.(Ty) = struct('Slope',single(F.Slope), 'Inter',single(F.Intercept));

    % null: identical pixels, taken through the whole measurement
    N = local_null(Med, Vtt - mean(RN2sub), Nrp, Dofs, RN2sub, DofZ, Nsim);
    % the per-pixel fit is on the TOTAL variance, so neither data nor null
    % subtracts a read noise and the intercept is RN^2 + g*T in both
    fprintf('    null: truth %.4f -> mean %.4f, median %.4f (%+.1f %%), MAD %.4f; %.0f s\n', ...
        Ge, N.SlopeMean, N.SlopeMedian, 100*(N.SlopeMedian/Ge-1), N.SlopeMAD, toc(T0));

    Use = isfinite(F.Slope) & isfinite(F.Intercept);
    if ~isempty(Mask)
        Use = Use & Mask;
    end
    Q = struct('Name',Lad(Il).Name, 'Type',Ty, 'Steps',Sid, 'StepMedian',Med, ...
               'StepVariance',Ven, 'Nframes',Nrp, 'GainEnsemble',Ge, 'OffsetEnsemble',Ce, ...
               'Npix',nnz(Use), 'Null',N);
    Q.Slope  = local_stat(F.Slope(Use),     N.Slope);
    Q.Inter  = local_stat(F.Intercept(Use), N.Inter);
    Q.Chi2   = local_stat(F.Chi2Dof(Use),   N.Chi2);
    Q.Joint  = local_joint(F.Slope(Use), F.Intercept(Use), Q.Slope, Q.Inter);
    Q.JointNull = local_joint(N.Slope, N.Inter, Q.Slope, Q.Inter);
    % Covariance of the two parameters, three ways. The ANALYTIC one is what the
    % fit itself predicts for each pixel, -Swx/D, and is strongly negative in any
    % straight-line fit whose points sit away from x = 0: raise the slope and the
    % intercept must fall. The MEASURED one is the covariance of the two maps over
    % pixels, and the NULL one is the same for identical pixels. Measured against
    % null is the test; the analytic value says how much of it the fit alone
    % explains, pixel by pixel.
    % Plain cov is dominated by the tails of a heavy-tailed pair, so the same
    % numbers are also computed on a common central window -- within 5 robust
    % sigmas of each parameter's median, the identical cut applied to the data and
    % to the null -- which is what makes the two comparable.
    Sv = double(F.Slope(Use));  Iv = double(F.Intercept(Use));
    Cm = cov(Sv, Iv);
    Cn = cov(double(N.Slope), double(N.Inter));
    Km = abs(Sv - Q.Slope.Median) < 5.*Q.Slope.NullMAD & abs(Iv - Q.Inter.Median) < 5.*Q.Inter.NullMAD;
    Kn = abs(N.Slope - Q.Slope.NullMedian) < 5.*Q.Slope.NullMAD & ...
         abs(N.Inter - Q.Inter.NullMedian) < 5.*Q.Inter.NullMAD;
    Cmr = cov(Sv(Km), Iv(Km));
    Cnr = cov(double(N.Slope(Kn)), double(N.Inter(Kn)));
    Q.Cov = struct('Analytic',median(double(F.CovSlopeIntercept(Use)), 'omitnan'), ...
                   'AnalyticMean',mean(double(F.CovSlopeIntercept(Use)), 'omitnan'), ...
                   'Measured',Cm(1,2), 'Null',Cn(1,2), ...
                   'CorrMeasured',Cm(1,2)./sqrt(Cm(1,1).*Cm(2,2)), ...
                   'CorrNull',Cn(1,2)./sqrt(Cn(1,1).*Cn(2,2)), ...
                   'CorrAnalytic',median(double(F.CovSlopeIntercept(Use))./ ...
                        sqrt(double(F.VarSlope(Use)).*double(F.VarIntercept(Use))), 'omitnan'), ...
                   'MeasuredRobust',Cmr(1,2), 'NullRobust',Cnr(1,2), ...
                   'CorrMeasuredRobust',Cmr(1,2)./sqrt(Cmr(1,1).*Cmr(2,2)), ...
                   'CorrNullRobust',Cnr(1,2)./sqrt(Cnr(1,1).*Cnr(2,2)), ...
                   'KeptFracMeasured',nnz(Km)./numel(Km), 'KeptFracNull',nnz(Kn)./numel(Kn));
    Q.Cov.Ratio       = Q.Cov.Measured./Q.Cov.Null;
    % The same fit read on Var - RN^2 instead of on the total variance. The slope
    % is identical -- subtracting a per-pixel CONSTANT cannot tilt a line -- so only
    % the intercept moves, by exactly that constant, and no refit is needed. This is
    % what an earlier version fitted, and it is kept here to show why it should not
    % be: the error of RN^2_i, measured from DofZ+1 frames, is subtracted once and
    % therefore lands entirely in the intercept.
    Ie = double(F.Intercept(Use)) - RN2(Use);
    Ne = double(N.Inter) - double(N.RN2Est);
    Q.InterExcess = local_stat(Ie, Ne);
    % and where that extra width lives: binned by the pixel's own read noise
    Rv = RN2(Use);
    Ed = quantile(Rv, 0:0.1:1);
    Dc = struct('Edges',Ed, 'RN2',nan(1,10), 'MADexcess',nan(1,10), 'MADtotal',nan(1,10), ...
                'Npix',nan(1,10), 'Predicted',nan(1,10));
    It = double(F.Intercept(Use));
    for Id = 1:1:10
        Kd = Rv>=Ed(Id) & Rv<Ed(Id+1);
        if nnz(Kd)<1000, continue; end
        Dc.Npix(Id)      = nnz(Kd);
        Dc.RN2(Id)       = median(Rv(Kd));
        Dc.MADexcess(Id) = 1.4826.*median(abs(Ie(Kd) - median(Ie(Kd))));
        Dc.MADtotal(Id)  = 1.4826.*median(abs(It(Kd) - median(It(Kd))));
        Dc.Predicted(Id) = Dc.RN2(Id).*sqrt(2./DofZ);   % sampling error of RN^2 itself
    end
    Q.ReadNoiseDecile = Dc;
    Sv2 = double(F2.Slope(Use));
    Sv2 = sort(Sv2(isfinite(Sv2)));
    Nt2 = max(round(0.001.*numel(Sv2)), 1);
    Q.ModelWeighted = struct('Mean',mean(Sv2), 'TrimMean',mean(Sv2(Nt2+1:end-Nt2)), ...
                             'Median',median(Sv2), ...
                             'MAD',1.4826.*median(abs(Sv2-median(Sv2))));
    clear F2
    Q.Cov.RatioRobust = Q.Cov.MeasuredRobust./Q.Cov.NullRobust;
    Out.(Ty) = Q;
    clear F
end

% the comparison the ensemble cannot make: the two gains of the SAME pixel
Dg  = double(Keep.D.Slope) - double(Keep.B.Slope);
Ok  = isfinite(Dg);
if ~isempty(Mask)
    Ok = Ok & Mask;
end
Nd  = Out.D.Null.Slope;  Nb = Out.B.Null.Slope;
Nn  = min(numel(Nd), numel(Nb));
Dn  = Nd(1:Nn) - Nb(randperm(Nn));                 % independent pixels, so the null difference
Out.Difference = local_stat(Dg(Ok), Dn);
% The MEAN is the one to quote. The per-pixel estimator is skewed, so its median
% sits well below the truth on both ladders (-5.9 % dark, -13.6 % bright), and the
% median of the difference inherits both biases unequally: it reads -0.7 % where
% the means differ by -9.3 %.
% Trimmed, not plain. Both per-pixel gains are heavy-tailed, and the plain mean of
% their difference is carried by the tails: it reads +0.040 where the trimmed mean
% reads the physical -0.11.
Out.Difference.MeanRel     = Out.Difference.TrimMean./Out.B.Slope.TrimMean;
Out.Difference.PlainMeanRel = Out.Difference.Mean./Out.B.Slope.Mean;
Out.Difference.MedianRel = Out.Difference.Median./Out.B.Slope.Median;
writeBin(fullfile(DieOut, 'gain_diff.bin'), Dg);

% Independent check on what survives: the PTC intercept and the stage 2 response
% fit both measure T, by routes that share no data beyond the frames themselves.
% If a common T of width sigma_T is in both, they must correlate by
% sigma_T^2/(sa*sb), so the measured r inverts to a shared spread.
Cross = struct('Available',false);
if isfile(fullfile(DieOut,'tdark.bin'))
    Td = readBin(fullfile(DieOut,'tdark.bin'), Siz, 'stage 2');
    Ug = isfinite(Keep.D.Slope) & isfinite(Td);
    if ~isempty(Mask), Ug = Ug & Mask; end
    Ta = (double(Keep.D.Inter(Ug)) - RN2(Ug))./Out.D.GainEnsemble;   % T from the PTC
    Tb = double(Td(Ug));                                            % T from the response
    Ka = abs(Ta-median(Ta)) < 5.*1.4826.*median(abs(Ta-median(Ta)));
    Kb = abs(Tb-median(Tb)) < 5.*1.4826.*median(abs(Tb-median(Tb)));
    Kk = Ka & Kb;
    Cc = corrcoef(Ta(Kk), Tb(Kk));
    Sa = 1.4826.*median(abs(Ta(Kk)-median(Ta(Kk))));
    Sb = 1.4826.*median(abs(Tb(Kk)-median(Tb(Kk))));
    Cross = struct('Available',true, 'Npix',nnz(Kk), 'Corr',Cc(1,2), ...
                   'WidthPTC',Sa, 'WidthResponse',Sb, ...
                   'SharedSigmaT',sqrt(max(Cc(1,2),0).*Sa.*Sb), ...
                   'MedianPTC',median(Ta(Kk)), 'MedianResponse',median(Tb(Kk)));
    fprintf(['\ntwo threshold maps, two routes: r = %+.4f over %.1f M pixels (widths %.2f and %.2f ADU)\n', ...
             '  -> a shared threshold spread of %.2f ADU\n'], Cross.Corr, Cross.Npix./1e6, Sa, Sb, ...
             Cross.SharedSigmaT);
end

S = struct('Stage',9, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, 'Size',Siz, ...
           'Nsim',Nsim, 'GainRange',DieGainRange, 'DofZ',DofZ, 'Ladder',Out, 'Cross',Cross);
Fid = fopen(fullfile(DieOut, 'ptc_perpixel.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf('\n%-8s %9s %9s %9s %9s %9s %9s %9s\n', ...
    'ladder', 'g mean', 'g trim', 'ensemble', 'g median', 'null med', 'MAD/null', 'chi2 med');
for Ty = {'D','B'}
    Q = Out.(Ty{1});
    fprintf('%-8s %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.3f\n', Q.Name, Q.Slope.Mean, ...
        Q.Slope.TrimMean, Q.GainEnsemble, Q.Slope.Median, Q.Null.SlopeMedian, ...
        Q.Slope.MADoverNull, Q.Chi2.Median);
end
fprintf(['  the plain mean is tail-sensitive: the per-pixel estimator is heavy-tailed and even the\n', ...
    '  null''s own mean misses its truth by 3 %%, so the trimmed mean is the one to compare with\n', ...
    '  the ensemble value.\n']);
fprintf('\nweight sensitivity (per-step scalar, as used, against the per-pixel model 1/(g*x+c)^2):\n');
for Ty = {'D','B'}
    Q = Out.(Ty{1});
    fprintf('  %-7s mean %7.4f -> %7.4f   trimmed %7.4f -> %7.4f   median %7.4f -> %7.4f\n', ...
        Q.Name, Q.Slope.Mean, Q.ModelWeighted.Mean, Q.Slope.TrimMean, Q.ModelWeighted.TrimMean, ...
        Q.Slope.Median, Q.ModelWeighted.Median);
end
fprintf('\nintercept (RN^2 + g*T, the per-pixel fit is on the total variance): dark %.2f, bright %.2f ADU^2\n', ...
    Out.D.Inter.Mean, Out.B.Inter.Mean);
fprintf('  width / null: %.3f (dark) and %.3f (bright). Fitting Var - RN^2 instead would give\n', ...
    Out.D.Inter.MADoverNull, Out.B.Inter.MADoverNull);
fprintf('  %.3f and %.3f -- the error of the subtracted RN^2 lands entirely in the intercept.\n', ...
    Out.D.InterExcess.MADoverNull, Out.B.InterExcess.MADoverNull);
fprintf('  by read-noise decile (dark, Var - RN^2): ');
for Id = 1:1:10
    fprintf('%.1f ', Out.D.ReadNoiseDecile.MADexcess(Id));
end
fprintf('ADU^2\n    against RN^2/sqrt(2) = ');
for Id = 1:1:10
    fprintf('%.1f ', Out.D.ReadNoiseDecile.Predicted(Id));
end
fprintf('\n');
fprintf(['\nCovariance of the two fit parameters. The analytic value is what the fit predicts for\n', ...
    'each pixel (-Swx/D); the null is identical pixels put through the same measurement. Plain cov\n', ...
    'is tail-dominated, so the robust columns repeat it on a common central window.\n\n']);
fprintf('%-8s %11s %11s %11s | %8s %8s %8s | %9s %9s\n', 'ladder', 'cov meas', 'cov null', ...
    'cov fit', 'r meas', 'r null', 'r fit', 'r meas(rb)', 'r null(rb)');
for Ty = {'D','B'}
    Cv = Out.(Ty{1}).Cov;
    fprintf('%-8s %11.4g %11.4g %11.4g | %8.4f %8.4f %8.4f | %9.4f %9.4f\n', Out.(Ty{1}).Name, ...
        Cv.Measured, Cv.Null, Cv.Analytic, Cv.CorrMeasured, Cv.CorrNull, Cv.CorrAnalytic, ...
        Cv.CorrMeasuredRobust, Cv.CorrNullRobust);
end
fprintf(['same pixel, dark gain minus bright gain: trimmed mean %+.4f ADU/e- (%+.1f %% of the\n', ...
    '  bright gain). Neither the plain mean (%+.4f, carried by the read-noise tail that\n', ...
    '  subtracting RN_i^2 introduces) nor the median (%+.4f, skewed) is the number to quote.\n'], ...
    Out.Difference.TrimMean, 100.*Out.Difference.MeanRel, Out.Difference.Mean, Out.Difference.Median);
fprintf('  spread %.4f against a null of %.4f -> ratio %.3f\n', ...
    Out.Difference.MAD, Out.Difference.NullMAD, Out.Difference.MADoverNull);
fprintf('[%4.0f s] PERPIXEL PTC DONE -> %s\n', toc(T0), DieOut);

function N = local_null(Med, Vex, Nrp, Dofs, RN2true, DofZ, Nsim)
    % Identical pixels taken through the whole measurement: integer frames drawn
    % with the TOTAL variance, the mean and variance computed from them, a read
    % noise subtracted that carries its own sampling error exactly as the measured
    % map does, and the same weights. Drawing the frames with the EXCESS variance
    % instead -- which an earlier version did -- leaves the null too quiet at the
    % low dark steps, where the read noise is most of the total, and the measured
    % distribution then looks far wider than it is.
    Ns  = numel(Med);
    Sw=0; Swx=0; Swy=0; Swxx=0; Swxy=0; Swyy=0;
    % each synthetic pixel: its own true read noise, and its own noisy ESTIMATE of
    % it, measured from DofZ+1 zero-exposure frames exactly as the map was
    Rt  = double(RN2true(:));
    Rn  = Rt.*sum(randn(Nsim, DofZ).^2, 2)./DofZ;   % each pixel's noisy RN^2 estimate
    for I = 1:1:Ns
        Vt = Rt + Vex(I);                            % total variance of this pixel
        Fr = round(Med(I) + sqrt(max(Vt,0)).*randn(Nsim, Nrp(I)));
        Xi = mean(Fr, 2);
        Yi = var(Fr, 0, 2);
        Wi = Dofs(I)./(2.*mean(Vt).^2) + 0.*Xi;
        Sw=Sw+Wi; Swx=Swx+Wi.*Xi; Swy=Swy+Wi.*Yi;
        Swxx=Swxx+Wi.*Xi.^2; Swxy=Swxy+Wi.*Xi.*Yi; Swyy=Swyy+Wi.*Yi.^2;
    end
    D  = Sw.*Swxx - Swx.^2;
    Sl = (Sw.*Swxy - Swx.*Swy)./D;
    In = (Swy.*Swxx - Swx.*Swxy)./D;
    Ch = (Swyy - 2.*In.*Swy - 2.*Sl.*Swxy + In.^2.*Sw + 2.*In.*Sl.*Swx + Sl.^2.*Swxx)./max(Ns-2,1);
    Tr = NaN;  %#ok<NASGU>
    N  = struct('Nsim',Nsim, 'Slope',Sl, 'Inter',In, 'Chi2',max(Ch,0), 'RN2Est',Rn, ...
                'SlopeMean',mean(Sl), 'SlopeMedian',median(Sl), ...
                'SlopeMAD',1.4826.*median(abs(Sl-median(Sl))));
end

function Q = local_stat(V, Vn)
    V  = double(V(:));  V = V(isfinite(V));
    Vn = double(Vn(:)); Vn = Vn(isfinite(Vn));
    Md = median(V);  Mn = median(Vn);
    % The mean of a per-pixel slope is tail-sensitive whatever is fitted: the
    % estimator itself is heavy-tailed, and even the null's own mean misses its
    % truth by 3 %. The symmetric 0.2 % trimmed mean is quoted beside it and is
    % the one to compare with the ensemble value.
    Vs = sort(V);
    Nt = max(round(0.001.*numel(Vs)), 1);
    Q  = struct('N',numel(V), 'Mean',mean(V), 'TrimMean',mean(Vs(Nt+1:end-Nt)), ...
                'Median',Md, 'Std',std(V), ...
                'MAD',1.4826.*median(abs(V-Md)), ...
                'NullMean',mean(Vn), 'NullMedian',Mn, 'NullStd',std(Vn), ...
                'NullMAD',1.4826.*median(abs(Vn-Mn)));
    Q.MADoverNull = Q.MAD./Q.NullMAD;
    Lo = Md - 6.*Q.NullMAD;  Hi = Md + 6.*Q.NullMAD;
    Ed = linspace(Lo, Hi, 401);
    Q.Edges = Ed;
    Q.Counts     = histcounts(V, Ed);
    Q.CountsNull = histcounts(Vn + (Md - Mn), Ed);   % null centred on the measured median
    Q.NullShift  = Md - Mn;
end

function J = local_joint(Sl, In, Qs, Qi)
    Sl = double(Sl(:));  In = double(In(:));
    Es = linspace(Qs.Median-5.*Qs.NullMAD, Qs.Median+5.*Qs.NullMAD, 201);
    Ei = linspace(Qi.Median-5.*Qi.NullMAD, Qi.Median+5.*Qi.NullMAD, 201);
    J  = struct('EdgesSlope',Es, 'EdgesInter',Ei, ...
                'Counts',histcounts2(Sl, In, Es, Ei), ...
                'Corr',local_corr(Sl, In));
end

function R = local_corr(A, B)
    Ok = isfinite(A) & isfinite(B);
    C  = corrcoef(A(Ok), B(Ok));
    R  = C(1,2);
end

function A = readBin(Path, Siz, Who)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage', 'stage 9 needs %s from %s', Path, Who);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
        A = single(A);
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end
