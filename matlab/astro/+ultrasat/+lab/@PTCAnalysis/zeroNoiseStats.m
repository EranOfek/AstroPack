function S = zeroNoiseStats(Obj, Args)
    % Bias and read-noise decomposition of the ZE frames, per pixel.
    %   Extends zeroStats (which reports the plain medians) with the
    %   quantities needed to compare setups in the individual-pixel regime:
    %     common mode   - the clipped MEAN level of each ZE frame (the
    %                     median of 10^4 integer-valued pixels is quantised
    %                     to whole ADU and would read exactly zero drift);
    %                     a frame-to-frame
    %                     offset (drift or pickup) that a reference/overscan
    %                     can remove, so its frame-to-frame deviation is
    %                     measured, reported and (by default) subtracted
    %                     before anything else; the mean level stays in, so
    %                     BiasLevel remains the absolute bias
    %     fixed pattern - the spatial spread of the bias map with its own
    %                     sampling noise mean(sigma^2)/Nframes removed in
    %                     quadrature, plus a row / column / residual
    %                     decomposition and the lag autocorrelations
    %     read noise    - the per-pixel std over the frames: its median, its
    %                     rms, a chi2-median-corrected robust estimate, its
    %                     tail fraction, and the INTRINSIC pixel-to-pixel
    %                     spread from varSpread (with only Nframes frames
    %                     the raw spread of the noise map is dominated by
    %                     chi2 sampling scatter and means nothing by itself)
    %   All of it for all pixels and, when Parity='rawcol', separately for
    %   the even and odd readout columns.
    % Input  : * ...,key,val,...
    %            'Mask'  - logical map of the pixels to use ([] = all); pass
    %                      badColumns.GoodMask to drop the bad columns.
    %            'RemoveCommonMode' - subtract the DEVIATION of each frame's
    %                      median from the mean of those medians before
    %                      computing the noise (default true), so the
    %                      frame-to-frame offset jitter does not inflate the
    %                      per-pixel read noise while the absolute bias
    %                      level is preserved. The un-subtracted read noise
    %                      is reported either way as ReadNoiseMedianRaw.
    %            'TailFactor' - tail threshold in units of the median read
    %                      noise (default 2).
    %            'MaxLag' - highest lag of the autocorrelations (default 3).
    %            'Frames' - indices of the ZE frames to use ([] = all).
    %                      Runs 39 / 39-2 have 10 ZE frames instead of 5, so
    %                      their noise spread is far better constrained;
    %                      'Frames',1:5 gives a point directly comparable
    %                      with the other runs.
    % Output : - Structure with Nframes, Dof, CommonMode (Levels, Std, PtP),
    %            Mask info, All / Even / Odd sub-structures (see below) and
    %            Structure (row/column/residual spread and lag correlations
    %            of the bias map, with ReadoutDim telling which image
    %            dimension the readout columns run along).
    %            Each of All / Even / Odd has: Npix, BiasLevel, BiasMean,
    %            FixedPatternObs, FixedPatternNoise, FixedPatternRMS,
    %            FixedPatternRel, ReadNoiseMedian, ReadNoiseMedianRaw,
    %            ReadNoiseRMS, ReadNoiseRobust, TailFrac, Spread (varSpread
    %            of the noise map) and SpreadSigmaRel (the same spread
    %            expressed as a relative spread of sigma, ~half of the
    %            relative spread of sigma^2).
    % Example: P.run;  B = P.badColumns;  Z = P.zeroNoiseStats('Mask',B.GoodMask)
    arguments
        Obj
        Args.Mask = [];
        Args.RemoveCommonMode logical = true;
        Args.TailFactor (1,1) double = 2;
        Args.MaxLag (1,1) double = 3;
        Args.Frames = [];
    end
    if ~strcmp(Obj.Mode, 'region')
        error('ultrasat:lab:PTCAnalysis:mode', 'zeroNoiseStats needs region mode (the ZE cube is not kept in full mode)');
    end
    Cube = double(Obj.loadFrames('ZE', []));
    if ~isempty(Args.Frames) && ~isempty(Cube)
        Cube = Cube(:,:,Args.Frames);
    end
    if isempty(Cube) || size(Cube,3)<2
        error('ultrasat:lab:PTCAnalysis:noZero', 'Need at least 2 ZE frames in %s', Obj.DeviceDir);
    end
    Nf  = size(Cube, 3);
    Dof = Nf - 1;
    Mask = Args.Mask;
    if isempty(Mask)
        Mask = true(size(Cube,1), size(Cube,2));
    end
    Mask = Mask & all(isfinite(Cube), 3);

    % per-frame common mode
    C2 = reshape(Cube, [], Nf);
    CM = zeros(1, Nf);                                % 5-sigma clipped mean per frame
    for If = 1:1:Nf
        V  = C2(Mask(:), If);
        V  = V(isfinite(V));
        Mv = median(V);
        Sv = 1.4826.*median(abs(V - Mv));
        if Sv>0
            V = V(abs(V - Mv) <= 5.*Sv);
        end
        CM(If) = mean(V);
    end
    S  = struct('Nframes',Nf, 'Dof',Dof, 'Npix',nnz(Mask), 'RemoveCommonMode',Args.RemoveCommonMode, ...
                'TailFactor',Args.TailFactor);
    S.CommonMode = struct('Levels',CM, 'Mean',mean(CM), 'Std',std(CM), 'PtP',max(CM)-min(CM));
    if Args.RemoveCommonMode
        R = Cube - reshape(CM - mean(CM), 1, 1, Nf);   % only the frame-to-frame deviation
    else
        R = Cube;
    end
    Bias    = Obj.combine(single(R));
    Bias    = double(Bias);
    Sigma2  = var(R, 0, 3);
    Sig2Raw = var(Cube, 0, 3);
    Chi2Med = 2.*gammaincinv(0.5, Dof./2);            % median of chi2(Dof)

    S.All = local_stat(Mask);
    if ~isempty(Obj.ParityMap)
        S.Even = local_stat(Mask & ~Obj.ParityMap);
        S.Odd  = local_stat(Mask &  Obj.ParityMap);
    end

    % spatial structure of the bias map
    G = Obj.rawColGeom;
    B = Bias;  B(~Mask) = NaN;
    Rm = mean(B, 2, 'omitnan');  Cm = mean(B, 1, 'omitnan');  Gm = mean(B(Mask));
    Res = B - Rm - Cm + Gm;
    S.Structure = struct('ReadoutDim',G.Dim, 'RowMeanStd',std(Rm,'omitnan'), ...
                         'ColMeanStd',std(Cm,'omitnan'), 'ResidStd',std(Res(isfinite(Res))), ...
                         'MaxLag',Args.MaxLag);
    S.Structure.LagDim1 = local_lag(Res, 1, Args.MaxLag);
    S.Structure.LagDim2 = local_lag(Res, 2, Args.MaxLag);

    function Q = local_stat(M)
        % ensemble statistics of one pixel subset
        N  = nnz(M);
        Q  = struct('Npix',N);
        if N<2
            return
        end
        Bm = Bias(M);  V = Sigma2(M);  Vr = Sig2Raw(M);
        Q.BiasLevel = median(Bm);
        Q.BiasMean  = mean(Bm);
        Q.FixedPatternObs   = std(Bm);
        Q.FixedPatternNoise = sqrt(mean(V)./Nf);
        Q.FixedPatternRMS   = sqrt(max(Q.FixedPatternObs.^2 - Q.FixedPatternNoise.^2, 0));
        Q.FixedPatternRel   = Q.FixedPatternRMS./abs(Q.BiasLevel);
        Q.ReadNoiseMedian    = median(sqrt(V));
        Q.ReadNoiseMedianRaw = median(sqrt(Vr));
        Q.ReadNoiseRMS       = sqrt(mean(V));
        Q.ReadNoiseRobust    = sqrt(median(V).*Dof./Chi2Med);
        Q.TailFrac = mean(sqrt(V) > Args.TailFactor.*Q.ReadNoiseMedian);
        Q.Spread   = ultrasat.lab.PTCAnalysis.varSpread(V, Dof);
        Q.SpreadSigmaRel   = Q.Spread.RelIntr./2;      % d(sigma)/sigma = 0.5*d(var)/var
        Q.SpreadSigmaUL95  = Q.Spread.RelUL95./2;
    end
end

function L = local_lag(Res, Dim, MaxLag)
    % autocorrelation of a map along one dimension, lags 1..MaxLag
    L = NaN(1, MaxLag);
    N = size(Res, Dim);
    for I = 1:1:min(MaxLag, N-1)
        if Dim==1
            A = Res(1:end-I,:);  B = Res(1+I:end,:);
        else
            A = Res(:,1:end-I);  B = Res(:,1+I:end);
        end
        Ok = isfinite(A) & isfinite(B);
        if nnz(Ok)>2
            Cc = corrcoef(A(Ok), B(Ok));
            L(I) = Cc(1,2);
        end
    end
end
