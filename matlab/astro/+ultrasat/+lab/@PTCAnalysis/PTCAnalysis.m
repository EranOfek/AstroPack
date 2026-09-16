classdef PTCAnalysis < Component
    % Reproduce the DESY PTC / dark-current / threshold analysis of a lab device.
    %   Works on the frames of one device directory (see ultrasat.lab.readPTC):
    %   ZE (zero exposure), D (dark, exposure ladder) and B (bright,
    %   intensity ladder) TIFF frames plus the PTC_Config.xlsx sidecar.
    %   Pipeline (run):
    %     read          - inventory of frames and sidecars
    %     subtractZero  - bias frame from the ZE frames (Combiner)
    %     combineSteps  - per step: combined signal, temporal / frame-difference /
    %                     spatial variance
    %     fitResponse   - per-pixel linear fit of signal vs exposure (D) or
    %                     vs intensity (B), using only the steps whose signal
    %                     lies inside FitRange (as in DESY UC-3400-TN175-05),
    %                     or the steps listed in FitSteps
    %     fitGain       - PTC gain [ADU/e-] from variance vs mean (B ladder)
    %     threshold     - dark method: -Intercept; light method:
    %                     -(Intercept + DC*ExpSen); ADU and e-
    %   Frames are read with ultrasat.lab.readPTC: by default the high-gain
    %   half in the DESY orientation (see its Gain / Orient arguments).
    %   Two modes: 'region' (CCDSEC read into memory, default the DESY
    %   100x100 region) and 'full' (whole die, streamed step by step; only
    %   the fit maps are kept).
    %   The conversion gain for ADU->e- is the measured PTC gain unless the
    %   Gain property is set (override, e.g. the DESY 1.05 ADU/e-).
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: P = ultrasat.lab.PTCAnalysis('/data/LOT_TH02954_W04_D07');
    %          P.run;  S = P.summary;
    %          P.plotResponse('D'); P.plotHistograms('D'); P.plotPTC;
    %          F = ultrasat.lab.PTCAnalysis('/data/LOT_TH02954_W04_D07', 'CCDSEC',[]);
    %          F.run;  F.plotMaps('D');

    properties
        % input
        DeviceDir  = '';                      % device directory
        Test       = 'PTC_int_hr';            % test sub-directory / frame tag
        CCDSEC     = [1361 1460 1861 1960];   % [Xmin Xmax Ymin Ymax] in the returned orientation; [] = full die (DESY rows 1860:1960, cols 1360:1460, 0-based)
        Gain       = 'high';                  % readPTC 'Gain': 'high' | 'low' | 'raw'
        Orient     = 'desy';                  % readPTC 'Orient': 'desy' | 'tiff'
        % options
        Combiner   = 'mean';                  % 'mean' | 'median' for ZE frames and step repeats
        FitRange   = [1000 2500];             % [ADU] signal window of the response fits; 1x2 for both types or 2x2 (row 1 = D, row 2 = B)
        FitSteps   = struct('D',[], 'B',[]);  % explicit step numbers to fit (overrides FitRange when non-empty)
        IntensityScale = 1000;                % bright X = Bright_Intensity * IntensityScale ("int" of the DESY plots = config value x 1000)
        GainRange  = [300 2500];              % [ADU] mean-signal window of the PTC gain fit (below the 3-5 kADU variance dip; validated against the deck)
        SatLevel   = 15000;                   % [ADU] steps with a mean above this are excluded from the gain fit (ADC 16383 - zero ~400 saturates at ~15985)
        GainADU    = [];                      % [ADU/e-] conversion override for ADU->e-; [] = measured PTC gain
        GainEstimator = 'temporal';           % PTC variance estimator used for GainUsed: 'temporal' | 'diff' | 'spatial'
        ExpSen     = [];                      % [s] sensor exposure of the bright frames; [] = PTC_ExpTime
        Verbosity  = 0;
    end

    properties (SetAccess = protected)
        Mode       = '';                      % 'region' | 'full'
        Frames                                % table from readPTC (inventory)
        Sidecar                               % Result, Log, Config, Calib
        Info       = struct;                  % Lot, Wafer, Device, Base
        AI                                    % AstroImage array (region mode only)
        Zero                                  % bias frame (single)
        ZeroNoise                             % per-pixel std of the ZE frames
        NZero      = 0;
        Dark       = struct;                  % ladder of the D frames (see combineSteps)
        Bright     = struct;                  % ladder of the B frames
        DarkFit    = struct;                  % fitResponse('D')
        BrightFit  = struct;                  % fitResponse('B')
        PTC        = struct;                  % fitGain
        Threshold  = struct;                  % threshold
    end

    methods % constructor
        function Obj = PTCAnalysis(DeviceDir, Args)
            % Construct a PTCAnalysis object.
            % Input  : - Device directory. Default is ''.
            %          * ...,key,val,... any public property, e.g.
            %            'Test', 'CCDSEC', 'Gain', 'Orient', 'Combiner',
            %            'FitRange', 'FitSteps', 'IntensityScale', 'GainRange',
            %            'SatLevel', 'GainADU', 'GainEstimator', 'ExpSen', 'Verbosity'.
            % Output : - A PTCAnalysis object (nothing read yet).
            % Example: P = ultrasat.lab.PTCAnalysis(Dir, 'CCDSEC',[1 200 1 200]);
            arguments
                DeviceDir           = '';
                Args.Test           = 'PTC_int_hr';
                Args.CCDSEC         = [1361 1460 1861 1960];
                Args.Gain           = 'high';
                Args.Orient         = 'desy';
                Args.Combiner       = 'mean';
                Args.FitRange       = [1000 2500];
                Args.FitSteps       = struct('D',[], 'B',[]);
                Args.IntensityScale = 1000;
                Args.GainRange      = [300 2500];
                Args.SatLevel       = 15000;
                Args.GainADU        = [];
                Args.GainEstimator  = 'temporal';
                Args.ExpSen         = [];
                Args.Verbosity      = 0;
            end
            Obj.DeviceDir = DeviceDir;
            Fn = fieldnames(Args);
            for If=1:1:numel(Fn)
                Obj.(Fn{If}) = Args.(Fn{If});
            end
        end
    end

    methods % pipeline
        function Obj = run(Obj)
            % Run the whole analysis: read, subtractZero, combineSteps,
            % fitResponse (D and B), fitGain, threshold.
            % Example: P.run
            Obj.read;
            Obj.subtractZero;
            Obj.combineSteps;
            Obj.fitResponse('D');
            Obj.fitResponse('B');
            Obj.fitGain;
            Obj.threshold;
        end

        function Obj = read(Obj)
            % Read the frame inventory and sidecars; in region mode also the pixels.
            % Sets Mode ('region' if CCDSEC is given, 'full' otherwise).
            % Example: P.read
            if isempty(Obj.CCDSEC)
                Obj.Mode = 'full';
                [~, Obj.Frames, Obj.Sidecar] = ultrasat.lab.readPTC(Obj.DeviceDir, 'Test',Obj.Test, 'ReadImage',false, ...
                                                       'Gain',Obj.Gain, 'Orient',Obj.Orient);
                Obj.AI = [];
            else
                Obj.Mode = 'region';
                [Obj.AI, Obj.Frames, Obj.Sidecar] = ultrasat.lab.readPTC(Obj.DeviceDir, 'Test',Obj.Test, ...
                                                       'CCDSEC',Obj.CCDSEC, 'Gain',Obj.Gain, 'Orient',Obj.Orient, 'Verbosity',Obj.Verbosity);
            end
            R = Obj.Sidecar.Result.Info;
            Obj.Info = struct('Lot',R.LOTID, 'Wafer',R.WaferID, 'Device',R.DeviceNo, ...
                              'Base',regexprep(Obj.Frames.FileName{1}, ['_', Obj.Test, '_#.*$'], ''), ...
                              'Test',Obj.Test);
            if isempty(Obj.ExpSen) && ~isempty(Obj.Sidecar.Config) && isfield(Obj.Sidecar.Config, 'PTC_ExpTime')
                Obj.ExpSen = Obj.Sidecar.Config.PTC_ExpTime(1);
            end
        end

        function Obj = subtractZero(Obj)
            % Build the bias frame from the ZE frames (Combiner) and its noise.
            % The subtraction itself is applied when steps are loaded.
            % Example: P.subtractZero
            Cube = Obj.loadFrames('ZE', []);
            if isempty(Cube)
                error('ultrasat:lab:PTCAnalysis:noZero', 'No ZE frames in %s', Obj.DeviceDir);
            end
            Obj.NZero     = size(Cube, 3);
            Obj.Zero      = Obj.combine(Cube);
            Obj.ZeroNoise = std(Cube, 0, 3);
        end

        function Obj = combineSteps(Obj)
            % Per step (D and B): combined bias-subtracted signal and the three
            % variance estimators. Fills Dark and Bright with fields:
            %   X       - exposure time [s] (D) or intensity (B, Bright_Intensity*IntensityScale) per step
            %   Step    - step numbers
            %   Nframes - repeats per step
            %   Mean    - [Ny Nx Nstep] combined signal (region mode only)
            %   VarTemporal - [Ny Nx Nstep] per-pixel variance over repeats (region mode only)
            %   RegionMean, RegionVarTemporal, RegionVarDiff, RegionVarSpatial
            %           - per-step scalars over all pixels read: mean signal,
            %             mean per-pixel variance over the repeats, variance of
            %             (frame1-frame2)/sqrt(2), mean over repeats of the
            %             spatial variance of a single frame (includes PRNU/FPN)
            % In full mode the per-pixel cubes are not kept; the response
            % fit sums are accumulated instead (see fitResponse).
            % Example: P.combineSteps
            Obj.Dark   = Obj.ladder('D');
            Obj.Bright = Obj.ladder('B');
        end

        function Obj = fitResponse(Obj, Type)
            % Per-pixel linear fit of signal vs X inside FitRange (per type),
            % or of the steps listed in FitSteps.(Type) when non-empty.
            % Input  : - 'D' (signal vs exposure time) or 'B' (vs intensity).
            % Output : - Obj with DarkFit / BrightFit: Slope, Intercept,
            %            ResidRMS, Nused maps; Used [Ny Nx Nstep] (region
            %            mode); Median*/Std* summaries; FitRange, FitSteps, X.
            % Example: P.fitResponse('D')
            L = Obj.ladderOf(Type);
            if strcmp(Obj.Mode, 'region')
                if ~isfield(L, 'Mean')
                    error('ultrasat:lab:PTCAnalysis:order', 'Run combineSteps before fitResponse');
                end
                [Range, Steps] = Obj.fitSelection(Type, L.Step);
                Fit = Obj.fitMasked(L.Mean(:,:,Steps), L.X(Steps), Range);
                if ~all(Steps)
                    Used = false(size(L.Mean));
                    Used(:,:,Steps) = Fit.Used;
                    Fit.Used = Used;
                end
            else
                Fit = L.Fit;   % accumulated while streaming
            end
            [Fit.FitRange, ~, Fit.FitSteps] = Obj.fitSelection(Type, L.Step);
            Fit.X        = L.X;
            Fit.Type     = Type;
            Fit = Obj.fitSummary(Fit);
            if strcmp(Type, 'D')
                Obj.DarkFit = Fit;
            else
                Obj.BrightFit = Fit;
            end
        end

        function Obj = fitGain(Obj)
            % PTC gain from variance vs mean of the bright ladder.
            % For each estimator ('temporal', 'diff', 'spatial') a straight
            % line Var = Gain*Mean + Offset is fitted to the steps whose
            % mean lies inside GainRange and below SatLevel. Gain is in
            % ADU/e-; the shot-noise reference is Var = Mean (Gain = 1).
            % Output : - Obj.PTC with Mean, VarTemporal, VarDiff, VarSpatial
            %            (per step), Fit.(estimator) = Gain, Offset, Npts,
            %            GainMeasured (GainEstimator), GainUsed, GainSource.
            % Example: P.fitGain
            B = Obj.Bright;
            P = struct('Mean',B.RegionMean, 'VarTemporal',B.RegionVarTemporal, ...
                       'VarDiff',B.RegionVarDiff, 'VarSpatial',B.RegionVarSpatial, 'GainRange',Obj.GainRange, ...
                       'SatLevel',Obj.SatLevel, 'Saturated',B.RegionMean>Obj.SatLevel);
            Est = {'temporal','VarTemporal'; 'diff','VarDiff'; 'spatial','VarSpatial'};
            for Ie=1:1:size(Est,1)
                V    = P.(Est{Ie,2});
                Flag = P.Mean>=Obj.GainRange(1) & P.Mean<=Obj.GainRange(2) & ~P.Saturated & isfinite(V) & V>0;
                F    = struct('Gain',NaN, 'Offset',NaN, 'Npts',sum(Flag));
                if F.Npts>=2
                    C = polyfit(P.Mean(Flag), V(Flag), 1);
                    F.Gain   = C(1);
                    F.Offset = C(2);
                end
                P.Fit.(Est{Ie,1}) = F;
            end
            P.GainMeasured = P.Fit.(Obj.GainEstimator).Gain;
            if isempty(Obj.GainADU)
                P.GainUsed   = P.GainMeasured;
                P.GainSource = ['measured (', Obj.GainEstimator, ')'];
            else
                P.GainUsed   = Obj.GainADU;
                P.GainSource = 'override';
            end
            Obj.PTC = P;
        end

        function Obj = threshold(Obj)
            % Threshold maps from the response fits.
            %   Dark method : ThresholdADU = -DarkFit.Intercept
            %   Light method: ThresholdADU = -(BrightFit.Intercept + DC*ExpSen),
            %                 DC = DarkFit.Slope [ADU/s], ExpSen [s]
            %   Electrons   : ADU / PTC.GainUsed (measured, or GainADU override)
            % The DESY deck quotes the (negative) intercept in e-, i.e.
            % -ThresholdE. Summary values are medians over pixels; the light
            % summary also uses the median DC (as in the deck).
            % Example: P.threshold
            G = Obj.PTC.GainUsed;
            T = struct('GainUsed',G, 'GainSource',Obj.PTC.GainSource, 'ExpSen',Obj.ExpSen);
            if ~isempty(fieldnames(Obj.DarkFit))
                T.DarkADU = -Obj.DarkFit.Intercept;
                T.DarkE   = T.DarkADU./G;
                T.MedianDarkADU = median(T.DarkADU(:), 'omitnan');
                T.StdDarkADU    = std(T.DarkADU(:), 'omitnan');
                T.MedianDarkE   = T.MedianDarkADU./G;
                T.StdDarkE      = T.StdDarkADU./G;
            end
            if ~isempty(fieldnames(Obj.BrightFit)) && ~isempty(fieldnames(Obj.DarkFit))
                DCterm = Obj.DarkFit.Slope.*Obj.ExpSen;
                T.LightADU = -(Obj.BrightFit.Intercept + DCterm);
                T.LightE   = T.LightADU./G;
                T.MedianDCtermADU = Obj.DarkFit.MedianSlope.*Obj.ExpSen;
                T.MedianLightADU  = -(Obj.BrightFit.MedianIntercept + T.MedianDCtermADU);
                T.StdLightADU     = Obj.BrightFit.StdIntercept;
                T.MedianLightE    = T.MedianLightADU./G;
                T.StdLightE       = T.StdLightADU./G;
            end
            Obj.Threshold = T;
        end

        function S = summary(Obj)
            % Collect the deck-style numbers into one structure.
            % Output : - Structure with Info, Mode, Npix, Dark/Bright fit
            %            medians and stds, Nused mode, gains, thresholds.
            % Example: S = P.summary
            S = Obj.Info;
            S.Mode     = Obj.Mode;
            S.Combiner = Obj.Combiner;
            S.Gain     = Obj.Gain;
            S.Orient   = Obj.Orient;
            S.IntensityScale = Obj.IntensityScale;
            for T = {'D','DarkFit'; 'B','BrightFit'}.'
                F = Obj.(T{2});
                if ~isempty(fieldnames(F))
                    S.(T{2}) = struct('MedianSlope',F.MedianSlope, 'StdSlope',F.StdSlope, ...
                                      'MedianIntercept',F.MedianIntercept, 'StdIntercept',F.StdIntercept, ...
                                      'MedianResidRMS',F.MedianResidRMS, 'StdResidRMS',F.StdResidRMS, ...
                                      'NusedMode',F.NusedMode, 'Nsteps',numel(F.X), 'Npix',numel(F.Slope), ...
                                      'FitRange',F.FitRange, 'FitSteps',F.FitSteps);
                end
            end
            if ~isempty(fieldnames(Obj.PTC))
                S.GainTemporal = Obj.PTC.Fit.temporal.Gain;
                S.GainDiff     = Obj.PTC.Fit.diff.Gain;
                S.GainSpatial  = Obj.PTC.Fit.spatial.Gain;
                S.GainUsed     = Obj.PTC.GainUsed;
                S.GainSource   = Obj.PTC.GainSource;
            end
            if ~isempty(fieldnames(Obj.Threshold))
                T = Obj.Threshold;
                Fn = {'ExpSen','MedianDarkADU','StdDarkADU','MedianDarkE','StdDarkE', ...
                      'MedianDCtermADU','MedianLightADU','StdLightADU','MedianLightE','StdLightE'};
                for If=1:1:numel(Fn)
                    if isfield(T, Fn{If})
                        S.(Fn{If}) = T.(Fn{If});
                    end
                end
            end
        end
    end

    methods % plots
        function H = plotPTC(Obj, Args)
            % Plot the photon transfer curve: variance vs mean signal.
            % Input  : * ...,key,val,...
            %            'Estimator' - 'temporal' | 'diff' | 'spatial' | 'all'.
            %                   Default is 'all'.
            %            'XLim' - x range to show ([] = all). Default is [].
            %            'Axes' - axes handle; [] = new figure. Default is [].
            % Output : - Axes handle.
            % Example: P.plotPTC('Estimator','temporal', 'XLim',[0 3000])
            arguments
                Obj
                Args.Estimator = 'all';
                Args.XLim      = [];
                Args.Axes      = [];
            end
            H = Obj.axesOf(Args.Axes);
            P = Obj.PTC;
            Est = {'temporal','VarTemporal','ko-'; 'diff','VarDiff','bs--'; 'spatial','VarSpatial','g^:'};
            if ~strcmp(Args.Estimator, 'all')
                Est = Est(strcmp(Est(:,1), Args.Estimator), :);
            end
            hold(H, 'on');
            for Ie=1:1:size(Est,1)
                F = P.Fit.(Est{Ie,1});
                plot(H, P.Mean, P.(Est{Ie,2}), Est{Ie,3}, 'MarkerSize',4, ...
                     'DisplayName',sprintf('%s: gain = %.3f ADU/e-', Est{Ie,1}, F.Gain));
            end
            Xl = [0, max(P.Mean)];
            plot(H, Xl, Xl, '-', 'Color',[1 .6 0], 'DisplayName','gain = 1');
            hold(H, 'off');
            grid(H, 'on');
            if ~isempty(Args.XLim)
                xlim(H, Args.XLim);
            end
            xlabel(H, 'Mean signal [ADU]');
            ylabel(H, 'Signal variance [ADU^2]');
            title(H, sprintf('%s | PTC | %s', Obj.Info.Base, Obj.Mode), 'Interpreter','none');
            legend(H, 'Location','northwest');
        end

        function H = plotResponse(Obj, Type, Args)
            % Plot signal vs exposure (D) or vs intensity (B): median over
            % pixels with typical-sigma bars, 16/84 percentile band, a
            % sample of individual pixels, the median fit line and the
            % steps excluded from the fit (median signal outside FitRange).
            % Input  : - 'D' or 'B'.
            %          * ...,key,val,...
            %            'Npix' - number of individual pixels to draw.
            %                   Default is 300.
            %            'Axes' - axes handle; [] = new figure. Default is [].
            % Output : - Axes handle.
            % Example: P.plotResponse('D')
            arguments
                Obj
                Type
                Args.Npix = 300;
                Args.Axes = [];
            end
            if ~strcmp(Obj.Mode, 'region')
                error('ultrasat:lab:PTCAnalysis:mode', 'plotResponse needs the per-pixel cubes (region mode)');
            end
            L   = Obj.ladderOf(Type);
            Fit = Obj.fitOf(Type);
            H   = Obj.axesOf(Args.Axes);
            Ns  = numel(L.X);
            Y   = reshape(L.Mean, [], Ns);           % [Npix Nstep]
            Npix = size(Y, 1);
            Med  = median(Y, 1, 'omitnan');
            P16  = prctile(Y, 16, 1);
            P84  = prctile(Y, 84, 1);
            Sig  = median(sqrt(reshape(L.VarTemporal, [], Ns)./L.Nframes), 1, 'omitnan');   % typical sigma of the combined signal
            hold(H, 'on');
            Sel = round(linspace(1, Npix, min(Args.Npix, Npix)));
            plot(H, L.X, Y(Sel,:).', '-', 'Color',[0 .5 0 .05], 'HandleVisibility','off');
            plot(H, L.X, P16, 'k:', 'DisplayName','16 / 84 percentile');
            plot(H, L.X, P84, 'k:', 'HandleVisibility','off');
            errorbar(H, L.X, Med, Sig, 'k-o', 'MarkerFaceColor','k', 'MarkerSize',4, ...
                     'DisplayName',sprintf('median of %d pixels (bars: typ. sigma)', Npix));
            Xf = [0, max(L.X)];
            plot(H, Xf, Fit.MedianIntercept + Fit.MedianSlope.*Xf, 'r--', ...
                 'DisplayName',sprintf('median fit (%.4g %s)', Fit.MedianSlope, Obj.slopeUnit(Type)));
            UsedFrac = squeeze(mean(reshape(Fit.Used, [], Ns), 1));      % fraction of pixels using each step
            Excl = UsedFrac(:).'<0.5;
            if any(Excl)
                plot(H, L.X(Excl), Med(Excl), 'rx', 'MarkerSize',10, 'LineWidth',1.5, ...
                     'DisplayName',sprintf('excluded from fit (%d)', sum(Excl)));
            end
            hold(H, 'off');
            grid(H, 'on');
            xlabel(H, Obj.xLabel(Type));
            ylabel(H, sprintf('%s %s - ZERO signal [ADU]', upper(Obj.Combiner), Obj.typeName(Type)));
            title(H, sprintf('%s | %s | fit: %d/%d points, median intercept = %.4g ADU', ...
                  Obj.Info.Base, Obj.typeName(Type), Fit.NusedMode, Ns, Fit.MedianIntercept), 'Interpreter','none');
            legend(H, 'Location','northwest');
        end

        function H = plotHistograms(Obj, Type, Args)
            % Histograms (log count) of the per-pixel slope, intercept and
            % residual RMS with their median and std.
            % Input  : - 'D' or 'B'.
            %          * ...,key,val,...
            %            'Nbins' - Default is 60.
            % Output : - Array of 3 axes handles.
            % Example: P.plotHistograms('D')
            arguments
                Obj
                Type
                Args.Nbins = 60;
            end
            Fit = Obj.fitOf(Type);
            figure;
            Q = {'Slope', ['slope [', Obj.slopeUnit(Type), ']']; 'Intercept', 'intercept [ADU]'; 'ResidRMS', 'residual RMS [ADU]'};
            H = gobjects(1,3);
            for Iq=1:1:3
                H(Iq) = subplot(1,3,Iq);
                V = Fit.(Q{Iq,1})(:);
                V = V(isfinite(V));
                histogram(H(Iq), V, Args.Nbins, 'FaceColor',[.7 .7 .7], 'EdgeColor',[.3 .3 .3]);
                set(H(Iq), 'YScale','log');
                hold(H(Iq), 'on');
                Yl = ylim(H(Iq));
                plot(H(Iq), Fit.(['Median', Q{Iq,1}]).*[1 1], Yl, 'r--', ...
                     'DisplayName',sprintf('median = %.4g\nstd = %.3g', Fit.(['Median', Q{Iq,1}]), Fit.(['Std', Q{Iq,1}])));
                hold(H(Iq), 'off');
                xlabel(H(Iq), Q{Iq,2});
                ylabel(H(Iq), 'pixel count');
                title(H(Iq), Q{Iq,2});
                legend(H(Iq));
            end
            sgtitle(sprintf('%s | %s | %d pixels', Obj.Info.Base, Obj.typeName(Type), numel(Fit.Slope)), 'Interpreter','none');
        end

        function H = plotMaps(Obj, Type)
            % Maps of slope, intercept and residual RMS (imagesc).
            % Input  : - 'D' or 'B'.
            % Output : - Array of 3 axes handles.
            % Example: F.plotMaps('D')
            Fit = Obj.fitOf(Type);
            figure;
            Q = {'Slope', ['slope [', Obj.slopeUnit(Type), ']']; 'Intercept', 'intercept [ADU]'; 'ResidRMS', 'residual RMS [ADU]'};
            H = gobjects(1,3);
            for Iq=1:1:3
                H(Iq) = subplot(1,3,Iq);
                M = Fit.(Q{Iq,1});
                imagesc(H(Iq), M, prctile(M(:), [1 99]));
                axis(H(Iq), 'image');
                colorbar(H(Iq));
                title(H(Iq), Q{Iq,2});
                xlabel(H(Iq), 'pixel column');
                ylabel(H(Iq), 'pixel row');
            end
            sgtitle(sprintf('%s | %s | %s', Obj.Info.Base, Obj.typeName(Type), Obj.Mode), 'Interpreter','none');
        end
    end

    methods (Static) % masked linear regression
        function S = accumulate(S, Y, X, FitRange)
            % Add one or more steps to the running sums of a masked linear fit.
            % Input  : - Sums structure (N, Sx, Sy, Sxx, Sxy, Syy) or [] to start.
            %          - Signal [Ny Nx Nstep] (single or double).
            %          - X value per step (vector of length Nstep).
            %          - [Low High] signal window; a step contributes to a
            %            pixel only if Low<=Y<=High and Y is finite.
            % Output : - Updated sums structure (double).
            % Example: S = ultrasat.lab.PTCAnalysis.accumulate([], Cube, X, [1000 2500]);
            Ns = size(Y, 3);
            if isempty(S)
                Z = zeros(size(Y,1), size(Y,2));
                S = struct('N',Z, 'Sx',Z, 'Sy',Z, 'Sxx',Z, 'Sxy',Z, 'Syy',Z);
            end
            for Is=1:1:Ns
                Yi = double(Y(:,:,Is));
                W  = isfinite(Yi) & Yi>=FitRange(1) & Yi<=FitRange(2);
                Yi(~W) = 0;
                Xi = double(X(Is));
                S.N   = S.N   + W;
                S.Sx  = S.Sx  + W.*Xi;
                S.Sy  = S.Sy  + Yi;
                S.Sxx = S.Sxx + W.*Xi.^2;
                S.Sxy = S.Sxy + Yi.*Xi;
                S.Syy = S.Syy + Yi.^2;
            end
        end

        function Fit = solve(S)
            % Solve the masked linear fit from its running sums.
            % Output : - Structure with Slope, Intercept, ResidRMS (rms of
            %            the residuals over the used points), Nused; NaN
            %            where fewer than 2 points were used.
            % Example: Fit = ultrasat.lab.PTCAnalysis.solve(S)
            D   = S.N.*S.Sxx - S.Sx.^2;
            Bad = S.N<2 | D<=0;
            D(Bad) = NaN;
            Slope     = (S.N.*S.Sxy - S.Sx.*S.Sy)./D;
            Intercept = (S.Sy - Slope.*S.Sx)./S.N;
            SS        = S.Syy - Intercept.*S.Sy - Slope.*S.Sxy;
            ResidRMS  = sqrt(max(SS, 0)./S.N);
            Slope(Bad) = NaN;  Intercept(Bad) = NaN;  ResidRMS(Bad) = NaN;
            Fit = struct('Slope',Slope, 'Intercept',Intercept, 'ResidRMS',ResidRMS, 'Nused',S.N);
        end

        function Fit = fitMasked(Y, X, FitRange)
            % Masked per-pixel linear fit of a cube (accumulate + solve).
            % Input  : - Signal [Ny Nx Nstep]; - X per step; - [Low High] window.
            % Output : - Fit structure (see solve) plus Used [Ny Nx Nstep].
            % Example: Fit = ultrasat.lab.PTCAnalysis.fitMasked(Cube, X, [1000 2500])
            S   = ultrasat.lab.PTCAnalysis.accumulate([], Y, X, FitRange);
            Fit = ultrasat.lab.PTCAnalysis.solve(S);
            Fit.Used = isfinite(Y) & Y>=FitRange(1) & Y<=FitRange(2);
        end

        Result = unitTest()   % implemented in @PTCAnalysis/unitTest.m
    end

    methods (Access = protected) % data access shared by the two modes
        function Cube = loadFrames(Obj, Type, Step)
            % Frames of one type (and step) as a single cube [Ny Nx Nframes];
            % from memory in region mode, from disk in full mode.
            if strcmp(Obj.Mode, 'region')
                Flag = strcmp(Obj.Frames.FrameType, Type);
                if ~isempty(Step)
                    Flag = Flag & Obj.Frames.Step==Step;
                end
                Ind  = find(Flag);
                if isempty(Ind)
                    Cube = [];
                    return;
                end
                Cube = zeros([size(Obj.AI(Ind(1)).Image), numel(Ind)], 'single');
                for Ii=1:1:numel(Ind)
                    Cube(:,:,Ii) = single(Obj.AI(Ind(Ii)).Image);
                end
            else
                A = ultrasat.lab.readPTC(Obj.DeviceDir, 'Test',Obj.Test, 'FrameType',Type, 'Step',Step, 'Verbosity',Obj.Verbosity);
                if isempty(A)
                    Cube = [];
                    return;
                end
                Cube = zeros([size(A(1).Image), numel(A)], 'single');
                for Ii=1:1:numel(A)
                    Cube(:,:,Ii) = single(A(Ii).Image);
                end
            end
        end

        function M = combine(Obj, Cube)
            % Combine the frames of a cube along the 3rd dimension.
            switch lower(Obj.Combiner)
                case 'mean'
                    M = mean(Cube, 3);
                case 'median'
                    M = median(Cube, 3);
                otherwise
                    error('ultrasat:lab:PTCAnalysis:combiner', 'Unknown Combiner %s', Obj.Combiner);
            end
        end

        function L = ladder(Obj, Type)
            % Reduce all steps of one frame type (see combineSteps).
            Flag  = strcmp(Obj.Frames.FrameType, Type);
            Steps = unique(Obj.Frames.Step(Flag)).';
            Ns    = numel(Steps);
            L = struct('Type',Type, 'Step',Steps, 'X',nan(1,Ns), 'Nframes',zeros(1,Ns), ...
                       'RegionMean',nan(1,Ns), 'RegionVarTemporal',nan(1,Ns), ...
                       'RegionVarDiff',nan(1,Ns), 'RegionVarSpatial',nan(1,Ns));
            IsRegion = strcmp(Obj.Mode, 'region');
            Sums = [];
            for Is=1:1:Ns
                Row = find(Flag & Obj.Frames.Step==Steps(Is), 1);
                if strcmp(Type, 'D')
                    L.X(Is) = Obj.Frames.ExpTime(Row);
                else
                    L.X(Is) = Obj.Frames.Intensity(Row).*Obj.IntensityScale;
                end
                if Obj.Verbosity>0
                    fprintf('PTCAnalysis: %s step %d/%d\n', Type, Is, Ns);
                end
                Cube = Obj.loadFrames(Type, Steps(Is)) - Obj.Zero;
                Nf   = size(Cube, 3);
                L.Nframes(Is) = Nf;
                M  = Obj.combine(Cube);
                V  = var(Cube, 0, 3);
                L.RegionMean(Is)        = mean(M(:), 'omitnan');
                L.RegionVarTemporal(Is) = mean(V(:), 'omitnan');
                L.RegionVarSpatial(Is)  = mean(var(reshape(Cube, [], Nf), 0, 1, 'omitnan'));
                if Nf>=2
                    Dif = Cube(:,:,1) - Cube(:,:,2);
                    L.RegionVarDiff(Is) = var(Dif(:), 'omitnan')./2;
                end
                if IsRegion
                    if Is==1
                        L.Mean        = zeros([size(M), Ns], 'single');
                        L.VarTemporal = zeros([size(M), Ns], 'single');
                    end
                    L.Mean(:,:,Is)        = M;
                    L.VarTemporal(:,:,Is) = V;
                else
                    [Range, Sel] = Obj.fitSelection(Type, Steps(Is));
                    if Sel
                        Sums = ultrasat.lab.PTCAnalysis.accumulate(Sums, M, L.X(Is), Range);
                    elseif isempty(Sums)
                        Sums = ultrasat.lab.PTCAnalysis.accumulate([], M, L.X(Is), [1 -1]);   % zero contribution, sizes set
                    end
                end
            end
            if ~IsRegion && Ns>0
                L.Fit = ultrasat.lab.PTCAnalysis.solve(Sums);
            end
        end

        function [Range, Sel, Steps] = fitSelection(Obj, Type, StepNumbers)
            % signal window of a frame type and the logical selection of the
            % given step numbers: all true when FitSteps.(Type) is empty
            % (selection by FitRange), otherwise the listed steps with an
            % open window.
            if size(Obj.FitRange, 1)==2
                Range = Obj.FitRange(1 + strcmp(Type, 'B'), :);
            else
                Range = Obj.FitRange;
            end
            Steps = [];
            if isfield(Obj.FitSteps, Type) && ~isempty(Obj.FitSteps.(Type))
                Steps = Obj.FitSteps.(Type);
                Sel   = ismember(StepNumbers, Steps);
                Range = [-Inf Inf];
            else
                Sel = true(size(StepNumbers));
            end
        end

        function Fit = fitSummary(Obj, Fit)
            % Median / std over pixels and the modal number of used points.
            for Q = {'Slope','Intercept','ResidRMS'}
                V = Fit.(Q{1})(:);
                Fit.(['Median', Q{1}]) = median(V, 'omitnan');
                Fit.(['Std', Q{1}])    = std(V, 'omitnan');
            end
            Fit.NusedMode = mode(Fit.Nused(:));
            Fit.Combiner  = Obj.Combiner;
        end

        function L = ladderOf(Obj, Type)
            if strcmp(Type, 'D')
                L = Obj.Dark;
            else
                L = Obj.Bright;
            end
        end

        function F = fitOf(Obj, Type)
            if strcmp(Type, 'D')
                F = Obj.DarkFit;
            else
                F = Obj.BrightFit;
            end
            if isempty(fieldnames(F))
                error('ultrasat:lab:PTCAnalysis:order', 'Run fitResponse(''%s'') first', Type);
            end
        end
    end

    methods (Static, Access = protected) % labels
        function H = axesOf(Ax)
            if isempty(Ax)
                figure;
                H = axes;
            else
                H = Ax;
            end
        end
        function S = typeName(Type)
            if strcmp(Type, 'D'), S = 'DARK'; else, S = 'BRIGHT'; end
        end
        function S = slopeUnit(Type)
            if strcmp(Type, 'D'), S = 'ADU/s'; else, S = 'ADU/int'; end
        end
        function S = xLabel(Type)
            if strcmp(Type, 'D'), S = 'exposure time [s]'; else, S = 'light intensity [int] (Bright_Intensity x IntensityScale)'; end
        end
    end
end
