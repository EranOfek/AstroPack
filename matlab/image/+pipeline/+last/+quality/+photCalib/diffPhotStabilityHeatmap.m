function Result = diffPhotStabilityHeatmap(A, B, Args)
    % Pixel-wise difference / ratio of two plotPhotStabilityXY heatmaps.
    % Description: Consumes two Result structs from
    %              pipeline.last.quality.photCalib.plotPhotStabilityXY (or
    %              paths to .mat files persisted via that function's
    %              OutFile arg) that were both computed in Mode='heatmap',
    %              and plots the pixel-wise difference (A - B) or ratio
    %              (A / B) or relative difference ((A - B) / B) between
    %              them. Uses the shared (HeatX, HeatY) grid; the two
    %              heatmaps MUST have matching BinSize and CropSize.
    %              Meant for A/B calibration comparisons (joint vs per-
    %              crop, cheby1_2 vs cheby1_4, before vs after a pipeline
    %              tweak) at a fixed magnitude window.
    % Input  : - A - Result struct (from plotPhotStabilityXY, Mode='heatmap')
    %                OR a path to a .mat file containing one under variable
    %                name 'Result'.
    %          - B - same as A. Both must share HeatX, HeatY, and the same
    %                'Mag' / 'BinStat' - the sibling checks and errors on
    %                mismatch.
    %          * ...,key,val,...
    %            'Mode'        - 'diff' (default) | 'reldiff' | 'ratio'.
    %                             diff    - A.HeatVal - B.HeatVal        [mag]
    %                             reldiff - (A - B) / B                   [dimless]
    %                             ratio   - A / B                         [dimless]
    %            'ColorMap'    - Colormap. Default 'parula' for ratio,
    %                             'RdBu' (diverging) synthesized inline for
    %                             diff / reldiff so 0 sits at the neutral
    %                             mid-band.
    %            'ColorLimits' - [Cmin Cmax] clamp. Default [] (auto:
    %                             symmetric around 0 for diff/reldiff, 2/98
    %                             percentile for ratio).
    %            'LogColor'    - Log-scale colour axis. Meaningful only for
    %                             'ratio'. Default false.
    %            'AxisEqual'   - axis equal tight. Default true.
    %            'CropSize'    - Pin axes to fixed extent (matches
    %                             plotPhotStabilityXY convention). Default
    %                             uses the heatmap's HeatX/HeatY range.
    %            'Title'       - Figure title override.
    %            'Plot'        - Draw the figure. Default true.
    %            'OutFile'     - Save the difference Result to this .mat.
    % Output : - Result struct with .HeatX, .HeatY, .HeatDiff, .HeatCountMin
    %                     (min count across A/B cells), .Mode, .Args.
    % Author : D. Kovaleva (Jul 2026)
    % See also: plotPhotStabilityXY (produces the input heatmaps).
    % Example:
    %   % Two calibrations, same MagRange, same BinSize/CropSize:
    %   Rpc = pipeline.last.quality.photCalib.plotPhotStabilityXY(MS_percrop, ...
    %             'Mode','heatmap','MagRange',[12 17], ...
    %             'BinSize',100,'CropSize',[6388 9576], ...
    %             'OutFile','/home/dana/tmp/heatmap_percrop.mat');
    %   Rjt = pipeline.last.quality.photCalib.plotPhotStabilityXY(MS_joint, ...
    %             'Mode','heatmap','MagRange',[12 17], ...
    %             'BinSize',100,'CropSize',[6388 9576], ...
    %             'OutFile','/home/dana/tmp/heatmap_joint.mat');
    %   pipeline.last.quality.photCalib.diffPhotStabilityHeatmap(Rjt, Rpc);
    %
    %   % Or from disk:
    %   pipeline.last.quality.photCalib.diffPhotStabilityHeatmap( ...
    %       '/home/dana/tmp/heatmap_joint.mat', ...
    %       '/home/dana/tmp/heatmap_percrop.mat', ...
    %       'Mode','reldiff', 'ColorLimits', [-0.5 0.5]);

    arguments
        A
        B
        Args.Mode        (1,:) char {mustBeMember(Args.Mode, {'diff','reldiff','ratio'})} = 'diff'
        Args.ColorMap    (1,:) char = ''
        Args.ColorLimits              = []
        Args.LogColor    (1,1) logical = false
        Args.AxisEqual   (1,1) logical = true
        Args.CropSize                  = []
        Args.Title       (1,:) char    = ''
        Args.Plot        (1,1) logical = true
        Args.OutFile     (1,:) char    = ''
    end

    A = i_loadHeatmap(A, 'A');
    B = i_loadHeatmap(B, 'B');

    % Sanity: matching grid + config.
    if ~isequal(size(A.HeatVal), size(B.HeatVal)) ...
       || ~isequal(size(A.HeatX),  size(B.HeatX)) ...
       || ~isequal(size(A.HeatY),  size(B.HeatY))
        error('pipeline:last:quality:photCalib:diffPhotStabilityHeatmap:GridMismatch', ...
            'A and B heatmaps have different grids. Rebuild with matching BinSize/CropSize.');
    end
    if max(abs(A.HeatX(:) - B.HeatX(:))) > 1e-6 || max(abs(A.HeatY(:) - B.HeatY(:))) > 1e-6
        error('pipeline:last:quality:photCalib:diffPhotStabilityHeatmap:CentresMismatch', ...
            'A and B heatmap bin centres differ. Rebuild with matching BinSize/CropSize.');
    end
    if isfield(A.Args, 'Mag') && isfield(B.Args, 'Mag') && ~strcmp(A.Args.Mag, B.Args.Mag)
        warning('pipeline:last:quality:photCalib:diffPhotStabilityHeatmap:MagMismatch', ...
            'A.Args.Mag (%s) != B.Args.Mag (%s); differencing anyway.', A.Args.Mag, B.Args.Mag);
    end

    switch lower(Args.Mode)
        case 'diff'
            HeatDiff = A.HeatVal - B.HeatVal;
            IsSigned = true;
        case 'reldiff'
            HeatDiff = (A.HeatVal - B.HeatVal) ./ B.HeatVal;
            IsSigned = true;
        case 'ratio'
            HeatDiff = A.HeatVal ./ B.HeatVal;
            IsSigned = false;
    end
    HeatCountMin = min(A.HeatCount, B.HeatCount);
    HeatDiff(~(A.HeatCount > 0 & B.HeatCount > 0)) = NaN;

    Result = struct( ...
        'HeatX',        A.HeatX, ...
        'HeatY',        A.HeatY, ...
        'HeatDiff',     HeatDiff, ...
        'HeatCountMin', HeatCountMin, ...
        'Mode',         Args.Mode, ...
        'A_Args',       A.Args, ...
        'B_Args',       B.Args, ...
        'Args',         Args);

    if ~isempty(Args.OutFile)
        [D, ~, ~] = fileparts(Args.OutFile);
        if ~isempty(D) && ~exist(D, 'dir'); mkdir(D); end
        save(Args.OutFile, 'Result', '-v7.3');
        fprintf('diffPhotStabilityHeatmap: Result saved to %s\n', Args.OutFile);
    end

    if ~Args.Plot
        return;
    end

    % --- Colour limits ---------------------------------------------------
    Finite = HeatDiff(isfinite(HeatDiff));
    if isempty(Args.ColorLimits)
        if IsSigned
            % Symmetric around 0 so 0-diff maps to the neutral colour.
            M = quantile(abs(Finite), 0.98);
            if ~isfinite(M) || M == 0; M = max(abs(Finite(:)), [], 'omitnan'); end
            CLim = [-M, M];
        else
            CLim = quantile(Finite, [0.02, 0.98]);
            if ~all(isfinite(CLim)) || CLim(1) == CLim(2)
                CLim = [min(Finite), max(Finite)];
            end
        end
    else
        CLim = Args.ColorLimits(:).';
    end
    DoLog = Args.LogColor && ~IsSigned;   % log only makes sense for ratio
    if DoLog
        CLimEff = [max(CLim(1), eps), max(CLim(2), CLim(1) + eps)];
    else
        CLimEff = CLim;
    end

    % --- Colormap: diverging for signed, sequential for ratio ------------
    if isempty(Args.ColorMap)
        if IsSigned
            Cmap = i_divergingRdBu(256);
        else
            Cmap = parula(256);
        end
    else
        Cmap = feval(Args.ColorMap, 256);
    end

    figure('WindowStyle','docked','Color',[1 1 1]);
    H = pcolor(A.HeatX, A.HeatY, HeatDiff.');
    H.EdgeColor = 'none';  shading flat;
    colormap(gca, Cmap);
    if all(isfinite(CLimEff)) && CLimEff(2) > CLimEff(1)
        caxis(CLimEff);
    end
    if DoLog
        try; set(gca, 'ColorScale', 'log'); catch; end
    end
    CB = colorbar;
    switch lower(Args.Mode)
        case 'diff';    CB.Label.String = 'A - B [mag]';
        case 'reldiff'; CB.Label.String = '(A - B) / B';
        case 'ratio';   CB.Label.String = 'A / B';
    end
    CB.Label.Interpreter = 'none';

    box on; grid on;
    if Args.AxisEqual; axis equal tight; end
    ExtX = i_cropExtent(Args.CropSize, 1);
    ExtY = i_cropExtent(Args.CropSize, 2);
    if ~isempty(ExtX); xlim(ExtX); end
    if ~isempty(ExtY); ylim(ExtY); end
    XLbl = 'X';  YLbl = 'Y';
    if isfield(A.Args, 'XField'); XLbl = A.Args.XField; end
    if isfield(A.Args, 'YField'); YLbl = A.Args.YField; end
    xlabel(XLbl, 'Interpreter','none');
    ylabel(YLbl, 'Interpreter','none');
    if ~isempty(Args.Title)
        title(Args.Title, 'Interpreter','none');
    else
        MagTag = '';
        if isfield(A.Args, 'MagRange') && ~isempty(A.Args.MagRange)
            MagTag = sprintf(', mag in [%g, %g]', A.Args.MagRange(1), A.Args.MagRange(2));
        end
        title(sprintf('Stability heatmap %s (A vs B)%s', Args.Mode, MagTag), ...
              'Interpreter','none');
    end
end


% =========================================================================
function R = i_loadHeatmap(In, Label)
    % Accept a Result struct or a path to a .mat file containing one.
    if ischar(In) || isstring(In)
        if ~isfile(In)
            error('pipeline:last:quality:photCalib:diffPhotStabilityHeatmap:NoFile', ...
                '%s: heatmap file not found: %s', Label, In);
        end
        S = load(In, 'Result');
        R = S.Result;
    elseif isstruct(In)
        R = In;
    else
        error('pipeline:last:quality:photCalib:diffPhotStabilityHeatmap:BadInput', ...
            '%s must be a Result struct or a .mat path.', Label);
    end
    Need = {'HeatX','HeatY','HeatVal','HeatCount'};
    for I = 1:numel(Need)
        if ~isfield(R, Need{I})
            error('pipeline:last:quality:photCalib:diffPhotStabilityHeatmap:NotHeatmap', ...
                '%s does not carry field %s - was it built with Mode=''heatmap''?', ...
                Label, Need{I});
        end
    end
end


% =========================================================================
function Xl = i_cropExtent(CropSize, Axis)
    if isempty(CropSize); Xl = []; return; end
    CropSize = double(CropSize(:).');
    if isscalar(CropSize); N = CropSize;
    else;                  N = CropSize(min(Axis, numel(CropSize))); end
    if ~(isfinite(N) && N > 0); Xl = []; return; end
    Xl = [0.5, N + 0.5];
end


% =========================================================================
function C = i_divergingRdBu(N)
    % Simple diverging blue-white-red colormap for signed diff plots. N rows.
    Half = floor(N / 2);
    T = linspace(0, 1, Half + 1).';
    Blue = [0.129 0.400 0.674];  White = [0.968 0.968 0.968];  Red = [0.698 0.094 0.168];
    C1 = (1 - T) .* Blue  + T .* White;
    C2 = (1 - T) .* White + T .* Red;
    C  = [C1(1:end-1, :); C2];
    % Pad to exactly N rows if rounding leaves us short by 1.
    while size(C, 1) < N; C = [C; Red]; end
    C = C(1:N, :);
end
