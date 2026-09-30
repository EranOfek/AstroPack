function S = badColumns(Obj, Args)
    % Identify and mask the bad raw readout columns of the region.
    %   Two criteria, both per raw readout column (image rows in the DESY
    %   orientation, see rawColGeom): excess read noise (median per-pixel
    %   ZE noise above NoiseFactor times the median over columns) and a
    %   dead or weak light response (median bright-ladder slope below
    %   RespFactor times the median over columns; the first columns of the
    %   high-gain half are blind to light).
    %   The returned GoodMask is meant to be passed to zeroNoiseStats /
    %   perPixelFits so that ensemble statistics and their tails are not
    %   contaminated, while the flagged columns stay available as a
    %   comparison metric in their own right.
    % Input  : * ...,key,val,...
    %            'NoiseSigma'  - reject a column more than this many robust
    %                            sigmas above the profile median (default 5).
    %                            A plain ratio to the median finds nothing
    %                            here: the column-to-column spread of the
    %                            read noise is a few per cent, so a column
    %                            twice as noisy is a 30-sigma outlier but
    %                            only 2x the median.
    %            'NoiseFactor' - also reject above this ratio (default 3).
    %            'RespSigma', 'RespFactor' - the same on the low side of the
    %                            response profile (5, 0.5); both ignored
    %                            when the bright fit is not done.
    % Output : - Structure with
    %            RawCol       - raw column index per image row/column
    %            NoiseProfile - median ZE noise per raw column [ADU],
    %                           with NoiseMedian / NoiseSigma (robust)
    %            RespProfile  - median bright slope per raw column ([] if
    %                           the bright fit is not available)
    %            BadNoise, BadResp - logical, per raw column
    %            BadRawCol    - list of the flagged raw column indices
    %            Nbad, Nrawcol
    %            GoodMask     - [Ny Nx] logical, false on flagged columns
    % Example: B = P.badColumns;  Z = P.zeroNoiseStats('Mask',B.GoodMask);
    arguments
        Obj
        Args.NoiseFactor (1,1) double = 3;
        Args.NoiseSigma  (1,1) double = 5;
        Args.RespFactor  (1,1) double = 0.5;
        Args.RespSigma   (1,1) double = 5;
    end
    if isempty(Obj.ZeroNoise)
        error('ultrasat:lab:PTCAnalysis:order', 'Run subtractZero before badColumns');
    end
    G   = Obj.rawColGeom;
    Red = 3 - G.Dim;                                  % dimension to reduce over
    S   = struct('RawCol',G.RawCol, 'Dim',G.Dim, 'Nrawcol',numel(G.RawCol));
    S.NoiseProfile = squeeze(median(double(Obj.ZeroNoise), Red, 'omitnan'));
    [Mn, Sn]       = robustLevel(S.NoiseProfile);
    S.NoiseMedian  = Mn;   S.NoiseSigma = Sn;
    S.BadNoise     = S.NoiseProfile > Mn + Args.NoiseSigma.*Sn | ...
                     S.NoiseProfile > Args.NoiseFactor.*Mn;
    S.RespProfile  = [];
    S.BadResp      = false(size(S.BadNoise));
    if isstruct(Obj.BrightFit) && isfield(Obj.BrightFit, 'Slope') && ~isempty(Obj.BrightFit.Slope)
        S.RespProfile = squeeze(median(double(Obj.BrightFit.Slope), Red, 'omitnan'));
        [Mr, Sr]      = robustLevel(S.RespProfile);
        S.RespMedian  = Mr;   S.RespSigma = Sr;
        S.BadResp     = ~isfinite(S.RespProfile) | ...
                        S.RespProfile < Mr - Args.RespSigma.*Sr | ...
                        S.RespProfile < Args.RespFactor.*Mr;
    end
    Bad          = S.BadNoise | S.BadResp;
    S.BadRawCol  = G.RawCol(Bad);
    S.Nbad       = nnz(Bad);
    S.NoiseFactor = Args.NoiseFactor;   S.NoiseSigmaCut = Args.NoiseSigma;
    S.RespFactor  = Args.RespFactor;    S.RespSigmaCut  = Args.RespSigma;
    if G.Dim==1
        S.GoodMask = repmat(~Bad(:), 1, G.Nx);
    else
        S.GoodMask = repmat(~Bad(:).', G.Ny, 1);
    end
end

function [M, S] = robustLevel(V)
    % median and robust sigma of a profile, ignoring non-finite entries
    V = double(V(isfinite(V)));
    M = median(V);
    S = 1.4826.*median(abs(V - M));
end
