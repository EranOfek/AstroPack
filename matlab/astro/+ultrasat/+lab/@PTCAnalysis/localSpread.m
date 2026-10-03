function S = localSpread(M, Args)
    % Pixel-to-pixel spread of a map, with the large-scale structure and the
    % measurement noise removed.
    %   The spread of a map over a whole die is a TOTAL non-uniformity, and on
    %   these devices it is dominated by structure on scales of hundreds of
    %   pixels: a 2:1 dark-current ramp along the readout columns, response
    %   patches, banding. That is not what a noise budget means by DSNU or
    %   PRNU, which are pixel-to-pixel terms -- a slow ramp is removed by any
    %   flat field, while the pixel-to-pixel part is what survives it.
    %   This takes the residual to a Block x Block median, so everything
    %   varying on scales above Block is absorbed, and then removes the known
    %   measurement sigma of the individual pixels (the fit noise of a
    %   per-pixel ladder fit, or any other known per-pixel sigma) in
    %   quadrature, exactly as paramSpread does for the spread over a whole
    %   population.
    %   Block is a compromise: too small and the block median follows the
    %   pixel-to-pixel variation itself, removing the signal being measured;
    %   too large and the structure survives. The default 32 leaves 1024
    %   pixels per block, whose median is 1.25/sqrt(1024) = 4 % of the spread
    %   being measured, while the structure inside one block is far below it.
    %   Measured on run 32 W04_D07: the dark current spreads 20.0 % over the
    %   die but 5.6 % pixel to pixel, against 5.7 % measured independently in
    %   the DESY 100x100 window; the response 1.08 % against 0.41 %.
    % Input  : - Map [Ny Nx].
    %          * ...,key,val,...
    %            'Block'  - block side in pixels. Default is 32.
    %            'StdFit' - known per-pixel measurement sigma, removed in
    %                       quadrature. Default is 0 (none). Pair a robust
    %                       observed spread with the ROBUST (median) fit
    %                       noise, as in paramSpread.
    %            'Robust' - MAD-based observed spread. Default is true.
    %            'Mask'   - logical map of the pixels to use ([] = all).
    % Output : - Structure with Block, Npix, Nblock, Level (median of the
    %            block medians), StdObs, StdFit, StdIntr and RelIntr.
    % Author : Sasha Krassilchtchikov (Oct 2026)
    % Example: S = ultrasat.lab.PTCAnalysis.localSpread(F.Slope, 'StdFit',F.All.SlopeSpread.StdFitRobust)
    arguments
        M
        Args.Block  (1,1) double = 32;
        Args.StdFit (1,1) double = 0;
        Args.Robust logical      = true;
        Args.Mask                = [];
    end
    B = Args.Block;
    C = double(M);
    if ~isempty(Args.Mask)
        C(~Args.Mask) = NaN;
    end
    [Ny, Nx] = size(C);
    ny = floor(Ny./B).*B;
    nx = floor(Nx./B).*B;
    if ny<B || nx<B
        error('ultrasat:lab:PTCAnalysis:block', 'Map %dx%d is smaller than one %dx%d block', Ny, Nx, B, B);
    end
    C   = reshape(permute(reshape(C(1:ny, 1:nx), B, ny./B, B, nx./B), [1 3 2 4]), B.*B, []);
    Med = median(C, 1, 'omitnan');
    Res = C - Med;
    Res = Res(isfinite(Res));
    S   = struct('Block',B, 'Npix',numel(Res), 'Nblock',size(C,2), ...
                 'Level',median(Med, 'omitnan'), 'StdObs',NaN, 'StdFit',Args.StdFit, ...
                 'StdIntr',NaN, 'RelIntr',NaN);
    if numel(Res)<3
        return
    end
    if Args.Robust
        S.StdObs = 1.4826.*median(abs(Res - median(Res)));
    else
        S.StdObs = std(Res);
    end
    S.StdIntr = sqrt(max(S.StdObs.^2 - Args.StdFit.^2, 0));
    S.RelIntr = S.StdIntr./abs(S.Level);
end
