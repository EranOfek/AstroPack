function [Result] = measureLabPSF(Image, Args)
    % measure PSF characteristics of an ULTRASAT detector image of a laboratory point source  
    %     to be used by ELOP for focusing tests
    % Input  : - Image matrix (counts, e-, or ADU: see 'IsADU') with 1 source [def. simulated with USim],
    %            or a cube of images (3rd dim) of the same source at different sub-pixel positions
    %          * ...,key,val,... 
    % Output : - Resulting structure. Widths "as imaged" include the integration over the pixel,
    %            "optical" ones are before it:
    %          .PSF  -- normalized PSF stamp
    %          .SNR  -- signal to noise ratio in an aperture of radius R50:
    %                   S / sqrt(S + pi*R50^2*V), S = PSFeff * source counts,
    %                   V = measured background variance per pixel (it includes the read noise)
    %          .R50, .R80 -- radii of 50% and 80% encircled energy [pix] around .X, .Y, as imaged,
    %                   100% being the light within CutRadius. The light is interpolated
    %                   between the pixels
    %          .R50opt, .R80opt -- optical R50 and R80 [pix], corrected for the pixel as for a Gaussian PSF
    %                   (NaN if the source is narrower than the pixel)
    %          .X, .Y -- source location on the matrix: the centroid of the light within CentRadius
    %                   or, if R50 is larger than CentRadius, within R50.
    %                   NB: R80 of a lopsided PSF depends on the choice of the center
    %          .FWHM -- FWHM [pix] as imaged: diameter of the circle with the area of the region above
    %                   the half maximum, smoothed over 0.3*R50 if R50 > 2.5 pix (the noise biases the maximum).
    %                   NB: up to 35% too high for the sharpest ULTRASAT PSFs, if measured from 1 image,
    %                   up to 12% from several sub-pixel positions. It does not describe
    %                   a narrow core on a broad halo (as in the visible at 6 deg): use R50, R80
    %          .FWHMopt, .FWHMmajor, .FWHMminor, .Angle -- optical FWHM [pix] of the best-fit elliptical
    %                   Gaussian integrated over the pixels: sqrt(major*minor), major and minor axes,
    %                   angle of the major axis [rad] from X to Y.
    %                   NB: an equivalent Gaussian, not the true optical FWHM: 1.4-3.6 times larger than
    %                   it at the best focus (more with a broad halo)
    %          .X2, .Y2, .XY -- second moments [pix^2] of this Gaussian
    %          If several sources are detected, the brightest one is measured, unless SrcXY is given.
    %          If there are several images or several rows in SrcXY, the PSF is reconstructed on a grid of
    %          0.5 pix from all of them, which removes most of the error due to the undersampling.
    %          At least 4 positions spread over the pixel (steps of 0.4-0.5 pix) are needed.
    %          Sources in the same image should be at least 2.5*R50 apart.
    %          Non-finite pixels (NaN, Inf) are ignored by the source search and count as no light
    %          in the measurement; a warning is given if any are within CutRadius of a measured source.
    %          The widths are those of the reconstructed PSF, .X, .Y, .SNR are vectors, and in addition:
    %          .PSFfine, .FineStep -- the reconstructed PSF (as imaged, sum 1) and its step [pix]
    %          .Single -- structure array with the results for each of the sources
    % Author : A.M. Krassilchtchikov (2024 Dec) 
    % Example: Res = ultrasat.measureLabPSF('T',10000,'XY',[1000 2300],'Plot',true)
    %          Res = ultrasat.measureLabPSF('DataFile','SimImage_tileB.fits')
    %          Res = ultrasat.measureLabPSF(ImageADU,'IsADU',true)        % 14-bit ADU with the gain bit
    %          Res = ultrasat.measureLabPSF(ImageADU,'GainMask',GainMap)  % 13-bit ADU and a gain map
    %          Res = ultrasat.measureLabPSF(Image,'SrcXY',[470 465])      % the source next to this position
    %          Res = ultrasat.measureLabPSF(cat(3,Im1,Im2,Im3,Im4))       % the PSF from 4 shifted images
    %          Res = ultrasat.measureLabPSF(Image,'SrcXY',[470 465; 479.2 465; 488.5 465; 497.9 465]) % from 4 sources
    arguments
        Image          = 'sim';     
        Args.DataFile  = [];          % input data file(s) with image in counts (e-) or ADU
        Args.IsADU     = false;       % the input image is in ADU (14-bit words, the top bit is the gain)
        Args.GainMask  = [];          % gain mask (1 = low gain): matrix or file name. If present, it is
                                      % presumed that the input image is in ADU
        Args.LowGain   = 0.074;       % low gain coefficient
        Args.HighGain  = 1.185;       % high gain coefficient
        Args.Bias      = [0 0];       % [high low] bias [ADU] subtracted from an ADU image (see ultrasat.ADU2e)
        Args.T         = 3000;        % BB spectrum temperature [K] of a simulated source
        Args.XY        = [2000 2000]; % pixel coordinates of a simulated source 
        Args.SrcXY     = [];          % approximate [X Y] of the source to be measured in the input image;
                                      % several rows: sources with the same PSF, or 1 row per image
        Args.CutRadius = 14;          % stamp half-size and the radius of 100% encircled energy [pix]
        Args.CentRadius = 4;          % radius of the light used for the source centroid [pix]; for a source
                                      % with a larger R50, the R50 is used instead
        Args.MinSN     = 20;          % minimal S/N of a detection
        Args.PSFeff    = 0.5;         % PSF efficiency at R50 (for SNR estimate)
        Args.PsfFunPar = {[0.1;0.7;3]}; % Gaussian sigmas [pix] of the detection templates; a source counts
                                      % as a star if a wider template fits better than the first one.
                                      % The ULTRASAT PSF is undersampled (sigma ~0.7 pix), so the
                                      % fwhm_fromMoments default [0.1 1.5 4] rejects it;
                                      % the widest template is for the defocused sources
        Args.Plot      = false;
    end
    
    % read the image(s) from FITS files
    if ~isempty(Args.DataFile)
        AI = AstroImage(Args.DataFile);
        Image = cat(3, AI.Image);
    end
    
    % simulate an image if no numeric matrix is input
    IsADU = Args.IsADU || ~isempty(Args.GainMask);
    if ~isnumeric(Image)
        if IsADU
            error('measureLabPSF: IsADU and GainMask require an input image, exiting..');
        end
        AI = ultrasat.usim('Cat',Args.XY,'Mag',16,'Exposure',[1 300],'SpecType','BB','Spec',Args.T,'OutType','AstroImage');
        Image = AI.Image;
    end

    % convert ADU into e-
    if IsADU
        GainMask = Args.GainMask;
        if ischar(GainMask) || isstring(GainMask)
            AM = AstroImage(char(GainMask));
            GainMask = AM.Image;
        end
        if ~isempty(GainMask) && size(GainMask,3)==1
            GainMask = repmat(GainMask, 1, 1, size(Image,3));   % one mask for all the images
        end
        [~, Image] = ultrasat.ADU2e(Image, 'GainMask',GainMask, 'LowGain',Args.LowGain, 'HighGain',Args.HighGain, ...
                                   'Bias',Args.Bias);
    end

    % an integer image: the negative values would be lost at the background subtraction
    if isinteger(Image)
        Image = single(Image);
    end

    % list of the sources: image number and approximate position
    Nimage = size(Image, 3);
    Nxy    = size(Args.SrcXY, 1);
    if Nimage==1
        Nsrc   = max(Nxy, 1);
        ImInd  = ones(Nsrc, 1);
    elseif Nxy<=1 || Nxy==Nimage
        Nsrc   = Nimage;
        ImInd  = (1:Nsrc).';
    else
        error('measureLabPSF: SrcXY should have 1 row or as many rows as there are images, exiting..');
    end
    SrcXY = Args.SrcXY;
    if Nxy==1
        SrcXY = repmat(SrcXY, Nsrc, 1);
    end

    % background-subtracted images, crude background and its variance
    ImageBS = zeros(size(Image), 'like', Image);
    BackVar = zeros(Nimage, 1);
    for Iim = 1:Nimage
        Frame          = Image(:,:,Iim);
        ImageBS(:,:,Iim) = Frame - clippedMean(Frame);
        BackVar(Iim)   = tools.math.stat.rstd(Frame(:)).^2;
    end

    % measure the sources one by one
    for Isrc = Nsrc:-1:1
        if isempty(SrcXY)
            XY = [];
        else
            XY = SrcXY(Isrc,:);
        end
        % the distance to the nearest other listed source of the same image
        Other = setdiff(find(ImInd==ImInd(Isrc)), Isrc);
        Space = Inf;
        if ~isempty(Other)
            Space = min(hypot(SrcXY(Other,1) - XY(1), SrcXY(Other,2) - XY(2)));
        end
        [X, Y] = findSource(Image(:,:,ImInd(Isrc)), XY, Space, Args);
        [Single(Isrc), CoreXY(Isrc,:)] = measureSingle(ImageBS(:,:,ImInd(Isrc)), BackVar(ImInd(Isrc)), X, Y, Args);
    end

    % combine them
    if Nsrc==1
        Result = rmfield(Single, 'EdgeFrac');
        EdgeFrac = Single.EdgeFrac;
    else
        [Result, EdgeFrac] = measureCombined(ImageBS, BackVar, ImInd, vertcat(Single.X), vertcat(Single.Y), CoreXY, Args);
        Result.Single      = rmfield(Single, 'EdgeFrac');
    end

    % warn about non-finite pixels near the measured sources: they count as no light
    NumBad  = 0;
    MinDist = Inf;
    for Isrc = 1:Nsrc
        [BadY, BadX] = find(~isfinite(Image(:,:,ImInd(Isrc))));
        Dist    = hypot(BadX - Single(Isrc).X, BadY - Single(Isrc).Y);
        NumBad  = NumBad + sum(Dist <= Args.CutRadius);
        MinDist = min([MinDist; Dist]);
    end
    if NumBad > 0
        warning('ultrasat:measureLabPSF:nonFiniteNearSource', ...
            '%d non-finite pixel(s) within %g pix of the source (nearest at %.1f pix) are counted as no light: R50 and R80 may be biased', ...
            NumBad, Args.CutRadius, MinDist);
    end
    % warn if there is still light at the edge of the stamp
    if EdgeFrac > 0.03
        warning('ultrasat:measureLabPSF:lightAtEdge', ...
            '%.1f%% of the light is found at %.1f-%g pix from the source: increase CutRadius or check for neighbours', ...
            100.*EdgeFrac, 0.85.*Args.CutRadius, Args.CutRadius);
    end
    % plot the PSF image on a log scale
    if Args.Plot
        figure(10)
        imagesc(log10(abs(Result.PSF))); colorbar; title('lg(PSF)'); set(gca,'YDir','normal');
    end
end

function [X, Y] = sourceCentroid(Image, X, Y, Radius)
    % Iterative centroid of the light within a given radius of the source position
    % Input  : - background-subtracted image
    %          - initial X
    %          - initial Y
    %          - radius [pix]
    % Output : - X of the centroid (the initial value if it is not found)
    %          - Y of the centroid
    % Author : A.M. Krassilchtchikov (Nov 2026)
    MaxIter = 50;
    Tol     = 1e-4;
    [SizeY, SizeX] = size(Image);
    X0 = X;
    Y0 = Y;
    Iter = 0;
    Step = Inf;
    while Iter < MaxIter && Step > Tol
        Iter  = Iter + 1;
        VecX  = max(1, floor(X-Radius)):min(SizeX, ceil(X+Radius));
        VecY  = (max(1, floor(Y-Radius)):min(SizeY, ceil(Y+Radius))).';
        Light = double(Image(VecY, VecX)) .* ((VecX-X).^2 + (VecY-Y).^2 < Radius.^2);
        Light(isnan(Light)) = 0;
        Flux  = sum(Light, 'all');
        NewX  = sum(Light.*VecX, 'all')./Flux;
        NewY  = sum(Light.*VecY, 'all')./Flux;
        Step  = hypot(NewX-X, NewY-Y);
        X     = NewX;
        Y     = NewY;
        % no light, or the centroid has left the source
        if ~(Flux > 0) || hypot(X-X0, Y-Y0) > Radius
            X    = X0;
            Y    = Y0;
            Step = 0;
        end
    end
end

function [X, Y] = findSource(Image, SrcXY, Space, Args)
    % Find a source in an image
    % Input  : - image
    %          - approximate [X Y] (if empty, the brightest source is taken)
    %          - distance to the nearest other listed source [pix] (Inf if there is none)
    %          - the arguments of measureLabPSF
    % Output : - X of the source
    %          - Y of the source
    % Author : A.M. Krassilchtchikov (Nov 2026)
    % non-finite pixels would spread through the filtering and hide every source
    Image(~isfinite(Image)) = median(Image(isfinite(Image)));
    [~,~,Info] = imUtil.psf.fwhm_fromMoments(Image, 'PsfFunPar', Args.PsfFunPar, 'MinSN', Args.MinSN);
    % keep the detections with positions inside the image
    [SizeY, SizeX] = size(Image);
    IndX   = round(Info.X1);
    IndY   = round(Info.Y1);
    IndIn  = find(IndX>=1 & IndX<=SizeX & IndY>=1 & IndY<=SizeY);
    if isempty(IndIn)
        error('measureLabPSF: no source found in the image, exiting..');
    end
    if isempty(SrcXY)
        % the brightest detection
        Peak    = Image(sub2ind(size(Image), IndY(IndIn), IndX(IndIn)));
        [~, Ib] = max(Peak);
    else
        % the detection nearest to the given position
        [Dist, Ib] = min(hypot(Info.X1(IndIn) - SrcXY(1), Info.Y1(IndIn) - SrcXY(2)));
        if Dist > Args.CutRadius
            error('measureLabPSF: no source found within %g pix of SrcXY, exiting..', Args.CutRadius);
        end
    end
    X = double(Info.X1(IndIn(Ib)));
    Y = double(Info.Y1(IndIn(Ib)));
    % not detected, the nearest detection is a neighbour: start from the given position
    if ~isempty(SrcXY) && hypot(X-SrcXY(1), Y-SrcXY(2)) > 0.5.*Space
        X = SrcXY(1);
        Y = SrcXY(2);
    end
end

function [Result, CoreXY] = measureSingle(Image, BackVar, X, Y, Args)
    % Measure a source in one image
    % Input  : - background-subtracted image
    %          - background variance
    %          - X of the detection
    %          - Y of the detection
    %          - the arguments of measureLabPSF
    % Output : - structure with the results (see measureLabPSF) and the fraction of light at the stamp edge
    %          - [X Y] of the centroid within CentRadius
    % Author : A.M. Krassilchtchikov (Nov 2026)
    [X, Y] = sourceCentroid(Image, X, Y, Args.CentRadius);
    CoreXY = [X, Y];
    [Radius, CumSum, R50] = measureGrowth(Image, X, Y, Args.CutRadius);
    % a broad source: CentRadius holds too little of its light, repeat with R50
    if R50 > Args.CentRadius
        [X, Y] = sourceCentroid(Image, X, Y, R50);
        [Radius, CumSum, R50] = measureGrowth(Image, X, Y, Args.CutRadius);
    end
    FineStep    = 0.1;
    Width.Flux  = CumSum(end);
    Width.R50   = R50;
    Width.R80   = encircledRadius(Radius, CumSum./Width.Flux, 0.8);
    Width.FWHM  = fineFWHM(fineStamp(Image, X, Y, Args.CutRadius, 1./FineStep), FineStep, Args.CutRadius, R50);
    Width.EdgeFlux = Width.Flux - interp1(Radius, CumSum, 0.85.*Args.CutRadius);
    % cut out the source PSF
    Cube = imUtil.cut.image2cutouts(Image, X, Y, Args.CutRadius);
    % normalize the PSF
    % 'omitnan': image2cutouts now NaN-pads cutout pixels outside the
    % image (issue #1199); without omitnan a single edge pixel would
    % NaN out the whole normalization.
    PSF = Cube./sum(Cube,'all','omitnan');
    % Gaussian fit
    Fit = fitGaussPixel(double(Cube), 1, [X-round(X), Y-round(Y)], Width.R50);
    % summarize the results
    Noise    = sqrt(pi.*Args.CutRadius.^2.*(1-0.85.^2).*BackVar + max(Width.EdgeFlux,0));
    Result   = summarizeWidths(PSF, X, Y, Width, Fit, BackVar, Width.Flux, Args);
    Result.EdgeFrac = Width.EdgeFlux./Width.Flux .* (Width.EdgeFlux > 5.*Noise);
end

function [Result, EdgeFrac] = measureCombined(Image, BackVar, ImInd, X, Y, CoreXY, Args)
    % Reconstruct the PSF from several sources and measure it
    % Input  : - background-subtracted image or a cube of images
    %          - background variance of each image
    %          - image number of each source
    %          - X of the sources
    %          - Y of the sources
    %          - [X Y] of the centroids within CentRadius
    %          - the arguments of measureLabPSF
    % Output : - structure with the results (see measureLabPSF)
    %          - fraction of light at the stamp edge
    % Author : A.M. Krassilchtchikov (Nov 2026)
    FineStep = 0.1;
    % sources with a close neighbour in the same image: the light of the neighbour shifts the wide centroid
    Dist  = hypot(CoreXY(:,1) - CoreXY(:,1).', CoreXY(:,2) - CoreXY(:,2).');
    Dist(ImInd ~= ImInd.' | eye(numel(ImInd))==1) = Inf;
    Near  = min(Dist, [], 2) < 2.*Args.CutRadius;
    X(Near) = CoreXY(Near,1);
    Y(Near) = CoreXY(Near,2);
    [Coef, Knot, X, Y, Flux] = reconstructPSF(Image, BackVar, ImInd, X, Y, Args.CutRadius);
    % the PSF on a fine grid, and its center
    NumHalf = round((Args.CutRadius+1)./FineStep);
    Vec     = (-NumHalf:NumHalf).'.*FineStep;
    Bas     = splineBasis(Vec, Knot);
    Fine    = full(Bas*Coef*Bas.');
    Cen     = NumHalf + 1;
    [Xc, Yc] = sourceCentroid(Fine, Cen, Cen, Args.CentRadius./FineStep);
    Width   = fineWidths(shiftedPSF(Coef, Knot, Vec, (Xc-Cen).*FineStep, (Yc-Cen).*FineStep), FineStep, Args.CutRadius);
    if Width.R50 > Args.CentRadius
        [Xc, Yc] = sourceCentroid(Fine, Xc, Yc, Width.R50./FineStep);
    end
    ShiftX  = (Xc-Cen).*FineStep;
    ShiftY  = (Yc-Cen).*FineStep;
    Fine    = shiftedPSF(Coef, Knot, Vec, ShiftX, ShiftY);
    Width   = fineWidths(Fine, FineStep, Args.CutRadius);
    if Width.R50 > 0.4.*min(Dist(:))
        warning('ultrasat:measureLabPSF:closeSources', ...
            'the sources are %.1f pix apart, too close for a PSF of R50 = %.1f pix: the result is not reliable', min(Dist(:)), Width.R50);
    end
    % the PSF stamp, one point per pixel, and the Gaussian fit to the PSF with 2 points per pixel
    PSF     = shiftedPSF(Coef, Knot, (-Args.CutRadius:Args.CutRadius).', ShiftX, ShiftY);
    HalfPix = shiftedPSF(Coef, Knot, (-Args.CutRadius:0.5:Args.CutRadius).', ShiftX, ShiftY);
    Fit     = fitGaussPixel(HalfPix, 0.5, [0 0], Width.R50);
    % summarize the results
    Result   = summarizeWidths(PSF./sum(PSF,'all'), X+ShiftX, Y+ShiftY, Width, Fit, BackVar(ImInd), Flux.*Width.Flux, Args);
    Fine(hypot(Vec, Vec.') > Args.CutRadius) = 0;   % not constrained by the data
    Result.PSFfine  = Fine./sum(Fine,'all');
    Result.FineStep = FineStep;
    % the light at the stamp edge, if it is above the noise
    Counts   = Flux.*Width.Flux;
    EdgeFrac = Width.EdgeFlux./Width.Flux;
    Noise    = sqrt(pi.*Args.CutRadius.^2.*(1-0.85.^2)./sum(Counts.^2./BackVar(ImInd)) + max(EdgeFrac,0)./sum(Counts));
    EdgeFrac = EdgeFrac .* (EdgeFrac > 5.*Noise);
end

function Result = summarizeWidths(PSF, X, Y, Width, Fit, BackVar, Flux, Args)
    % Compile the output structure
    % Input  : - PSF stamp
    %          - X
    %          - Y
    %          - structure from fineWidths
    %          - structure from fitGaussPixel
    %          - background variance
    %          - source counts within CutRadius
    %          - the arguments of measureLabPSF
    % Output : - structure (see measureLabPSF)
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Sig = Args.PSFeff .* Flux;   % source counts within R50
    Result.PSF  = PSF;
    Result.SNR  = Sig ./ sqrt(Sig + pi.*Width.R50.^2 .* BackVar);
    Result.R50  = Width.R50;
    Result.R80  = Width.R80;
    Result.R50opt = opticalRadius(Width.R50, 0.5);
    Result.R80opt = opticalRadius(Width.R80, 0.8);
    Result.X    = X;
    Result.Y    = Y;
    Result.FWHM = Width.FWHM;
    Result.FWHMopt   = Fit.FWHM;
    Result.FWHMmajor = Fit.FWHMmajor;
    Result.FWHMminor = Fit.FWHMminor;
    Result.Angle     = Fit.Angle;
    Result.X2   = Fit.X2;
    Result.Y2   = Fit.Y2;
    Result.XY   = Fit.XY;
end

function Ropt = opticalRadius(R, Level)
    % Remove the integration over the pixel from an encircled energy radius of a Gaussian PSF:
    %   R^2 = -2*log(1-Level)*Sigma^2, and the pixel adds 1/12 pix^2 to Sigma^2
    % Input  : - radius [pix]
    %          - level of the encircled energy
    % Output : - optical radius [pix] (NaN for a source narrower than the pixel)
    % Author : A.M. Krassilchtchikov (Nov 2026)
    R2   = R.^2 + 2.*log(1-Level)./12;
    Ropt = NaN;
    if R2 > 0
        Ropt = sqrt(R2);
    end
end

function [Radius, CumSum, R50] = measureGrowth(Image, X, Y, MaxRadius)
    % Curve of growth and R50 of a source in an image
    % Input  : - background-subtracted image
    %          - X of the source
    %          - Y of the source
    %          - maximal radius [pix]
    % Output : - radii [pix]
    %          - light within the radii
    %          - R50 [pix]
    % Author : A.M. Krassilchtchikov (Nov 2026)
    % the stamp is wider than MaxRadius, as the circle is not centered on a pixel
    [Stamp, RoundX, RoundY] = imUtil.cut.image2cutouts(Image, X, Y, MaxRadius+1);
    [Radius, CumSum] = growthCurve(Stamp, X-RoundX, Y-RoundY, MaxRadius);
    R50 = encircledRadius(Radius, CumSum./CumSum(end), 0.5);
end

function [Radius, CumSum] = growthCurve(Stamp, DX, DY, MaxRadius)
    % Light within circles around a source: the stamp is interpolated (spline) on a fine grid
    % Input  : - background-subtracted stamp (odd size)
    %          - X offset of the source from the stamp center [pix]
    %          - Y offset
    %          - maximal radius [pix]
    % Output : - radii [pix]
    %          - light within the radii
    % Author : A.M. Krassilchtchikov (Nov 2026)
    OverSamp = 10;     % points per pixel
    StepR    = 0.05;   % radius step [pix]
    Stamp = double(Stamp);
    Stamp(isnan(Stamp)) = 0;
    Half  = (size(Stamp,1)-1)./2;
    Vec   = (-Half-0.5+0.5./OverSamp : 1./OverSamp : Half+0.5);
    Interpolant = griddedInterpolant({-Half:Half, -Half:Half}, Stamp, 'spline', 'nearest');
    Fine  = Interpolant({Vec, Vec})./OverSamp.^2;
    MatR  = hypot(Vec-DX, Vec.'-DY);
    Nrad  = round(MaxRadius./StepR);
    Radius = (1:Nrad).'.*MaxRadius./Nrad;
    Bin    = max(ceil(MatR(:)./MaxRadius.*Nrad), 1);
    Flag   = Bin <= Nrad;
    CumSum = cumsum(accumarray(Bin(Flag), Fine(Flag), [Nrad 1]));
end

function Fine = fineStamp(Image, X, Y, Half, OverSamp)
    % Cut out a stamp centered on the source with a fine step (spline interpolation)
    % Input  : - background-subtracted image
    %          - X of the source
    %          - Y of the source
    %          - half-size [pix]
    %          - points per pixel
    % Output : - stamp of 2*Half*OverSamp+1 points, the source being in the central one
    % Author : A.M. Krassilchtchikov (Nov 2026)
    [Stamp, RoundX, RoundY] = imUtil.cut.image2cutouts(Image, X, Y, Half+1);
    Stamp = double(Stamp);
    Stamp(isnan(Stamp)) = 0;
    Pix   = -(Half+1):(Half+1);
    Vec   = (-Half*OverSamp:Half*OverSamp)./OverSamp;
    Interpolant = griddedInterpolant({Pix, Pix}, Stamp, 'spline', 'nearest');
    Fine  = Interpolant({Vec + Y-RoundY, Vec + X-RoundX});
end

function Result = fineWidths(Fine, Step, MaxRadius)
    % Curve of growth, R50, R80, and FWHM of a PSF on a fine grid
    % Input  : - PSF (odd size), centered on the source
    %          - step [pix]
    %          - radius of 100% light [pix]
    % Output : - structure with Flux, EdgeFlux (beyond 85% of the radius), R50, R80, FWHM
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Half   = (size(Fine,1)-1)./2;
    Vec    = (-Half:Half).*Step;
    % curve of growth: the radius of each point is taken over its area
    SubS   = 2;
    Off    = ((1:SubS)-0.5)./SubS - 0.5;
    Nrad   = round(MaxRadius./0.05);
    Radius = (1:Nrad).'.*MaxRadius./Nrad;
    CumSum = zeros(Nrad, 1);
    for Ix = 1:SubS
        for Iy = 1:SubS
            MatS = hypot(Vec + Off(Ix).*Step, Vec.' + Off(Iy).*Step);
            Bin  = max(ceil(MatS(:)./MaxRadius.*Nrad), 1);
            Flag = Bin <= Nrad;
            CumSum = CumSum + accumarray(Bin(Flag), Fine(Flag), [Nrad 1]);
        end
    end
    CumSum = cumsum(CumSum).*Step.^2./SubS.^2;
    Result.Flux = CumSum(end);
    Result.EdgeFlux = Result.Flux - interp1(Radius, CumSum, 0.85.*MaxRadius);
    Result.R50  = encircledRadius(Radius, CumSum./Result.Flux, 0.5);
    Result.R80  = encircledRadius(Radius, CumSum./Result.Flux, 0.8);
    Result.FWHM = fineFWHM(Fine, Step, MaxRadius, Result.R50);
end

function FWHM = fineFWHM(Fine, Step, MaxRadius, R50)
    % FWHM of a PSF on a fine grid from the area above the half maximum; a broad PSF is smoothed over 0.3*R50
    % Input  : - PSF (odd size), centered on the source
    %          - step [pix]
    %          - radius of 100% light [pix]
    %          - R50 [pix]
    % Output : - FWHM [pix]
    % Author : A.M. Krassilchtchikov (Nov 2026)
    MinR50 = 2.5;   % [pix] sharper sources are not smoothed
    FWHM   = NaN;
    if isfinite(R50)
        Half = (size(Fine,1)-1)./2;
        Vec  = (-Half:Half).*Step;
        MatR = hypot(Vec, Vec.');
        if R50 > MinR50
            Rsm  = 0.3.*R50./Step;
            Nsm  = ceil(Rsm);
            Kern = double(hypot(-Nsm:Nsm, (-Nsm:Nsm).') <= Rsm);
            Fine = conv2(Fine, Kern./sum(Kern,'all'), 'same');
        end
        Fine(MatR >= MaxRadius-1) = -Inf;
        FWHM = 2.*sqrt(sum(Fine > 0.5.*max(Fine(:)), 'all').*Step.^2./pi);
    end
end

function Result = fitGaussPixel(Stamp, Step, PosXY, R50)
    % Fit an elliptical Gaussian, integrated over the pixel, with imUtil.psf.fitFunPSF
    % Input  : - stamp (odd size) of a PSF as imaged
    %          - step [pix]
    %          - [X Y] of the source from the stamp center [pix]
    %          - R50 of the source [pix]
    % Output : - structure with FWHM, FWHMmajor, FWHMminor, Angle, X2, Y2, XY (NaN if the fit fails)
    % Author : A.M. Krassilchtchikov (Nov 2026)
    MinSigma = 0.15;   % [pix]
    Result = struct('FWHM',NaN, 'FWHMmajor',NaN, 'FWHMminor',NaN, 'Angle',NaN, 'X2',NaN, 'Y2',NaN, 'XY',NaN);
    Half   = (size(Stamp,1)-1)./2;
    NumFit = min(Half, ceil(max(8, 1.5.*R50)./Step));
    Ind    = Half + 1 + (-NumFit:NumFit);
    Stamp  = Stamp(Ind, Ind);
    Stamp(isnan(Stamp)) = 0;
    Vec    = (-NumFit:NumFit).*Step;
    Norm0  = sum(Stamp,'all').*Step.^2;
    if isfinite(R50) && Norm0 > 0
        Sig0 = max(R50./1.18, 0.4);
        Par0 = [Sig0 0 Sig0 PosXY];
        LB   = [0   MinSigma -20 MinSigma PosXY-2];
        UB   = [Inf 20        20 20       PosXY+2];
        Opt  = optimoptions('lsqnonlin', 'Display','off');
        try
            Fit = imUtil.psf.fitFunPSF(Stamp./Norm0, 'Funs',{@(Par,~,~) gaussPixel(Par, Vec)}, 'Par0',{Par0}, ...
                                       'Norm0',1, 'LB',LB, 'UB',UB, 'LsqOptions',Opt);
            Par = Fit.Par(2:end);
            Result.X2 = Par(1).^2;
            Result.XY = Par(1).*Par(2);
            Result.Y2 = Par(2).^2 + Par(3).^2;
            AB        = imUtil.psf.mom2shape(Result.X2, Result.Y2, Result.XY);
            Factor    = 2.*sqrt(2.*log(2));
            Result.FWHMmajor = Factor.*AB.A;
            Result.FWHMminor = Factor.*AB.B;
            Result.FWHM      = Factor.*sqrt(AB.A.*AB.B);
            Result.Angle     = AB.Theta;
        catch ME
            warning('ultrasat:measureLabPSF:gaussFit', 'the Gaussian fit has failed: %s', ME.message);
        end
    end
end

function Model = gaussPixel(Par, Vec)
    % 2D Gaussian of unit integral, averaged over the pixel (Gauss-Legendre, 6x6 points)
    % Input  : - [L11 L21 L22 X Y], where L is the Cholesky factor of the covariance [pix]
    %          - vector of the coordinates (the same for X and Y) [pix]
    % Output : - matrix of the model
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Node   = [0.2386191860831969 0.6612093864662645 0.9324695142031521]./2;
    Weight = [0.4679139345726910 0.3607615730481386 0.1713244923791704]./2;
    Node   = [-Node Node];
    Weight = [Weight Weight];
    X = reshape(Vec - Par(4), 1, []);
    Y = reshape(Vec - Par(5), [], 1);
    Model = zeros(numel(Y), numel(X));
    for Ix = 1:numel(Node)
        U = (X + Node(Ix))./Par(1);
        for Iy = 1:numel(Node)
            V = (Y + Node(Iy) - Par(2).*U)./Par(3);
            Model = Model + Weight(Ix).*Weight(Iy).*exp(-0.5.*(U.^2 + V.^2));
        end
    end
    Model = Model./(2.*pi.*Par(1).*Par(3));
end

function [Coef, Knot, X, Y, Flux] = reconstructPSF(Image, BackVar, ImInd, X, Y, MaxRadius)
    % Reconstruct a PSF (as imaged) from several sources at different sub-pixel positions:
    %   the PSF is a smooth surface of cubic B-splines fitted to all the pixels of all the sources;
    %   the sources of the same image are fitted together
    % Input  : - background-subtracted image or a cube of images
    %          - background variance of each image
    %          - image number of each source
    %          - X of the sources
    %          - Y of the sources
    %          - radius of the PSF [pix]
    % Output : - matrix of the spline coefficients (rows - Y)
    %          - knots [pix]
    %          - X of the sources
    %          - Y of the sources
    %          - flux factors of the sources
    % Author : A.M. Krassilchtchikov (Nov 2026)
    GridStep = 0.5;    % [pix]
    Smooth   = 1e-2;   % strength of the smoothing, relative to the weight of the PSF core
    Niter    = 4;
    PosHalf  = 6;      % half-size of the box in which the positions are refined [pix]
    Nsrc     = numel(X);
    Image    = double(Image);
    Image(isnan(Image)) = 0;
    [SizeY, SizeX, Nimage] = size(Image);
    Edge     = MaxRadius + 1;
    Knot     = -(Edge+2.*GridStep):GridStep:(Edge+2.*GridStep);
    Nk       = numel(Knot);
    Diff2    = spdiags(ones(Nk,1)*[1 -2 1], 0:2, Nk-2, Nk);
    Penalty  = kron(Diff2.'*Diff2, speye(Nk)) + kron(speye(Nk), Diff2.'*Diff2);
    % only the coefficients which matter within MaxRadius
    Use      = find(hypot(Knot, Knot.') <= Edge + 2.*GridStep);
    Penalty  = Penalty(Use, Use);
    BasKnot  = splineBasis(Knot.', Knot);
    InCircle = hypot(Knot, Knot.') <= MaxRadius;
    % starting fluxes: the light within 4 pix
    Flux = zeros(Nsrc,1);
    for Isrc = 1:Nsrc
        MatR2 = ((1:SizeX)-X(Isrc)).^2 + ((1:SizeY).'-Y(Isrc)).^2;
        Frame = Image(:,:,ImInd(Isrc));
        Flux(Isrc) = max(sum(Frame(MatR2 < 16)), eps);
    end
    Coef = [];
    for Iter = 1:Niter
        % the PSF from all the sources
        Normal = sparse(Nk.^2, Nk.^2);
        Right  = zeros(Nk.^2, 1);
        Design = cell(Nsrc, 1);
        Pixels = cell(Nimage, 1);
        for Iim = 1:Nimage
            Frame    = Image(:,:,Iim);
            IndSrc   = find(ImInd(:).'==Iim);
            Pixels{Iim} = find(nearSource(X(IndSrc), Y(IndSrc), MaxRadius+0.5, [SizeY SizeX]));
            Total    = sparse(numel(Pixels{Iim}), Nk.^2);
            for Isrc = IndSrc
                Design{Isrc} = splineDesign(Pixels{Iim}, [SizeY SizeX], X(Isrc), Y(Isrc), Knot, Edge);
                Total        = Total + Flux(Isrc).*Design{Isrc};
            end
            if ~isempty(Coef)
                Model = Total*Coef(:);
            else
                Model = Frame(Pixels{Iim});
            end
            Var    = BackVar(Iim) + max(Model, 0);
            Var    = Var + 1e-6.*max(Var);
            Weight = spdiags(1./Var, 0, numel(Var), numel(Var));
            Normal = Normal + Total.'*Weight*Total;
            Right  = Right + Total.'*(Weight*Frame(Pixels{Iim}));
        end
        % scaled by the weight of the coefficients within 1 pix of the center: it grows with the
        % source flux as the data do, so the smoothing does not flatten the core of bright sources
        Core  = find(hypot(Knot, Knot.') <= 1);
        Scale = full(mean(diag(Normal(Core,Core))));
        Coef  = zeros(Nk, Nk);
        Coef(Use) = (Normal(Use,Use) + Scale.*(Smooth.*Penalty + 1e-9.*speye(numel(Use)))) \ Right(Use);
        Value = full(BasKnot*Coef*BasKnot.');
        Norm  = sum(Value(InCircle)).*GridStep.^2;
        Coef  = Coef./Norm;
        % fluxes, then positions
        for Iim = 1:Nimage
            Frame  = Image(:,:,Iim);
            IndSrc = find(ImInd(:).'==Iim);
            Model  = zeros(numel(Pixels{Iim}), numel(IndSrc));
            for Is = 1:numel(IndSrc)
                Model(:,Is) = Design{IndSrc(Is)}*Coef(:);
            end
            Var   = BackVar(Iim) + max(Model*(Flux(IndSrc).*Norm), 0);
            Var   = Var + 1e-6.*max(Var);
            Flux(IndSrc) = (Model./sqrt(Var)) \ (Frame(Pixels{Iim})./sqrt(Var));
            if Iter < Niter
                Resid = Frame;
                Resid(Pixels{Iim}) = Frame(Pixels{Iim}) - Model*Flux(IndSrc);
                for Is = 1:numel(IndSrc)
                    Isrc = IndSrc(Is);
                    Own  = Resid;
                    Own(Pixels{Iim}) = Own(Pixels{Iim}) + Model(:,Is).*Flux(Isrc);
                    VecX = max(1, round(X(Isrc))-PosHalf):min(SizeX, round(X(Isrc))+PosHalf);
                    VecY = max(1, round(Y(Isrc))-PosHalf):min(SizeY, round(Y(Isrc))+PosHalf);
                    Own  = Own(VecY, VecX);
                    Var  = BackVar(Iim) + max(Frame(VecY,VecX), 0);
                    Var  = Var + 1e-6.*max(Var,[],'all');
                    Fun  = @(Pos) positionChi2(Pos, Own, Var, VecX, VecY, Coef, Knot);
                    Pos  = fminsearch(Fun, [X(Isrc) Y(Isrc)], optimset('TolX',2e-3, 'TolFun',1e-2, 'Display','off', 'MaxIter',60));
                    if hypot(Pos(1)-X(Isrc), Pos(2)-Y(Isrc)) < 1
                        X(Isrc) = Pos(1);
                        Y(Isrc) = Pos(2);
                    end
                end
            end
        end
    end
end

function Flag = nearSource(X, Y, Radius, SizeYX)
    % Pixels within a radius of any of the sources
    % Input  : - X of the sources
    %          - Y of the sources
    %          - radius [pix]
    %          - [SizeY SizeX] of the image
    % Output : - logical matrix
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Flag = false(SizeYX);
    for Isrc = 1:numel(X)
        Flag = Flag | ((1:SizeYX(2))-X(Isrc)).^2 + ((1:SizeYX(1)).'-Y(Isrc)).^2 <= Radius.^2;
    end
end

function Design = splineDesign(Pixels, SizeYX, X, Y, Knot, Edge)
    % Design matrix of a source: values of the spline basis in the pixels
    % Input  : - indices of the pixels
    %          - [SizeY SizeX] of the image
    %          - X of the source
    %          - Y of the source
    %          - knots [pix]
    %          - half-size of the PSF [pix]
    % Output : - sparse matrix (pixels, coefficients)
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Nk   = numel(Knot);
    VecX = max(1, ceil(X-Edge)):min(SizeYX(2), floor(X+Edge));
    VecY = max(1, ceil(Y-Edge)):min(SizeYX(1), floor(Y+Edge));
    [RowB, Col, Val] = find(kron(splineBasis(VecX.'-X, Knot), splineBasis(VecY.'-Y, Knot)));
    [MatX, MatY]     = meshgrid(VecX, VecY);
    [IsIn, RowP]     = ismember(sub2ind(SizeYX, MatY(:), MatX(:)), Pixels);
    Keep   = IsIn(RowB);
    Design = sparse(RowP(RowB(Keep)), Col(Keep), Val(Keep), numel(Pixels), Nk.^2);
end

function Basis = splineBasis(Vec, Knot)
    % Cubic B-spline basis on equidistant knots
    % Input  : - column vector of coordinates
    %          - row vector of knots
    % Output : - sparse matrix (coordinates, knots)
    % Author : A.M. Krassilchtchikov (Nov 2026)
    T     = abs(Vec - Knot)./(Knot(2)-Knot(1));
    Basis = sparse((T<1).*(2./3 - T.^2 + T.^3./2) + (T>=1 & T<2).*(2-T).^3./6);
end

function Fine = shiftedPSF(Coef, Knot, Vec, ShiftX, ShiftY)
    % Values of a spline PSF on a grid with a shifted origin
    % Input  : - spline coefficients
    %          - knots [pix]
    %          - column vector of coordinates [pix]
    %          - X of the origin [pix]
    %          - Y of the origin [pix]
    % Output : - matrix
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Fine = full(splineBasis(Vec+ShiftY, Knot)*Coef*splineBasis(Vec+ShiftX, Knot).');
end

function Chi2 = positionChi2(Pos, Stamp, Var, VecX, VecY, Coef, Knot)
    % Chi2 of a spline PSF at a given position, with the best flux
    % Input  : - [X Y]
    %          - stamp
    %          - variance
    %          - X of the stamp columns
    %          - Y of the stamp rows
    %          - spline coefficients
    %          - knots [pix]
    % Output : - chi2
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Model = full(splineBasis(VecY.'-Pos(2), Knot)*Coef*splineBasis(VecX.'-Pos(1), Knot).');
    Flux  = sum(Stamp.*Model./Var,'all')./sum(Model.^2./Var,'all');
    Chi2  = sum((Stamp - Flux.*Model).^2./Var,'all');
end

function R = encircledRadius(Radius, Frac, Level)
    % The first radius at which the curve of growth reaches a given level, linearly interpolated
    % Input  : - radii
    %          - encircled fraction of light
    %          - level
    % Output : - radius (NaN if the level is not reached)
    % Author : A.M. Krassilchtchikov (Nov 2026)
    Ind = find(Frac >= Level, 1);
    if isempty(Ind)
        R = NaN;
    elseif Ind == 1
        R = Radius(1);
    else
        R = Radius(Ind-1) + (Level - Frac(Ind-1))./(Frac(Ind) - Frac(Ind-1)).*(Radius(Ind) - Radius(Ind-1));
    end
end

function Back = clippedMean(Frame)
    % The background level of an image: the mean after 3-sigma clipping about it.
    %   Not the median: in an image in whole ADU with a noise of a few ADU (the low
    %   gain), the median is a whole number of ADU and differs from the background
    %   by up to half an ADU (issue #1507).
    % Input  : - an image.
    % Output : - the background level.
    % Author : A.M. Krassilchtchikov (Nov 2026)
    % Example: Back = clippedMean(Image);
    Values = double(Frame(isfinite(Frame)));
    Sigma  = tools.math.stat.rstd(Values);
    Back   = median(Values);
    for Iter = 1:5
        Back = mean(Values(abs(Values - Back) < 3*Sigma));
    end
end
