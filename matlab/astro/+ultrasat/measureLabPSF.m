function [Result] = measureLabPSF(Image, Args)
    % measure PSF characteristics of an ULTRASAT detector image of a laboratory point source  
    %     to be used by ELOP for focusing tests
    % Input  : - Image matrix (counts, e-, or ADU: see 'IsADU') with 1 source [def. simulated with USim]
    %          * ...,key,val,... 
    % Output : - Resulting structure:
    %          .PSF  -- normalized PSF stamp
    %          .SNR  -- signal to noise ratio in an aperture of radius R50:
    %                   S / sqrt(S + pi*R50^2*V), S = PSFeff * source counts,
    %                   V = measured background variance per pixel (it includes the read noise)
    %          .R50, .R80 -- radii of 50% and 80% encircled energy [pix] around .X, .Y,
    %                   100% being the light within CutRadius. The light is interpolated
    %                   between the pixels, so it is the PSF after the integration over
    %                   the pixel that is measured
    %          .X, .Y -- source location on the matrix: the centroid of the light within CentRadius
    %                   or, if R50 is larger than CentRadius, within R50.
    %                   NB: R80 of a lopsided PSF depends on the choice of the center
    %          .FWHM -- FWHM [pix] from imUtil.psf.fwhm_fromMoments: the median over all the
    %                   detections. NB: biased low for the undersampled ULTRASAT PSF;
    %                   R50 is the robust width measure
    %          .X2, .Y2, .XY -- second moments of the source from imUtil.psf.fwhm_fromMoments
    %                   (fixed Gaussian weight, around the detection and not .X, .Y)
    %          If several sources are detected, the brightest one is measured, unless SrcXY is given.
    % Author : A.M. Krassilchtchikov (2024 Dec) 
    % Example: Res = ultrasat.measureLabPSF('T',10000,'XY',[1000 2300],'Plot',true)
    %          Res = ultrasat.measureLabPSF('DataFile','SimImage_tileB.fits')
    %          Res = ultrasat.measureLabPSF(ImageADU,'IsADU',true)        % 14-bit ADU with the gain bit
    %          Res = ultrasat.measureLabPSF(ImageADU,'GainMask',GainMap)  % 13-bit ADU and a gain map
    %          Res = ultrasat.measureLabPSF(Image,'SrcXY',[470 465])      % the source next to this position
    arguments
        Image          = 'sim';     
        Args.DataFile  = [];          % input data file with image in counts (e-) or ADU
        Args.IsADU     = false;       % the input image is in ADU (14-bit words, the top bit is the gain)
        Args.GainMask  = [];          % gain mask (1 = low gain): matrix or file name. If present, it is
                                      % presumed that the input image is in ADU
        Args.LowGain   = 0.074;       % low gain coefficient
        Args.HighGain  = 1.185;       % high gain coefficient
        Args.T         = 3000;        % BB spectrum temperature [K] of a simulated source
        Args.XY        = [2000 2000]; % pixel coordinates of a simulated source 
        Args.SrcXY     = [];          % approximate [X Y] of the source to be measured in the input image
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
    
    % read the image from a FITS file
    if ~isempty(Args.DataFile)
        AI = AstroImage(Args.DataFile);
        Image = AI.Image;
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
        [~, Image] = ultrasat.ADU2e(Image, 'GainMask',GainMask, 'LowGain',Args.LowGain, 'HighGain',Args.HighGain);
    end

    % an integer image: the negative values would be lost at the background subtraction
    if isinteger(Image)
        Image = single(Image);
    end

    % find the object, measure moments
    [FWHM,~,Info] = imUtil.psf.fwhm_fromMoments(Image, 'PsfFunPar', Args.PsfFunPar, 'MinSN', Args.MinSN);
    % keep the detections with positions inside the image
    [SizeY, SizeX] = size(Image);
    IndX   = round(Info.X1);
    IndY   = round(Info.Y1);
    IndIn  = find(IndX>=1 & IndX<=SizeX & IndY>=1 & IndY<=SizeY);
    if isempty(IndIn)
        error('measureLabPSF: no source found in the image, exiting..');
    end
    if isempty(Args.SrcXY)
        % the brightest detection
        Peak    = Image(sub2ind(size(Image), IndY(IndIn), IndX(IndIn)));
        [~, Ib] = max(Peak);
    else
        % the detection nearest to the given position
        [Dist, Ib] = min(hypot(Info.X1(IndIn) - Args.SrcXY(1), Info.Y1(IndIn) - Args.SrcXY(2)));
        if Dist > Args.CutRadius
            error('measureLabPSF: no source found within %g pix of SrcXY, exiting..', Args.CutRadius);
        end
    end
    Ib = IndIn(Ib);
    % measure the background (crude) and its variance
    Back    = median(Image,'all','omitnan');
    BackVar = tools.math.stat.rstd(Image(:)).^2;
    ImageBS = Image - Back;
    % source center, curve of growth, R50
    [X, Y] = sourceCentroid(ImageBS, Info.X1(Ib), Info.Y1(Ib), Args.CentRadius);
    [Radius, CumSum, R50] = measureGrowth(ImageBS, X, Y, Args.CutRadius);
    % a broad source: CentRadius holds too little of its light, repeat with R50
    if R50 > Args.CentRadius
        [X, Y] = sourceCentroid(ImageBS, X, Y, R50);
        [Radius, CumSum, R50] = measureGrowth(ImageBS, X, Y, Args.CutRadius);
    end
    Src = CumSum(end);          % source counts
    R80 = encircledRadius(Radius, CumSum./Src, 0.8);
    % cut out the source PSF
    Cube = imUtil.cut.image2cutouts(ImageBS, X, Y, Args.CutRadius);
    % normalize the PSF
    % 'omitnan': image2cutouts now NaN-pads cutout pixels outside the
    % image (issue #1199); without omitnan a single edge pixel would
    % NaN out the whole normalization.
    PSF = Cube./sum(Cube,'all','omitnan');
    % warn if there is still light at the edge of the stamp
    Redge    = 0.85.*Args.CutRadius;
    EdgeFlux = Src - interp1(Radius, CumSum, Redge);
    EdgeErr  = sqrt(pi.*(Args.CutRadius.^2 - Redge.^2).*BackVar + max(EdgeFlux,0));
    if EdgeFlux > 0.03.*Src && EdgeFlux > 5.*EdgeErr
        warning('ultrasat:measureLabPSF:lightAtEdge', ...
            '%.1f%% of the light is found at %.1f-%g pix from the source: increase CutRadius or check for neighbours', ...
            100.*EdgeFlux./Src, Redge, Args.CutRadius);
    end
    % calculate SNR
    Sig = Args.PSFeff * Src;   % source counts within R50
    SNR = Sig / sqrt(Sig + pi*R50^2 * BackVar);
    % summarize the results
    Result.PSF = PSF;    
    Result.SNR = SNR; 
    Result.R50 = R50; 
    Result.R80 = R80;
    Result.X  = X; 
    Result.Y  = Y; 
    Result.FWHM = FWHM;
    Result.X2 = Info.X2(Ib);
    Result.Y2 = Info.Y2(Ib);
    Result.XY = Info.XY(Ib);
    % plot the PSF image on a log scale
    if Args.Plot
        figure(10)
        imagesc(log10(abs(PSF))); colorbar; title('lg(PSF)'); set(gca,'YDir','normal');
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
