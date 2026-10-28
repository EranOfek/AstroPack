function [Result] = measureLabPSF(Image, Args)
    % measure PSF characteristics of an ULTRASAT detector image of a laboratory point source  
    %     to be used by ELOP for focusing tests
    % Input  : - Image matrix (counts, e-) with 1 source [def. simulated with USim]
    %          * ...,key,val,... 
    % Output : - Resulting structure:
    %          .PSF  -- normalized PSF stamp
    %          .SNR  -- signal to noise ratio in an aperture of radius R50:
    %                   S / sqrt(S + pi*R50^2*B), S = PSFeff * source counts, B = background per pixel
    %          .R50  -- radius of 50% encircled energy [pix]
    %          .X, .Y -- source location on the matrix
    %          .FWHM -- FWHM [pix] from imUtil.psf.fwhm_fromMoments. NB: biased low for the
    %                   undersampled ULTRASAT PSF; R50 is the robust width measure
    %          .X2, .Y2, .XY -- second moments of the source
    %          If several sources are detected, the brightest one is measured.
    % Author : A.M. Krassilchtchikov (2024 Dec) 
    % Example: Res = ultrasat.measureLabPSF('T',10000,'XY',[1000 2300],'Plot',true)
    %          Res = ultrasat.measureLabPSF('DataFile','SimImage_tileB.fits')
    arguments
        Image          = 'sim';     
        Args.DataFile  = [];          % input data file with image in counts (e-)
        Args.GainMask  = [];          % input data file with gain mask (if present, it is presumed that DataFile is in ADU) 
        Args.LowGain   = 0.074;       % low gain coefficient
        Args.HighGain  = 1.185;       % high gain coefficient
        Args.T         = 3000;        % BB spectrum temperature [K] of a simulated source
        Args.XY        = [2000 2000]; % pixel coordinates of a simulated source 
        Args.CutRadius = 12;          % stamp size [pix] 
        Args.PSFeff    = 0.5;         % PSF efficiency at R50 (for SNR estimate)
        Args.PsfFunPar = {[0.1;0.7;1.5]}; % Gaussian sigmas [pix] of the detection templates; a source counts
                                      % as a star if a wider template fits better than the first one.
                                      % The ULTRASAT PSF is undersampled (sigma ~0.7 pix), so the
                                      % fwhm_fromMoments default [0.1 1.5 4] rejects it
        Args.Plot      = false;
    end
    
    % read the image from a FITS file
    if ~isempty(Args.DataFile)
        AI = AstroImage(Args.DataFile);
        Image = AI.Image;
    end
    
    % simualte an image if no numeric matrix is input 
    if ~isnumeric(Image)
        AI = ultrasat.usim('Cat',Args.XY,'Mag',16,'Exposure',[1 300],'SpecType','BB','Spec',Args.T,'OutType','AstroImage');
        Image = AI.Image;
    end
    %
    if ~isempty(Args.GainMask) % if a mask image is input, we assume that the input image is in ADU raher than in e-
        Image = ultrasat.ADU2e(Image); % should be tested 
    end

    % find the object, measure moments
    [FWHM,Nstars,Info] = imUtil.psf.fwhm_fromMoments(Image, 'PsfFunPar', Args.PsfFunPar);
    if Nstars < 1
        error('measureLabPSF: no source found in the image, exiting..');
    end
    % keep the brightest detection
    Peak = Image(sub2ind(size(Image), round(Info.Y1), round(Info.X1)));
    [~, Ib] = max(Peak);
    % measure the background (crude)
    Back = median(Image,'all','omitnan');
    % cut out the source PSF
    [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image-Back, Info.X1(Ib), Info.Y1(Ib), Args.CutRadius);
    % normalize the PSF
    % 'omitnan': image2cutouts now NaN-pads cutout pixels outside the
    % image (issue #1199); without omitnan a single edge pixel would
    % NaN out the whole normalization.
    PSF = Cube./sum(Cube,'all','omitnan');
    % calculate R50
    X0 = Args.CutRadius+1; Y0 = X0;        
    CurveOfGrowth = imUtil.psf.curve_of_growth(PSF,[X0, Y0],0.1);
    Ind50 = find(CurveOfGrowth.CumSum>0.5,1);
    R50   = CurveOfGrowth.Radius(Ind50);
    % calculate SNR
    Src = sum(Cube,'all','omitnan');
    Sig = Args.PSFeff * Src;   % source counts within R50
    SNR = Sig / sqrt(Sig + pi*R50^2 * Back); 
    % summarize the results
    Result.PSF = PSF;    
    Result.SNR = SNR; 
    Result.R50 = R50; 
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
