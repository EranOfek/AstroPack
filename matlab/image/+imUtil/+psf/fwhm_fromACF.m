function [FWHM, Nstars, Info, ACF] = fwhm_fromACF(Image, Args)
    % Estimate FWHM using the auto-correlation function
    %     This should be used when the FWHM is large or annulus shape.
    % Input  : - An image.
    %          * ...,key,val,... 
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of region in which to
    %                   measure FWHM. If empty use entire image. Default is [].
    %            'HalfSize' - Image half size. If 'CCDSEC' is empty, and this
    %                   argument is provided, then run this program on centeral
    %                   image with this half size. Default is [].
    %
    %            'BackStep' - Expedite background calculation, by taking
    %                   every N pixel. Default is 1.
    %            'CorrFrac' - Correlation fraction that defines the FWHM.
    %                   If the cumulative flux in the first bin is largre
    %                   than this value, then the FWHM is linearly
    %                   interpolated within the first step, according to
    %                   the flux fraction.
    %                   Default is 0.65.
    %            'Nsigma0' - Number of sigmas above image std which will be
    %                   set to zero. Default is 10.
    %            'MaxRadius' - Max radius [pix] for which to calculate the
    %                   ACF. Default is 200.
    %            'Step' - Step size at which to calculate the ACF.
    %                   Default is 1.
    %            'Convert2single' - If true, then will convert input to
    %                   single. Default is true.
    %            'SatLevel' - Saturation level. If the median of pixels are above
    %                   this value, then declare the image as saturated.
    %                   Default is 30000
    %            'UseMexRP' - Use mex function to calculate radial profile.
    %                   Default is false.
    %
    % Output : - The estimated FWHM.
    %            This is not formally the FWHM, but a factor that scales
    %            like the FWHM.
    %          - Number of stars (for consistency with other function.
    %            Always NaN.
    %          - An information structure with additional information.
    %            .Status field indicate if the image is not saturated and
    %                   ACF is not NaN.
    %          - ACF image.
    % Author : Eran Ofek (2024 Nov) 
    % Example: [FWHM, Res]=imUtil.psf.fwhm_fromACF(Image)
    % for Sig=1:1:20, K=randn(6001,6001).*0.01+10000.*imUtil.kernel2.gauss(Sig,[6001 6001]); [FWHM(Sig), Res]=imUtil.psf.fwhm_fromACF(K,'HalfSize',500); end
    % Calibrate CorrFrac:
    % F=dir('LAST*.fits'); Nf=numel(F);
    % for I=1:1:Nf,
    % AI= AstroImage(F(I).name);
    % [FWHM_ACF(I)] = imUtil.psf.fwhm_fromACF(AI.Image,'HalfSize',500, 'MaxRadius',50);
    % [FWHM_Mom(I)] = imUtil.psf.fwhm_fromMoments(AI.Image,'HalfSize',500);
    % end


    
    arguments
        Image
        Args.CCDSEC       = [];
        Args.HalfSize     = [];

        Args.Back              = [];
        Args.BackStep          = 1;
        Args.CorrFrac          = 0.65; %84;
        
        Args.Nsigma0           = 10; %0; %-10;
        Args.MaxRadius         = 200;
        Args.Step              = 1;
        Args.Convert2single logical = true;

        Args.SatLevel          = 30000;

        Args.UseMexRP logical    = false;
    end
    
    Nstars = NaN;
    
    if ~isempty(Args.CCDSEC)
        Image = Image(Args.CCDSEC(1,3):Args.CCDSEC(1,4), Args.CCDSEC(1,1):Args.CCDSEC(1,2));

    else
        if ~isempty(Args.HalfSize)
            SizeIm   = size(Image);
            CenterIm = floor(SizeIm.*0.5);
            Args.CCDSEC = [CenterIm(2)-Args.HalfSize, CenterIm(2)+Args.HalfSize, CenterIm(1)-Args.HalfSize, CenterIm(1)+Args.HalfSize];    
            Image = Image(Args.CCDSEC(1,3):Args.CCDSEC(1,4), Args.CCDSEC(1,1):Args.CCDSEC(1,2));
        end
    end


    % if ~isempty(Args.Trim)
    %     Image = imUtil.cut.trim(Image, Args.Trim, Args.TrimMethod);
    % end
    
    if Args.Convert2single
        Image = single(Image);
    end
    
    % background
    if isempty(Args.Back)
        MedianImage = median(Image,'all','omitnan');
    else
        MedianImage = Args.Back;
    end

    % check if image is saturated
    if MedianImage>Args.SatLevel
        % image is saturated
        FWHM = NaN;
        Info.Status = false;
    else

        % quick background subtraction
        Image(isnan(Image)) = 0;
        Image = Image - MedianImage;
        
        % std
        %Std = tools.math.stat.rstd(Image(:),1,1);
        %Std = tools.math.stat.mex.rstd_mex(Image(:));
        Std = tools.math.stat.std_mad(Image(:),1);

        Image(Image<(Args.Nsigma0.*Std)) = 0;
        %Image = Image - Args.Nsigma0.*Std;
        %Image = log10(Image);
        
        %ACF = fftshift(fft2(Image).*conj(fft2(Image)));
        ACF = fftshift(ifft2(fft2(Image).*conj(fft2(Image))));
        %ACF = ACF./(Std.^2);
        %ACF = ACF - median(ACF(:));
        
        SizeACF = size(ACF);
        
        if Args.UseMexRP
            % new version with MEX
            CenterPix = floor((SizeACF + 1).*0.5);  % [Y, X]
            [Rad, Mean] = imUtil.psf.mex.radialProfile_mex(ACF, CenterPix(2), CenterPix(1), Args.MaxRadius, Args.Step);

            CumVal = cumsum(Mean);
            CumVal = CumVal./CumVal(end);
        else
            % old code
            RR = imUtil.psf.radialProfile(ACF, [], 'Cut',true, 'Step',Args.Step, 'Radius',Args.MaxRadius);
           
            % legacy:
            Rad = RR.MeanR(2:end);
            CumVal = cumsum(RR.MeanV(2:end));
            %Rad = RR.MeanR(1:end);
            %CumVal = cumsum(RR.MeanV(1:end));

            CumVal = CumVal./CumVal(end);
        end

        if any(isnan(CumVal))
            FWHM = NaN;
            Info.Status = false;
        else
            % possible inconsistency: 
            %   This is half FWHM? + Frac doesn't make sense


            % In order to make sure that CumVal is monothonic increasing
            % function - add epsilon (Issue #853):
            CumVal = CumVal(:) +  (1:1:numel(CumVal)).'.*1e-7;

            % THIS SHOULD BE UNCOMMENTED
            % BUT NEED TO VERIFY THIS IS NOT AFFECTING LAST FOCUS
            % OPERATIONS
            %%%
            %if CumVal(1)>=Args.CorrFrac
            %   % PSF is smaller than Step size
            %   % interpolate:
            %   FWHM = Args.Step .*Args.CorrFrac./CumVal(1);
            %else
                FWHM = interp1(CumVal, Rad(:), Args.CorrFrac);
            %end
            %%%

            
            Info.Rad = Rad;
            Info.CumVal = CumVal;
            %plot(Result.Rad, RR.MeanV(2:end)); %Result.CumVal)
    
            Info.Status = true;
        end
    end

end
