function [FWHM, Nstars, Info] = fwhm_fromACF(Image, Args)
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
    %            'CorrFrac' - Correlation fraction that defines the FWHM.
    %                   Default is 0.84.
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
    %
    % Output : - The estimated FWHM.
    %            This is not formally the FWHM, but a factor that scales
    %            like the FWHM.
    %          - Number of stars (for consistency with other function.
    %            Always NaN.
    %          - An information structure with additional information.
    %            .Status field indicate if the image is not saturated and
    %                   ACF is not NaN.
    % Author : Eran Ofek (2024 Nov) 
    % Example: [FWHM, Res]=imUtil.psf.fwhm_fromACF(Image)

    arguments
        Image
        Args.CCDSEC       = [];
        Args.HalfSize     = [];

        
        Args.CorrFrac          = 0.84;
        
        Args.Nsigma0           = 10;
        Args.MaxRadius         = 200;
        Args.Step              = 1;
        Args.Convert2single logical = true;

        Args.SatLevel          = 30000;
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
    
    % check if image is saturated
    if median(Image,'all','omitnan')>Args.SatLevel
        % image is saturated
        FWHM = NaN;
        Info.Status = false;
    else

        % quick background subtraction
        Image(isnan(Image)) = 0;
        Image = Image - median(Image,'all');
        
        % std
        Std = tools.math.stat.rstd(Image(:),1,1);
        Image(Image<(Args.Nsigma0.*Std)) = 0;
        %Image = Image - Args.Nsigma0.*Std;
        %Image = log10(Image);
        
        %ACF = fftshift(fft2(Image).*conj(fft2(Image)));
        ACF = fftshift(ifft2(fft2(Image).*conj(fft2(Image))));
        %ACF = ACF./(Std.^2);
        %ACF = ACF - median(ACF(:));
        
        RR = imUtil.psf.radialProfile(ACF, [], 'Cut',true, 'Step',Args.Step, 'Radius',Args.MaxRadius);
        Rad = RR.MeanR(2:end);
        CumVal = cumsum(RR.MeanV(2:end));
        CumVal = CumVal./CumVal(end);
        
        if any(isnan(CumVal))
            FWHM = NaN;
            Info.Status = false;
        else

            FWHM = interp1(CumVal, Rad, Args.CorrFrac);
            
            
            Info.Rad = Rad;
            Info.CumVal = CumVal;
            %plot(Result.Rad, RR.MeanV(2:end)); %Result.CumVal)
    
            Info.Status = true;
        end
    end

end
