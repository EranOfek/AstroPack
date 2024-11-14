function [FWHM, Result] = fwhm_fromACF(Image, Args)
    % Estimate FWHM using the auto-correlation function
    %     This should be used when the FWHM is large or annulus shape.
    % Input  : - An image.
    %          * ...,key,val,... 
    %            'TrimMethod' - Trim method. See 'Trim'.
    %                   Default is 'center'.
    %            'Trim' - If not empty, then will crop the input image prior
    %                   to processing.
    %                   If 'TrimMethod' is 'ccdsec', then this should be
    %                   [Xmin Xmax Ymin Ymax]
    %                   If 'TrimMethod' is 'center' then this is
    %                   [XhalfWidth YhalfWidth] relative to the image
    %                   center, or [X Y XhalfWidth YhalfWidth].
    %                   Default is [1000 1000].
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
    %          - A structure with additional information.
    %            .Status field indicate if the image is not saturated.
    % Author : Eran Ofek (2024 Nov) 
    % Example: [FWHM, Res]=imUtil.psf.fwhm_fromACF(Image)

    arguments
        Image
        Args.TrimMethod        = 'center';
        Args.Trim              = [1000 1000]; % [] - no trim
        Args.CorrFrac          = 0.84;
        
        Args.Nsigma0           = 10;
        Args.MaxRadius         = 200;
        Args.Step              = 1;
        Args.Convert2single logical = true;

        Args.SatLevel          = 30000;
    end
    
    
    if ~isempty(Args.Trim)
        Image = imUtil.cut.trim(Image, Args.Trim, Args.TrimMethod);
    end
    
    if Args.Convert2single
        Image = single(Image);
    end
    
    % check if image is saturated
    if median(Image,'all','omitnan')
        % image is saturated
        FWHM = NaN;
        Result.Status = false;
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
        
        FWHM = interp1(CumVal, Rad, Args.CorrFrac);
        
        
        Result.Rad = Rad;
        Result.CumVal = CumVal;
        %plot(Result.Rad, RR.MeanV(2:end)); %Result.CumVal)

        Result.Status = true;
    end

end
