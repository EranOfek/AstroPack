function Result = oversampling(PSF, OriginalOversampling, NewOversampling, Args)
        % resample an array of PSF stamps to a different pixel scale 
        % NOTE: resampling changes the sum of pixels, so usually we need to renormalize afterwards 
        % Input: - PSF stamp (X, Y)
        %        - the orginal oversampling of the stamp 
        %        - the new oversampling of the stamp  
        %        * ...,key,val,...
        %        'InterpMethod' - interpolation method to use for resampling
        %        'ReNorm'       - whether to renormalize the PSF stamp
        %        'ReNormMethod' - 'int' or 'rms' 
        % Output: - a resampled PSF stamp at the new oversampling scale
        % Author: A.M. Krassilchtchikov (Oct 2023)
        % Example: P0 = imUtil.kernel2.gauss; P1 = imUtil.psf.oversampling(P0,1,2); 
        %          P0 = imUtil.kernel2.gauss; P1 = imUtil.psf.oversampling(P0,3,5,'InterpMethod','lanczos2');
        arguments
            PSF
            OriginalOversampling = 1;
            NewOversampling      = 1;  
            Args.InterpMethod    = 'bilinear';
            Args.ReNorm          = true;
            Args.ReNormMethod    = 'int';  % 'int' | 'rms'
        end
        %
        NPSF = size(PSF,3);
        Factor = NewOversampling./OriginalOversampling;
        XYsize = round( Factor .* size(PSF(:,:,1)) );
        Result = zeros(XYsize(1), XYsize(2), NPSF);
        for Ipsf = 1:NPSF                
            Result(:,:,Ipsf) = imresize(PSF(:,:,Ipsf), XYsize, Args.InterpMethod);            
        end
        if Args.ReNorm
                Result = imUtil.psf.normPSF(Result,'ReNormMethod',Args.ReNormMethod);
        end
end