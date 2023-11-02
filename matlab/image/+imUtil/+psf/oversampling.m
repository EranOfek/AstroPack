function Result = oversampling(PSF, OriginalOversampling, NewOversampling, Args)
        % resample a PSF stamp to a different pixel scale 
        % NOTE: resampling changes the sum of pixels, so usually we need to renormalize afterwards 
        % Input: - PSF stamp (X, Y)
        %        - the orginal oversampling of the stamp 
        %        - the new oversampling of the stamp  
        %        * ...,key,val,...
        %        'InterpMethod' - which interpolation method to use for resampling
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
            Args.ReNormMethod   = 'int';  % 'int' | 'rms'
        end
        %
        NewSize = round( (NewOversampling./OriginalOversampling) .* size(PSF) );
        Result  = imresize(PSF, NewSize, Args.InterpMethod);
        if Args.ReNorm
            Result = imUtil.psf.normPSF(Result,'ReNormMethod',Args.ReNormMethod);
        end        
end