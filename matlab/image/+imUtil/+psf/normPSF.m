function Result = normPSF(PSF, Args)
    % normalize a PSF stamp 
    % Input: - a PSF stamp (may be N+2 dimentional, N = 0..M, the first 2 dimensions are pixel dimensions X and Y)
    %        * ...,key,val,... 
    %        'ReNormMethod' - 'int' -- normalize to the sum of pixel values; 'rms' -- normalize to rms
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Examples: PSF = 10.*rand(5)
    %           PSF1 = imUtil.psf.normPSF(PSF)
    %           PSF2 = imUtil.psf.normPSF(PSF,'ReNormMethod','rms')
    %           sum(PSF1,'all')
    %           sum(PSF2,'all')
    arguments
        PSF
        Args.ReNormMethod = 'int' % 'int' or 'rms'
    end
    %
    switch Args.ReNormMethod
        case 'int'
            Result = PSF./sum(PSF,[1 2]);
        case 'rms'
            Result = PSF./rms(PSF,[1 2]);
        otherwise
            error('Requested renormalization method is not known');
    end
end