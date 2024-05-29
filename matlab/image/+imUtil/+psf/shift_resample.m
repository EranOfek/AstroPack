function PSF = shift_resample(PSF, Shift, Oversample, Args)
    % Resample the PSF to Oversampling = 1, make it odd-sized, and shift
    %     NB: after all the operations, some small negative values may appear at the borders
    %         one may need to employ imUtil.psf.suppressEdges 
    % Input  : - a PSF stack or a cell-array of PSFs
    %          - array of subpixel shifts
    %          - vector or array of oversampling factors
    %          * ...,key,val,... 
    %         'Recenter' - whether to fftshift the PSFs on the subpixel size 
    %         'Renorm'   - whether to renormalize the stamps
    %         'ForceOdd' - whether to make the even-sized stamps odd-sized
    %         
    % Output : - a stack or cell array of resampled and shifted PSFs
    % Author : A.M. Krassilchtchikov (2024 May)
    % Example:  for i = 1:4; P(:,:,i) = imUtil.kernel2.gauss([2 2 0],[12 12]) + 1e-2*rand(12,12); end
    %           Shift = rand(4,2); Oversample = 3; 
    %           imUtil.psf.shift_resample(P, Shift, Oversample, 'ForceOdd', true);
    arguments
        PSF
        Shift                  = [0 0];
        Oversample             = 0;
        Args.Recenter logical  = true;
        Args.Renorm   logical  = true;        
        Args.ForceOdd logical  = false;
    end
    
    if iscell(PSF)
        error('treatment of cell arrays of PSFs is not yet coded')
    end
        
    % rescale, but do not normalize as of yet     
    % NB: will work only with Oversample = scalar or a 2-element vector,
    % i.e. the same oversampling factors for all the PSFs 
    if all(Oversample > 0)
        PSF = imUtil.psf.oversampling(PSF,Oversample, 1,'ReNorm',false,'InterpMethod','bilinear');
    end
    
    % force odd size (currently for square stamps only!)    
    if rem( size(PSF,1), 2 ) == 0
        PSF = padarray(PSF, [1, 1], 0, 'post');
        PSF = imUtil.trans.shift_fft(PSF, 0.5, 0.5);
    end
    
    % shift
    if Args.Recenter
        PSF = imUtil.trans.shift_fft(PSF, Shift(:,1), Shift(:,2));
    end
    
    % normalize
    if Args.Renorm
        PSF = imUtil.psf.normPSF(PSF);
    end

end
