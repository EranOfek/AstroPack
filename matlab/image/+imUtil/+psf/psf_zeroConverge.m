function PSF = psf_zeroConverge(PSF, Args)
    % Set the tail of the PSF to converge to zero.
    % Input  : - A PSF stamp.
    %          * ...,key,val,...
    %            'Method' - One of the following methods:
    %                   'radius' - Set the PSF value to zero above this
    %                       radius from PSF center.
    %                   'kernel' - Multiple the PSF above some radius, by a
    %                       kernel.
    %                   'mult' - Multiply PSF by a function ('Kernel') with
    %                           arguments ('KernelSigma').
    %                   Default is 'kernel'.
    %            'Rdaius' - Radius for setting the PSF to zero, ot above which
    %                   to multiple the PSF by the kernel.
    %                   Default is 7.
    %            'Kernel' - Kernel function handle, for the 'kernel' method.
    %                   Default is @imUtil.kernel2.gauss.
    %            'KernelSigma' - First argument to pass to the kernel
    %                   function. Default is 1.5.
    %            'SetNegativeTo0' - A logical indicating if to set negative
    %                   values to 0. Default is true.
    % Output : - The new PSF.
    % Author : Eran Ofek (Dec 2021)
    % Example: PSF = imUtil.psf.psf_zeroConverge(ones(16,16),'Method','radius')
    %          PSF = imUtil.psf.psf_zeroConverge(ones(16,16),'Method','kernel');
    
    arguments
        PSF
        Args.Method            = 'kernel';   % 'radius'|'kernel'|'mult'
        Args.Radius            = 5;        % radius above to set the zero (Radius method)
        Args.Kernel            = @imUtil.kernel2.gauss
        Args.KernelSigma       = 1.5
        Args.SetNegativeTo0 logical = true;
    end
    
    switch lower(Args.Method)
        case 'mult'
            
            Size = size(PSF);
            Fun  = Args.Kernel(Args.KernelSigma, [Size(2) Size(1)]);
            PSF  = PSF .* Fun;
            
        case 'radius'
            SizePSF  = size(PSF);
            CenterIJ = (SizePSF -1).*0.5;
            
            VecX = (-CenterIJ(2):1:CenterIJ(2));
            VecY = (-CenterIJ(1):1:CenterIJ(1)).';
            
            MatR2 = VecX.^2 + VecY.^2;
            
            PSF(MatR2 > Args.Radius.^2) = 0;
            
        case 'kernel'
            SizePSF  = size(PSF);
            Kernel = Args.Kernel(Args.KernelSigma, fliplr(SizePSF));
            
            CenterIJ = SizePSF.*0.5;
            
            ValAtRadius = Kernel(ceil(CenterIJ(1)), floor(ceil(CenterIJ(2))-Args.Radius));
            Kernel      = Kernel./ValAtRadius;
            Kernel(Kernel>1) = 1;
            
            % multiply the PSF by the kernel (at the edges)
            PSF = Kernel.*PSF;
            
        otherwise
            error('Unknown Method option');
    end
    if Args.SetNegativeTo0
        PSF(PSF<0) = 0;
    end

end
