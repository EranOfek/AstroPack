function PSF = shift_resample(PSF, Shift, Oversample, Args)
    % Resample a PSF stack / cell array downto Oversampling = 1, make it odd-sized, and shift
    %     NB: after all the operations, some small negative values may appear at the borders
    %         one may need to employ imUtil.psf.suppressEdges 
    % Input  : - a PSF stack or a cell-array of PSFs 
    %          - an array of subpixel shifts
    %          - a vector or an array of oversampling factors
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
    %
    if ~iscell(PSF) % if all the PSFs are of the same dimensions 
        % rescale, but do not normalize as of yet
        % NB: will work only with Oversample = scalar or a 2-element vector,
        % i.e. the same oversampling factors for all the PSFs
        if all(Oversample > 0)
            PSF = imUtil.psf.oversampling(PSF, Oversample, 1,'ReNorm',false,'InterpMethod','bilinear');
        end        
        % force odd size (currently for square stamps only!)
        if Args.ForceOdd
            if rem( size(PSF,1), 2 ) == 0
                PSF = padarray(PSF, [1, 1], 0, 'post');
                PSF = imUtil.trans.shift_fft(PSF, 0.5, 0.5);
            end
        end        
        % shift
        if Args.Recenter
            PSF = imUtil.trans.shift_fft(PSF, Shift(:,1), Shift(:,2));
        end        
        % normalize
        if Args.Renorm
            PSF = imUtil.psf.normPSF(PSF);
        end 
       
    else % if the PSF stack is a cell array, we are do work one by one

        for Ipsf = 1:numel(PSF)
            % rescale
            if all(Oversample > 0)
                PSF{Ipsf} = imUtil.psf.oversampling(PSF{Ipsf}, Oversample, 1,'ReNorm',false,'InterpMethod','bilinear');
            end
            % force odd
            if Args.ForceOdd
                if rem( size(PSF{Ipsf},1), 2 ) == 0
                    PSF{Ipsf} = padarray(PSF{Ipsf}, [1, 1], 0, 'post');
                    PSF{Ipsf} = imUtil.trans.shift_fft(PSF{Ipsf}, 0.5, 0.5);
                end
            end
            % shift
            if Args.Recenter
                if numel(Shift) == 2 
                    ShiftX = Shift(1); ShiftY = Shift(2); 
                else
                    ShiftX = Shift(Ipsf,1); ShiftY = Shift(Ipsf,2);
                end
                PSF{Ipsf} = imUtil.trans.shift_fft(PSF{Ipsf}, ShiftX, ShiftY);
            end
            % normalize
            if Args.Renorm
                PSF{Ipsf} = imUtil.psf.normPSF(PSF{Ipsf});
            end
        end
    end
end
