function PSF = shiftResampleRotate(PSF, Shift, Oversample, RotAngle, Args)
    % Resample a PSF stack / cell array downto Oversampling = 1, make it odd-sized, rotate and shift
    %     NB: after all the operations, some small negative values may appear at the borders
    %         one may need to employ imUtil.psf.suppressEdges 
    % Input  : - a 3D PSF array with the source index in the 3rd dimension or a cell-array of 2D PSFs 
    %          - an 2-column array of XY subpixel shifts
    %          - a vector of oversampling factors
    %          - a vector of rotation angles [deg]
    %          * ...,key,val,... 
    %         'Recenter' - true/false whether to fftshift the PSFs on the subpixel size 
    %         'RecenterMethod' - 'fft' or 'nearest'; usually 'nearest' goes with Oversmapling > 1
    %         'Renorm'   - true/false whether to renormalize the stamps
    %         'ForceOdd' - false/true whether to make the even-sized stamps odd-sized
    % Output : - a stack or a cell array of resampled and shifted PSFs
    % Author : A.M. Krassilchtchikov (2024 May)
    % Example:  for i = 1:4; P(:,:,i) = imUtil.kernel2.gauss([2 2 0],[12 12]) + 1e-2*rand(12,12); end
    %           Shift = rand(4,2); Oversample = 3; 
    %           imUtil.psf.shiftResampleRotate(P, Shift, Oversample, 'ForceOdd', true);
    arguments
        PSF
        Shift                  = [0 0];
        Oversample             = 0;
        RotAngle               = [];      % [deg] counterclockwise
        Args.Recenter logical  = true;
        Args.RecenterMethod    = 'fft';
        Args.Renorm   logical  = true;        
        Args.ForceOdd logical  = false;
    end
    %  
    if ~iscell(PSF)
        NumPsf = size(PSF,3);
    else
        NumPsf = numel(PSF);
    end   
    %
    if ~isempty(RotAngle) && numel(RotAngle) < NumPsf 
        RotAngle = repmat(RotAngle(1),1,NumPsf);
    end
    %
    if ~iscell(PSF) % if the PSFs are in an 3D array (hence they are of the same dimensions)        
        % rotate      
        if any( abs(RotAngle) > 1 & abs(RotAngle-360) > 1 ) 
            M = size(PSF,1);
            M1 = ceil(M * abs(cosd(RotAngle)) + M * abs(sind(RotAngle)) ); % the sizes of rotated images
            MaxSize = max(M1); RotPSF  = repmat(0,MaxSize,MaxSize,NumPsf);
            for Isrc = 1:1:NumPsf
                X1   = ceil(MaxSize/2-M1(Isrc)/2)+1; % the position of the lower left corner 
                RotPSF(X1:X1+M1(Isrc)-1,X1:X1+M1(Isrc)-1,Isrc) = imrotate(PSF(:,:,Isrc), RotAngle(Isrc), 'bilinear', 'loose');                      
            end
            PSF = RotPSF; 
        end
        % rescale, but do not normalize as of yet
        % NB: will work only with Oversample = scalar or a 2-element vector,
        % i.e. the same oversampling factors for all the PSFs
        if all(Oversample > 0) 
            if numel(Oversample) < 2
                Oversample(2) = Oversample(1);
            end
            if Args.Recenter && strcmpi(Args.RecenterMethod,'nearest') 
                % need to check the following block and, probably, make it faster and more compact                 
                ShiftX = round(Shift(:,1) * Oversample(1)); % to the scale of the oversampled PSF 
                ShiftY = round(Shift(:,2) * Oversample(2));
                ShiftedPSF = repmat(0,size(PSF)); 
                for Ipsf = 1:NumPsf
                    if ShiftX(Ipsf) > 0
                        ShiftedPSF(:, ShiftX(Ipsf)+1:end, Ipsf) = PSF(:, 1:end-ShiftX(Ipsf), Ipsf);
                    else
                        ShiftedPSF(:, 1:end+ShiftX(Ipsf), Ipsf) = PSF(:, -ShiftX(Ipsf)+1:end, Ipsf);
                    end
                    if ShiftY(Ipsf) > 0
                        ShiftedPSF(ShiftY(Ipsf)+1:end, :, Ipsf) = ShiftedPSF(1:end-ShiftY(Ipsf), :, Ipsf);
                        ShiftedPSF(1:ShiftY(Ipsf), :, Ipsf) = 0;
                    else
                        ShiftedPSF(1:end+ShiftY(Ipsf), :, Ipsf) = ShiftedPSF(-ShiftY(Ipsf)+1:end, :, Ipsf);
                        ShiftedPSF(end+ShiftY(Ipsf)+1:end, :, Ipsf) = 0;
                    end
                end
                PSF = ShiftedPSF; 
            end
            PSF = imUtil.psf.oversampling(PSF, Oversample, 1,'ReNorm',false,'InterpMethod','bilinear');
        end        
        % force odd size (currently for square stamps only!)
        if Args.ForceOdd
            if rem( size(PSF,1), 2 ) == 0
                PSF = padarray(PSF, [1, 1], 0, 'post');
                PSF = imUtil.trans.shift_fft(PSF, 0.5, 0.5);
            end
        end        
        % fft shift (usually, on subpixel scales) 
        if Args.Recenter && strcmpi(Args.RecenterMethod,'fft') 
            PSF = imUtil.trans.shift_fft(PSF, Shift(:,1), Shift(:,2));
        end        
        % normalize
        if Args.Renorm
            PSF = imUtil.psf.normPSF(PSF);
        end        
    else % if the PSF stack is a cell array, we are do work one by one
        for Ipsf = 1:NumPsf
            % rotate
            if any( abs(RotAngle) > 1 & abs(RotAngle-360) > 1 )               
                PSF{Ipsf} = imrotate(PSF{Ipsf}, RotAngle{Ipsf}, 'bilinear', 'loose');
            end
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
            if Args.Recenter && strcmpi(Args.RecenterMethod,'fft') 
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
