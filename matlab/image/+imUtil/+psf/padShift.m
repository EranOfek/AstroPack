function Result=padShift(StampPSF, NewSizeIJ, Args)
    % Pad a PSF with zeros and shift its center
    % Input  : - A PSF stamp.
    %          - The [I, J] size of the the required output zero
    %            padded PSF.
    %          * ...,key,val,...
    %            'fftshift' - One of the following options:
    %                   'fftshift' - apply fftshift to the result
    %                   'ifftshift' - apply ifftshift to the result.
    %                   'none' - Returned centered PSF.
    %                   Default is 'none'.
    % Output : - An array with the zero padded PSF.
    % Example: P=imUtil.kernel2.gauss;
    %          R = imUtil.psf.padShift(P,[30 30]);
    %          imUtil.image.moment2(R,15 ,15)

    arguments
        StampPSF
        NewSizeIJ
        Args.fftshift    = 'none';
    end

    SizePSF  = size(StampPSF);

    % assuming SizePSF is odd
    if any((SizePSF.*0.5)==floor(SizePSF.*0.5))
        error('PSF stamp must have odd size - Even size PSF is not yet supported for padShift')
    end

    Result    = zeros(NewSizeIJ);
    NewCenterIJ = NewSizeIJ.*0.5;

    if floor(NewCenterIJ(1))==NewCenterIJ(1)
        % NewSizeIJ(1) is even number
        % NewCenterIJ(1) is whole number
        ShiftY = 0.5;
    else
        % NewSizeIJ(1) is odd number
        ShiftY = 0.0;
    end
    if floor(NewCenterIJ(2))==NewCenterIJ(2)
        % NewSizeIJ(2) is even number
        % NewCenterIJ(2) is whole number
        ShiftX = 0.5;
    else
        % NewSizeIJ(2) is odd number
        ShiftX = 0.0;
    end

    % fft shift stamp
    if ShiftX~=0 || ShiftY~=0
        StampPSF = imUtil.trans.shift_fft(StampPSF, ShiftX, ShiftY);
    end

    NewCenterIJ = ceil(NewCenterIJ);
    Result      = imUtil.cut.cutouts2image(StampPSF, Result, NewCenterIJ(2), NewCenterIJ(1));

    switch lower(Args.fftshift)
        case 'none'
            % do nothing
        case 'fftshift'
            Result = fftshift(fftshift(Result,1),2);
        case 'ifftshift'
            Result = ifftshift(ifftshift(Result,1),2);
        otherwise
            error('Unknown fftshift option');
    end
    
    
end
