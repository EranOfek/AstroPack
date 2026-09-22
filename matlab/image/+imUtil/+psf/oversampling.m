function Result = oversampling(PSF, OriginalOversampling, NewOversampling, Args)
        % resample an array of PSF stamps to a different pixel scale 
        % NOTE: resampling changes the sum of pixels, so usually we need to renormalize afterwards 
        % Input: - PSF stamp (X, Y)
        %        - the orginal oversampling of the stamp (1 or 2 values)
        %        - the new oversampling of the stamp (1 or 2 values) 
        %        * ...,key,val,...
        %        'InterpMethod' - interpolation method to use for resampling
        %                 NB: when downsampling by an integer factor the stamp is
        %                 first zero-padded symmetrically, if needed, so that its
        %                 size is an exact multiple of that factor. Otherwise the
        %                 output size has to be rounded and imresize then works at
        %                 a slightly different scale than the one requested (a
        %                 108 px stamp at Oversample 5 gives round(21.6) = 22 and
        %                 hence an effective factor of 4.91, i.e. a 1.7% error in
        %                 the PSF scale). Padding keeps the stamp centered, which
        %                 passing the scale to imresize instead would not.
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
        [PSF, XYsize] = matchResampleGrid(PSF, Factor);
        Result = zeros(XYsize(1), XYsize(2), NPSF);
        for Ipsf = 1:NPSF                
            Result(:,:,Ipsf) = imresize(PSF(:,:,Ipsf), XYsize, Args.InterpMethod);            
        end
        if Args.ReNorm
                Result = imUtil.psf.normPSF(Result,'ReNormMethod',Args.ReNormMethod);
        end
end

function [PSF, XYsize] = matchResampleGrid(PSF, Factor)
    % Pad a PSF stamp so that an integer downsampling factor divides its size exactly
    %     Only then can the output size be size/Factor, which is what makes imresize
    %     resample at exactly the requested scale. The padding is symmetric, so the
    %     stamp stays centered. If no symmetric padding can do it (an even factor
    %     with an odd stamp size), the size is rounded as before.
    % Input  : - A PSF stamp or cube.
    %          - The resampling factor (1 or 2 elements; > 1 upsamples).
    % Output : - The stamp, zero-padded if needed.
    %          - The output size [rows, columns] to resample to.
    % Author : A.M. Krassilchtchikov (Sep 2026)
    SizeRC = size(PSF, [1 2]);
    Fac    = Factor(:).';
    if isscalar(Fac)
        Fac = [Fac Fac];
    end
    PadRC  = [0 0];
    for Idim = 1:2
        Down = 1./Fac(Idim);
        if Fac(Idim) < 1 && abs(Down - round(Down)) < 1e-10 && mod(SizeRC(Idim), round(Down)) ~= 0
            Down = round(Down);
            IsFound = false;
            Ipad    = 0;
            while ~IsFound && Ipad < Down     % an even factor with an odd size has no solution
                Ipad = Ipad + 1;
                IsFound = mod(SizeRC(Idim) + 2.*Ipad, Down) == 0;
            end
            if IsFound
                PadRC(Idim) = Ipad;
            end
        end
    end
    if any(PadRC > 0)
        PSF    = padarray(PSF, PadRC, 0, 'both');
        SizeRC = size(PSF, [1 2]);
    end
    XYsize = round( Fac .* SizeRC );
end
