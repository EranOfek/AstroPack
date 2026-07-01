function [CubePSF, XY] = createSourceCube(PSF0, X1Y1, Flux, Args)
    % Rescale, rotate, and shift a cube/cell-array of PSF stamps to whole pixel positions
    %     This is a low-level function to be used for source injection into
    %     an astronomical image or source removal therefrom
    % Input  : - a cube or a cell array of PSF stamps or a single PSF stamp
    %          - a 2-column (X, Y) table of injection positions
    %          - a vector of source flux values or a single value for all the sources
    %          * ...,key,val,...
    %          'Oversample' - PSF scaling factor(s) (1 scalar, 2 scalars, 1 vector,
    %                    a 2-column matrix of scaling factors), do not rescale if Oversample = 0 (default)
    %          'RotAngle' - PSF rotation angle(s): a scalar or a vector
    %          'Recenter' - true (shift the stamps according to X1Y1) or
    %                       false (just round the X1Y1 values to XY and do not shift the PSF)
    %          'RecenterMethod' - 'lanczos' (default), 'fft', or 'nearest'; usually 'nearest' goes with Oversampling > 1
    %          'FixPSFWings' - logical, whether to suppress PSF wings 
    %          'EmptyPSFsize' - size of the output empty PSF for the case when an empty PSF was given at input
    % Output : - a cube / cell array of shifted, rescaled and fluxed PSF stamps
    %          - a 2-column (X, Y) table of whole pixel injection positions
    % Author : A.M. Krassilchtchikov (2024 May)
    % Example: for i = 1:10; P(:,:,i) = imUtil.kernel2.gauss([4 4 0],[24 24]) + 1e-2*rand(24,24); end
    %          X1Y1 = 100.*rand(10,2); Flux = 100.*rand(10,1);
    %          [CubePSF, XY] = imUtil.art.createSourceCube(P, X1Y1, Flux, 'Recenter', false, 'Oversample', 3, 'FixPSFWings', true);
    %
    %          for i = 1:3; P{i} = imUtil.kernel2.gauss([4 4 0],[21+3*i 21+3*i]) + 1e-2*rand(21+3*i,21+3*i); end
    %          X1Y1 = 100.*rand(3,2); Flux = 100.*rand(3,1);
    %          [CubePSF, XY] = imUtil.art.createSourceCube(P, X1Y1, Flux, 'Recenter', false, 'Oversample', 3, 'FixPSFWings', true);
    arguments
        PSF0
        X1Y1
        Flux
        Args.Oversample          = 0;
        Args.RotAngle            = [];
        Args.Recenter    logical = true;
        Args.RecenterMethod      = 'lanczos';  % lanczos, fft, or nearest
        
        Args.FixPSFWings   logical  = false;
        Args.WingsMethod         = 'analytic';
        Args.WingsPowerLaw       = 2;
        Args.SuppressFun         = @imUtil.kernel2.cosbell;
        Args.SuppressThreshold   = 1e-3;
        Args.SuppressFunPars     = 3; % or # from edge
        Args.EmptyPSFsize        = [25 25];
    end

    Nsrc = size(X1Y1,1);           % get the number of input sources

    % whole pixel coordinates and subpixel shifts
    XY      = max(round(X1Y1), 1); % the rounding should not produce 0
    XYshift = X1Y1 - XY;

    % if the input PSF is empty, give an empty cube
    if isempty(PSF0)
        CubePSF = zeros([Args.EmptyPSFsize 0]);
        return
    end

    % check the number of input flux values
    if numel(Flux) == 1
        Flux = repmat(Flux, 1, Nsrc)';
    elseif numel(Flux) ~= Nsrc
        error ('The size of the source flux vector does not match that of the coordinate matrix');
    end

    % check the size and type of PSF stamps
    if ismatrix(PSF0)
        PSF = repmat(PSF0, [1 1 Nsrc]);
    elseif iscell(PSF0)
        if numel(PSF0) ~= Nsrc
            error ('The size of the PSF array does not match that of the coordinate matrix');
        end
        PSF = PSF0;
    else
        if size(PSF0,3) ~= Nsrc
           error ('The size of the PSF stack does not match that of the coordinate matrix');
        end
        PSF = PSF0;
    end

    % shift and resample the PSF stamps, forcing odd-sized and normalized stamps
    if Args.Recenter || all(Args.Oversample > 0)
        PSF = imUtil.psf.shiftResampleRotate(PSF,XYshift,Args.Oversample,Args.RotAngle,...
            'ForceOdd',true,'Recenter',Args.Recenter,'RecenterMethod',Args.RecenterMethod,'Renorm',true);
    end

    % suppress PSF wings
    if Args.FixPSFWings
        if iscell(PSF)
            for Isrc = 1:Nsrc
%                 PSF{Isrc} = imUtil.psf.suppressWings(PSF{Isrc}, Args.SuppressWingsArgs{:});
                [PSF{Isrc},~] = imUtil.psf.wingsFix(PSF{Isrc},'WingsMethod',Args.WingsMethod,...
                                                             'SuppressThreshold',Args.SuppressThreshold,...
                                                             'WingsPowerLaw',Args.WingsPowerLaw,...
                                                             'SuppressFun',Args.SuppressFun,...
                                                             'SuppressFunPars',Args.SuppressFunPars,...
                                                             'ExtendedSize',Args.ExtendedSize);
            end
        else
            for Isrc = 1:Nsrc
%                 PSF(:,:,Isrc) = imUtil.psf.suppressWings(PSF(:,:,Isrc), Args.SuppressWingsArgs{:});
                [PSF(:,:,Isrc),~] = imUtil.psf.wingsFix(PSF(:,:,Isrc),'WingsMethod',Args.WingsMethod,...
                                                             'SuppressThreshold',Args.SuppressThreshold,...
                                                             'WingsPowerLaw',Args.WingsPowerLaw,...
                                                             'SuppressFun',Args.SuppressFun,...
                                                             'SuppressFunPars',Args.SuppressFunPars,...
                                                             'ExtendedSize',Args.ExtendedSize);
            end
        end
    end

    % make fluxed PSF cubes
    if iscell(PSF)
        CubePSF = cell(Nsrc,1);
        for Ipsf = 1:Nsrc
            CubePSF{Ipsf} = Flux(Ipsf) .* PSF{Ipsf};
        end
    else
        CubePSF = reshape(Flux, 1, 1, Nsrc) .* PSF;
    end
end
