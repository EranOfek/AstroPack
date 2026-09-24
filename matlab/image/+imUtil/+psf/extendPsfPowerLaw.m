function PsfExt = extendPsfPowerLaw(Psf, NewSize, Args)
    % Extend an odd-sized centered PSF using a power-law wing.
    %
    % Description:
    %   The input PSF is kept unchanged inside InnerRadius.
    %   Outside OuterRadius the PSF is replaced by a circular power-law tail:
    %
    %       Tail(r) = Amp * r^(-Alpha)
    %
    %   In the transition annulus [InnerRadius, OuterRadius], each pixel is
    %   constructed by moving along its radial direction:
    %
    %       PsfExt(r,theta) = (1 - W) * PsfInner(theta) + W * PsfOuter
    %
    %   where PsfInner(theta) is the interpolated input PSF value at
    %   InnerRadius along direction theta, and PsfOuter is the circular
    %   power-law value at OuterRadius.
    %
    % Input  : - (Psf) 2D PSF stamp, odd x odd. The PSF is assumed centered on
    %            the central pixel.
    %          - (NewSize) New output size [Ny Nx], also odd x odd.
    %
    %          * ...,key,val,...
    %            'Alpha'        - Power-law index. Tail is proportional to r^(-Alpha).
    %                       Default is 1.
    %            'AnnulusRadii' - [InnerRadius OuterRadius].
    %                       Default is [5 8].
    %            'DeltaRadius'  - Width of the normalization annulus inside OuterRadius.
    %                       The power-law amplitude is estimated in:
    %                           [OuterRadius - DeltaRadius, OuterRadius]
    %                       Default is 1.
    %            'Renormalize'  - Preserve total PSF flux after extension.
    %                       Default is true.
    % Output : (PsfExt) Extended PSF image.
    % Author : ChatGPT + Eran Ofek (Jun 2026)
    % Example:
    %   PsfExt = imUtil.psf.extendPsfPowerLaw(Psf, [257 257], ...
    %       Alpha=1, AnnulusRadii=[5 10], DeltaRadius=1);
    
    arguments
        Psf 
        NewSize 
        Args.Alpha        = 1
        Args.AnnulusRadii = [5 8]
        Args.DeltaRadius  = 1
        Args.Renormalize  = true
    end
    
    Alpha        = Args.Alpha;
    AnnulusRadii = Args.AnnulusRadii(:).';
    InnerRadius  = AnnulusRadii(1);
    OuterRadius  = AnnulusRadii(2);
    DeltaRadius  = Args.DeltaRadius;
    
    if InnerRadius >= OuterRadius
        error('AnnulusRadii must satisfy InnerRadius < OuterRadius.');
    end
    
    if DeltaRadius <= 0
        error('DeltaRadius must be positive.');
    end
    
    [Ny0, Nx0] = size(Psf);
    Ny = NewSize(1);
    Nx = NewSize(2);
    
    % Require odd x odd input PSF.
    if mod(Ny0, 2) == 0 || mod(Nx0, 2) == 0
        error('Input Psf size must be odd x odd.');
    end
    
    % Require odd x odd output size.
    if mod(Ny, 2) == 0 || mod(Nx, 2) == 0
        error('NewSize must be odd x odd.');
    end
    
    if Ny < Ny0 || Nx < Nx0
        error('NewSize must be larger than or equal to size(Psf).');
    end
    
    if OuterRadius >= floor(min([Ny0, Nx0])./2)
        warning('OuterRadius is close to or outside the original PSF stamp edge.');
    end
    
    % Coordinates of the original PSF.
    Y0 = (1:Ny0).' - (Ny0 + 1)./2;
    X0 = (1:Nx0)  - (Nx0 + 1)./2;
    [Xgrid0, Ygrid0] = meshgrid(X0, Y0);
    R0 = hypot(Xgrid0, Ygrid0);
    
    % Coordinates of the enlarged PSF.
    Y = (1:Ny).' - (Ny + 1)./2;
    X = (1:Nx)  - (Nx + 1)./2;
    [Xgrid, Ygrid] = meshgrid(X, Y);
    R = hypot(Xgrid, Ygrid);
    
    % Estimate circularly symmetric power-law normalization near OuterRadius.
    %
    % Model:
    %   Psf(r) = Amp * r^(-Alpha)
    %
    % Therefore:
    %   Amp = Psf(r) * r^Alpha
    %
    % The normalization is estimated in:
    %   [OuterRadius - DeltaRadius, OuterRadius]
    NormInnerRadius = max(0, OuterRadius - DeltaRadius);
    
    NormAnnulusMask = R0 >= NormInnerRadius & ...
                      R0 <= OuterRadius & ...
                      isfinite(Psf);
    
    PositiveMask = NormAnnulusMask & Psf > 0;
    
    if any(PositiveMask, 'all')
        Amp = median(Psf(PositiveMask) .* R0(PositiveMask).^Alpha, 'omitnan');
    elseif any(NormAnnulusMask, 'all')
        Amp = median(Psf(NormAnnulusMask) .* R0(NormAnnulusMask).^Alpha, 'omitnan');
    else
        error('No valid pixels found in the normalization annulus.');
    end
    
    if ~isfinite(Amp)
        error('Could not estimate a finite power-law normalization.');
    end
    
    % Circularly symmetric power-law tail on the enlarged grid.
    Tail = Amp .* max(R, eps).^(-Alpha);
    
    % Embed original PSF in the center of the large image.
    PsfLarge = zeros(Ny, Nx, 'like', Psf);
    
    Y1 = (Ny - Ny0)./2 + 1;
    Y2 = Y1 + Ny0 - 1;
    
    X1 = (Nx - Nx0)./2 + 1;
    X2 = X1 + Nx0 - 1;
    
    PsfLarge(Y1:Y2, X1:X2) = Psf;
    
    % Construct output.
    PsfExt = zeros(Ny, Nx, 'like', Psf);
    
    % Region 1: inside InnerRadius, use original PSF.
    InnerMask = R <= InnerRadius;
    PsfExt(InnerMask) = PsfLarge(InnerMask);
    
    % Region 2: outside OuterRadius, use circular power law.
    OuterMask = R >= OuterRadius;
    PsfExt(OuterMask) = Tail(OuterMask);
    
    % Region 3:
    % In the transition annulus, interpolate along each radial ray from:
    %   value at InnerRadius along that ray
    % to:
    %   circular power-law value at OuterRadius.
    BlendMask = R > InnerRadius & R < OuterRadius;
    
    if any(BlendMask, 'all')
    
        ThetaBlend = atan2(Ygrid(BlendMask), Xgrid(BlendMask));
    
        Xinner = InnerRadius .* cos(ThetaBlend);
        Yinner = InnerRadius .* sin(ThetaBlend);
    
        % Sample the embedded original PSF at the inner boundary.
        PsfInner = interp2(Xgrid, Ygrid, PsfLarge, ...
                           Xinner, Yinner, 'linear', NaN);
    
        % Fallback to nearest interpolation if needed.
        Bad = ~isfinite(PsfInner);
        if any(Bad)
            PsfInnerNearest = interp2(Xgrid, Ygrid, PsfLarge, ...
                                      Xinner(Bad), Yinner(Bad), 'nearest', NaN);
            PsfInner(Bad) = PsfInnerNearest;
        end
    
        % Circular power-law value at the outer boundary.
        PsfOuter = Amp .* OuterRadius.^(-Alpha);
    
        % Smooth transition coordinate.
        T = (R(BlendMask) - InnerRadius) ./ (OuterRadius - InnerRadius);
    
        % Fifth-order smootherstep:
        % W=0 at InnerRadius, W=1 at OuterRadius.
        % First and second derivatives are zero at both boundaries.
        W = T.^3 .* (10 - 15.*T + 6.*T.^2);
    
        PsfExt(BlendMask) = (1 - W).*PsfInner + W.*PsfOuter;
    end
    
    % Optional flux preservation.
    if Args.Renormalize
        Sum0 = sum(Psf, 'all', 'omitnan');
        Sum1 = sum(PsfExt, 'all', 'omitnan');
    
        if Sum1 ~= 0 && isfinite(Sum0) && isfinite(Sum1)
            PsfExt = PsfExt .* (Sum0 ./ Sum1);
        end
    end

end