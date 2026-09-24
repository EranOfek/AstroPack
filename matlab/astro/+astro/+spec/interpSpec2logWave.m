function [WaveN, SpecN] = interpSpec2logWave(Wave, Spec, Args)
    % Interpolate spectrum into a log-wavelength grid.
    % Input  : - A vector of wavelength
    %          - A vector or matrix of spectra. If matrix, then the
    %            dimension of the wavelength is set by Args.DimW
    %            (default is 1).
    %          * ...,key,val,... 
    %            'DimW' - Dimension of the wavelength in the input spectra
    %                   matrix. If input spectra is a vector then this
    %                   argument is ignored. Default is 1.
    %            'OverN' - Oversampling in number of points.
    %                   If empty, then oversampling is set to StopW/StartW.
    %                   Default is [].
    %            'InterpMethod' - Interolation method. Default is 'linear'.
    % Output : - A column vector of new wavelength grid.
    %          - A column vector or a matrix of spectra grid.
    %            The wavelength grid is always along the 1st dim,
    %            regardless of the input DimW.
    % Author : Eran Ofek (2026 Feb) 
    % Example: W=(4000:10:9000).'; Spec=rand(numel(W),3);
    %          [NW,NS]=astro.spec.interpSpec2logWave(W,Spec);

    arguments
        Wave
        Spec
        Args.DimW              = 1;
        Args.OverN             = []; 
        Args.InterpMethod      = 'linear';
    end

    Wave = Wave(:);

    if isvector(Spec)
        Args.DimW = 1;
        Spec = Spec(:);
    else
        if Args.DimW==2
            Spec = Spec.';
        end
    end

    StartW = min(Wave);
    StopW  = max(Wave);
    if isempty(Args.OverN)
        Args.OverN = StopW./StartW;
    end

    Npt    = ceil(numel(Wave).*Args.OverN);

    WaveN  = logspace(log10(StartW), log10(StopW), Npt).';
    
    SpecN  = interp1(Wave, Spec, WaveN, Args.InterpMethod);

end
