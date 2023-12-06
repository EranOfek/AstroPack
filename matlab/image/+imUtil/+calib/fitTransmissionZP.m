function fitTransmissionZP(InstMag, Err, SpecMatrix, Args)
    %
    % Example: InstMag = rand(100,1); Err=rand(100,1); SpecMatrix=rand(343,100);
    %          imUtil.calib.fitTransmissionZP(InstMag, Err, SpecMatrix)
    
    arguments
        InstMag
        Err
        SpecMatrix                         % by default columns contain spectra
        Args.Transpose logical  = false;   % transpose SpecMatrix
        Args.SpecWave           = 'GAIA-DR3';  % or vector of wavelength [Nwave, Nsrc]
        Args.IsFlux logical     = true;  % InstMag is in flux units
        Args.ZP                 = 25;     % ZP for mag to flux conversion
        Args.Trans              = [3800 0; 3801 1; 8000 1; 8001 0];
        Args.PolyOrder          = [];
        Args.PolyFun            = {@(x) x, @(x) 0.5.*(3.*x.^2 - 1), @(x) 0.5.*(5.*x.^3 - 3.*x), @(x) 0.125.*(35.*x.^4 -30.*x.^2 + 3), @(x) 0.125.*(63.*x.^5 -70.*x.^3 + 15.*x)};
        Args.InterpMethod       = 'cubic';
        Args.Algo               = 'lscov';   % '\','lscov'
    end
    
    % make sure that the matrix of spectra contains the spectra in columns
    if Args.Transpose
        SpecMatrix = SpecMatrix.';
    end
    
    % convert the magnitude to flux
    if Args.IsFlux
        InstFlux = InstMag;
        %Err doesnt change
    else
        InstFlux = 10.^(-0.4.*(InstMag - Args.ZP));
        Err      = InstFlux.*Err./1.086;
    end
    
    % get the wavelength of the spectra
    if ischar(Args.SpecWave)
        switch Args.SpecWave
            case 'GAIA-DR3'
                Wave = (3360:20:10200).';
            otherwise
                error('Unknown SpecWave option');
        end
    else
        Wave = Args.SpecWave(:);
    end
    Nwave = numel(Wave);
    
    % read the basic (unperturbed) transmission
    if isa(Args.Trans,'AstFilter')
        Trans = Args.Trans.nT;
    elseif isa(Args.Trans, 'AstroTransmission')
        Trans = [Args.Trans.Wave(:), Args.Trans.Tran(:)];
    elseif isa(Args.Trans, 'AstroSpec')
        Trans = [Args.Trans.Wave(:), Args.Trans.Flux(:)];
    elseif isnumeric(Args.Trans)
        Trans = Args.Trans;
    else
        error('Unknown Trans type');
    end
    
    % interp Trans to SpecMatrix wavelength
    TransT = interp1(Trans(:,1), Trans(:,2), Wave, Args.InterpMethod,0);
    
    % prepare polynomials expension
    % select polynomial order
    if isempty(Args.PolyOrder)
        PolyOrder = numel(Args.PolyFun);
    else
        PolyOrder    = Args.PolyOrder;
        Args.PolyFun = Args.PolyFun{1:1:PolyOrder};
    end
    
    % normalize wavelength
    MinW     = min(Trans(:,1));
    MaxW     = max(Trans(:,1));
    RangeW   = MaxW - MinW;
    MidW     = 0.5.*(MinW + MaxW);
    NormWave = (Wave - MidW)./(2.*RangeW);
    
    % Apply polynomials to normalized wavelength vector
    AllPolyWave = zeros(Nwave, PolyOrder);
    for Ipoly=1:1:PolyOrder
        AllPolyWave(:,Ipoly) = Args.PolyFun{Ipoly}(NormWave);
    end
    AllPolyWave = reshape(AllPolyWave,[Nwave 1 PolyOrder]);
    
    Npar = PolyOrder + 1;
    %Hflux = zeros(Nspec,Npar);
    
    %PolyPower = reshape((0:1:Args.PolyOrder),[1 1 Npar]);
    % integrate over a cube
    Hflux     = trapz(Wave, SpecMatrix.*TransT.*AllPolyWave);
    Hflux     = squeeze(Hflux);
    
    % solve
    switch Args.Algo
        case '\'
            Par    = Hflux\InstFlux;
            ParErr = nan(size(Par));
        case 'lscov'
            [Par, ParErr] = lscov(Hflux, InstFlux, 1./Err.^2);
        otherwise
            error('Unknown Algo option');
    end
    
    Resid = InstFlux - Hflux*Par;
    Std   = std(Resid);
    
    
    
    
end
