function [Result] = fit_rmsCurve(Mag, ResidStd, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Feb) 
    % Example: R=imUtil.calib.fit_rmsCurve(Mag, ResidStd)

    arguments
        Mag
        ResidStd
        Args.IsMag logical     = true;  % for Flux use false
    end

    % fit a function of the form
    % Where 1.086 / (SN) = ResidStd
    % SN = 1.086./ResidStd
    % F = 10.^(-0.4.*Mag)
    % SN = a*F/(c*F + b)
    
    Mag      = Mag(:);
    ResidStd = ResidStd(:);

    if Args.IsMag
        Flux = 10.^(-0.4.*Mag);
    else
        Flux = Mag;
    end

    if Args.IsMag
        Const = 1.086;
    else
        Const = 1;
    end
    SN = Const./ResidStd;
   
    Flux = Flux(:);

    % fit SN=a*Flux/(c*Flux + b)
    Fun = @(Par, Flux) Par(1).*Flux./sqrt(Par(3).*Flux + Par(2));

    Fnn = ~isnan(Flux) & ~isnan(SN) & ~isinf(SN);

    % approximation in order to findi initial values forparameters
    ParInit = Flux(Fnn)\sqrt(SN(Fnn));
    X0 = [ParInit 1 ParInit];

    Result.Par = lsqcurvefit(Fun, X0, Flux(Fnn), SN(Fnn),[0 0 0]);

    
    Result.PredResidStd = Const./Fun(Result.Par, Flux);



end
