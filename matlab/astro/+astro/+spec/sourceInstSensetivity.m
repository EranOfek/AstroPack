function S=sourceInstSensetivity(Lambda, Response, Args)
    % Calculate the Source independent instrumental sensitivities
    %   S_nu or S_lambda (see Bohlin et al. 2014 for definitions)
    % Input  : - An array of wavelengths or frequencies.
    %            The integration is done over the dimension specified by
    %            the 'Dim' argument.
    %          - An array of responses (transmissions).
    %          * ...,key,val,...
    %            'Dim' - Dimension in the array over which to integrate.
    %                   Default is 1.
    %            'SpecificUnit' - Specific flux units.
    %                   Options are 'wave' (for wavelength) or 'freq' (for
    %                   frequency).
    %                   Default is 'wave'.
    %            'A' - Instrument effective area.
    %                   Default is 1.
    %            'AUnits' - Units of the effective area length.
    %                   Default is 'cm'.
    % Output : - Source independent instrumental sensitivity.
    % Author : Eran Ofek (Oct 2023)
    % Example: S=astro.spec.sourceInstSensetivity((1:10)',rand(10,3))
    
    arguments
        Lambda
        Response
        Args.A               = 1;       % effective area [cm^2]
        Args.AUnits          = 'cm';    % for [cm^2]
        Args.SpecificUnit    = 'wave';
        Args.Dim             = 1;
    end
    
    
    
    A = Args.A .* convert.length(Args.AUnits,'cm',1).^2;
    
    switch lower(Args.SpecificUnit)
        case 'wave'
            LambdaResponse = Lambda.*Response;
            
            S = constant.h.*constant.c./(A.*trapz(Lambda, LambdaResponse, Args.Dim));
        case 'freq'
            LambdaResponse = Response./Lambda;
            S = constant.h./(A.*trapz(Lambda, LambdaResponse, Args.Dim));
        otherwise
            error('Unknown SpecificUnit option');
    end
    
end
