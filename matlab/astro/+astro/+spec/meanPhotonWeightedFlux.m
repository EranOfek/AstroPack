function [MeanF,ErrMeanF]=meanPhotonWeightedFlux(Lambda, F, Response, Args)
    % Calculate the mean flux (photon-weighted) multiplied by the response.
    %   According to one of the following options:
    %   <F_lamba> = integral(f_lambda *lambda*response)/integral(lambda*response)
    %   <F_nu> = integral(f_nu *nu^-1*response)/integral(nu^-1*response)
    % Input  : - An array of wavelengths or frequencies.
    %            The integration is done over the dimension specified by
    %            the 'Dim' argument.
    %          - An array of specific flux (f_lambda, or f_nu)
    %          - An array of responses (transmissions).
    %          * ...,key,val,...
    %            'Dim' - Dimension in the array over which to integrate.
    %                   Default is 1.
    %            'SpecificUnit' - Specific flux units.
    %                   Options are 'wave' (for wavelength) or 'freq' (for
    %                   frequency).
    %                   Default is 'wave'.
    %            'ErrF' - Optional array of errors in flux.
    %                   default is [].
    % Output : - Mean (photon-weighted) specific flux.
    %          - Error in mean specific flux.
    %            Empty, if error flux is not provided.
    % Reference: See Bohlin et al. 2014 for definitions
    % Author : Eran Ofek (Oct 2023)
    % Example: [MeanF,ErrMeanF]=astro.spec.meanPhotonWeightedFlux((1:10)', rand(10,3), rand(10,3), 'ErrF',0.01)
    
    arguments
        Lambda
        F
        Response
        Args.Dim          = 1;
        Args.SpecificUnit = 'wave';  % 'wave'|'freq'
        Args.ErrF         = [];
    end
    
    switch lower(Args.SpecificUnit)
        case 'wave'
            LambdaResponse     = Lambda.*Response;
        case 'freq'
            LambdaResponse     = Response./Lambda;
        otherwise
            error('Unknown SpecificUnit option');
    end
    
    if isempty(Args.ErrF)
        IntNum    = trapz(Lambda, F.*LambdaResponse, Args.Dim);
        IntDen = trapz(Lambda, LambdaResponse, Args.Dim);
        MeanF    = IntNum./IntDen;
        ErrMeanF  = [];
    else
        [IntNum,IntNumErr] = tools.math.integral.trapzErr(Lambda, F.*LambdaResponse, Args.ErrF.*LambdaResponse, Args.Dim);
        IntDen = trapz(Lambda, LambdaResponse, Args.Dim);
        MeanF    = IntNum./IntDen;
        ErrMeanF = IntNumErr./IntDen;
    end
    
end

