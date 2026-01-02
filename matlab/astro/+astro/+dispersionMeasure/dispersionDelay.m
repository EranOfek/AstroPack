function [Tau_Nu, DTau_DNu] = dispersionDelay(DM, Nu)
    % Time delay due to dispersion measure.
    % Input  : - Dispersion measure [cm^-3 pc].
    %          - Frequency [Hz].
    % Output : - Time delay between frequency Nu and ininite frequency [s].
    %          - dTau/dNu - The derivative of the time delay in respect to
    %            nu [s^2].
    % Author : Eran Ofek (2026 Jan) 
    % Example: astro.dispersionMeasure.dispersionDelay(500, 1e9)

    arguments
        DM
        Nu
    end

    Tau_Nu = DM.*constant.e.^2.*constant.pc./(2.*pi.*constant.me.*constant.c .* Nu.^2);
    DTau_DNu = -2.*Tau_Nu./Nu;
end
