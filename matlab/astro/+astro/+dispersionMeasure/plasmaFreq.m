function [Omega_e] = plasmaFreq(Ne, Output)
    % Calculate the electron plasma frequency (nu_e/omega_e)
    %   Neglecting ions.
    %   See: https://arxiv.org/pdf/2007.02886
    % Input  : - n_e, electron density [cm^-3]
    %          - Output: 'omega'|'nu'. Default is 'nu'.
    % Output : - Plasma frequency
    % Author : Eran Ofek (2026 Jan) 
    % Example: nu_e=astro.dispersionMeasure.plasmaFreq(1)

    arguments
        Ne = 1;
        Output = 'nu';
    end

   

    Omega_e = sqrt(4.*pi.*Ne.*constant.e.^2./constant.me);
    if strcmpi(Output, 'omega')
        Omega_e = Omega_e ./ (2 .* pi); % Convert to frequency if output is 'omega'
    end


end
