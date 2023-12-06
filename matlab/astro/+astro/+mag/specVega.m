function Spec = specVega(Type)
    % Return the Vega (Alpha Lyr) standar spectrum for Vega mag calculations
    % Input  : - Spectrum type:
    %            'alpha_lyr_mod_004' - STSCI model 4 from CALSPEC
    %                       Default.
    %            'dovi' - 'vega_spec.mat' file from Dovi.
    % Output : - Vega s[pectrum [Wave [Ang], f_lambda [erg/cm^2/s/A]].
    % Author : Eran Ofek (Oct 2021)
    % Example: Spec = astro.mag.specVega
    
    arguments
        Type = 'alpha_lyr_mod_004'
    end
    
    switch lower(Type)
        case 'alpha_lyr_mod_004'
            Spec = io.files.load2('alpha_lyr_mod_004.mat');
        case 'dovi'
            Spec = io.files.load2('vega_spec.mat');
        otherwise
            error('Unknown Type option');
    end
    
end
    