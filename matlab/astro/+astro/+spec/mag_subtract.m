function Result = mag_subtract(Mag, RefMag)
    % Subtract magnitudes
    % Input  : - Array of magnitudes.
    %          - Reference magnitudes to subtract from array of magnitudes.
    % Output : - Subtracted magnitude
    % Author : Eran Ofek (Dec 2022)
    % Example: astro.spec.mag_subtract(12,13)
    
    F  = 10.^(-0.4.*Mag);
    FR = 10.^(-0.4.*RefMag);
    
    Result = -2.5.*log10(F - FR);
    
end
    
