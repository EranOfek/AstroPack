function [AltGeometrical,RefAng] = invRefraction(AltRefracted, Args)
    % Refracted altitude (apparent) to geometrical altitude (real).
    % Input  : - Refracted altitude of star (apparent position).
    %          * ...,key,val,... 
    %            'InUnits' - Default is 'rad'.
    %            'OutUnits' - Default is 'rad'.
    %            'Lam' - Wavelength. Default is 5000 Ang.
    %            'T' - Temperature. Default is 15 C.
    %            'P' - Pressure. Default is 760 mmHg.
    %            'Pw' - Water vapor pressure. Default is 8 mmHg.
    %
    % Output : - Geometrical altitude of source.
    %          - Refraction angle needed to be added to Alt refracted in
    %            order to get Alt geometrica;.
    % Author : Eran Ofek (2024 Jan) 
    % Example: AltRefracted = 30./RAD;
    %          [AltGeometrical] = celestial.coo.invRefraction(AltRefracted)
    %          RefAng = celestial.coo.refraction_wave(AltGeometrical); 
    %          %AltRefracted should be equal to: AltGeometrical - RefAng

    arguments
        AltRefracted
        Args.InUnits           = 'rad';
        Args.OutUnits          = 'rad';
        
        Args.Lam               = 5000;
        Args.T                 = 15;
        Args.P                 = 760;
        Args.Pw                = 8;
    end
    RAD = 180./pi;
    
    AltRefractedRad = convert.angular(Args.InUnits, 'rad', AltRefracted);
    
    [RefAng] = celestial.coo.refraction_wave(AltRefractedRad, Args.Lam, Args.T, Args.P, Args.Pw);
    AltRefractedRad1 =  AltRefractedRad + RefAng;  % approximate unrefracted
    [RefAng1] = celestial.coo.refraction_wave(AltRefractedRad1, Args.Lam, Args.T, Args.P, Args.Pw);
    DeltaRef = (RefAng - RefAng1); %.*RAD.*3600
    RefAng = RefAng - DeltaRef;
    AltGeometrical = AltRefracted + RefAng;

    AngFactor = convert.angular('rad',Args.OutUnits);

end
