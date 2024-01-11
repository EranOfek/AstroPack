function [AltGeometrical] = invRefraction(AltRefracted, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Jan) 
    % Example: [AltGeometrical] = celestial.coo.invRefraction(30./RAD)

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
    
    for I=1:1:10
        AltRefractedRad1 =  AltRefractedRad + RefAng;
        [RefAng] = celestial.coo.refraction_wave(AltRefractedRad1, Args.Lam, Args.T, Args.P, Args.Pw);
        AltRefractedRad2 = AltRefractedRad1 -RefAng;
        [AltRefractedRad2 - AltRefracted]
        
    end
    AltGeometrical = 1;
end
