function [Area,AreaErr] = surfaceAreaFromMag(Mag, MagErr, Args)
    % Given its magnitude, calculate the surface area of a simple solar system reflector
    % Input  : - Mag
    %          - Mag error.
    %          * ...,key,val,...
    %            'r' - Sun-reflector distance [au]. Default is 1.
    %            'Delta' - Observer-reflector distance [au]. Default is 1.
    %            'P'     - Reflector albedo. Default is 1.
    %            'Mag_Sun' - Sun apparent mag in band. Default is -26.817.
    %            'Family' - Filter family. If this is given along with
    %                   filter name, than Mag_Sun is calculated for filter
    %                   using the solar spectrum.
    %                   Default is [].
    %            'Band' - Filter name. Default is [].
    %            'MagSys' - Magnitude system. Default is 'Vega'.
    % Output : - Surface area of reflector [cm^2].
    %          - Error in surface area of reflector [cm^2]
    % Author : Eran Ofek (Nov 2022)
    % Example: [Area,AreaErr] = celestial.SolarSys.surfaceAreaFromMag(12.6, 0.1);
    %          [Area,AreaErr] = celestial.SolarSys.surfaceAreaFromMag(12.6, 0.1,'r',1.05,'Delta',0.076,'P',0.15);
    %          [Area,AreaErr] = celestial.SolarSys.surfaceAreaFromMag(12.6, 0.1,'r',1.05,'Delta',0.076,'P',0.15,'Family','GAIA','Band','Bp');
    
    arguments
        Mag
        MagErr  = [];
        
        Args.r       = 1;
        Args.Delta   = 1;
        Args.P       = 1;
        Args.Mag_Sun = -26.817;
        Args.Family  = ''
        Args.Band    = '';
        Args.MagSys  = 'Vega';
    end

    if ~isempty(Args.Family) && ~isempty(Args.Band)
        AS = AstroSpec.sunSpec;                                    
        Args.Mag_Sun = AS.synphot(Args.Family,Args.Band,'IsOutMat',true,'MagSys',Args.MagSys);
    end
    
    Area = 10.^(0.4.*(Args.Mag_Sun - Mag) + log10(4.*pi) + 2.*log10(Args.Delta) + 2.*log10(Args.r) - log10(Args.P)) .*constant.au.^2;

    if ~isempty(MagErr)
        %vectorize(simplify(tools.math.symbolic.symerror(10.^(0.4.*(MagSun - Mag) + log10(4.*pi) + 2.*log10(Delta) + 2.*log10(r) - log10(P)),Mag)))
        AreaErr = (2.*log(10).*((10.^((4.*Args.Mag_Sun)./5 - (4.*Mag)./5 + 2475200567005943./1125899906842624).*MagErr.^2.*Args.Delta.^4.*Args.r.^4)./Args.P.^2).^(1./2))./5 .*constant.au.^2;
    else
        AreaErr = [];
    end
end