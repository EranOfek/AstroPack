function [Res,Fun]=ps_lens(Args)
% Calculate deflection, magnification and time delay for point mass lens
% Package: astro
% Description: Calculate deflection, magnification and time delay for point
%              mass lens.
% Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'Mass' - Lens mass. Default is 1.
%            'MassUnits' - Lens mass units. Default is 'SunM'
%            'Dl'   - Lens distance or redshift. Default is 5000;
%            'Ds'   - Source distance or redshift. Default is 1e4;
%            'DistUnits' - Distance units. Default is 'pc'.
%            'Beta' - Impact parameter. Default is logspace(-3,3,7)'.
%            'BetaUnits' - Impact parameter units. Default is 'ThetaE'.
%            'BetaMin' - Minimum impact parameter. Default is 0.
%                       If provided than the lens source distance is
%                       calculated from sqrt(Beta^2 + BetaMin^2). BetaMin
%                       has the units specified in BetaUnits.
%            'OutUnits' - Units of angular output. Default is 'arcsec'.
% Output : - A structure containing the microlensing properties.
%          - A structure of useful functions.
% License: GNU general public license version 3
%     By : Eran O. Ofek                    May 2018
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Res=astro.microlensing.ps_lens
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Args.Mass                 = 1;
    Args.MassUnits            = 'SunM';
    Args.Dl                   = 5000;
    Args.Ds                   = 1e4;
    Args.DistUnits            = 'pc'; % 'z' | 'pc' | 'cm'
    Args.Beta                 = logspace(-3,3,7)';
    Args.BetaUnits            = 'ThetaE'; % 'rad' | 'arcsec' | 'mas' | 'ThetaE'  
    Args.BetaMin              = 0;
    Args.OutUnits             = 'arcsec'; % 'rad' | 'arcsec' | ...
end

%Args = InArg.populate_keyval(DefV,varargin,mfilename);

% unit conversion
G  = constant.G;
c  = constant.c;
pc = constant.pc; 
switch lower(Args.DistUnits)
    case 'z'
        Dl  = astro.cosmo.ad_dist(Args.Dl);
        Ds  = astro.cosmo.ad_dist(Args.Ds);
        Dls = astro.cosmo.ad_dist([Args.Dl,Args.Ds]);
        
        % output is pc
    otherwise
        Dl  = Args.Dl;
        Ds  = Args.Ds;
        Dls = Args.Ds - Args.Dl;
        switch lower(Args.DistUnits)
            case 'pc'
                % already in pc
            case 'cm'
                % convert cm to pc
                Dl  = Dl./pc;
                Ds  = Ds./pc;
                Dls = Dls./pc;
            case 'kpc'
                % convert kpc to pc
                Dl  = Dl.*1000;
                Ds  = Ds.*1000;
                Dls = Dls.*1000;
            case 'mpc'
                % convert Mpc to pc
                Dl  = Dl.*1e6;
                Ds  = Ds.*1e6;
                Dls = Dls.*1e6;    
            otherwise
                error('Unknwon InUnits option');
        end
end

M  = convert.mass(Args.MassUnits,'gr',Args.Mass);

FunThetaE = @(M,Dl,Ds,Dls) sqrt( (4.*G.*M./(c.^2)) .* Dls./(Dl.*Ds.*pc) );
TE = FunThetaE(M,Dl,Ds,Dls);  % Einstein radius in radians

switch lower(Args.BetaUnits)
    case 'thetae'
        % convert beta from ThetaE to radians
        Beta    = Args.Beta.*TE;
        BetaMin = Args.BetaMin.*TE;
    otherwise
        % convert beta from angular units to radians
        Beta    = convert.angular(Args.BetaUnits,'rad',Args.Beta);
        BetaMin = convert.angular(Args.BetaUnits,'rad',Args.BetaMin);
end

% add BetaMin
Beta = sqrt(Beta.^2 + BetaMin.^2);  % radians

FunThetaP = @(beta,ThetaE) 0.5.*(beta + sqrt(beta.^2 +4.*ThetaE.^2));
FunThetaM = @(beta,ThetaE) 0.5.*(beta - sqrt(beta.^2 +4.*ThetaE.^2)); 
% note magnification may be negative (abs value should be taken)
FunMuP = @(beta,ThetaE) 1./(1 - (ThetaE./FunThetaP(beta,ThetaE)).^4 );
FunMuM = @(beta,ThetaE) 1./(1 - (ThetaE./FunThetaM(beta,ThetaE)).^4 );

ThetaP = FunThetaP(Beta,TE);
ThetaM = FunThetaM(Beta,TE);
MuP    = FunMuP(Beta,TE);
MuM    = FunMuM(Beta,TE);

ShiftReLens   = (abs(MuP).*ThetaP + abs(MuM).*ThetaM)./(abs(MuP) + abs(MuM));
ShiftReSource = ShiftReLens - Beta;

% convert angular values to output units
ConvAng = convert.angular('rad',Args.OutUnits);

Res.ER      = TE.*ConvAng;
Res.ThetaP  = ThetaP.*ConvAng;
Res.ThetaM  = ThetaM.*ConvAng;
Res.MuP     = MuP;
Res.MuM     = MuM;
Res.MuTot   = abs(MuP) + abs(MuM);
Res.ShiftReLens   = ShiftReLens.*ConvAng;
Res.ShiftReSource = ShiftReSource.*ConvAng;
Res.Beta          = Beta.*ConvAng;

% potential
Phi1    = TE.^2.*log(abs(ThetaP));
Phi2    = TE.^2.*log(abs(ThetaM));

% time delay
D   = Dls./(Dl.*Ds.*pc);
switch lower(Args.DistUnits)
    case 'z'
        % note: Args.Dl contains redshift
        Res.Delay1 = (1 + Args.Dl).*(0.5.*(ThetaP - Beta).^2 - Phi1)./(c.*D);
        Res.Delay2 = (1 + Args.Dl).*(0.5.*(ThetaM - Beta).^2 - Phi2)./(c.*D);
    otherwise
        % assume not cosmological time dilation
        %Res.Delay1 = (0.5.*(ThetaP - Beta).^2 - Phi1)./(c.*D);
        %Res.Delay2 = (0.5.*(ThetaM - Beta).^2 - Phi2)./(c.*D);
        A = -2.*G.*M./(c.^3);
        Res.Delay1 = A .* log(1 - cos(Beta));
        Res.Delay2 = A .* log(1 - cos(Beta));
        
%         Res.dDelay1_db1 = A.* (-sin(Beta))./(cos(Beta)-1);
%         Res.dDelay1_db2 = A./(cos(Beta)-1);
%         Res.dDelay1_db3 = A.*sin(Beta)./((cos(Beta)-1).^2);
%         [dbdt1,dbdt2,dbdt3] = Util.fun.numerical_diff(Res.Beta);
%         Res.dDelay1_dt1 = Res.dDelay1_db1.*dbdt1;
%         Res.dDelay1_dt2 = Res.dDelay1_db2.*dbdt2;
%         Res.dDelay1_dt3 = Res.dDelay1_db3.*dbdt3;
end

if (nargout>1)
    Fun.FunThetaP = FunThetaP;
    Fun.FunThetaM = FunThetaM;
    Fun.FunMuP    = FunMuP;
    Fun.FunMuM    = FunMuM;
end
