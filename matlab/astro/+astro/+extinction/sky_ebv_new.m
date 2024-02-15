function Ebv = sky_ebv_new(RA, Dec, Args) 
    % Get full Galactic extinction for a list of coordinates [Gontcharov et al. arXiv:2402.06474]
    % 
    % Input  : - J2000.0 RA or longitude [H M S] or [RAD] or sexagesimal.
    %          - J2000.0 Dec or latitude [Sign D M S] or [RAD] or sexagesimal.
    %          * ...,key,val,...
    %          'CooType' - Coordinates type:
    %                      'eq' : J2000.0 equatorial (default).
    %                      'g'  : Galactic.
    %                      'ec'  : ecliptic.
    %          'CorrectionHigh' - true (default) |false:  correct E(B-V) when > 0.1,
    %            using the Adams et al. (2013) correction 
    %          'Rv' - R_V, default is Galactic 3.08
    %          'InterpMethod' - interolation method, default = 'nearest neighbor'
    % Output : - E(B-V) [mag]
    % Reference: Gontcharov et al. arXiv:2402.06474
    % Author: A.M. Krassilchtchikov (Feb 2024)
    % Example: [Ebv]=astro.extinction.sky_ebv_new(1,1);
    arguments
        RA  = pi; % some placeholder values
        Dec = pi/2;
        Args.CooType = 'eq';
        Args.CorrectHigh = true;
        Args.Rv = 3.08;
        Args.InterpMethod = 'nearest';
    end
    
    RAD = 180/pi;
    
%     DataFile = '~/matlab/data/+cats/+maps/GMK2023_2D_AV.txt';
%     Data = readmatrix(DataFile);  

    FN = 'GMK2023_2D_AV';    
    Data = cats.maps.(FN);
    L0 = Data(:,1);
    B0 = Data(:,2);
    AV0 = Data(:,5);
    AV = scatteredInterpolant(L0,B0,AV0,Args.InterpMethod); % a bit slow, should replace by a faster method 

%     io.files.load1('Gontcharov24.mat');   % keeping AV in a .mat object is just a bit faster: 9.6 sec vs. 11.2 sec

    RA  = celestial.coo.convertdms(RA,'gH','r');
    Dec = celestial.coo.convertdms(Dec,'gD','r');

    switch Args.CooType
     case 'eq'
        % convert Equatorial to Galactic        
        [Lon, Lat] = celestial.coo.convert_coo(RA,Dec,'J2000.0','g');
     case 'g'
        % do nothing - already in Galactic
        Lon = RA; Lat = Dec;
     case 'ec'
        % convert Ecliptic to Galactic
        [Lon, Lat] = elestial.coo.convert_coo(RA,Dec,'e','g'); 
     otherwise
        error('Unknown CooType Option');
    end

    L = Lon .* RAD;    % Galactic longitude in [deg]
    B = Lat .* RAD;    % Galactic latitude  in [deg]
    
    Ebv = AV(L,B) ./ Args.Rv;
        
    % The E(B-V) is probably overestimated when > 0.1 mag
    % Stanek 1998; Arce & Goodman 1999; use correction from Adams et al. (2013):
    if Args.CorrectHigh 
        Ind = Ebv > 0.1;
        Ebv(Ind) = 0.1 + 0.65.*(Ebv(Ind)-0.1);
    end
    
    % this map is for |B| > 13 only, so if |B| < 13 deg, replace the values by those of SFD98:
    Ind = abs(B) < 13; 
    if sum(Ind) > 0
        Ebv(Ind) = astro.extinction.sky_ebv(Lon(Ind),Lat(Ind),'g');    
    end
    
end





