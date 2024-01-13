function Lum=limb_darkening(R, ModelType, Pars)
% Limb darkening function
% Package: AstroUtil.binary
% Description: Calculate the star luminosity per unit area as a function
%              of its radius, and given the limb-darkening parameters.
% Input  : - Radius [stellar radius unit].
%          - Limb darkning model. Options are:
%            'Constant' - no limb darkning.
%            'Milne' - Milne (1921)  [1 parameter]
%                      Lum  = 1-Pars(1).*(1-Mu);
%            'Wade'  - Wade & Rucinski (1985)  [3 parameters]
%                      Lum  = 1-Pars(1).*(1-Mu)-Pars(2).*(1-Mu).^Pars(3);
%            'Kling' - Klingesmith & Sobieski (1970)  [2 parameters]
%                      Lum  = 1-Pars(1).*(1-Mu)-Pars(2).*Mu.*log(Mu);
%            'Diaz'  - Diaz-Cordoves & Gimenez (1992) [2 parameters]
%                      Lum  = 1-Pars(1).*(1-Mu)-Pars(2).*(1-sqrt(Mu));
%            Default is 'Milne'.
%          - Vector of parameters for the model.
% Output : - Luminosity per unit area at radius, R. [consistent lum. unit].
% Tested : Matlab 5.3
%     By : Eran O. Ofek                    Aug 2001
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Reference: Claret et al. 1995, A&ASS, 114, 247
% Example: Lum=astro.binary.limb_darkening(0.1,'Diaz',[1 1]);
% Reliable: 2
%------------------------------------------------------------------------------
Mu   = sqrt(1-R.^2);  % cos(Gamma)    


% select from different models:
switch lower(ModelType)
    case 'try'
        Lum  = 1 - 0.1.*R;  
    case 'constant'
        Lum = 1;
    case 'milne'
        % Milne (1921)  [1 parameter]
        Lum  = 1 - Pars(1).*(1-Mu);
    case 'wade'
        % Wade & Rucinski (1985)  [3 parameters]
        Lum  = 1 - Pars(1).*(1-Mu) - Pars(2).*(1-Mu).^Pars(3);
    case 'kling'
        % Klingesmith & Sobieski (1970)  [2 parameters]
        Lum  = 1 - Pars(1).*(1-Mu) - Pars(2).*Mu.*log(Mu);
    case 'diaz'
        % Diaz-Cordoves & Gimenez (1992) [2 parameters]
        Lum  = 1 - Pars(1).*(1-Mu) - Pars(2).*(1 - sqrt(Mu));
    otherwise
        error('Unknown limb-darkening model type');
end
