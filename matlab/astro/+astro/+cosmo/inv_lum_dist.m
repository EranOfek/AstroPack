function [Z]=inv_lum_dist(DM,Type,CosmoPars)
% Distance modulus to redshift
% Package: astro.cosmo
% Description: Given the distance modulus, use the luminosity distance
%              to calculate the corresponding redshift.
% Input  : - Vector of distance modulus or luminosity distance.
%          - Type of first input argument:
%            'DM'  - distance modulus
%            'LD'  - luminosity distance (default).
%          - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],
%            or cosmological parmeters structure, or a string containing
%            parameters source name, default is 'wmap3' (see cosmo_pars.m).
% Output : - Redshift.
% Reference : Perlmutter et al. 1997 ApJ, 483, 565
%             Oke & Sandage 1968 ApJ, 154, 21
%             Peterson, B.M., 1997, AGN, p.165
% Tested : Matlab 5.1
%     By : Eran O. Ofek                  March 2008
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Z=astro.cosmo.inv_lum_dist(35,'dm');    % distance modulus to redshift
%          Z=astro.cosmo.inv_lum_dist(150e6,'ld'); % lum. dist. to redshift
% Reliable: 2
%--------------------------------------------------------------------------
Def.Type = 'LD';
Def.CosmoPars = 'wmap3';

if (nargin==1)
   Type      = Def.Type;
   CosmoPars = Def.CosmoPars;
elseif (nargin==2)
   CosmoPars = Def.CosmoPars;
elseif (nargin==5)
   % do nothing
else
   error('Illegal number of input arguments');
end

if (ischar(CosmoPars)==0 && isstruct(CosmoPars)==0)
   % do nothing
else
   Par = astro.cosmo.cosmo_pars(CosmoPars);
   CosmoPars = [Par.H0, Par.OmegaM, Par.OmegaL, Par.OmegaRad];
end
H0       = CosmoPars(1);
OmegaM   = CosmoPars(2);
OmegaL   = CosmoPars(3);
if (length(CosmoPars)==3)
   OmegaRad = 0;
else
   OmegaRad = CosmoPars(4);
end

switch lower(Type)
 case 'dm'
    LumDist = 10.^((DM + 5)./5);
 case 'ld'
    LumDist = DM;
 otherwise
    error('Unknown Type option');
end
Ndm     = length(DM);

Z       = zeros(Ndm,1);
for Idm=1:1:Ndm
   Z(Idm) = tools.find.fun_binsearch(@astro.cosmo.lum_dist,LumDist(Idm),[1e-15 100],1e-3,CosmoPars);
end


