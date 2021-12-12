function TD=witt_timedelay_formula(zl,zs,ri,rj)
% Calculate the time delay for isothermal elliptical potential (Witt formula)
% Package: +TimeDelay
% Input  : - Lens redshift
%          - Source redshift;
%          - image 1 position relative to lens [arcsec]
%          - image 2 position relative to lens [arcsec]
% Output : - Time delay [days]
% Reference: Witt et al. 2000 ApJ 544, 98
% Example: TD=TimeDelay.witt_timedelay_formula(0.5,2,0.5,0.1)

RAD = 180./pi;

Dls = AstroUtil.cosmo.ad_dist([zl zs]);
Dl  = AstroUtil.cosmo.ad_dist([zl]);
Ds  = AstroUtil.cosmo.ad_dist([zs]);

D   = Dl.*Ds./Dls;

TD = constant.pc.*D./(2*constant.c).*(1+zl).* ( (ri./(RAD.*3600)).^2 - (rj./(RAD.*3600)).^2)./86400;