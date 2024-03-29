function [L,B,R]=ple_uranus(Date)
% Low accuracy ephemeris for Uranus
% Package: celestial.SolarSys
% Description: Low accuracy ephemeris for Uranus. Calculate Uranus
%              heliocentric longitude, latitude and radius vector
%              referred to mean ecliptic and equinox of date.
%              Accuarcy: ~1' in long/lat, ~0.001 au in dist.
% Input  : - matrix of dates, [D M Y frac_day] per line,
%            or JD per line. In TT time scale.
% Output : - Longitude in radians.
%          - Latitude in radians.
%          - Radius vector in au.
% Reference: VSOP87
% See also: ple_planet.m
% Tested : Matlab 5.3
%     By : Eran O. Ofek                    Oct 2001
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [L,B,R]=celestial.SolarSys.ple_uranus([1 1 2000 0])
% Reliable: 2
%------------------------------------------------------------------------------


RAD = 180./pi;

FunTPI = @(X) (X./(2.*pi) - floor(X./(2.*pi))).*2.*pi;

SizeDate = size(Date);
N        = SizeDate(1);
ColN     = SizeDate(2);

if (ColN==4),
   JD = celestial.time.julday(Date);
elseif (ColN==1),
   JD = Date;
else
   error('Illigal number of columns in date matrix');
end

Tau = (JD(:) - 2451545.0)./365250.0;

SumL0 = 548129294 ...
   + 9260408.*cos(0.8910642 + 74.7815986.*Tau) ...
   + 1504248.*cos(3.6271926 + 1.4844727.*Tau) ...
   + 365982.*cos(1.899622 + 73.297126.*Tau) ...
   + 272328.*cos(3.358237 + 149.563197.*Tau) ...
   + 70328.*cos(5.39254 + 63.73590.*Tau) ...
   + 68893.*cos(6.09292 + 76.26607.*Tau) ...
   + 61999.*cos(2.26952 + 2.96895.*Tau) ...
   + 61951.*cos(2.85099 + 11.04570.*Tau) ...
   + 26469.*cos(3.14152 + 71.81265.*Tau) ...
   + 25711.*cos(6.11380 + 454.90937.*Tau) ...
   + 21079.*cos(4.36059 + 148.07872.*Tau) ...
   + 17819.*cos(1.74437 + 36.64856.*Tau) ...
   + 14613.*cos(4.73732 + 3.93215.*Tau) ...
   + 11163.*cos(5.82682 + 224.34480.*Tau) ...
   + 10998.*cos(0.48865 + 138.51750.*Tau) ...
   + 9527.*cos(2.9552 + 35.1641.*Tau) ...
   + 7546.*cos(5.2363 + 109.9457.*Tau) ...
   + 4220.*cos(3.2333 + 70.8494.*Tau) ...
   + 4052.*cos(2.2775 + 151.0477.*Tau);

SumL1 = 7502543122 ...
   + 154458.*cos(5.242017 + 74.781599.*Tau) ...
   + 24456.*cos(1.71256 + 1.48447.*Tau) ...
   + 9258.*cos(0.4284 + 11.0457.*Tau) ...
   + 8266.*cos(1.5022 + 63.7359.*Tau) ...
   + 7842.*cos(1.3198 + 149.5632.*Tau) ...
   + 3899.*cos(0.4648 + 3.9322.*Tau) ...
   + 2284.*cos(4.1737 + 76.2661.*Tau) ...
   + 1927.*cos(0.5301 + 2.9689.*Tau) ...
   + 1233.*cos(1.5863 + 70.8494.*Tau) ...
   + 791.*cos(5.436 + 3.181.*Tau) ...
   + 767.*cos(1.996 + 73.297.*Tau);

SumL2 = 53033 ...
   + 2358.*cos(2.2601 + 74.7816.*Tau) ...
   + 769.*cos(4.526 + 11.046.*Tau) ...
   + 552.*cos(3.258 + 63.736.*Tau) ...
   + 542.*cos(2.276 + 3.932.*Tau) ...
   + 529.*cos(4.923 + 1.484.*Tau);

SumL3 = 121.*cos(0.024 + 74.782.*Tau) ...
   + 68.*cos(4.12 + 3.93.*Tau) ...
   + 53.*cos(2.39 + 11.05.*Tau) ...
   + 46;

SumL4 = -114 ...
   + 6.*cos(4.58 + 74.78.*Tau);

L = SumL0 + SumL1.*Tau + SumL2.*Tau.^2 ...
          + SumL3.*Tau.^3 + SumL4.*Tau.^4;
L = L.*1e-8;

L = FunTPI(L);

SumB0 = 1346278.*cos(2.6187781 + 74.7815986.*Tau) ...
   + 62341.*cos(5.08111 + 149.56320.*Tau) ...
   - 61601 ...
   + 9964.*cos(1.6160 + 76.2661.*Tau) ...
   + 9926.*cos(0.5763 + 73.2971.*Tau) ...
   + 3259.*cos(1.2612 + 224.3448.*Tau);

SumB1 = 206366.*cos(4.123943 + 74.781599.*Tau) ...
   + 8563.*cos(0.3382 + 149.5632.*Tau) ...
   + 1726.*cos(2.1219 + 73.2971.*Tau) ...
   + 1374 ...
   + 1369.*cos(3.0686 + 76.2661.*Tau) ...
   + 451.*cos(3.777 + 1.484.*Tau);

SumB2 = 9212.*cos(5.8004 + 74.7816.*Tau) ...
   + 557 ...
   + 286.*cos(2.177 + 149.563.*Tau) ...
   + 95.*cos(3.84 + 73.30.*Tau);

SumB3 = 268.*cos(1.251 + 74.782.*Tau) ...
   + 11 ...
   + 6.*cos(4.01 + 149.56.*Tau);

SumB4 = 6.*cos(2.85 + 74.78.*Tau);

B = SumB0 + SumB1.*Tau + SumB2.*Tau.^2 ...
          + SumB3.*Tau.^3 + SumB4.*Tau.^4;
B = B.*1e-8;

%B = FunTPI(B);

SumR0 = 1921264848 ...
   + 88784984.*cos(5.60377527 + 74.78159857.*Tau) ...
   + 3440836.*cos(0.328361 + 73.2971259.*Tau) ...
   + 2055653.*cos(1.7829517 + 149.5631971.*Tau) ...
   + 649322.*cos(4.522473 + 76.266071.*Tau) ...
   + 602248.*cos(3.860038 + 63.735898.*Tau) ...
   + 496404.*cos(1.401399 + 454.909367.*Tau) ...
   + 338526.*cos(1.580027 + 138.517497.*Tau) ...
   + 243508.*cos(1.570866 + 71.812653.*Tau) ...
   + 190522.*cos(1.998094 + 1.484473.*Tau) ...
   + 161858.*cos(2.791379 + 148.078724.*Tau) ...
   + 143706.*cos(1.383686 + 11.045700.*Tau) ...
   + 93192.*cos(0.17437 + 36.64856.*Tau) ...
   + 89806.*cos(3.66105 + 109.94569.*Tau) ...
   + 71424.*cos(4.24509 + 224.34480.*Tau) ...
   + 46677.*cos(1.39977 + 35.16409.*Tau) ...
   + 39026.*cos(3.36235 + 277.03499.*Tau) ...
   + 39010.*cos(1.66971 + 70.84945.*Tau) ...
   + 36755.*cos(3.88649 + 146.59425.*Tau) ...
   + 30349.*cos(0.70100 + 151.04767.*Tau) ...
   + 29156.*cos(3.18056 + 77.75054.*Tau) ...
   + 25786.*cos(3.78538 + 85.82730.*Tau) ...
   + 25620.*cos(5.25656 + 380.12777.*Tau) ...
   + 22637.*cos(0.72519 + 529.69097.*Tau) ...
   + 20473.*cos(2.79640 + 70.32818.*Tau) ...
   + 20472.*cos(1.55589 + 202.25340.*Tau) ...
   + 17901.*cos(0.55455 + 2.96895.*Tau) ...
   + 15503.*cos(5.35405 + 38.13304.*Tau) ...
   + 14702.*cos(4.90434 + 108.46122.*Tau) ...
   + 12897.*cos(2.62154 + 111.43016.*Tau) ...
   + 12328.*cos(5.96039 + 127.47180.*Tau) ...
   + 11959.*cos(1.75044 + 984.60033.*Tau) ...
   + 11853.*cos(0.99343 + 52.69020.*Tau) ...
   + 11696.*cos(3.29826 + 3.93215.*Tau) ...
   + 11495.*cos(0.43774 + 65.22037.*Tau) ...
   + 10793.*cos(1.42105 + 213.29910.*Tau) ...
   + 9111.*cos(4.9964 + 62.2514.*Tau) ...
   + 8421.*cos(5.2535 + 222.8603.*Tau) ...
   + 8402.*cos(5.0388 + 415.5525.*Tau) ...
   + 7449.*cos(0.7949 + 351.8166.*Tau) ...
   + 7329.*cos(3.9728 + 183.2428.*Tau) ...
   + 6046.*cos(5.6796 + 78.7138.*Tau) ...
   + 5524.*cos(3.1150 + 9.5612.*Tau) ...
   + 5445.*cos(5.1058 + 145.1098.*Tau) ...
   + 5238.*cos(2.6296 + 33.6796.*Tau) ...
   + 4079.*cos(3.2206 + 340.7709.*Tau);

SumR1 = 1479896.*cos(3.6720571 + 74.7815986.*Tau) ...
   + 71212.*cos(6.22601 + 63.73590.*Tau) ...
   + 68627.*cos(6.13411 + 149.56320.*Tau) ...
   - 24060 ...
   + 21468.*cos(2.60177 + 76.26607.*Tau) ...
   + 20857.*cos(5.24625 + 11.04570.*Tau) ...
   + 11405.*cos(0.01848 + 70.84945.*Tau) ...
   + 7497.*cos(0.4236 + 73.2971.*Tau) ...
   + 4244.*cos(1.4169 + 85.8273.*Tau) ...
   + 3927.*cos(3.1551 + 71.8127.*Tau) ...
   + 3578.*cos(2.3116 + 224.3448.*Tau) ...
   + 3506.*cos(2.5835 + 138.5175.*Tau) ...
   + 3229.*cos(5.2550 + 3.9322.*Tau) ...
   + 3060.*cos(0.1532 + 1.4845.*Tau) ...
   + 2564.*cos(0.9808 + 148.0787.*Tau) ...
   + 2429.*cos(3.9944 + 52.6902.*Tau) ...
   + 1645.*cos(2.6535 + 127.4718.*Tau) ...
   + 1584.*cos(1.4305 + 78.7138.*Tau) ...
   + 1508.*cos(5.0600 + 151.0477.*Tau);

SumR2 = 22440.*cos(0.69953 + 74.78160.*Tau) ...
   + 4727.*cos(1.6990 + 63.7359.*Tau) ...
   + 1682.*cos(4.6483 + 70.8494.*Tau) ...
   + 1650.*cos(3.0966 + 11.0457.*Tau) ...
   + 1434.*cos(3.5212 + 149.5632.*Tau) ...
   + 770 ...
   + 500.*cos(6.172 + 76.266.*Tau) ...
   + 461.*cos(0.767 + 3.932.*Tau) ...
   + 390.*cos(4.496 + 56.622.*Tau) ...
   + 390.*cos(5.527 + 85.827.*Tau);

SumR3 = 1164.*cos(4.7345 + 74.7816.*Tau) ...
   + 212.*cos(3.343 + 63.736.*Tau) ...
   + 196.*cos(2.980 + 70.849.*Tau) ...
   + 105.*cos(0.958 + 11.046.*Tau);

SumR4 = 53.*cos(3.01 + 74.78.*Tau) ...
   + 10.*cos(1.91 + 56.62.*Tau);

R = SumR0 + SumR1.*Tau + SumR2.*Tau.^2 ...
          + SumR3.*Tau.^3 + SumR4.*Tau.^4;
R = R.*1e-8;

