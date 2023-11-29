function Result = unitTest
    % unitTest for celestial.Targets
    % Example: celestial.Targets.unitTest
    
    [Alt, Az] = celestial.Targets.sunAlt(2451545, [1 1]);
    [Time, IsRise] = celestial.Targets.nextSunHorizon;
    [RA,Dec]=celestial.Targets.earthShadow(2451545 +(0:0.1:365)');

    
    T=celestial.Targets;

    T=celestial.Targets;
    T.generateTargetList('last');
    T.write('try1.mat');
    T=celestial.Targets('try1.mat');
    !rm try1.mat
            
    
    T = celestial.Targets;
    T.generateTargetList('last');

    T = celestial.Targets.generateTargetList('last');
    T.write('try1aaa.mat');
    !rm try1aaa.mat

    [Lon, Lat] = T.ecliptic;

    [Lon, Lat] = T.galactic;

    [Sun] = T.sunCoo;

    [Moon] = T.moonCoo;

    [MD, Moon] = T.moonDist;

    [Az, Alt] = T.azalt;

    [HA, LST] = T.ha;

    [VisibilityTime] = leftVisibilityTime(T);

    [FlagAll, Flag] = isVisible(T);

    [T, P] = calcPriority(T, 2451545.5, 'cycle');

            
    Result = true;
end
