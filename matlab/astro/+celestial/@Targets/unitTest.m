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

    T=celestial.Targets;
    T.generateTargetList('last');
    T.write('try1.mat');

    T=celestial.Targets;
    T.generateTargetList('last');
    [Lon, Lat] = T.ecliptic;

    T=celestial.Targets;
    T.generateTargetList('last');
    [Lon, Lat] = T.galactic;

    T.generateTargetList('last');
    [Sun] = T.sunCoo;

    T.generateTargetList('last');
    [Moon] = T.moonCoo;

    T.generateTargetList('last');
    [MD, Moon] = T.moonDist;

    T.generateTargetList('last');
    [Az, Alt] = T.azalt;

    T.generateTargetList('last');
    [HA, LST] = T.ha;

    T=celestial.Targets;
    T.generateTargetList('last');
    [VisibilityTime] = leftVisibilityTime(T);

    T=celestial.Targets;
    T.generateTargetList('last');
    [FlagAll, Flag] = isVisible(T);

    T=celestial.Targets;
    T.generateTargetList('last');
    [T, P] = calcPriority(T, 2451545.5, 'west2east');

            
    Result = true;
end
