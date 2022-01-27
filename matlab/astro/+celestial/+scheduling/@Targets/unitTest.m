function Result = unitTest
    % unitTest for celestial.scheduling.Targets
    % Example: celestial.scheduling.Targets.unitTest
    
    [Alt, Az] = celestial.scheduling.Targets.sunAlt(2451545, [1 1]);
    [Time, IsRise] = celestial.scheduling.Targets.nextSunHorizon;
    [RA,Dec]=celestial.scheduling.Targets.earthShadow(2451545 +(0:0.1:365)');

    
    T=celestial.scheduling.Targets;

    T=celestial.scheduling.Targets;
    T.generateTargetList('last');
    T.write('try1.mat');
    T=celestial.scheduling.Targets('try1.mat');
    !rm try1.mat
            
    
    T = celestial.scheduling.Targets;
    T.generateTargetList('last');

    T=celestial.scheduling.Targets;
    T.generateTargetList('last');
    T.write('try1.mat');

    T=celestial.scheduling.Targets;
    T.generateTargetList('last');
    [Lon, Lat] = T.ecliptic;

    T=celestial.scheduling.Targets;
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

    T=celestial.scheduling.Targets;
    T.generateTargetList('last');
    [VisibilityTime] = leftVisibilityTime(T);

    T=celestial.scheduling.Targets;
    T.generateTargetList('last');
    [FlagAll, Flag] = isVisible(T);

    T=celestial.scheduling.Targets;
    T.generateTargetList('last');
    [T, P] = calcPriority(T, 2451545.5, 'west2east');

            
    Result = true;
end
