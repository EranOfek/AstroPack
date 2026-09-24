function Result=unitTest()
    % UnitTest for telescope.ObsScheduler

    %% define object
    S=telescope.ObsScheduler;

    %% generate table
    S.generateRegularGrid;

    %% Calculate some properties
    S.Alt;
    S.HA;
    S.GalLat;
    S.LST;
    [SunAlt, SunAz] = S.getSun;
    [MoonAlt, MoonAz] = S.getMoon;
    S.sunRiseSet;
    S.timeToSunRise;
    S.timeToSunSet;
    S.sphere_dist(1,1);
    S.isDark;
    S.isTwighlight;

    %%





    Result = true;

end
