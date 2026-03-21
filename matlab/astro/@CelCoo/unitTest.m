function Result = unitTest()
    % CelCoo.unitTest
    % Round-trip tests for coordinate conversion and propagation.

    RAD = 180./pi;

    %%
    % -----------------------------
    % Test 1: Units conversion round-trip (deg -> rad -> deg)
    % -----------------------------
    RAdeg0  = [12.3; 145.7; 301.2];
    Decdeg0 = [-20.2; 10.5; 55.1];

    C = CelCoo;
    C.populate(RAdeg0, Decdeg0, 'deg');
    Deg0 = C.Deg;

    C.Units = 'rad';
    C.Units = 'deg';
    Deg1 = C.Deg;

    if max(abs(Deg1(:) - Deg0(:))) > 1e-12
        error('CelCoo unit conversion round-trip failed');
    end

    %%
    % -----------------------------
    % Test 2: Cosine-direction round-trip
    % -----------------------------
    Rad0 = C.Rad;
    [X,Y,Z] = celestial.coo.coo2cosined(Rad0(:,1), Rad0(:,2));
    [RA1,Dec1] = celestial.coo.cosined2coo(X,Y,Z,false);

    if max(abs(RA1(:) - Rad0(:,1))) > 1e-12 || max(abs(Dec1(:) - Rad0(:,2))) > 1e-12
        error('CelCoo cosine-direction round-trip failed');
    end

    %%
    % -----------------------------
    % Test 3: Precession forward/backward round-trip
    % -----------------------------
    Cpre = CelCoo;
    Cpre.populate([15; 123; 278], [-22; 0.5; 47], 'deg');
    Cpre.Units   = 'deg';
    Cpre.Equinox = 2000;
    Cpre.IsTrue  = false;

    CpreF = Cpre.precess(2015.25, 'OutIsTrue', false, 'CreateNewObj', true);
    CpreB = CpreF.precess(2000.0, 'OutIsTrue', false, 'CreateNewObj', true);

    D0 = Cpre.Deg;
    D1 = CpreB.Deg;
    Dra = mod(D1(:,1) - D0(:,1) + 180, 360) - 180;
    Dde = D1(:,2) - D0(:,2);
    if max(abs(Dra)) > 5e-7 || max(abs(Dde)) > 5e-7
        error('CelCoo precession forward/backward round-trip failed');
    end

    %%
    % -----------------------------
    % Test 4: Proper-motion forward/backward round-trip (no parallax)
    % -----------------------------
    Cpm = CelCoo;
    Cpm.populate([22; 150; 301], [-30; 2; 63], 'deg');
    Cpm.Units = 'deg';
    Cpm.Epoch = 2000.0;
    Cpm.PM_RA  = [120; -35; 80];   % mas/yr
    Cpm.PM_Dec = [-50; 40; -15];   % mas/yr
    Cpm.Plx    = [5; 2; 1];        % mas
    Cpm.RadVel = [20; -10; 5];     % km/s

    CpmF = Cpm.properMotion(2020.0, 'ApplyPlx', false, 'CreateNewObj', true);
    CpmB = CpmF.properMotion(2000.0, 'ApplyPlx', false, 'CreateNewObj', true);

    P0 = Cpm.Deg;
    P1 = CpmB.Deg;
    Dra = mod(P1(:,1) - P0(:,1) + 180, 360) - 180;
    Dde = P1(:,2) - P0(:,2);
    if max(abs(Dra)) > 5e-8 || max(abs(Dde)) > 5e-8
        error('CelCoo properMotion forward/backward round-trip failed');
    end

    %%

    Result = true;
end

