function [Result] = binaryAGN_selfLensing(Args)
    % Binary AGN accretion-disk self-lensing light curve (with inclinations)
    %   Compute the self-lensing light curve of a binary supermassive black
    %   hole in which ONE black hole (mass Mqso) hosts an accretion disk (the
    %   extended source) and the OTHER black hole (mass M) is a dark, point-like
    %   lens (its finite size is its Schwarzschild radius, and the resulting
    %   occultation of the lensed images is handled inside
    %   astro.binary.self_microlensing). The two masses can differ; the orbit
    %   uses the total mass (Mqso+M).
    %
    %   Relative to astro.binary.binaryAGN_selfLensingLC this function adds two
    %   inclination options:
    %     1. DiskIncl  - inclination of the accretion disk (0=face-on; default).
    %                    Projects the circular disk source into an ellipse with
    %                    axis ratio cos(DiskIncl). Passed to self_microlensing
    %                    via 'FunLimbInclination'.
    %     2. OrbitIncl - inclination of the binary orbit (90=edge-on; default).
    %                    Sets the transit impact parameter b=a*cos(OrbitIncl)
    %                    (in source-radius units, added to Args.ImpactPar) and
    %                    the line-of-sight lens-source separation Dls=a*sin(OrbitIncl).
    %   At DiskIncl=0 and OrbitIncl=90 this reduces exactly to binaryAGN_selfLensingLC.
    %
    %   Geometry of OrbitIncl vs BetaVec: near conjunction the projected
    %   lens-source separation splits into two orthogonal components -
    %   BetaVec is the along-track coordinate (it sweeps with time and is NOT
    %   affected by OrbitIncl), while OrbitIncl fixes the perpendicular
    %   closest-approach impact parameter b=a*cos(OrbitIncl). They combine as
    %   Beta=sqrt(b^2+BetaVec^2), so BetaVec=0 is conjunction and the SMALLEST
    %   separation reached during the transit equals b (=0 for an edge-on
    %   orbit). Since a>>SrcRad (with the defaults a/SrcRad~300), a detectable
    %   event requires the orbit within a small fraction of a degree of edge-on:
    %   b<~SrcRad needs cos(OrbitIncl)<~SrcRad/a, otherwise the lens misses the
    %   disk and there is no lensing.
    %
    % Input  : * ...,key,val,...
    %            'M'         - Lens (companion) black hole mass. Sets the Einstein
    %                          radius and the lens size (LensRad=Rs of M). Default 1e8.
    %            'Mqso'      - Accretor (QSO/source) black hole mass that hosts the
    %                          accretion disk. Sets the disk spectrum and the 'rs'
    %                          radius scale for Rin/Rout. Default is 1e8.
    %            'Mdot'      - Accretion rate. Default is 0.1.
    %            'MassUnits' - Default is 'SunM'.
    %            'TimeUnits' - Default is 'yr'.
    %            'Rin'       - Disk inner radius (or vector of R). Default is 10.
    %            'RinUnits'  - Units of Rin: 'rs' (Schwarzschild radii) or any
    %                          unit understood by convert.length. Default is 'rs'.
    %            'Rout'      - Disk outer radius. Default is 1e15.
    %            'RoutUnits' - Units of Rout: 'rs' (Schwarzschild radii) or any
    %                          unit understood by convert.length. Default is 'cm'.
    %            'Nstep'     - Number of disk radius steps. Default is 100.
    %            'Wave'      - Wavelength vector [Ang]. Default is (4000:100:5000).'.
    %            'Sep'       - Physical binary separation [cm]. Default is 0.1*pc.
    %            'Dl'        - Observer-lens distance [pc]. Default is 1e9.
    %            'DiskIncl'  - Accretion-disk inclination [deg]. 0=face-on.
    %                          Default is 0.
    %            'OrbitIncl' - Orbit inclination [deg]. 90=edge-on. Default is 90.
    %            'ImpactPar' - Additional fixed transit impact parameter, in
    %                          source-radius units, added to the orbit-induced
    %                          impact parameter. Default is 0.
    %            'BetaVec'   - Vector of along-transit coordinate [source-radius
    %                          units]. Default is (0:0.05:2).'.
    %            'Time'      - If given [same units as TimeUnits], overrides
    %                          BetaVec via the orbital velocity. Default is [].
    %            'Nsim'      - Number of Monte-Carlo source points per impact
    %                          parameter (2d algorithm). Default is 1e6.
    % Output : - A structure with the following fields:
    %            .Time          - Time vector [TimeUnits].
    %            .Amplification - Total magnification at each time.
    %            .Beta          - Total source-lens separation [source-radius units].
    %            .ImpactPar     - Total transit impact parameter [source-radius units].
    %            .DiskIncl      - Accretion-disk inclination [deg].
    %            .OrbitIncl     - Orbit inclination [deg].
    %            .ResSL         - Full self_microlensing diagnostics structure.
    %            .VecR          - Disk radii [cm].
    %            .RsQso         - QSO (disk) BH Schwarzschild radius [cm].
    %            .RsLens        - Lens BH Schwarzschild radius [cm].
    %            .Rs            - = RsLens (kept for backward compatibility).
    %            .Vel           - Relative orbital velocity [cm/s].
    %            .Dls           - Line-of-sight lens-source separation [pc].
    % Author : Eran Ofek + Claude (2024 Jan)
    % Example: R=astro.binary.binaryAGN_selfLensing
    %          % inclined disk, slightly non edge-on orbit:
    %          R=astro.binary.binaryAGN_selfLensing('DiskIncl',60, 'OrbitIncl',89.99);

    arguments
        Args.M         = 1e8;   % lens (companion) black hole mass
        Args.Mqso      = 1e8;   % accretor (QSO/source) black hole mass hosting the disk
        Args.Mdot      = 0.1;
        Args.MassUnits = 'SunM'
        Args.TimeUnits = 'yr';
        Args.Rin       = 10;  % Or vector of R
        Args.RinUnits  = 'rs';   % Rin units: 'rs' (Schwarzschild radii) or any convert.length unit
        Args.Rout      = 500;
        Args.RoutUnits = 'rs';   % Rout units: 'rs' (Schwarzschild radii) or any convert.length unit
        Args.Nstep     = 100;
        Args.Wave      = (4000:100:5000).';  % Ang

        Args.Sep       = 0.1.*constant.pc;  % cm
        Args.Dl        = 1e9; % pc

        Args.DiskIncl   = 0;    % [deg] accretion disk inclination (0=face on)
        Args.OrbitIncl  = 90;   % [deg] orbit inclination (90=edge on)

        Args.ImpactPar  = 0;
        Args.BetaVec    = (0:0.05:2)';  % [src rad units]
        Args.Time       = [];
        Args.Nsim       = 1e6;
    end



    % Schwarzschild radii [cm]: the disk is around the QSO BH (Mqso), while the
    % lens is the companion BH (M).
    RsQso  = 2.*constant.G.*Args.Mqso.*convert.mass(Args.MassUnits,'gr')./(constant.c.^2);
    RsLens = 2.*constant.G.*Args.M   .*convert.mass(Args.MassUnits,'gr')./(constant.c.^2);

    % Convert Rin/Rout to cm. RinUnits/RoutUnits may be 'rs' (Schwarzschild radii,
    % of the QSO BH) or any unit understood by convert.length. Rin/Rout are then
    % passed to accretionDiskSpec in 'cm', so they can carry independent units.
    Rin_cm  = local_r2cm(Args.Rin,  Args.RinUnits,  RsQso);
    Rout_cm = local_r2cm(Args.Rout, Args.RoutUnits, RsQso);

    % Guard: Rin must be inside Rout. Otherwise accretionDiskSpec builds a radius
    % grid with R<Rin, the disk temperature (1-sqrt(Rin/R))^0.25 becomes complex,
    % and the amplification comes out complex. This is easy to trigger with mixed
    % units (e.g. Rin in 'rs' of a large Mqso exceeding a small Rout in 'cm').
    if isscalar(Rin_cm) && Rin_cm >= Rout_cm
        error('binaryAGN_selfLensing:RinGeRout',...
              ['Inner disk radius (%.3g cm) >= outer radius (%.3g cm).\n',...
               'Check Rin/RinUnits (=%.3g %s; ''rs'' uses RsQso=%.3g cm of Mqso=%g) ',...
               'vs Rout/RoutUnits (=%.3g %s).'],...
               Rin_cm, Rout_cm, Args.Rin, Args.RinUnits, RsQso, Args.Mqso, Args.Rout, Args.RoutUnits);
    end

    [IntegratedSpec, Wave, VecR, T, Ibb] = astro.spec.accretionDiskSpec('M',Args.Mqso,...
                                                                            'Mdot',Args.Mdot,...
                                                                            'MassUnits',Args.MassUnits,...
                                                                            'TimeUnits',Args.TimeUnits,...
                                                                            'Rin',Rin_cm,...
                                                                            'Rout',Rout_cm,...
                                                                            'RUnits','cm',...
                                                                            'Nstep',Args.Nstep,...
                                                                            'Wave',Args.Wave);


    % relative orbit of the two MBH -> use the total (Mqso+M) mass:
    K=celestial.Kepler.kepler3law((Args.Mqso + Args.M).*convert.mass(Args.MassUnits,'gr'), 'a', Args.Sep);

    % line-of-sight lens-source separation at conjunction (a*sin(i_orbit))
    Dls = K.a.*sind(Args.OrbitIncl)./constant.pc;

    if ~isempty(Args.Time)
        Args.BetaVec = Args.Time .* K.v.*86400.*365.25 ./max(VecR);
    end


    % accretion disk profile - face on
    FluxAsFunRadius = sum(Ibb, 1);
    accDiskProfile = @(RR) interp1(VecR./max(VecR), FluxAsFunRadius, RR)./FluxAsFunRadius(2);
    RR = (0:0.05:1)';
    FunLimbMatrix  = [RR, accDiskProfile(RR)];
    IsN = isnan(FunLimbMatrix(:,2));
    FunLimbMatrix(IsN,2) = 0;

    % LensRad is the lens BH Schwarzschild radius RsLens (computed above).

    % orbit inclination sets the transit impact parameter b = a*cos(i_orbit),
    % in source-radius units, added to the user-supplied ImpactPar.
    % NOTE: BetaVec is the along-track (X) coordinate and the impact parameter is
    % the PERPENDICULAR (Y) offset of the lens track. They must be passed as two
    % separate components (not folded into a scalar sqrt(b^2+BetaVec^2)), because
    % for an inclined (elliptical/edge-on) disk the direction matters: with the
    % disk foreshortened along Y, a scalar offset would keep the lens on the disk
    % major axis and, for DiskIncl->90, make it graze the 1-D source (singular).
    ImpactPar = Args.ImpactPar + K.a.*cosd(Args.OrbitIncl)./max(VecR);
    Beta = sqrt(ImpactPar.^2 + Args.BetaVec.^2);   % scalar lens-source-centre distance (for reference)
    [TM,ResSL]=astro.binary.self_microlensing(Args.BetaVec, 'PerpImpactPar',ImpactPar,...
                                                            'Dls',Dls, 'Dl',Args.Dl, 'Mass',Args.M, 'MassUnits',Args.MassUnits, 'SrcRad',max(VecR), 'SrcRadUnits','cm',...
                                                            'FunLimb', FunLimbMatrix, 'FunLimbInclination',Args.DiskIncl,...
                                                            'LensRad',RsLens, 'Algo','2d','Nsim',Args.Nsim);

    % Impact par to time
    Time = Args.BetaVec.*max(VecR)./(K.v.*86400.*365.25);  % [yr]



    %plot(Time, TM)
    %hold on;

    Result.Time          = Time(:);
    Result.Amplification = TM(:);
    Result.Beta          = Beta;
    Result.ImpactPar     = ImpactPar;
    Result.DiskIncl      = Args.DiskIncl;
    Result.OrbitIncl     = Args.OrbitIncl;
    Result.ResSL         = ResSL;
    Result.VecR          = VecR;
    Result.RsQso         = RsQso;    % QSO (disk) BH Schwarzschild radius [cm]
    Result.RsLens        = RsLens;   % lens BH Schwarzschild radius [cm]
    Result.Rs            = RsLens;   % (kept for backward compat = lens Rs)
    Result.Vel           = K.v;
    Result.Dls           = Dls;
end

function Rcm = local_r2cm(R, Units, Rs)
    % Convert a radius R given in Units to cm.
    % Units may be 'rs' (Schwarzschild radii; Rs given in cm) or any unit
    % understood by convert.length.
    if strcmpi(Units, 'rs')
        Rcm = R.*Rs;
    else
        Rcm = R.*convert.length(Units, 'cm');
    end
end
