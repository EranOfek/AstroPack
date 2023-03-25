function Result = conjunctionsSearchStarMP(Args)
    %
    % Example: Result = celestial.SolarSys.conjunctionsSearchStarMP

    arguments
        Args.DistRange = [9 Inf];
        Args.HRange    = [-Inf 7];
        Args.StartDate = [1 9 2023];
        Args.EndDate   = [9 1 2024];
        Args.AddPlanets  = {'799','899'};  % Uranus, Neptune
        Args.PlanetsRadius = [25362, 24622];

        Args.ObsCoo    = [35 32];
    end

    E = celestial.OrbitalEl.loadSolarSystem;

    Ie = 1;
    Flag = E(Ie).A(:)>Args.DistRange(1) & E(Ie).A(:)<Args.DistRange(2) & ...
           E(Ie).MagPar(:,1)>Args.HRange(1) & E(Ie).MagPar(:,1)<Args.HRange(2);

    Ind  = find(Flag);

    Nast = sum(Flag);
    Result = [];
    for Iast=1:1:Nast
        [Iast, Nast]

        II = Ind(Iast);

        ObjName = sprintf('%d',E(Ie).Number(II));

        try
            [EphemCat] = celestial.SolarSys.jpl_horizons('ObjectInd',ObjName, 'StartJD',Args.StartDate,'StopJD',Args.EndDate, 'StepSize',3,'StepSizeUnits','h');
    
            Hmag = E(Ie).MagPar(1);
            [OcculterRadius] = celestial.SolarSys.asteroid_radius(Hmag, 0.15);
        
            Result = celestial.SolarSys.conjunctionsStars(EphemCat, 'Result',Result, 'ObjName',ObjName,'ObsCoo',Args.ObsCoo, 'OcculterRadius',OcculterRadius);
        catch
            fprintf('Failed %d',Iast);
        end
    end

    
    Npl = numel(Args.AddPlanets);
    for Ipl=1:1:Npl
        [EphemCat] = celestial.SolarSys.jpl_horizons('ObjectInd',Args.AddPlanets{Ipl}, 'StartJD',Args.StartDate,'StopJD',Args.EndDate, 'StepSize',3,'StepSizeUnits','h');
    
        Result = celestial.SolarSys.conjunctionsStars(EphemCat, 'Result',Result, 'ObjName',ObjName,'ObsCoo',Args.ObsCoo, 'OcculterRadius',Args.PlanetsRadius(Ipl));
    end
    
end
