function Result = conjunctionsSearchStarMP(Args)
    %
    % Example: Result = celestial.SolarSys.conjunctionsSearchStarMP

    arguments
        Args.DistRange = [9 Inf];
        Args.HRange    = [-Inf 7];
        Args.StartDate = [1 3 2023];
        Args.EndDate   = [5 9 2023];

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

        [EphemCat] = celestial.SolarSys.jpl_horizons('ObjectInd',ObjName, 'StartJD',Args.StartDate,'StopJD',Args.EndDate, 'StepSize',3,'StepSizeUnits','h');

        Result = celestial.SolarSys.conjunctionsStars(EphemCat, 'Result',Result, 'ObjName',ObjName,'ObsCoo',Args.ObsCoo);
    end

end
