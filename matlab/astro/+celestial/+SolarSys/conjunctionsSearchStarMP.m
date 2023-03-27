function Result = conjunctionsSearchStarMP(Args)
    % Search for conjunctions/occultations of minor planets and GAIA stars
    %   The code run over known selected asteroids and use 
    %   celestial.SolarSys.jpl_horizons to generate ephemerides and
    %   celestial.SolarSys.conjunctionsStars to search for the
    %   conjunctions.
    % Input  : * ...,key,val,...
    %            see code.
    % Output : - A structure array of found events.
    % Example: 


    arguments
        Args.ElementsIndex = 2;  % 1 for numbered asteroid; 2 for unnumbered
        Args.DistRange = [9 Inf];
        Args.HRange    = [-Inf 7];
        Args.StartDate = [9 3 2023];
        Args.EndDate   = [9 6 2024];
        Args.AddPlanets  = {'799','899'};  % Uranus, Neptune
        Args.PlanetsRadius = [25362, 24622];

        Args.ObsCoo    = [35 32];
        Args.MaxSunAlt = -11.0;
        Args.MinAlt    = 25;
    end

    E = celestial.OrbitalEl.loadSolarSystem;

    Ie = Args.ElementsIndex;
    Flag = E(Ie).A(:)>Args.DistRange(1) & E(Ie).A(:)<Args.DistRange(2) & ...
           E(Ie).MagPar(:,1)>Args.HRange(1) & E(Ie).MagPar(:,1)<Args.HRange(2);

    Ind  = find(Flag);

    Nast = sum(Flag);
    Result = [];
    for Iast=1:1:Nast
        [Iast, Nast]

        II = Ind(Iast);

        switch Ie
            case 1
                ObjName = sprintf('%d',E(Ie).Number(II));
            case 2
                ObjName = E(Ie).Designation{II};
            otherwise
                error('Unknown Ie option');
        end
        
        %ObjName
        
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
    
    if ~isempty(Result)
        Flag = [Result.SunAlt]<Args.MaxSunAlt & [Result.Alt]<Args.MinAlt;
        Result = Result(Flag);
    end
    
end
