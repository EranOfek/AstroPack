function [Result, Table, TableCompact] = conjunctionsSearchStarMP(Args)
    % Search for conjunctions/occultations of minor planets and GAIA stars
    %   The code run over known selected asteroids and use 
    %   celestial.SolarSys.jpl_horizons to generate ephemerides and
    %   celestial.SolarSys.conjunctionsStars to search for the
    %   conjunctions.
    % Input  : * ...,key,val,...
    %            see code.
    % Output : - A structure array of found events.
    %          - A table output.
    %          - table with selected columns.
    % Requirements: The GAIA-DR3 catsHTM catalog should be installed.
    % Example: [Result, Table] = celestial.conjunctions.conjunctionsSearchStarMP('ElementsIndex',1);


    arguments
        Args.ElementsIndex = 2;  % 1 for numbered asteroid; 2 for unnumbered
        Args.DistRange = [9 Inf];
        Args.HRange    = [-Inf 7];
        Args.StartDate = [20 3 2023];
        Args.EndDate   = [9 6 2023];
        Args.AddPlanets  = {'799','899'};  % Uranus, Neptune
        Args.PlanetsRadius = [25362, 24622];

        Args.ObsCoo    = [35 30 0.415];
        Args.MaxSunAlt = -11.0;
        Args.MinAlt    = 25;
        
    end
    
    RAD = 180./pi;

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
        
            Result = celestial.conjunctions.conjunctionsStars(EphemCat, 'Result',Result, 'ObjName',ObjName,'ObsCoo',Args.ObsCoo, 'OcculterRadius',OcculterRadius);
        catch ME
            ME
            fprintf('Failed %d',Iast);
        end
    end
    
    Npl = numel(Args.AddPlanets);
    for Ipl=1:1:Npl
        [EphemCat] = celestial.SolarSys.jpl_horizons('ObjectInd',Args.AddPlanets{Ipl}, 'StartJD',Args.StartDate,'StopJD',Args.EndDate, 'StepSize',3,'StepSizeUnits','h');
    
        ObjName = Args.AddPlanets{Ipl};
        Result = celestial.conjunctions.conjunctionsStars(EphemCat, 'Result',Result, 'ObjName',ObjName,'ObsCoo',Args.ObsCoo, 'OcculterRadius',Args.PlanetsRadius(Ipl));
    end
    
    if ~isempty(Result)
        Flag = [Result.SunAlt]<Args.MaxSunAlt & [Result.Alt]<Args.MinAlt;
        Result = Result(Flag);
    end
    
    Table = struct2table(Result);
    
    TableCompact = table(Table.ObjName,...
                         convert.date2str(Table.Date),...
                         Table.phot_bp_mean_mag,...
                         Table.ImpactPar_inOcculterAngRadiusUnits,...
                         Table.AngSpeed,...
                         Table.CrossingTime,...
                         Table.MagMP,...
                         Table.RA.*RAD,...
                         Table.Dec.*RAD,'VariableNames',{'ObjName','Date','MagStar_Bp','ImPar_InOccAngRadUnits','AngSpeed','CrossingTime','MagAst','RA','Dec'});
    
    %Table = sortrows(TableCompact, 'JD');
    
                     
    %FID=fopen('Occ.txt','w');
    %tools.table.fprintf(FID,'%20s  %25s  %7.2f %6.2f %6.3f %5.1f %6.2f  %10.6f %10.6f\n',T);
    %fclose(FID)

end
