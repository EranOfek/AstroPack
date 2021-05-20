function Result = applyProperMotion(Obj, EpochIn, EpochOut, Args)
    %
   
    arguments
        Obj AstroCatalog
        EpochIn                 
        EpochOut
        Args.ColEpochIn
        Args.ColRA cell         = Obj(1).DefNamesRA;
        Args.ColDec cell        = Obj(1).DefNamesDec;
        Args.ColPM_RA cell      = Obj(1).DefNamesPMRA;
        Args.ColPM_Dec cell     = Obj(1).DefNamesPMDec;
        Args.ColRV cell         = Obj(1).DefNamesRV;
        Args.ColPlx cell        = Obj(1).DefNamesPlx;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
       
        if isempty(EpochIn)
        
        
        [RA, Dec, PM_RA, PM_Dec, Plx, RV] = getRADecPM(Obj(Iobj), 'OutCooUnits','rad',...
                                                                  'OutPMUnits','mas/yr',...
                                                                  'OutPlxUnits','mas',...
                                                                  'OutRVUnits','km/s');
                
        
            
        % remove NaNs
        PM_RA(isnan(PM_RA))   = 0;
        PM_Dec(isnan(PM_Dec)) = 0;
        
        % check if RV exiat
        RV(isnan(RV)) = 0;
        
        % Check if Plx exist
        if all(isnan(Plx))
            % no parallax
            [NewRA, NewDec] = celestial.coo.proper_motion(EpochOut, EpochIn, EpochIn, RA,Dec, PM_RA, PM_Dec, Plx, RV);
        else
            Plx(isnan(Plx)) = 0;
            [NewRA, NewDec] = celestial.coo.proper_motion_parallax(EpochOut, EpochIn, EpochIn, RA, Dec, PM_RA, PM_Dec, Plx, RV);
        end
    end
end
