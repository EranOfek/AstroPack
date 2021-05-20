function Result = applyProperMotion(Obj, EpochIn, EpochOut, Args)
    %
    % Example: C = catsHTM.cone_search('GAIADR2',1,1,100,'OutType','astrocatalog');
    %          Result = imProc.cat.applyProperMotion(C, 2015,2021,'EpochInUnits','J','EpochOutUnits','J')

    
    arguments
        Obj AstroCatalog
        EpochIn                 
        EpochOut
        Args.ColEpochIn         = {'Epoch'};
        Args.EpochInUnits       = 'day';
        Args.EpochOutUnits      = 'day';
        Args.ColRA cell         = Obj(1).DefNamesRA;
        Args.ColDec cell        = Obj(1).DefNamesDec;
        Args.ColPM_RA cell      = Obj(1).DefNamesPMRA;
        Args.ColPM_Dec cell     = Obj(1).DefNamesPMDec;
        Args.ColRV cell         = Obj(1).DefNamesRV;
        Args.ColPlx cell        = Obj(1).DefNamesPlx;
        Args.CreateNewObj       = [];
    end
    
    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end
    if Args.CreateNewObj
        Result = Obj.copyObject;
    else
        Result = Obj;
    end
    
    % convert EpochOut to days
    EpochOutJD = convert.time(EpochOut, Args.EpochOutUnits, 'JD');
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
       
        if isempty(EpochIn)
            % try to read EpochIn from catalog
            [ColInd_Epoch] = colnameDict2ind(Obj(Iobj), Arg.ColEpochIn);
            [EpochIn, EpochInUnits] = getCol(Obj(Iobj), ColInd_Epoch);
        else
            EpochInUnits = {Args.EpochInUnits};
        end
        % convert EpochIn to days
        EpochInJD = convert.time(EpochIn, EpochInUnits{1}, 'JD');
        
        
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
            [NewRA, NewDec] = celestial.coo.proper_motion(EpochOutJD, EpochInJD, EpochInJD, RA,Dec, PM_RA, PM_Dec, Plx, RV);
        else
            Plx(isnan(Plx)) = 0;
            [NewRA, NewDec] = celestial.coo.proper_motion_parallax(EpochOutJD, EpochInJD, EpochInJD, RA, Dec, PM_RA, PM_Dec, Plx, RV);
        end
        
        ColIndRA  = colnameDict2ind(Obj(Iobj), Args.ColRA);
        ColIndDec = colnameDict2ind(Obj(Iobj), Args.ColDec);
        
        Result(Iobj) = replaceCol(Result(Iobj), [NewRA, NewDec], [ColIndRA, ColIndDec]);
    end
end
