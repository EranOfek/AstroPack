function Result = applyProperMotion(Obj, EpochIn, EpochOut, Args)
    % Apply proper motion and parallax to sources in AstroCatalog object
    % Input  : - An AstroCatalog object
    %          - Catalog epoch (e.g., in Julian years or JD).
    %            If empty, will attempt to use the catalog epoch column.
    %          - Output epoch.
    %          * ...,key,val,...
    %            'ColEpochIn' - If catalog epoch is not provided (empty)
    %                   then this is the column name from which to read the input
    %                   epoch.
    %            'EpochInUnits' - Units of input epoch ('mjd','jd','j').
    %                   Default is 'jd'.
    %            'EpochOutUnits' - Units of output epoch ('mjd','jd','j').
    %                   Default is 'jd'.
    %            'ApplyPlx' - A logical indicating if to apply parallax.
    %                   Requires the VSOP87 data directory.
    %                   Defaut is false.
    %            'ColRA' - Default is Obj(1).DefNamesRA
    %            'ColDec' - Default is Obj(1).DefNamesDec
    %            'ColPM_RA' - Default is Obj(1).DefNamesPMRA
    %            'ColPM_Dec' - Default is Obj(1).DefNamesPMDec
    %            'ColRV' - Default is Obj(1).DefNamesRV
    %            'ColPlx' - Default is Obj(1).DefNamesPlx
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   Default is false.
    % Output : - An AstroCatalog object with the RA/Dec at the new epoch.
    % Author : Eran Ofek (May 2021)
    % Requires: VSOP87 data directory
    % Example: C = catsHTM.cone_search('GAIADR2',1,1,1000,'OutType','astrocatalog');
    %          Result = imProc.cat.applyProperMotion(C, 2015,2021,'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',0)

    
    arguments
        Obj AstroCatalog
        EpochIn                 
        EpochOut
        Args.ColEpochIn            = {'Epoch'};
        Args.EpochInUnits          = 'jd';
        Args.EpochOutUnits         = 'jd';
        Args.ApplyPlx(1,1) logical = false;
        Args.ColRA cell            = Obj(1).DefNamesRA;
        Args.ColDec cell           = Obj(1).DefNamesDec;
        Args.ColPM_RA cell         = Obj(1).DefNamesPMRA;
        Args.ColPM_Dec cell        = Obj(1).DefNamesPMDec;
        Args.ColRV cell            = Obj(1).DefNamesRV;
        Args.ColPlx cell           = Obj(1).DefNamesPlx;
        Args.CreateNewObj logical  = false;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end
    
    % convert EpochOut to days
    EpochOutJD = convert.time(EpochOut, Args.EpochOutUnits, 'JD');
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
       
        if isempty(EpochIn)
            % try to read EpochIn from catalog
            [ColInd_Epoch] = colnameDict2ind(Obj(Iobj), Args.ColEpochIn);
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
        PM_RA(isnan(PM_RA))   = 0; % slow
        %PM_RA = PM_RA .* (~isnan(PM_RA)); % fast but introduce NaNs...

        PM_Dec(isnan(PM_Dec)) = 0; % slow
        %PM_Dec = PM_Dec .* (~isnan(PM_Dec)); % fast but introduce NaNs...
        
        % check if RV exiat
        RV(isnan(RV)) = 0; % slow
        %RV = RV .* (~isnan(RV)); % fast but introduce NaNs...

        % Check if Plx exist
        if Args.ApplyPlx && ~all(isnan(Plx))
            Plx(isnan(Plx)) = 0;
            [NewRA, NewDec] = celestial.coo.proper_motion_parallax(EpochOutJD, EpochInJD, EpochInJD, RA, Dec, PM_RA, PM_Dec, Plx, RV);
        else
            % no parallax
            [NewRA, NewDec] = celestial.coo.proper_motion(EpochOutJD, EpochInJD, EpochInJD, RA,Dec, PM_RA, PM_Dec, Plx, RV);
        end
        
        ColIndRA  = colnameDict2ind(Obj(Iobj), Args.ColRA);
        ColIndDec = colnameDict2ind(Obj(Iobj), Args.ColDec);
        
        Result(Iobj) = replaceCol(Result(Iobj), [NewRA, NewDec], [ColIndRA, ColIndDec]);
    end
end
