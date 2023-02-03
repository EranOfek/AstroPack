function zp_external(Obj, Args)
    %
   
    arguments
        Obj MatchedSources
        Args.CalibFun function_handle     =
        Args.CalibFunArgs function_handle = {};
        
        Args.FieldMagExternal             = {};      % if not in MatchedSources than query ExternalCat
        Args.FieldMagErrExternal          = {};      % if not in MatchedSources than query ExternalCat
        Args.FieldMagMeasured             = 'MAG_PSF';
        Args.FieldMagErrMeasured          = 'MAGERR_PSF';
        
        Args.FieldRA                      = 'RA';
        Args.FieldDec                     = 'Dec';
        Args.CooUnits                     = 'deg';
        
        
        Args.CalibCat                     = 'GAIADR3';
        Args.MatchRadius                  = 3;
        Args.MatchRadiusUnits             = 'arcsec';
        
        Args.CreateNewObj logical         = true;
        Args.UpdateMagFields              = {'MAG_PSF'};
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    NextMag = numel(Args.FieldMagExternal);
    Nupdate = numel(Args.UpdateMagFields);
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each MatchedSources object
        
        Nepoch = Obj(Iobj).Nepoch;
        Nsrc   = Obj(Iobj).Nsrc;
        
        for Iepoch=1:1:Nepoch
            MagInst    = Obj(Iobj).Data.(Args.FieldMagMeasured)(Iepoch,:).';
            MagErrInst = Obj(Iobj).Data.(Args.FieldMagErrMeasured)(Iepoch,:).';
        
            MagCat = nan(Nsrc,NextMag);
            for IextMag=1:1:NextMag
                MagCat(:,IextMag)    = Obj(Iobj).Data.(Args.FieldMagExternal)(Iepoch,:).';
                MagErrCat(:,IextMag) = Obj(Iobj).Data.(Args.FieldMagErrExternal)(Iepoch,:).';
            end
            
            % call calibration function
            ResZP(Iobj,Iepoch) = Args.CalibFun(MagInst, MagErrInst, MagCat, MagErrCat, Argss.CalibFunArgs{:});
        end
        
        ZP = [ResZP(Iobj,:).ZP];
        ZP = ZP(:);
        
        for Iupdate=1:1:Nupdate
            Result(Iobj).Data.(Args.UpdateMagFields{Iupdate}) = Obj(Iobj).Data.(Args.UpdateMagFields{Iupdate}) + ZP;
        end
        
    end
    
end