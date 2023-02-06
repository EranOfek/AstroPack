function zp_external(Obj, Args)
    %
   
    arguments
        Obj MatchedSources

        Args.FieldFlux                    = 'FLUX_PSF';
        Args.FieldErr                     = 'MAGERR_PSF';
        Args.IsMag logical                = false;
        Args.FunConvertErr                = @(F) F./1.086;    % conversion to relative error
        Args.FieldRefMag                  = 'phot_bp_mean_mag';
        Args.FieldRefMagErr               = 'phot_bp_mean_flux_over_error';
        Args.FunConvertRefErr             = @(F) 1.086./F;    % conversion to relative error

        Args.FluxErrRange                 = [0.003 0.02];

%         Args.CalibFun function_handle     =
%         Args.CalibFunArgs function_handle = {};
%         
%         Args.FieldMagExternal             = {};      % if not in MatchedSources than query ExternalCat
%         Args.FieldMagErrExternal          = {};      % if not in MatchedSources than query ExternalCat
%         Args.FieldMagMeasured             = 'MAG_PSF';
%         Args.FieldMagErrMeasured          = 'MAGERR_PSF';
%         
%         Args.FieldRA                      = 'RA';
%         Args.FieldDec                     = 'Dec';
%         Args.CooUnits                     = 'deg';
%         
%         
%         Args.CalibCat                     = 'GAIADR3';
%         Args.MatchRadius                  = 3;
%         Args.MatchRadiusUnits             = 'arcsec';
%         
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
        
        RefMag    = Obj(Iobj).SrcDara.(Args.FieldRefMag);
        % convert to flux
        if Args.IsMag
            error('IsMag true option is not yet available');
        end
        RefMagErr = Obj(Iobj).SrcDara.(Args.FieldRefMagErr);
        RefMagErr = Args.FunConvertRefErr(RefMagErr);

        InstFlux    = Obj(Iobj).Dara.(Args.FieldFlux);
        InstFluxErr = Obj(Iobj).Dara.(Args.FieldErr);

        MeanInstFluxErr = mean(InstFluxErr,1,'omitnan');
        FlagRef         = MeanInstFluxErr>min(Args.FluxErrRange) & MeanInstFluxErr<max(Args.FluxErrRange);


        MagDiff = convert.luptitude(InstFlux(:,FlagRef)) - RefMag(FlagRef);
        Nref    = sum(FlagRef);
        ZP      = median(MagDiff,2,'omitnan');
        StdZP   = std(MagDiff,[],2,'omitnan');
        ErrZP   = StdZP./sqrt(Nref);

        for Iupdate=1:1:Nupdate
            Result(Iobj).Data.(Args.UpdateMagFields{Iupdate}) = Obj(Iobj).Data.(Args.UpdateMagFields{Iupdate}) + ZP;
        end

        
    end


end