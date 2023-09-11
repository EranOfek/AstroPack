function [Result,LC] = zp_external_mod(Obj, Args)
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

        Args.FluxErrRange                 = [0.003 0.03];
        Args.LCNSource                    = 1; % return LC for LCNSource

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
        Args.UpdateMagFields              = {'FLUX_PSF'};
        Args.ZP                           = 25;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nupdate = numel(Args.UpdateMagFields);
    
    Nobj = numel(Obj);
    LC   = zeros(0,7);
    for Iobj=1:1:Nobj
        Iobj
        % for each MatchedSources object
        try
            
        Nepoch = Obj(Iobj).Nepoch;
        Nsrc   = Obj(Iobj).Nsrc;
        
        RefMag    = Obj(Iobj).SrcData.(Args.FieldRefMag);
        % convert to flux
        if Args.IsMag
            error('IsMag true option is not yet available');
        end
        RefMagErr = Obj(Iobj).SrcData.(Args.FieldRefMagErr);
        RefMagErr = Args.FunConvertRefErr(RefMagErr);

         
        
        InstFlux    = Obj(Iobj).Data.(Args.FieldFlux);
        InstFluxErr = real(Obj(Iobj).Data.(Args.FieldErr));
        
        FlagGoodRef = RefMagErr<0.05 & InstFlux>0 & InstFluxErr>min(Args.FluxErrRange); % & InstFluxErr<max(Args.FluxErrRange);
        
        Color = Result(Iobj).SrcData.phot_bp_mean_mag - Result(Iobj).SrcData.phot_g_mean_mag;

        
        Distance = sqrt(((Obj(Iobj).Data.X-Obj(Iobj).Data.X(:,Args.LCNSource)).^2 + (Obj(Iobj).Data.Y-Obj(Iobj).Data.Y(:,Args.LCNSource)).^2));
        FlagGoodRef = FlagGoodRef & Color>0.2 & Color<0.8 & Distance<100 & RefMag<20;
        sum(FlagGoodRef)
        Nref = sum(FlagGoodRef,2);
        
        
        
        
        RefMagMat               = RefMag.*FlagGoodRef;
        RefMagMat(RefMagMat==0) = NaN;
        RefFluxMat              = 10.^(-0.4.*RefMagMat);
        
        InstFlux(~FlagGoodRef)  = NaN;
        
        InstMag                 = Args.ZP -2.5.*log10(InstFlux);
        
        
        
        
        DeltaMag = InstMag - RefMagMat;
        ZP       = median(DeltaMag,2,'omitnan')
        StdZP    = tools.math.stat.rstd(DeltaMag,2)
        ErrZP    = StdZP./sqrt(Nref);
        FluxZP   = 10.^(0.4.*(ZP));
        
        RealZP = Args.ZP - ZP;
        
       
        for Iupdate=1:1:Nupdate
            Result(Iobj).Data.(Args.UpdateMagFields{Iupdate}) = Obj(Iobj).Data.(Args.UpdateMagFields{Iupdate}).*FluxZP;
        end

        Err = sqrt(Result(Iobj).Data.MAGERR_PSF(:,Args.LCNSource).^2 + ErrZP.^2);
        LC = [LC; [Result(Iobj).JD(:),...
                   Result(Iobj).Data.FLUX_PSF(:,Args.LCNSource),...
                   Result(Iobj).Data.MAGERR_PSF(:,Args.LCNSource),...
                   Result(Iobj).Data.Chi2dof(:,Args.LCNSource),...
                   Result(Iobj).Data.FLAGS(:,Args.LCNSource),...
                   Result(Iobj).Data.FLAG_POS(:,Args.LCNSource),...
                   Result(Iobj).Data.MAG_PSF(:,Args.LCNSource),...
                   ErrZP]];
        Result(Iobj).Data.MAG_PSF(:,Args.LCNSource)
        Obj(Iobj).Data.MAG_PSF(:,Args.LCNSource)
        end
    end


end