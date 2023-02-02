function [Result, Info] = zp_sumFlux(Obj, Args)
    %
    % Example: Fzp = rand(100,1).*1000; 
    %          Flux = randn(100,200) + Fzp;
    %          Result = lcUtil.zp_sumFlux(Flux, 'MaxRelErr',[]);
    % 
    %          Fzp   = 1 + rand(100,1);
    %          Fstar = rand(1,200).*3900 + 100; 
    %          Flux = Fzp.*Fstar;
    %          Flux = poissrnd(Flux);
    %          FluxErr = sqrt(Flux);
    %          MS = MatchedSources;
    %          MS.addMatrix({Flux, FluxErr},{'FLUX','FLUXERR'});
    %          [Result, Info] = lcUtil.zp_sumFlux(MS);
    % if
    % any(abs(Info.FluxZP./median(Info.FluxZP)./(Fzp./median(Fzp))-1)>0.01),
    % error
    %
   
    arguments
        Obj(1,1)
        Args.FluxColName                  = 'FLUX_PSF';
        Args.MinSource                    = 5;
        Args.FluxErrColName               = 'FLUXERR';
        Args.MagErrColName                = 'MAGERR_PSF';
        Args.MaxRelErr                    = 0.05;  % reject sources with higer errors   . If empty do nothing  
        Args.MaxFlux                      = 50000;
        Args.NormFluxFun function_handle  = @median;
        
        Args.Itarget = 1;
    end
    
    if isnumeric(Obj)
        Tmp = Obj;
        Obj = MatchedSources;
        Obj.addMatrix(Tmp, Args.FluxColName);
    end
    
    % add SrcData vectors
    Obj.addSrcData;
    
    
    Itarget = Args.Itarget;
    
    % select epochs in which the primary target have measurments
    FlagNN = ~isnan(Obj.Data.(Args.FluxColName)(:,Itarget));
    ObjS = Obj.SelectByEpoch(FlagNN, 'CreateNewObj',true);
    
    Flag = notNanSources(Obj, Args.FluxColName); 
    % select reference stars that have measurments in all epochs
    ObjS = ObjS.selectBySrcIndex(Flag, 'CreateNewObj',false);
    
    
    % reference stars
    % remove stars with large errors
    if ~isempty(Args.MaxRelErr)
        
        FlagRef = ObjS.SrcData.(Args.MagErrColName) < Args.MaxRelErr & ObjS.SrcData.(Args.FluxColName)<Args.MaxFlux;
        
        ObjR = ObjS.selectBySrcIndex(FlagRef, 'CreateNewObj',true);
        
    end
    
    % use reference star to find ZP
    Flux = ObjR.Data.(Args.FluxColName);
    
    FluxZP = sum(Flux,2);
    FluxZP = FluxZP./median(FluxZP);
    
    
    
    NormFlux = Flux./FluxZP;
    
    Std  = std(NormFlux,[],1);
    Mean = median(NormFlux,1);
    Err  = Std./Mean;
    
    FlagErr = Err<0.1;
    Flux    = Flux(:,FlagErr);
    
    FluxZP = sum(Flux,2);
    FluxZP = FluxZP./median(FluxZP);
    
    NormFlux = Flux./FluxZP;
    
    Std  = std(NormFlux,[],1);
    Mean = median(NormFlux,1);
    Err  = Std./Mean;
    
    
    loglog(Mean, Err, '.')
    
    
    plot(ObjS.Data.(Args.FluxColName)(:,1)./FluxZP,'.')
    
    
    
    
    
    
    Ndet = sum(~isnan(Obj.Data.FLUX_PSF),2)
    
    SumFlux = sum(Obj.Data.FLUX_PSF(:,FlagRef), 2);
    
    if sum(Flag)<Args.MinSource
        Result.ZP = NaN;
    else
        SumFlux       = sum(Obj.Data.(Args.FluxColName)(:,Flag), 2);
        NormFlux      = 1./Args.NormFluxFun(SumFlux);
        FluxZP        = SumFlux.*NormFlux;
        
        CalibFlux     = Obj.Data.(Args.FluxColName) .* FluxZP;
        Diff  = Obj.Data.(Args.FluxColName) - CalibFlux;
        Ratio = Obj.Data.(Args.FluxColName)./CalibFlux;
        
        std(Ratio,[],2);
        std(Diff,[],2);
        
        Chi2 = (Diff./Obj.Data.(Args.FluxErrColName)).^2;
        Chi2_Source = sum(Chi2, 1);
        Chi2_Epoch  = sum(Chi2, 2);
        
        
        Result.CalibFlux = CalibFlux;
        Info.FluxZP = FluxZP;
        
    end
    
end