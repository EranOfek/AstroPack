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
        Obj
        Args.FluxColName                  = 'FLUX';
        Args.MinSource                    = 5;
        Args.FluxErrColName               = 'FLUXERR';
        Args.MaxRelErr                    = 0.05;  % reject sources with higer errors   . If empty do nothing                    
        Args.NormFluxFun function_handle  = @median;
        
        
    end
    
    if isnumeric(Obj)
        Tmp = Obj;
        Obj = MatchedSources;
        Obj.addMatrix(Tmp, Args.FluxColName);
    end
    
    
    
    Flag = notNanSources(Obj, Args.FluxColName); 

    % remove stars with large errors
    if ~isempty(Args.MaxRelErr)
        RelErr = Obj.Data.(Args.FluxErrColName)./Obj.Data.(Args.FluxColName);
        
        FlagSmallErr = all(RelErr<Args.MaxRelErr, 1);
        
        Flag = Flag & FlagSmallErr;
    end
    
    
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