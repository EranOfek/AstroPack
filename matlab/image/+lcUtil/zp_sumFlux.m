function Result = zp_sumFlux(Obj, Args)
    %
   
    arguments
        Obj
        Args.FluxColName                  = 'FLUX';
        Args.MinSourceNN                  = 5;
        Args.NormFluxFun function_handle  = @median;
        
    end
    
    if isnumeric(Obj)
        Tmp = Obj;
        Obj = MatchedSources;
        Obj.addMatrix(Tmp, Args.FluxColName);
    end
    
    
    
    FlagNN = notNanSources(Obj, Args.FluxColName); 

    
    
    if sum(FlagNN)<Args.MinSourceNN
        Result.ZP = NaN;
    else
        SumFlux     = sum(Obj.Data.(Args.FluxColName)(:,FlagNN), 2);
        NormFlux    = 1./Args.NormFluxFun(SumFlux);
        NormSumFlux = SumFlux.*NormFlux;
        
        CalibFlux   = Obj.Data.(Args.FluxColName) .* NormSumFlux;
        
    end
    
end