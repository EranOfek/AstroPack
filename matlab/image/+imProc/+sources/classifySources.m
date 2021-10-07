function classifySources(Obj, Args)
    %
   
    arguments
        Obj
        Args.ColName_SharpSN     = 'SN_1';
        Args.ColNamsSN           = 'SN_%d';   % or cell array of column names
        Args.SigmaPSF            = [];
        
        Args.ColNameBack         = 'BACK_ANNULUS';
        Args.ColNameStd          = 'STD_ANNULUS';
        
        Args.ColNameMom2         = {'X2','Y2','XY'};
        
    end
    
    NsigmaPSF  = numel(Args.SigmaPSF);
    if NsigmaPSF==0
        error('SigmaPSF must be provided and not be empty');
    end
    if ischar(Args.ColNamsSN)
        % template name
        ColNamsSN = cell(1, NsigmaPSF);
        for Ipsf=1:1:NsigmaPSF
            ColNamsSN{Ipsf} = sprintf(Args.ColNamsSN,Ipsf);
        end
    else
        ColNamsSN = Args.ColNamsSN;
    end
    
    Nobj = numel(Obj);
    
    for Iobj=1:1:Nobj
    
        getCol(Obj, Args.ColNameBack);
        getCol(Obj, Args.ColNameStd);
        getCol(Obj, ColNamsSN);
        getCol(Obj, Args.ColNameMom2);
    end
        
    
    
end
