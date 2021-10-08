function [Result, Flag] = classifySources(Obj, Args)
    % Classify sources found by findMeasureSources.
    %   The source classification includes:
    %   
    % Example: [Result, Flag] = classifySources(Obj)
   
    
    arguments
        Obj
        Args.ColNamsSN           = 'SN_%d';   % or cell array of column names, sharp is always in 1
        Args.SigmaPSF            = [];
        
        Args.ThresholdSN         = 5;
        Args.BackRatioLimit      = 1.5;
        Args.VarRatioLimit       = 1.5;
        
        Args.ColNameFlux         = {'FLUX_CONV_2','FLUX_CONV_3'};
        Args.ColNameBackAnn      = 'BACK_ANNULUS';
        Args.ColNameStdAnn       = 'STD_ANNULUS';
        Args.ColNameBackIm       = 'BACK_IM';
        Args.ColNameVarIm        = 'VAR_IM';
        
        Args.ColNameMom2         = {'X2','Y2','XY'};
        
        Args.CreateNewObj logical = false;
    end
    
    AreaPSF    = 4.*pi.*Args.SigmaPSF.^2;
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
    
    
    Result = Obj;
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            if Args.CreateNewObj
                Cat = Obj(Iobj).CatData.copy;
            else
                Cat = Obj(Iobj).CatData;
            end
        elseif isa(Obj, 'AstroCatalog')
            if Args.CreateNewObj
                Cat = Obj(Iobj).copy;
            else
                Cat = Obj(Iobj);
            end
        else
            error('First input argument must be an AstroImage or AstroCatalog object');
        end
        
        BackAnn = getCol(Cat, Args.ColNameBackAnn);
        StdAnn  = getCol(Cat, Args.ColNameStdAnn);
        BackIm  = getCol(Cat, Args.ColNameBackIm);
        VarIm   = getCol(Cat, Args.ColNameVarIm);
        SN      = getCol(Cat, ColNamsSN);
        Mom2    = getCol(Cat, Args.ColNameMom2);
        Flux    = getCol(Cat, Args.ColNameFlux);
        
        SN_FluxAnn = Flux./(StdAnn.*sqrt(AreaPSF.'));
        
        Flag.BadSN = all(SN_FluxAnn < Args.ThresholdSN, 2);
        Flag.CR    = SN(:,1) > SN(:,2);
        
        [~,MaxI] = max(SN(:, 2:end-1),[],2);  % index of matche filter that maximize S/N
        ModeI    = mode(MaxI) + 1;
        
        if ModeI>(NsigmaPSF+1)
            % the typical sources has the largest match filter size
            % can't find extended sources
            Flag.Extended = false([size(SN,1) 1]);
        else
            Flag.Extended = SN(:,ModeI+2) > SN(:,ModeI);
        end
        
        Flag.AnomBack = (BackAnn./BackIm) > Args.BackRatioLimit;
        Flag.AnomVar  = (StdAnn.^2./VarIm) > Args.VarRatioLimit;
        
        % remove bad sources
        
        
        % add columns with flags
        
        
        % save in Result
        if isa(Obj, 'AstroImage')
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj)         = Cat;
        end
        
    end    
    
end
