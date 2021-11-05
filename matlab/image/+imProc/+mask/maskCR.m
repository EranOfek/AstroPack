function Result = maskCR(Obj, Args)
    %
    % There is an implementation in imProc.sources.cleanSources
    
    arguments
        Obj AstroImage
                
        Args.AlgoCR                  = 'DeltaHT';
        
        Args.CRBitName               = 'CR_DeltaHT';
        Args.CreateNewObj logical    = false;
        
    end
    
      
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image
    
        switch lower(Args.AlgoCR)
            case 'deltaht'
                % apply hypothesis testing using existing catalog
                % with S/N for dekta function and "point sources"
                
                
            otherwise
                error('Unknown AlgoCR option');
        end
        
    end
    
end