function [Result] = srcInjection(Obj, Args)
    % In progress: break to several funs
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Mar) 
    % Example: 

    arguments
        Obj                 % An AstroZOGY object
        Args.CreateNewObj      = true; % do not change unless you understand what you are doing
        Args.CCDSEC            = 'UNIQSEC'; % if empty, use image size
        Args.NearEdgeDist      = 10;
        Args.Nsrc              = 50;
        Args.KeyZP             = 'PH_ZP';  % or number
        Args.KeyLimMag         = 'LIMMAG'; % or number
        Args.MagRange          = 5;
    end

    % create a deep copy of Obj
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Cor = Result.New.cooImage(Args.CCDSEC);
    
    Nobj = numel(Result);
    for Iobj=1:1:Nobj
    
        % Get size of new
        [SizeNewY, SizeNewX] = Result(Iobj).New.sizeImage;
        
        % RA Dec of corners in: Cor(Iobj).Corners
        CCDSEC = Cor(Iobj).CCDSEC;
        
        % count pixels by FLAGS
        
        
        
        % get size of ref
        

        % calculate overlap area between new and ref

        % Generate random positions in New image
        PosSrc = rand(Args.Nsrc,2);
        PosSrc = [CCDSEC(1) + (CCDSEC(2)-CCDSEC(1)+1), CCDSEC(3) + (CCDSEC(4)-CCDSEC(3)+1)].*PosSrc;
        PosSrc = round(PosSrc);
        % remove sources NearEdge
        IndFlag = find(PosSrc(:,1)>Args.NearEdge & PosSrc(:,1)<(SizeNewX-Args.NearEdge) & PosSrc(:,2)>Args.NearEdge & PosSrc(:,2)<(SizeNewY-Args.NearEdge));
        Ninject = numel(IndFlag);
        PosSrc  = PosSrc(IndFlag,:);
        
        
        % Construct Flux of injected sources
        % Get ZP of new image
        RandMag = rand(Ninject,1);
        if isnumeric(Args.KeyZP)
            ZP = Args.KeyZP;
        else
            ZP = Result(Iobj).New.HeaderData.getVal(Args.KeyZP);
        end
        if isnumeric(Args.KeyLimMag)
            LimMag = Args.KeyLimMag;
        else
            LimMag = Result(Iobj).New.HeaderData.getVal(Args.KeyLimMag);
        end
        
        MagInjected  = LimMag - Args.MagRange.*RandMag;
        FluxInjected = 10.^(0.4.*(ZP - MagInjected));
                
        % inject new sources into new
        % works only with odd PSF size:
        NewPSF = Result(Iobj).New.PSFData.getPSF;
        Result(Iobj).New.Image = imUtil.art.injectSources_NS(Result(Iobj).New.Image, [PosSrc, FluxInjected], NewPSF);

        % perform subtraction
        % use the regualr tools
        
        % measure transients properties at the injected positions only
        % use the regular tools
        
        % calculate statistics, completness and purity
        
        % 

    end
    
end
