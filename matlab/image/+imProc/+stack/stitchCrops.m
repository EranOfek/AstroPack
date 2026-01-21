function [Result] = stitchCrops(AI, Args)
    % Stitch together several crops originating from the same image 
    %     Optional detailed description
    % Input  : - a stack of AstroImages (containing individual crops)    
    %          * ...,key,val,... 
    % Output : - a stiched AI 
    % Author : A.M. Krassilchtchikov (2026 Jan) 
    % Example: AIs = imProc.stack.stitchCrops(AI)

    arguments
        AI
        Args.CCDSEC                  = 'CCDSEC';
        Args.UNIQSEC                 = 'UNIQSEC';
        Args.ORIGSEC                 = 'ORIGSEC';
        Args.Properties              = {'Image','Back','Var','Mask','PSF','Cat'};
    end
    %
    Ncrop = numel(AI);
    MCat  = repmat(AstroCatalog,1,Ncrop);
    Xmin  = zeros(Ncrop,1); Xmax  = zeros(Ncrop,1);
    Ymin  = zeros(Ncrop,1); Ymax  = zeros(Ncrop,1);
    
    % read the sizes and locations   
    for Icrop = 1:Ncrop                        
        CCDSEC(Icrop,:) = AI(Icrop).HeaderData.getVal(Args.CCDSEC,'ReadCCDSEC',true);
        Uniq(Icrop,:)   = AI(Icrop).HeaderData.getVal(Args.UNIQSEC,'ReadCCDSEC',true);
        Orig            = AI(Icrop).HeaderData.getVal(Args.ORIGSEC,'ReadCCDSEC',true);
        [Xmin(Icrop), Xmax(Icrop), Ymin(Icrop), Ymax(Icrop)] = deal(Orig(1),Orig(2),Orig(3),Orig(4));
    end
    
    X0 = min(Xmin); Y0 = min(Ymin); % the corner of the stitch on the whole image 
    Nx = max(Xmax)-X0+1;
    Ny = max(Ymax)-Y0+1;
    Result = AstroImage({nan(Nx,Ny)},'Back',{nan(Nx,Ny)},'Var',{nan(Nx,Ny)});
    
    % determine the overlaps
    overlapX = (Xmin < Xmax.') & (Xmax > Xmin.');
    overlapY = (Ymin < Ymax.') & (Ymax > Ymin.');
    overlap = overlapX & overlapY;
    overlap(1:Ncrop+1:end) = false;
    fromLeft  = overlap & (Xmax.' > Xmin) & (Xmin.' < Xmin);
    fromRight = overlap & (Xmin.' < Xmax) & (Xmax.' > Xmax);
    fromBottom= overlap & (Ymax.' > Ymin) & (Ymin.' < Ymin);
    fromTop   = overlap & (Ymin.' < Ymax) & (Ymax.' > Ymax);
    hasLeft   = any(fromLeft,   2);
    hasRight  = any(fromRight,  2);
    hasBottom = any(fromBottom, 2);
    hasTop    = any(fromTop,    2);
    
    for Icrop = 1:Ncrop
        if hasLeft(Icrop)
            XUmin = Uniq(Icrop,1);
        else
            XUmin = CCDSEC(Icrop,1);
        end
        if hasRight(Icrop)
            XUmax = Uniq(Icrop,2);
        else
            XUmax = CCDSEC(Icrop,2);
        end
        if hasBottom(Icrop)
            YUmin = Uniq(Icrop,3);
        else
            YUmin = CCDSEC(Icrop,3);
        end
        if hasTop(Icrop)
            YUmax = Uniq(Icrop,4);
        else
            YUmax = CCDSEC(Icrop,4);
        end
        
        AIc = crop(AI(Icrop),[XUmin XUmax YUmin YUmax],'UpdateCat',true,'CreateNewObj',true);     
        MCat(Icrop) = AIc.CatData;
        
        ShiftX = Xmin(Icrop)-X0;
        ShiftY = Ymin(Icrop)-Y0;
        
        IndX = MCat(Icrop).colname2ind({'XPEAK','X1','X'});
        IndY = MCat(Icrop).colname2ind({'YPEAK','Y1','Y'});        
        MCat(Icrop).Catalog(:,IndX) = MCat(Icrop).Catalog(:,IndX) + ShiftX;
        MCat(Icrop).Catalog(:,IndY) = MCat(Icrop).Catalog(:,IndY) + ShiftY;
        
        Result.Image(ShiftX+1:ShiftX+XUmax-XUmin+1, ShiftY+1:ShiftY+YUmax-YUmin+1) = AIc.Image';
    end
                    
    % merge the catalogs:
    Result.CatData = merge(MCat);
    Result.CatData.JD = MCat(1).julday;  
    RA0  = mean(Result.Table.RA);
    Dec0 = mean(Result.Table.Dec);
            
    % build WCS from the merged catalog 
    [~, Result.CatData, ~] = imProc.astrometry.astrometryRefine(Result.CatData,'RA',RA0,'Dec',Dec0);
    
end
