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
    CCDSEC= zeros(Ncrop,4); 
    Uniq  = zeros(Ncrop,4);
    
    % read the sizes and locations   
    for Icrop = 1:Ncrop                        
        CCDSEC(Icrop,:) = AI(Icrop).HeaderData.getVal(Args.CCDSEC,'ReadCCDSEC',true);
        Uniq(Icrop,:)   = AI(Icrop).HeaderData.getVal(Args.UNIQSEC,'ReadCCDSEC',true);
        Orig            = AI(Icrop).HeaderData.getVal(Args.ORIGSEC,'ReadCCDSEC',true);
        [Xmin(Icrop), Xmax(Icrop), Ymin(Icrop), Ymax(Icrop)] = deal(Orig(1),Orig(2),Orig(3),Orig(4));
    end
    
    X0 = min(Xmin); % the lower left corner of the stitch on the whole image 
    Y0 = min(Ymin); 
    CatShiftX = Xmin-X0; % shift of pixel coordinates in the catalogs        
    CatShiftY = Ymin-Y0; 
        
    Nx = max(Xmax)-X0+1;
    Ny = max(Ymax)-Y0+1;
    Result = AstroImage({nan(Nx,Ny)}); % ,'Back',{nan(Nx,Ny)},'Var',{nan(Nx,Ny)});
    
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
            XUmin = ceil((Uniq(Icrop,1)+CCDSEC(Icrop,1))/2);
            ImaShiftX = CCDSEC(Icrop,2)-XUmin;
        else
            XUmin = CCDSEC(Icrop,1);
            ImaShiftX = XUmin-1;
        end
        if hasRight(Icrop)
            XUmax = CCDSEC(Icrop,2)-Uniq(Icrop,1)/2;
%             XUmax = CCDSEC(Icrop,2)-Uniq(Icrop,1);
        else
            XUmax = CCDSEC(Icrop,2);
        end
        if hasBottom(Icrop)
            YUmin = ceil((Uniq(Icrop,3)+CCDSEC(Icrop,3))/2); 
            ImaShiftY = CCDSEC(Icrop,4)-YUmin;
        else
            YUmin = CCDSEC(Icrop,3);
            ImaShiftY = YUmin-1;
        end
        if hasTop(Icrop)
            YUmax = CCDSEC(Icrop,4)-Uniq(Icrop,3)/2;
%             YUmax = CCDSEC(Icrop,4)-Uniq(Icrop,3);
        else
            YUmax = CCDSEC(Icrop,4);
        end
        
        AIc = crop(AI(Icrop),[XUmin XUmax YUmin YUmax],'UpdateCat',true,'CreateNewObj',true);             
        MCat(Icrop) = AIc.CatData;
        
        IndX = MCat(Icrop).colname2ind({'XPEAK','X1','X'});
        IndY = MCat(Icrop).colname2ind({'YPEAK','Y1','Y'});        
        MCat(Icrop).Catalog(:,IndX) = MCat(Icrop).Catalog(:,IndX) + CatShiftX(Icrop);
        MCat(Icrop).Catalog(:,IndY) = MCat(Icrop).Catalog(:,IndY) + CatShiftY(Icrop);
        
%         Result.Image(ImaShiftX+1:ImaShiftX+XUmax-XUmin+1, ImaShiftY+1:ImaShiftY+YUmax-YUmin+1) = AIc.Image;
        Result.Image(ImaShiftY+1:ImaShiftY+YUmax-YUmin+1, ImaShiftX+1:ImaShiftX+XUmax-XUmin+1) = AIc.Image;
    end
                    
    % merge the catalogs:
    Result.CatData = merge(MCat);
    Result.CatData.JD = MCat(1).julday;  
    RA0  = mean(Result.CatData.getCol('RA'));
    Dec0 = mean(Result.CatData.getCol('Dec'));
            
    % build WCS from the merged catalog 
    [FitRes, Result.CatData, ~] = imProc.astrometry.astrometryRefine(Result.CatData,'RA',RA0,'Dec',Dec0);
    Result.WCS=FitRes.WCS;
    Result.propagateWCS('UpdateCat',false,'OnlyIfSuccess',false);  
end
