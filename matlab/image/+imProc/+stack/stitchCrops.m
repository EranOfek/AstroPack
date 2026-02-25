function [Result] = stitchCrops(AI, Args)
    % Stitch together several crops originating from the same image 
    %     Optional detailed description
    % Input  : - a stack of AstroImages (containing individual crops)    
    %         * ...,key,val,... 
    %         'UpdateWCS' - whether to build a WCS for the stitched image from the merged catalog
    %         'UpdateZP'  - whether to calculate a new photometric ZP for the stitched image  
    % Output : - a stiched AstroImage with a merged catalog and updated WCS
    % Author : A.M. Krassilchtchikov (2026 Jan) 
    % Example: AIs = imProc.stack.stitchCrops(AI)
    % 
    arguments
        AI
        Args.CCDSEC                  = 'CCDSEC';
        Args.UNIQSEC                 = 'UNIQSEC';
        Args.ORIGSEC                 = 'ORIGSEC';
        Args.UpdateWCS               = false;
        Args.UpdateZP                = false;
        Args.HalfOverlapX            = 64; % pix;
        Args.HalfOverlapY            = 97; % pix;
    end
    %
    Ncrop = numel(AI);
    MCat  = repmat(AstroCatalog,1,Ncrop);
    Xmin  = zeros(Ncrop,1); Xmax  = zeros(Ncrop,1);
    Ymin  = zeros(Ncrop,1); Ymax  = zeros(Ncrop,1);
    CCDSEC= zeros(Ncrop,4); 
    Uniq  = zeros(Ncrop,4);
    
    % read the sizes and locations, determine the overlaps   
    for Icrop = 1:Ncrop                        
        CCDSEC(Icrop,:) = AI(Icrop).HeaderData.getVal(Args.CCDSEC,'ReadCCDSEC',true);
        Uniq(Icrop,:)   = AI(Icrop).HeaderData.getVal(Args.UNIQSEC,'ReadCCDSEC',true);
        Orig            = AI(Icrop).HeaderData.getVal(Args.ORIGSEC,'ReadCCDSEC',true);
        [Xmin(Icrop), Xmax(Icrop), Ymin(Icrop), Ymax(Icrop)] = deal(Orig(1),Orig(2),Orig(3),Orig(4));
    end
    
    O = tools.math.geometry.overlapRectangles(Xmin, Xmax, Ymin, Ymax);
    
    % find the lower left corner of the stitch on the whole image
    X0 = min(Xmin); 
    Y0 = min(Ymin); 
    
    % find a major shift of pixel coordinates in the catalogs        
    CatShiftX = Xmin-X0; 
    CatShiftY = Ymin-Y0; 
    
    % make an empty AstroImage    
    Nx = max(Xmax)-X0+1;
    Ny = max(Ymax)-Y0+1;
    Result = AstroImage({nan(Nx,Ny)}); 
    
    % fill the new image with chopped crops, shift the catalog pixels    
    for Icrop = 1:Ncrop                
        if O.hasLeft(Icrop)
            XUmin = Args.HalfOverlapX; % round((CCDSEC(Icrop,2)-Uniq(Icrop,2))/2);
            ImaShiftX = CCDSEC(Icrop,2)-XUmin;
        else
            XUmin = CCDSEC(Icrop,1);
            ImaShiftX = XUmin-1;
        end
        if O.hasRight(Icrop)
            XUmax = CCDSEC(Icrop,2)-Args.HalfOverlapX; % round((CCDSEC(Icrop,2)+Uniq(Icrop,2))/2);
        else
            XUmax = CCDSEC(Icrop,2);
        end
        if O.hasBottom(Icrop)
            YUmin = Args.HalfOverlapY; % round((CCDSEC(Icrop,4)-Uniq(Icrop,4))/2);
            ImaShiftY = CCDSEC(Icrop,4)-YUmin;
        else
            YUmin = CCDSEC(Icrop,3);
            ImaShiftY = YUmin-1;
        end
        if O.hasTop(Icrop)
            YUmax = CCDSEC(Icrop,4)-Args.HalfOverlapY; %round((CCDSEC(Icrop,4)+Uniq(Icrop,4))/2);
        else
            YUmax = CCDSEC(Icrop,4);
        end
        
        AIc = crop(AI(Icrop),[XUmin XUmax YUmin YUmax],'UpdateCat',true,'CreateNewObj',true);             
        MCat(Icrop) = AIc.CatData;
        
        IndX = MCat(Icrop).colname2ind({'XPEAK','X1','X'});
        IndY = MCat(Icrop).colname2ind({'YPEAK','Y1','Y'});        
        MCat(Icrop).Catalog(:,IndX) = MCat(Icrop).Catalog(:,IndX) + CatShiftX(Icrop) + XUmin - 1;
        MCat(Icrop).Catalog(:,IndY) = MCat(Icrop).Catalog(:,IndY) + CatShiftY(Icrop) + YUmin - 1;
        
        Result.Image(ImaShiftY+1:ImaShiftY+YUmax-YUmin+1, ImaShiftX+1:ImaShiftX+XUmax-XUmin+1) = AIc.Image;
    end
                    
    % merge the catalogs:
    Result.CatData = merge(MCat);
    Result.CatData.JD = MCat(1).julday;  
    RA0  = mean(Result.CatData.getCol('RA'));
    Dec0 = mean(Result.CatData.getCol('Dec'));
            
    % build WCS from the merged catalog 
    if Args.UpdateWCS
        [FitRes, Result.CatData, ~] = imProc.astrometry.astrometryRefine(Result.CatData,'RA',RA0,'Dec',Dec0);
        Result.WCS         = FitRes.WCS;
        Result.WCS.Tran2D  = FitRes.Tran;
        Result.WCS.ResFit  = FitRes.ResFit;
        Result.WCS.Success = FitRes.Success;
        Result.propagateWCS('UpdateCat',false);
    end
    % calculate a new photometric ZP
    if Args.UpdateZP
        [Result, ~, ~] = imProc.calib.photometricZP(Result);
    end
    
    % add a mean JD (should be the same, but still):
    MeanJD = mean(julday(AI));        
    Result.HeaderData = replaceVal(Result.HeaderData, 'MIDJD', MeanJD);
    
    % add a mean EXPTIME:
    MeanExp = mean([AI.getStructKey('EXPTIME').EXPTIME]);
    Result.HeaderData = replaceVal(Result.HeaderData, 'EXPTIME', MeanExp);
    
    % add mount and camera number from the first crop:
    Mount = AI(1).getStructKey('MOUNTNUM').MOUNTNUM; 
    Camera= AI(1).getStructKey('CAMNUM').CAMNUM; 
    Result.HeaderData = replaceVal(Result.HeaderData, 'MOUNTNUM', Mount);
    Result.HeaderData = replaceVal(Result.HeaderData, 'CAMNUM'  , Camera);
end
