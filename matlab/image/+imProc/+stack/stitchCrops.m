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
        Args.ORIGUSEC                = 'ORIGUSEC';
        Args.Properties              = {'Image','Back','Var','Mask','PSF','Cat'};
        Args.Border                  = 65;
    end
    %
    Ncrop = numel(AI);
    MaxX  = 1; MaxY  = 1;
    MCat   = repmat(AstroCatalog,1,Ncrop);
       
    for Icrop = 1:Ncrop                
        ReadCCDSEC = AI(Icrop).getStructKey(Args.CCDSEC).(Args.CCDSEC);        
        ReadUniq   = AI(Icrop).getStructKey(Args.UNIQSEC).(Args.UNIQSEC);
        ReadOrig   = AI(Icrop).getStructKey(Args.ORIGUSEC).(Args.ORIGUSEC); 
        CCDSEC(Icrop,:) = sscanf(ReadCCDSEC(2:end-1), '%d').';
        Uniq(Icrop,:)   = sscanf(ReadUniq(2:end-1), '%d').';        
        Orig(Icrop,:)   = sscanf(ReadOrig(2:end-1), '%d').';    
        AIuniq = crop(AI(Icrop),( Uniq(Icrop,:) + CCDSEC(Icrop,:) )/2,'UpdateCat',true,'CreateNewObj',true);             
        MCat(Icrop) = AIuniq.CatData;        
        MaxX  = max(MaxX,  Orig(Icrop,2));
        MaxY  = max(MaxY,  Orig(Icrop,4));
    end
    
    Result = AstroImage({nan(MaxX,MaxY)},'Back',{nan(MaxX,MaxY)},'Var',{nan(MaxX,MaxY)});
    
    for Icrop = 1:Ncrop
        Result.Image(Orig(Icrop,1):Orig(Icrop,2),Orig(Icrop,3):Orig(Icrop,4)) = ...
            AI(Icrop).Image(1:Uniq(Icrop,2)+Args.Border,1:Uniq(Icrop,4)+Args.Border);
        ShiftX = Orig(Icrop,1);
        ShiftY = Orig(Icrop,3);
        IndX = MCat(Icrop).colname2ind({'XPEAK','X1','X'});
        IndY = MCat(Icrop).colname2ind({'YPEAK','Y1','Y'});        
        MCat(Icrop).Catalog(:,IndX) = MCat(Icrop).Catalog(:,IndX) + ShiftX;
        MCat(Icrop).Catalog(:,IndY) = MCat(Icrop).Catalog(:,IndY) + ShiftY;
    end
    
    % merge the catalogs:
    Result.CatData = merge(MCat);
    Result.CatData.JD = MCat(1).julday;  
    RA0  = mean(Result.Table.RA);
    Dec0 = mean(Result.Table.Dec);
            
    % build WCS from the merged catalog 
    [~, Result.CatData, ~] = imProc.astrometry.astrometryRefine(Result.CatData,'RA',RA0,'Dec',Dec0);
    
end
