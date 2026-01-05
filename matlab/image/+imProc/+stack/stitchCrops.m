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
        Args.UNIQSEC                 = 'UNIQSEC';
        Args.ORIGUSEC                = 'ORIGUSEC';
        Args.Properties              = {'Image','Back','Var','Mask','PSF','Cat'};
        Args.Border                  = 65;
    end
    %
    Ncrop = numel(AI);
    MaxX  = 1;
    MaxY  = 1;
    for Icrop = 1:Ncrop
        ReadUniq  = AI(Icrop).getStructKey(Args.UNIQSEC).(Args.UNIQSEC);
        ReadOrig  = AI(Icrop).getStructKey(Args.ORIGUSEC).(Args.ORIGUSEC); 
        Uniq(Icrop,:) = sscanf(ReadUniq(2:end-1), '%d').';
        Orig(Icrop,:) = sscanf(ReadOrig(2:end-1), '%d').';
        MaxX  = max(MaxX,  Orig(Icrop,2));
        MaxY  = max(MaxY,  Orig(Icrop,4));
    end
    
    Result = AstroImage({nan(MaxX,MaxY)},'Back',{nan(MaxX,MaxY)},'Var',{nan(MaxX,MaxY)});
    
    for Icrop = 1:Ncrop
        Result.Image(Orig(Icrop,1):Orig(Icrop,2),Orig(Icrop,3):Orig(Icrop,4)) = ...
            AI(Icrop).Image(1:Uniq(Icrop,2)+Args.Border,1:Uniq(Icrop,4)+Args.Border);
    end
    
end
