function [Result] = maskNaN(Obj, Args)
    % Mask NaN pixels in the bit mask image.
    % Input  : - An AstroImage object with populated Mask.
    %          * ...,key,val,...
    %            'BitNameNaN' - NaN bit name to flag.
    %                   Default is 'NaN'.
    %            'CreateNewObj' - If true, then will create a new copy of
    %                   the AstroImage object. Default is false.
    %
    % Output : - An updated AstroImage object.
    % Author : Eran Ofek (Jan 2024)
    % Example: AI = imProc.mask.maskNaN(AI)
  
    arguments
        Obj AstroImage
           
        Args.BitNameNaN              = 'NaN';     
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
        FlagImage = isnan(Obj(Iobj).Image);
        Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, FlagImage, Args.BitNameNaN, 1);
    end
end
