function [Result] = xcorrWithPSF(Obj, Args)
    % Cross-correlate an image in AstroImage object with its PSF.
    % Input  : - An AstroImage object
    %          * ...,key,val,... 
    %            'CreateNewObj' - Create a new copy of the input object.
    %                   Default is true.
    % Output : - An AstroImage in which the image was cross-corellated
    %            (filtered) with its PSF as stored in the input AstroImage.
    % Author : Eran Ofek (2025 Dec) 
    % Example: AI=imProc.image.xcorrWithPSF(AI);

    arguments
        Obj
        Args.CreateNewObj     = true;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end


    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        Result(Iobj).ImageData.Image = imUtil.filter.filter2_fast(Result(Iobj).ImageData.Image, Result(Iobj).PSFData.Data);
    end

end
