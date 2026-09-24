function [Result] = subBackDivideStd(Obj, Args)
    % Subtract the background and divide the image by its Std
    %   Output image has units of std.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'CreateNewObj' - Create a new copy of the input object.
    %                   Default is true.
    % Output : - An AstroImage object in which the images are background
    %            subtracted and std normalized.
    % Author : Eran Ofek (2025 Dec) 
    % Example: AI=imProc.image.subBackDivideStd(AI);

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
        Result(Iobj).ImageData.Image = (Result(Iobj).ImageData.Image - Result(Iobj).BackData.Image)./sqrt(Result(Iobj).VarData.Image);
    end
end
