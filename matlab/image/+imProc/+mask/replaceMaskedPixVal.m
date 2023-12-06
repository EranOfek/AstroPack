function Result = replaceMaskedPixVal(Obj,  BitNames, ReplaceVal, Args)
    % Replace the values of image pixels which have specific bit mask
    % Input  : - An AstroImage object.
    %          - A cell array of mask bit names. Pixels in which this bits
    %            are on (any | or) will be set to ReplaceVal.
    %          - Value to replace (ReplaceVal).
    %          * ...,key,val,...
    %            'Method' - Indicating if to look for pixels in
    %                   which all the requested bits are on
    %                   ('all'), or one or more of the requested
    %                   bits are on ('any').
    %                   Default is 'any'.
    %            'DataProp' - Data property in AstroImage for which to
    %                   modify value. Default is 'Image'.
    %            'CreateNewObj' - Logical indicating if to copy the input
    %                   object. Default is false.
    % Output : - An AstroImage object with the pixel values replaced.
    % Author : Eran Ofek (Aug 2021)
    % Example: imProc.mask.replaceMaskedPixVal(AI,  'Saturated', NaN);

    arguments
        Obj AstroImage
        BitNames
        ReplaceVal                = NaN;
        Args.Method               = 'any';
        Args.DataProp             = 'Image';
        Args.CreateNewObj logical = false;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        [Flag] = findBit(Obj(Iobj).MaskData, BitNames, 'Method',Args.Method);
    
        Result(Iobj).(Args.DataProp)(Flag) = ReplaceVal;
    end
end