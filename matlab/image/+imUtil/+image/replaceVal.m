function [Result,Flag] = replaceVal(Image, InVal, OutVal, Args)
    % Search all pixels in image that are equal to some value or in/out of range with a new value.
    % Input  : - An array.
    %          - Value to replace, or a [Min Max] range.
    %            If scalar, than search for this value and replace these
    %            pixels with the value in the OutVal argument.
    %            If [Min Max] than a range. In this case will search for
    %            values in the range (if 'UseOutRange'= false), or out of
    %            range (if 'UseOutRange'= true).
    %            Default is NaN.
    %          - Value by which to replace all the selected pixels.
    %            Default is 0.
    %          * ...,key,val,...
    %            'UseOutRange' - A logical. If false then will search for
    %                   values in range. If true, then will search for
    %                   values out of range.
    %                   Default is true (i.e., in range).
    % Output : - An array with the replaced values.
    %          - Array of flags of selected pixels for which the value was
    %            replaced.
    % Author : Eran Ofek (May 2023)
    % Example: Image = [0 NaN 1]; Result = imUtil.image.replaceVal(Image)
    %          Image = [2 1 2 3]; Result = imUtil.image.replaceVal(Image,[1.5 2.5])
    %          Image = [2 1 2 3]; Result = imUtil.image.replaceVal(Image,[1.5 2.5],0,'UseOutRange',true)

    arguments
        Image
        InVal      = NaN;
        OutVal     = 0;
        Args.UseOutRange logical     = false;
    end

    if isnan(InVal)
        Flag = isnan(Image);
    else
        if numel(InVal)==1
            Flag = Image == InVal;
        else
            if Args.UseOutRange
                % Treat InVal as out of range selection
                Flag = Image<min(InVal) | Image>max(InVal);
            else
                Flag = Image>min(InVal) & Image<max(InVal);
            end
        end
    end
    Result       = Image;
    Result(Flag) = OutVal;
    
end
