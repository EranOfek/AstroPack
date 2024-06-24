function Image = addBackground(Image, Back, Args)
    % Add background to an image or subtract it 
    %     Optional detailed description
    % Input  : - an Image matrix or a single value
    %          - a Background matrix or a single value
    %          * ...,key,val,... 
    %         'Subtract' - subtract or add the background
    %         'Size' - forced image size 
    % Output : - an image with added or subtracted background 
    % Author : A.M.Krassilchtchikov (2024 May) 
    % Example: ImageBack = imUtil.art.addBackground(Image, Back, 'Subtract', true);
    %
    arguments
        Image
        Back          = 0;
        Args.Subtract = false;
        Args.Size     = [];
    end
    % 
    if any(size(Image) < 2)
        if ~isempty(Image(1))
            Image = repmat(Image(1),Args.Size);
        elseif ~isempty(Args.Size)
            Image = repmat(0,Args.Size);
        else
            error('Neither the value nor the size of the input image is defined')
        end        
    end
    %
    if ~Args.Subtract
        Image = Image + Back;
    else
        Image = Image - Back;
    end
end
