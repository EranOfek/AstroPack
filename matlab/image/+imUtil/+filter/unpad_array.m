function Result = unpad_array(Image, PadSize)
    % A simple unpadding of 2D matrices
    % Input  : - An image.
    %          - PadSize [I, J], if scalar use the same pad length for both
    %            axes.
    % Output : - An image with the trimmed padded area.
    % Author : Eran Ofek (Jul 2021)
    % Example: Array = ones(3,4);
    %          Array = padarray(Array,[3 3],0,'both')
    %          imUtil.filter.unpad_array(Array,[3 3])
   
    arguments
        Image
        PadSize
    end
    
    if numel(PadSize)==1
        PadSize = [PadSize, PadSize];
    end
    
    Size  = size(Image);
    Result = Image(PadSize(1)+1:Size(1)-PadSize(1), PadSize(2)+1:Size(2)-PadSize(2));
    
end