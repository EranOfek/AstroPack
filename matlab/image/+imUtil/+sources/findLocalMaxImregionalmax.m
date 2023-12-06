function [IndLocalMax, I, J, BW] = findLocalMaxImregionalmax(Image, Thresh, Conn)
    % 2D local maxima using imregionalmax - find local max above threshold value.
    % Input  : - A 2D image. NaN are ignored.
    %          - Threshold above to search local maxima.
    %            Default is 5.
    %          - Local maxima connectivity. Either 4, or 8.
    %            Default is 8.
    % Output : - A linear index of the local maxima.
    %          - I (rows) index of the local maxima.
    %          - J (columns) index of the local maximx.
    %          - A logical image indicating the pixels with local maxima.
    % Author : Eran Ofek (Nov 2021)
    % Example: Image=randn(1600,1600);
    %          [Ind,I,J]=imUtil.sources.findLocalMaxImregionalmax(Image,2,8);
    
    arguments
        Image
        Thresh       = 5;
        Conn         = 8;
    end
    
    if all(Image<Thresh,'all') || all(isnan(Image),'all')
        Ind = [];
        I   = [];
        J   = [];
        if nargout>3
            BW = false(size(Image));
        end
    else
    
        ThresholdedSN = Image;
        ThresholdedSN(Image<Thresh) = 0;
        BW = imregionalmax(ThresholdedSN,Conn);

        IndLocalMax   = find(BW);
        
        if nargout>1
            [I,J] = imUtil.image.ind2sub_fast(size(Image),IndLocalMax);
        end
    end
end