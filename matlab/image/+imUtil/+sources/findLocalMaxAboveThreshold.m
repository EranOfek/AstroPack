function [ListInd, II, JJ, BW] = findLocalMaxAboveThreshold(Image, Thresh, Conn, AllocateFrac)
    % Fast 2D local maxima finder - find local max above threshold value.
    %       This function was designed to be a faster version of
    %       imregionalmax for some specific cases.
    %       Its gives an order of magnitude speed improvment over
    %       imregionalmax.
    %       Unlike imregionalmax, local maxima are searched only in pixels
    %       which value is larer than some threshold.
    % Input  : - A 2D image. NaN are ignored.
    %          - Threshold above to search local maxima.
    %            Default is 5.
    %          - Local maxima connectivity. Either 4, or 8.
    %            Default is 8.
    %          - Allocate memory parameter. Default is 0.1 of matrix size.
    % Output : - A linear index of the local maxima.
    %          - I (rows) index of the local maxima.
    %          - J (columns) index of the local maximx.
    %          - A logical image indicating the pixels with local maxima.
    % Author : Eran Ofek (Nov 2021)
    % Example: Image=randn(1600,1600);
    %          [Ind,I,J]=imUtil.sources.findLocalMaxAboveThreshold(Image,2,8)
    
    arguments
        Image
        Thresh       = 5;
        Conn         = 8;
        AllocateFrac = 0.1;
    end
   
    [SizeI, SizeJ] = size(Image);
    Npix           = numel(Image);
    
    ListInd = zeros(ceil(AllocateFrac.*SizeI.*SizeJ), 1);
    K = find(Image>Thresh);
    Nk = numel(K);
    Counter = 0;
    
    if Conn==8
        for Ik=1:1:Nk
            Ki = K(Ik);
            Val = Image(Ki);
            if Ki>(SizeI+1) && Ki<(Npix-SizeI-1) && ...
                    Val>Image(Ki+1) && Val>Image(Ki-1) && ...
                    Val>Image(Ki+SizeI) && Val>Image(Ki+SizeI-1) && Val>Image(Ki+SizeI+1) && ...
                    Val>Image(Ki-SizeI) && Val>Image(Ki-SizeI-1) && Val>Image(Ki-SizeI+1)
                % local max found
                Counter = Counter + 1;
                ListInd(Counter) = Ki;
            end
        end
    elseif Conn==4
        for Ik=1:1:Nk
            Ki = K(Ik);
            Val = Image(Ki);
            if Ki>(SizeI+1) && Ki<(Npix-SizeI-1) && ...
                    Val>Image(Ki+1) && Val>Image(Ki-1) && ...
                    Val>Image(Ki+SizeI) && ...
                    Val>Image(Ki-SizeI) 
                % local max found
                Counter = Counter + 1;
                ListInd(Counter) = Ki;
            end
        end
    else
        error('Connmust be 4 or 8');
    end
    ListInd = ListInd(1:Counter);
    
    if nargout>1
        [II, JJ] = imUtil.image.ind2sub_fast([SizeI, SizeJ], ListInd);
        if nargout>3
            BW = false(size(Image));
            BW(ListInd) = true;
        end
    end
    
    
%     SizeIm = size(Image);
%     for I=2:1:SizeIm(1)-1
%         for J=2:1:SizeIm(2)-1
%             if Image(I,J)>Thresh
%                 Image(I,J)>Image(I+1,J+1);
%             end
%         end
%     end
    
    
    
    
end