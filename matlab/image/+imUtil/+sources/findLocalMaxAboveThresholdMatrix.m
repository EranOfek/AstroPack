    function [ListInd, II, JJ, BW] = findLocalMaxAboveThresholdMatrix(Image, Thresh, Conn)
    % Fast 2D local maxima finder - find local max above threshold value without looping 
    %       over indices, by using logical operators on the whole image matrix.
    %       Should be faster than imregionalmax.
    % Input  : - A 2D image. NaN are ignored.
    %          - Threshold above to search local maxima.
    %            Default is 5.
    %          - Local maxima connectivity. Either 4, or 8.
    %            Default is 8.
    % Output : - A linear index of the local maxima.
    %          - I (rows) index of the local maxima.
    %          - J (columns) index of the local maximx.
    %          - A logical image indicating the pixels with local maxima.
    % Author : Amir Sharon (Nov 2021)
    % Example: Image=randn(1600,1600);
    %          [Ind,I,J]=imUtil.sources.findLocalMaxAboveThreshold(Image,2,8)
    
    arguments
        Image
        Thresh       = 5;
        Conn         = 8;
    end
    
    if Conn~=8 && Conn~=4
        error('Conn must be 4 or 8');
    end
    
    ImageCenter = Image(2:end-1,2:end-1); % just for convenience, not obligatory
    
    BW = ImageCenter>Thresh & ImageCenter>Image(1:end-2,2:end-1) & ImageCenter>Image(3:end,2:end-1) &...
     ImageCenter>Image(2:end-1,1:end-2) & ImageCenter>Image(2:end-1,3:end);
 
    if Conn==8
        BW = BW & ImageCenter>Image(1:end-2,1:end-2) & ImageCenter>Image(3:end,1:end-2) &...
         ImageCenter>Image(1:end-2,3:end) & ImageCenter>Image(3:end,3:end);
    end
    
    BW = padarray(BW,[1 1],false);
 
    ListInd   = find(BW); 
    if nargout>1
        [II, JJ] = imUtil.image.ind2sub_fast(size(BW),ListInd);
    end
    
    end