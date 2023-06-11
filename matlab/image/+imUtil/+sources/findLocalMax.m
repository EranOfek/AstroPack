function [Pos, BW, MaxIsn] = findLocalMax(Image, Args)
    % Find local maxima in a set of 2D images using various algorithms.
    %       Given a cube if images (image index is in the 3rd dim),
    %       normalize the image by its variance and threshold it.
    %       Calc the max over all images, and search for local maxima using
    %       findLocalMaxAboveThreshold or imregionalmax.
    % Input  : - Image. Either a 2D image or a cube in which the image
    %            index is in the 3rd dimension.
    %          * ...,key,val,...
    %            'Variance' - Variance image (or cube).
    %                   For example, if the Imgae is a S/N image, then this parameter
    %                   should be 1.
    %                   Default is var(Image,0,[1 2]).
    %           'Threshold' - Threshold in units of std (=sqrt(Variance)).
    %                   The function is searching only for local maxima above this
    %                   threshold.
    %                   Default is 5.
    %           'Conn' - The connectivity parameter used by imregionalmax for searching
    %                   local maxima.
    %                   Default is 8 (i.e., requires that all 8 surrounding pixels
    %                   will be below the local max).
    %           'Algo' - Either:
    %                   'findlocal' - use findLocalMaxAboveThreshold.
    %                   'findlocalmex' - use
    %                           findLocalMaxAboveThreshold_mex_single/double.
    %                   'imregionalmax' - use imregionalmax.
    %                   Default is 'findlocalmex'.
    % Output : - A five column matrix of [X,Y,SN,ImageIndex,LinaerIndexIn2D].
    %          - A matrix of logical indicating if a pixel in the input image
    %            is a local maximum above the threshold.
    %          - If the first input argument is a cube in which the 3rd
    %            dimension is the image index, then this is a matrix
    %            indicating in each pixel which image index have the maximum
    %            value (the thresholding and local maxima is done on the max of
    %            the cube, so this enables to identify which plane in the cube
    %            contributed to the maximum).
    %            If the first input is a 2D matrix, then this paarameter is
    %            empty.
    % Author : Eran Ofek (Nov 2021)
    % Example: Image = randn(1600,1600,5);
    %          [Pos, BW, MaxIsn] = imUtil.sources.findLocalMax(Image,'Variance',1,'Threshold',3);
    
    
    arguments
        Image
        Args.Variance         = [];   % if empty, calc from image
        Args.Threshold        = 5;
        Args.Conn             = 8;
        Args.Algo             = 'findlocalmex';  % 'findlocal' | 'findlocalmex' | 'imregionalmax'
    end
    
    
    BW = [];
    if isempty(Args.Variance)
        Args.Variance = var(Image,0,[1 2]);
    end
    
    if Args.Variance==1
        SN = Image;
    else
        % in some cases user supplies Var=1 - faster...
        SN = Image./sqrt(Args.Variance);
    end
    
    % check if SN is a cube
    % If this is the case, then take max over third dimension
    % (image-index)
    if ~ismatrix(SN)
        % assume SN is a cube in which the 3rd dim is the image index
        [SN,MaxIsn] = max(SN,[],3);
    else
       MaxIsn = [];
    end
    
    switch lower(Args.Algo)
        case 'findlocal'
            % use the fast findLocalMaxAboveThreshold algorithm
            if nargout>1
                [IndLocalMax, Y, X, BW] = imUtil.sources.findLocalMaxAboveThreshold(SN, Args.Threshold, Args.Conn);
            else
                [IndLocalMax, Y, X] = imUtil.sources.findLocalMaxAboveThreshold(SN, Args.Threshold, Args.Conn);
            end            
            
        case 'findlocalmex'
            if isa(SN,'double')
                if nargout>1
                    [IndLocalMax, Y, X, BW] = imUtil.sources.mex.mex_findLocalMaxAboveThreshold_double(SN, Args.Threshold, Args.Conn);
                else
                    [IndLocalMax, Y, X] = imUtil.sources.mex.mex_findLocalMaxAboveThreshold_double(SN, Args.Threshold, Args.Conn);
                end  
                X = double(X);
                Y = double(Y);
                IndLocalMax = double(IndLocalMax);
            elseif isa(SN,'single')
                if nargout>1
                    [IndLocalMax, Y, X, BW] = imUtil.sources.mex.mex_findLocalMaxAboveThreshold_single(SN, Args.Threshold, Args.Conn);
                else
                    [IndLocalMax, Y, X] = imUtil.sources.mex.mex_findLocalMaxAboveThreshold_single(SN, Args.Threshold, Args.Conn);
                end  
                X = double(X);
                Y = double(Y);
                IndLocalMax = double(IndLocalMax);
            else
                error('findlocalmex supports only double or single input');
            end
            
        case 'imregionalmax'
            [IndLocalMax, Y, X, BW]=imUtil.sources.findLocalMaxImregionalmax(SN, Args.Threshold, Args.Conn);
            
        otherwise
            error('Unknown Algo option');
    end
    
    if isempty(MaxIsn)
        % input is an image - set cube image index to 1
        Pos   = [X,Y, SN(IndLocalMax), ones(size(X)), IndLocalMax];
    else
        % inpt is a cube
        Pos   = [X,Y, SN(IndLocalMax), MaxIsn(IndLocalMax), IndLocalMax];
    end
    
end
    