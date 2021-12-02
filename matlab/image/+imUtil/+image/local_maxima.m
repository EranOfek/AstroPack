function [BW,Pos,MaxIsn]=local_maxima(Image, Variance, Threshold, Conn, Algo)
% Identify local maxima above a threshold in a 2D image
% Package: @imUtil.image
% Description: This function identify local maxima, above some threshold,
%              in an image, or alternatively, in a max over cube of images.
%              If the first input is an image and given a threshold image
%              (e.g., background std image multiply by number of sigmas
%              for detection), identify local maxima above the threshold.
%              Return a matrix of logical indicating if a pixel is a local
%              maxima, and a list of [X,Y] coordinates of the local maxima
%              in the image.
%              Alternatively, if the first input is a cube of images, in
%              which the third dimension is the image index, then start by
%              dividing the cube by the variance image (or cube), and than
%              take the max over the 3rd dimension.
%              The thresholding and local-maxima identification is done on
%              this max image.
% Input  : - Image, or a cube of image in which the third dimension is the
%            image index.
%          - Variance image (or cube).
%            For example, if the Imgae is a S/N image, then this parameter
%            should be 1.
%            Default is var(Image,0,[1 2]).
%          - Threshold in units of std (=sqrt(Variance)).
%            The function is searching only for local maxima above this
%            threshold.
%            Default is 5.
%          - The connectivity parameter used by imregionalmax for searching
%            local maxima.
%            Default is 8 (i.e., requires that all 8 surrounding pixels
%            will be below the local max).
% Output : - A matrix of logical indicating if a pixel in the input image
%            is a local maximum above the threshold.
%          - A four column matrix of [X,Y,SN,index].
%            Each row corresponds to one local maxima identified in the S/N
%            image. X and Y are the position of the local maxima, SN is its
%            S/N value, and index is the image-index that contains the
%            maximum in the cube input (1 if input is a matrix).
%          - If the first input argument is a cube in which the 3rd
%            dimension is the image index, then this is a matrix
%            indicating in each pixel which image index have the maximum
%            value (the thresholding and local maxima is done on the max of
%            the cube, so this enables to identify which plane in the cube
%            contributed to the maximum).
%            If the first input is a 2D matrix, then this paarameter is
%            empty.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Image = imUtil.kernel2.gauss(1.5,[1024 1024]).*30 + randn(1024,1024);
%          Template = imUtil.kernel2.gauss(1.5);
%          [SN,Flux]=imUtil.filter.filter2_sn(Image,[],[],Template);
%          [BW,Pos]=imUtil.image.local_maxima(SN,1,5);
%          % Example with a cube input (find the optimal focus)
%          Image = imUtil.kernel2.gauss(2.2,[1024 1024]).*300 + randn(1024,1024);
%          SN=imUtil.filter.filter2_snBank(Image,[],[],@imUtil.kernel2.gauss,(1:0.2:6).');
%          [BW,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,5);
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Image
    Variance               = [];
    Threshold              = 5;
    Conn                   = 8;
    Algo                   = 'imregionalmax'; %findlocalmax';  % 'imregionalmax' | 'findlocalmax'
end

if isempty(Variance)
    Variance = var(Image,0,[1 2]);
end

% if nargin<4
%     Conn = 8;
%     if nargin<3
%         Threshold = 5;
%         if nargin<2
%             Variance = var(Image,0,[1 2]);
%         end
%     end
% end

if Variance==1
    SN = Image;
else
    % in some cases user supplies Var=1 - faster...
    SN = Image./sqrt(Variance);
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
    




ThresholdedSN = SN;
ThresholdedSN(SN<Threshold) = 0;

if all(ThresholdedSN==0,'all') || all(isnan(ThresholdedSN),'all')
    % The entire ThresholdedSN image is zeros - there is no local max
    % however, in this case imregionalmax retrun that there is local max
    % everywhere...
    % overide this problem
    BW = false(size(ThresholdedSN));
else
    
    BW = imregionalmax(ThresholdedSN,Conn);
    %L = watershed(ThresholdedSN, Conn);
    %[cent, varargout]=fastPeakFind(ThresholdedSN,5,1);
    
end

if nargout>1
    IndLocalMax   = find(BW);  % must use find (can't use logical indexing)
    [Y,X] = imUtil.image.ind2sub_fast(size(Image),IndLocalMax);
    if isempty(MaxIsn)
        % input is an image - set cube image index to 1
        Pos   = [X,Y, SN(IndLocalMax), ones(size(X))];
    else
        % inpt is a cube
        Pos   = [X,Y, SN(IndLocalMax), MaxIsn(IndLocalMax)];
    end
    
end





% 
% 
% ThresholdedSN = SN;
% ThresholdedSN(SN<Threshold) = 0;
%             
% if all(ThresholdedSN==0,'all') || all(isnan(ThresholdedSN),'all')
%     % The entire ThresholdedSN image is zeros - there is no local max
%     % however, in this case imregionalmax retrun that there is local max
%     % everywhere...
%     % overide this problem
%     BW = false(size(SN));
%     IndLocalMax = [];
% else
%     
%     switch lower(Algo)
%         case 'imregionalmax'
%             
%             BW = imregionalmax(ThresholdedSN,Conn);
%             if nargout>1
%                 IndLocalMax   = find(BW);  % must use find (can't use logical indexing)
%             end
%         case 'findlocalmax'
%             BW = [];
%             [IndLocalMax]      = imUtil.sources.findLocalMax(SN, Threshold, Conn);
%         otherwise
%             error('Unknown Algo option');
%     end
% end
% 
% if nargout>1
%     %IndLocalMax   = find(BW);  % must use find (can't use logical indexing)
%     [Y,X] = imUtil.image.ind2sub_fast(size(Image),IndLocalMax);
%     if isempty(MaxIsn)
%         % input is an image - set cube image index to 1
%         Pos   = [X,Y, SN(IndLocalMax), ones(size(X))];
%     else
%         % inpt is a cube
%         Pos   = [X,Y, SN(IndLocalMax), MaxIsn(IndLocalMax)];
%     end
%     
% end
% 
% 
% 
