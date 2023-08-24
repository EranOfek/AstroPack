function image = createSegments(sizes,point1,point2,Args)
% Creates an image with N impainted streaks
%
% Input:
%    sizes (1,2) dimension of the resulting image matrix
%    point1 (N,2)  x and y of the start point of each of the N segments
%    point2 (N,2)  x and y of the end point of each of the N segments
%                  (extremes outside the grid are supported)
%    key-value argument pairs:
%      - 'shape'  'gaussian' (default) or 'flat'
%      - 'width'   width of the flat streak, sigma for gaussian streak
%                  (default 1)
% Output:
%    a matrix of doubles of dimension sizes
%
%    Note on normalization: 'flat' streaks result in a value 1 at all
%      impainted pixels. 'gaussian' profiles are unnormalized.
%      Multiplication by an amplitude factor is left to the user (one would
%      then argue about the pixel value representing the pointwise value
%      rather than the flux integral over the pixel area)
%
% algorithm taken from
% https://math.stackexchange.com/questions/330269/the-distance-from-a-point-to-a-line-segment
%
% Example:
%   image=imUtil.art.createSegments([250,400],[322,233;98,0],[54,11;145,211],'width',5);
%   imagesc(image); axis xy
    arguments
        sizes(1,2);
        point1(:,2);
        point2(:,2);
        Args.width=1;
        Args.shape='gaussian';
    end

    image=zeros(sizes);

    for i=1:size(point1,1)
        x1=point1(i,1);
        x2=point2(i,1);
        y1=point1(i,2);
        y2=point2(i,2);

        [px,py]=meshgrid(1:sizes(2),1:sizes(1));

        t=((px-x1)*(x2-x1)+(py-y1)*(y2-y1))/((x2-x1)^2+(y2-y1)^2);
        t=min(max(t,0),1);

        sx=x1+t*(x2-x1);
        sy=y1+t*(y2-y1);

        d2=(sx-px).^2+(sy-py).^2;

        switch Args.shape
            case 'gaussian'
                image = image + exp(-d2/Args.width^2);
            otherwise
                image = image + (d2<(Args.width/2)^2);
        end
    end