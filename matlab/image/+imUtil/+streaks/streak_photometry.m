function [phot,extsegs,curve]=streak_photometry(im,segs,offlength,offside,sigmaclip)
% given an image and a set of candidate streak segments, select for each one
% all image pixels lying on a stripe offside broad and extended offlength beyond seg
% extremes. With the values of those pixels, compute:
%  - the presumed extended extremes of the line. The streak is extended
%     longitudinally as long as enough (define enough) pixels above a
%     brightness threshold (define threshold) are found. Not yet
%     implemented.
%  - the estimate of the photometric intensity of the streak per length
%     unit
%  - the least squares best fitting parabola
%
% Inputs:
%    im: a [background subtracted, std divided, PSF filtered] image
%    segs: [x1; y1; x2; y2] segments (pixel coordinates)
%    offlength: number of pixels to extend the search, along the direction
%               of seg. Default 1.
%    offside: number of the lateral pixels of the search region. Default 3.
%    sigmaclip: upper thresold of pixel intensity to consider for
%               aperture photometry and curve fit, in units of std of the
%               intensity over the search strip. Used to ignore bright 
%               sources within the strip. Default 5.
%  Outputs:
%    - phot: esimated intensity/unit length of each streak
%    - extseg: [x1e; y1e; x2e; y2e]
%    - curve: coefficients {a,b,c} of the parabolic fits
%             to the offset w.r.o the base segs, h(t) = a+b*t+c*t^2
%             where {x,y} = {x1e, y1e} for t=0 and {x,y} = {x2e, y2e} for t=1
%
% Author: Enrico Segre, Jan 2026
% Taking from what I have previously done in imUtil.art.createSegments,
%  i.e. inspired from 
%  https://math.stackexchange.com/questions/330269/the-distance-from-a-point-to-a-line-segment

    arguments
        im
        segs
        offlength=3;
        offside=3;
        sigmaclip=5; % might be increased if offside is large or image is very clean
    end
    
    nsegs=size(segs,2);
    phot=nan(1,nsegs);
    extsegs=nan(size(segs));
    curve=nan(3,nsegs);
    
    for i=1:nsegs
        x1=segs(1,i);
        x2=segs(3,i);
        y1=segs(2,i);
        y2=segs(4,i);
        L=sqrt((x2-x1)^2 + (y2-y1)^2);

        [py,px]=meshgrid(1:size(im,2),1:size(im,1));

        t=((px-x1)*(x2-x1)+(py-y1)*(y2-y1))/((x2-x1)^2+(y2-y1)^2);
        dt=offlength/L;
        t=min(max(t,-dt),1+dt);

        % for the time being, just grow them offlength
        extsegs(1,i)=x1-dt*(x2-x1);
        extsegs(2,i)=y1-dt*(y2-y1);
        extsegs(3,i)=x1+(1+dt)*(x2-x1);
        extsegs(4,i)=y1+(1+dt)*(y2-y1);
        Lext=sqrt((extsegs(3,i)-extsegs(1,i))^2 + (extsegs(4,i)-extsegs(2,i))^2);

        sx=x1+t*(x2-x1);
        sy=y1+t*(y2-y1);

        d2=(sx-px).^2+(sy-py).^2;

        mask = (d2<offside^2);
        
        pp=im(mask);
        mpp=nanmean(pp);
        spp=nanstd(pp);
        % sigma clipped sum (clip only brighter, not darker)
        smask=mask & im<mpp+sigmaclip*spp;
        scpp=im(smask);
        phot(i)=sum(scpp)/Lext *numel(pp)/numel(scpp);
        % why not this (which as of now gives results farther from
        %  implanted)?
        %phot(i)=median(scpp)*numel(pp)/Lext;

        % fit a parabola
        curve(:,i) = weightedParabolicOffset([x1,y1],[x2,y2],px(smask),py(smask),scpp);
        % offset at extremes: [curve(3,i), sum(curve(:,i))]
        % max offset: -curve(2,i)^2/(4*curve(1,i)) + curve(3,i)

        % second pass photometry: only consider the pixels traversed by
        %  the fitting parabola. Rationale, we are working with images
        %  which are already PSF-filtered, including sidewings will bias
        %  the estimate toward 0.
    end
end
    
%%
function C = weightedParabolicOffset(X1,X2,x,y,W,testplot)
% X1: [x1,y1]; X2: [x2,y2]
% X,Y,S: Nx1 vectors
%
% Example
%{
    X1=[0,4]; X2=[3,-2]; N=20;
    T=sort(rand(N,1));
    H=2*T.*(1-T);
    X= X1(1)+(X2(1)-X1(1))*T - H*(X2(2)-X1(2)) +...
        + [5*randn(N/2,1); 0.1*randn(N/2,1)];
    Y= X1(2)+(X2(2)-X1(2))*T + 0.1*randn(size(T)) + H*(X2(1)-X1(1));
    W=[0.1*ones(N/2,1); 2*ones(N/2,1)];
    weigthedParabolicOffset(X1,X2,X,Y,W,true)
%}

    arguments
        X1
        X2
        x
        y
        W=1;
        testplot=false;
    end
    N=numel(x);
    if numel(W)==1
        W=W*ones(N,1);
    end
    % transform to intrinsic coordinates (t(X1)=0, t(X2)=1)
    L=sqrt((X2-X1)*(X2-X1)');
    T=((X2(1)-X1(1))*(x-X1(1)) + (X2(2)-X1(2))*(y-X1(2)))/L^2;
    D=((X2(1)-X1(1))*(y-X1(2)) - (X2(2)-X1(2))*(x-X1(1)))/L;

    A = [T.^2 T ones(N,1)];
    AW = (W.*A)';
    C = (AW*A)\(AW*D);

    if testplot
        [X,Y]=segmentParabolicOffset(X1,X2,C,linspace(0,1,100));
        clf
        plot([X1(1),X2(1)],[X1(2),X2(2)],'--', x,y,'o', X,Y,'-')
        hold off
    end
end

%%
function [X,Y]=segmentParabolicOffset(X1,X2,C,t)
    % generates X,Y points of a parabola offset from a segments, with extremes
    %  X1 and X2
    % The equation of the parabola is
    %   h(t) = C(1)*t^2 + C(2)*t + C(3)
    % where h(t) is the orthogonal distance from the segment. t is the
    %  intrinsic coordinate, so that h(0) is the distance from X1 and h(1)
    %  that from X2. If omitted, t=linspace(0,1,100)
    arguments
        X1 (1,2) double
        X2 (1,2) double
        C (3,1) double
        t (1,:) double = linspace(0,1,100);
    end

    L=sqrt((X2-X1)*(X2-X1)');
    h= C(1)*t.^2 + C(2)*t + C(3);
    X = X1(1) + (X2(1)-X1(1))*t - (X2(2)-X1(2))*h/L;
    Y = X1(2) + (X2(2)-X1(2))*t + (X2(1)-X1(1))*h/L;
end


    