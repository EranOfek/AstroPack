function [phot,extsegs,curve,stripeindices]=...
            streak_photometry(im,segs,offlength,offside,Args)
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
% Key-Value pairs
%    'sigmaclip': control parameter to discard outlier intensities, whose meaning
%               depends on the method used:
%               -for 'clipping'='sigma'
%                   upper threshold of pixel intensity to consider for
%                   aperture photometry and curve fit, in units of std of the
%                   intensity over the search strip. Used to ignore bright 
%                   sources within the strip. Default 5.
%               -for 'clipping'='quantile'
%                   pixels whose intensity is above the given quantile are
%                   discarded. Default 0.98, must be between 0 and 1.
%               -for 'clipping'='gaussianfit'
%                   the intensity values of individual slices of the search
%                   strip are fitted to a gaussian function. The whole
%                   slice is discarded if R^2 of the fit is < sigmaclip
%                   (default 0.7, must be <1)
%    'clipping': the source clipping method used: 'sigma','quantile' or
%              'gaussianfit' (default, slower)
%    'slice_width': width of the anlalysis slice in pixels. default 10.
%  Outputs:
%    - phot: esimated intensity/unit length of each streak
%    - extseg: [x1e; y1e; x2e; y2e]
%    - curve: coefficients {a,b,c} of the parabolic fits
%             to the offset w.r.o the base segs, h(t) = a+b*t+c*t^2
%             where {x,y} = {x1e, y1e} for t=0 and {x,y} = {x2e, y2e} for t=1
%    - stripeindices: if requested, a cell with the list of pixels used to
%                     analize each stripe (discarding intensity outliers)
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
        Args.sigmaclip=[]; % defaults set below according to method
        Args.clipping {mustBeMember(Args.clipping, {'sigma','quantile','gaussianfit'})} ...
             = 'gaussianfit';
        Args.slice_width=10; % pixel units
    end
    
    if isempty(Args.sigmaclip)
        switch Args.clipping
            case 'gaussianfit'
                Args.sigmaclip=0.7;
            case 'sigma'
                Args.sigmaclip=5;
            case 'quantile'
                Args.sigmaclip=0.98;
        end
    end
    nsegs=size(segs,2);
    phot=nan(1,nsegs);
    extsegs=nan(size(segs));
    curve=struct('parfit',nan(3,1),'coord',zeros(2,0),'linephot',[]);
    if nargout==4
        stripeindices=cell(1,nsegs);
    end
    
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
        mpp=mean(pp,'omitnan');
        spp=std(pp,'omitnan');

        % contaminators clipping methods
        switch Args.clipping
            case 'gaussianfit'
                % exploring slice fits
                [C,goodindices,tm] = sliceGaussianProfile([x1,y1],[x2,y2],...
                                        px(mask),py(mask),pp,...
                                        'slice_width',Args.slice_width,...
                                        'rthreshold',Args.sigmaclip);
                curve(i).linephot=C(1,:).*C(3,:)*sqrt(2*pi);
                scpp=pp(goodindices);
                smask= false(size(mask));
                mindexes=find(mask);
                smask(mindexes(goodindices)) = true;
            case 'sigma'
                % sigma clipped sum (clip only brighter, not darker)
                smask=mask & im<mpp+Args.sigmaclip*spp;
                scpp=im(smask);
            case 'quantile'
                % quantile clip
                smask=mask & im<quantile(pp,Args.sigmaclip);
                scpp=im(smask);
        end

        if nargout==4
            stripeindices{i}=find(smask);
        end

        phot(i)= numel(pp)* mean(scpp,'omitnan')/Lext;
        % why not this (which as of now gives results farther from
        %  implanted)?
        %phot(i)=median(scpp)*numel(pp)/Lext;

        % fit a parabola
        curve(i).parfit = weightedParabolicOffset([x1,y1],[x2,y2],...
                                              px(smask),py(smask),scpp);
        % offsets at extremes: [curve(i).parfit(3), sum(curve(i).parfit)]
        % max offset:
        %  -curve(i).parfit(2)^2/(4*curve(i).parfit(1)) + curve(i).parfit(3)

        % sliced photometry for simpler masking methods
        switch Args.clipping
            case 'gaussianfit'
            otherwise
                num_slices=ceil(Lext/Args.slice_width);
                tm=(1/2:1:num_slices)/num_slices; % note that t may be extended
                for k=1:num_slices
                    q = t>(k-1)/num_slices & t<=k/num_slices;
                    pp=im(mask & q);
                    scpp=im(smask & q);
                    if ~isempty(scpp)
                        curve(i).linephot(k)=...
                            numel(pp)* mean(scpp,'omitnan')/Args.slice_width;
                    else
                        curve(i).linephot(k)=NaN;
                    end
                end
        end
                
        [X,Y]=segmentParabolicOffset([x1,y1],[x2,y2],curve(i).parfit,tm);
        curve(i).coord=[X',Y'];

        % second pass photometry: only consider the pixels traversed by
        %  the fitting parabola. Rationale, we are working with images
        %  which are already PSF-filtered, including sidewings will bias
        %  the estimate toward 0.
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

%%
function [C,goodindices,tm] = sliceGaussianProfile(X1,X2,x,y,W,Args)
% divide the rasterized strip in slices, and fit gaussians to the intensity
%  values W in each slice
% Input:
%  X1: [x1,y1]; X2: [x2,y2] of the base segment
%  x,y,W: Nx1 vectors
%  x,y: coordinates in pixels of the pixels belonging to the streak strip
%  W:   intensity of the pixels
%
% Name-value pairs:
%  'slice_width': length in pixel of each sectionof the strip to be analysed
%               separately (default 10px)
%  'rthreshold': minimal value of R^2 for accepting a fit. Usually sections
%              of streaks contaminated by a neighboring source lead to
%              poorer transverse fits than the rest. Default 0.7
%  'medianclip': discard slices whose fit have A and sigma larger than
%                medianclip times the respective median. Default 2
%  'testplot':  plot somethibg for debugging, default false
%
% Output:
%   C:           4xM for each slice, (A,sigma,mu_h,r). M is L/Args.slice_width.
% goodindices: logical vector 1xN, true for indices of elements of W which
%                lead to an acceptable fit (R-square>Args.rthreshold)
%   tm:  vector of values of the intrinsic segment coordinate, at the
%        mid of each slice. To associate the photometry of each slice with
%        pixel coordinates, via 
%          [X,Y]=segmentParabolicOffset([x1,y1],[x2,y2],curve(i).parfit,tm)

    arguments
        X1
        X2
        x
        y
        W double
        Args.slice_width=10; % pixel units
        Args.rthreshold=0.7;
        Args.medianclip=2;
        Args.testplot=false;
    end

    % transform to intrinsic coordinates (t(X1)=0, t(X2)=1)
    L=sqrt((X2-X1)*(X2-X1)');
    T=((X2(1)-X1(1))*(x-X1(1)) + (X2(2)-X1(2))*(y-X1(2)))/L^2;
    D=((X2(1)-X1(1))*(y-X1(2)) - (X2(2)-X1(2))*(x-X1(1)))/L;

    num_slices=ceil(L/Args.slice_width);
    C=NaN(4,num_slices);
    goodindices=false(size(W));

    % for fit
    %opt=fitoptions('gauss1','Lower',[0 -Args.slice_width, 0],...
    %    'Upper',[Inf Args.slice_width Args.slice_width]);
 
    % for lsqcurvefit
    % with constant background
    %gaussianModel = @(params, D) params(1) * exp(-((D - params(2)).^2) / (2 * params(3)^2)) + params(4);
    % with no background level
    gaussianModel = @(params, D) params(1) * exp(-((D - params(2)).^2) / (2 * params(3)^2));
    opt = optimoptions('lsqcurvefit', 'Display', 'off');

    for i=1:num_slices
        q = T>(i-1)/num_slices & T<=i/num_slices;
        Dq=D(q);
        Wq=W(q);
        try
            % fit() has a simpler call but is slower
            %         [result,gof]=fit(D(q),W(q),'gauss1',opt);
            %         C(1:3,i)=[result.a1; result.b1; result.c1];
            %         C(4,i)=gof.rsquare;
            % initialParams = [max(W(q)), mean(D(q)), std(D(q)), min(W(q))];
            initialParams = [max(Wq), mean(Dq), std(Dq)];
            fitParams = lsqcurvefit(gaussianModel, initialParams, Dq, Wq,...
                [0 -Args.slice_width, 0], [Inf Args.slice_width Inf], opt);
%               [0 -Args.slice_width, 0 -Inf], [Inf Args.slice_width Args.slice_width Inf], opt);
            C(1:3,i)=fitParams(1:3)';
            % Compute R-squared
            W_fit = gaussianModel(fitParams, D(q));
            SS_res = sum((Wq - W_fit).^2);       % Residual sum of squares
            SS_tot = sum((Wq - mean(Wq)).^2);  % Total sum of squares
            C(4,i) = 1 - (SS_res / SS_tot);        % R-squared value
            if C(4,i)>Args.rthreshold
                goodindices(q)=true;
            end
        catch
            fprintf('no fit for slice %d\n',i)
        end
    end

    % median amplitude
    Amedian=median(C(1,:),'omitnan');
    Smedian=median(C(3,:),'omitnan');

    % repeat loop to enforce medianclip on pixel indices
    for i=1:num_slices
        q = T>(i-1)/num_slices & T<=i/num_slices;
        if C(1,i)>Args.medianclip*Amedian || C(3,i)>Args.medianclip*Smedian
            C(:,i)=NaN;
            goodindices(q)=false;
        end
    end

    tm=(1/2:1:num_slices)/num_slices;

    if Args.testplot
        clf
        scatter(T,D,[],W,'filled')
        hold on
        H=C(2,:)';
        H(C(4,:)<Args.rthreshold)=NaN;
        plot((0.5:1:num_slices)/num_slices, H, '-k','LineWidth',2)
        hold off
    end
end
