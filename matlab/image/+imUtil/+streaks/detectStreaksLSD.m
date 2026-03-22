function [segs,phot,parfit]=detectStreaksLSD(Im,filtIm,Args)
% detect streaks using a modified version of the LSD algorithm, detecting
%  ridges in the Hessian of the filtered image
%
% Input  : - (Im) A single precision (2D) background subtracted image.
%          - (filtIm) The same image, cross-correlated (filtered) with its PSF
%            and normalized by the std (i.e., units of S/N).
%
% Optional named arguments:
%     Subsample - empirical best subsample factor. Default 1/3. Probably a
%                 good guess is ~1/FWHM of the image.
%     AngTol    - empirical colinearity angular tolerance, default 5*pi/180 = 5°
%     EdgeGuard - empirical edge guard, default 16 (pixel). Result segments
%                 with both extremes closer than that to an edge are just
%                 discarderd, they are artefacts of LSD. Probably a better
%                 default should be proportional to max(size(Im))/scale.
%     StripHalfWidth - empirical half width of the transverse streak aperture
%                      window. Default 6 pixel, increase for long streaks
%                      with pronounced concavity
%
% Outputs:
%    segs   - 4xN array of detected segment coordinates (x1,y1,x2,y2)
%    phot   - 1xN array of corresponding estimated streak strengths (per
%                 unit length)
%    parfit - 3xN array of the parameters of the model h(t) = at^2 + bt + c
%                 describing the transverse offset from the base detected
%                 segment, as fitted from the pixel intensity data
%
%   Example:
%{
     AI=AstroImage;
     AI.Image=single(zeros(1726,1726));
     AI.ImageData.Image = imUtil.streaks.addLineToImage(AI.ImageData.Image,...
        [1345,678,998,109; 1845,478,1098,509],[30 20], 3 ,[-3,1]);
     AI.PSF=imUtil.kernel2.gauss;
     im=AI.Image;
     AI=imProc.image.xcorrWithPSF(AI);
     [segs,phot,parfit]=imUtil.streaks.detectStreaksLSD(im,AI.Image)
%}
%
%   Authors: Enrico Segre, February 2026
%            LSD: Grompone von Gioi, Jakubowicz, Morel and Randall,
%                 10.5201/ipol.2012.gjmr-lsd
    arguments
        Im single
        filtIm
        Args.Subsample = 1/3;  % 1/3 - empirical best subsample factor
        Args.AngTol = 5*pi/180;  % 5° - empirical colinearity tolerance
        Args.EdgeGuard = 16; % 16px - empirical edge guard
        Args.StripHalfWidth = 6; % 6px - empirical half width of the transverse streak aperure window
    end

   
    segs=imUtil.streaks.mex.lsd_single_scale_mex(Im,Args.Subsample);
    
    segs=merge_segments(segs,[],Args.AngTol);
    
    segs=purge_edge_segments(segs,size(Im),Args.EdgeGuard);

    % photometry on the original image
    % 1px - empirical longitudinal extension of the streak region (fixed, yet)
    [phot,~,parfit]=imUtil.streaks.streak_photometry(filtIm,segs,1,Args.StripHalfWidth);
    
end


%% internal ancillary functions

function merged=merge_segments(segs,offline,angle,score)
    % given a matrix of 7xN segments returned by LSD, try to merge greedily
    %  all those which are nearly colinear. The result is dependent on the
    %  order of thesegments returned by LSD.
    % LSD at high image resolution (no subsampling) is known to return many
    %  short segments instead of long lines; moreover, in our typical
    %  astronomical images we can safely assume that streaks are long and not
    %  intermittent
    % LSD is essentially local in identifying segment candidates, and weak
    %  streaks are also intermittently detected because of noise
    %
    % Inputs: segs     - only segs([1:4,7},:) are used, i.e. x1,y1,x2,y2
    %         offline  - offset threshold of a segment point from the searched
    %                    line. If the start point of the second segment is less
    %                    than that, the segment is merged in
    %         angle    - angular tolerance to group together segments
    %         score    - threshold on -log(NFA) (segs(7,:)). Input segments
    %                    below that score are neglected
    % Output: merged   - an array of 4xN segment coordinates (no attempt is
    %                    made to return a merged value of the corresponding
    %                    values of segs(5:7,:) )
    arguments
        segs double
        offline = 2;
        angle  = 2*pi/180;
        score = -Inf;
    end

    sintol=sin(angle);

    % remove segments with low score
    segs=segs(:,segs(7,:)>score);

    % initialize empty merged
    merged=zeros(4,0);

    % matched segments are removed from segs till none is left
    while ~isempty(segs)

        % pick up the first orphan
        testsegment=segs(1:4,1);

        % find all other segs which are colinear (that will include testsegment
        %  itself):
        % - check that both extremes of the new segment are within
        %  tolerance on the line of the tested segment,
        % - check that the segments are nearly parallel
        % the check could be made slightly more efficient, computing L3 and s2
        %  only for the segments passing the check on s1
        L1=sqrt((segs(3,:)-segs(1,:)).^2 + (segs(4,:)-segs(2,:)).^2);
        L2=sqrt((segs(1,:)-testsegment(1)).^2 + (segs(2,:)-testsegment(2)).^2);
        % L2(1) is always 0
        L3=sqrt((segs(3,:)-testsegment(1)).^2 + (segs(4,:)-testsegment(2)).^2);
        s1=( (testsegment(3)-testsegment(1))*(testsegment(2)-segs(2,:)) - ...
            (testsegment(4)-testsegment(2))*(testsegment(1)-segs(1,:)) )./(L1(1)*L2);
        s1(1)=0;
        s2=( (testsegment(3)-testsegment(1))*(testsegment(2)-segs(4,:)) - ...
            (testsegment(4)-testsegment(2))*(testsegment(1)-segs(3,:)) )./(L1(1)*L3);
        s3=( (testsegment(3)-testsegment(1))*(segs(4,:)-segs(2,:)) - ...
            (testsegment(4)-testsegment(2))*(segs(3,:)-segs(1,:)) )./(L1(1)*L1);
        candidates=find(abs(s1)<sintol & abs(s2)<sintol & abs(s3)<sintol);

        % project the extremes of each candidate segment on testsegment
        p1 = segs(1,candidates)*(testsegment(3)-testsegment(1)) + ...
            segs(2,candidates)*(testsegment(4)-testsegment(2));
        p2 = segs(3,candidates)*(testsegment(3)-testsegment(1)) + ...
            segs(4,candidates)*(testsegment(4)-testsegment(2));

        % reorient candidate segments so that p1<p2 always
        toreverse=(p1>p2);
        temp=segs(1,candidates(toreverse));
        segs(1,candidates(toreverse))=segs(3,candidates(toreverse));
        segs(3,candidates(toreverse))=temp;
        temp=segs(2,candidates(toreverse));
        segs(2,candidates(toreverse))=segs(4,candidates(toreverse));
        segs(4,candidates(toreverse))=temp;

        % take the two extremal points as extremes of the merged segment
        % This implies overly relying on their plausibility. Maybe some kind of
        %  fit which takes all the intermediate points into account would be
        %  better?
        [~,i1]=min(p1);
        [~,i2]=max(p2);
        X1=segs(1:2,candidates(i1));
        X2=segs(3:4,candidates(i2));

        merged=[merged,[X1;X2]];

        % remove from segs all the candidates used
        segs(:,candidates)=[];
    end

end

%%
function purged=purge_edge_segments(segs,imsize,edge)
% remove all segments which are too close to the image boundaries (within
%  edge pixels), which are likely artefacts of LSD
    if ~isempty(segs)
        q= (segs(1,:)<edge & segs(3,:)<edge) | ...
            (segs(2,:)<edge & segs(4,:)<edge) | ...
            (segs(1,:)> imsize(1)-edge & segs(3,:)> imsize(1)-edge) | ...
            (segs(2,:)> imsize(2)-edge & segs(4,:)> imsize(2)-edge);
        purged=segs(:,~q);
    else
        purged=segs;
    end
end

