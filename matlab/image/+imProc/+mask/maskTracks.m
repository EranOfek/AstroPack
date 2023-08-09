function [bwmask,lines]=maskTracks(AstroImg,Args)
% Detect satellite/airplane tracks as lines in the image with the Matlab
%  stock Hough transform, and update the image mask
%
% Inputs:
%  - AstroImg: an AstroImage, or an array of AstroImages, with .Back and 
%              .Var already populated
%  - Key,Val arguments:
%        VarLevel:     threshold level for suspected bad line [default 2.5]
%        MinLineLength: minimal length of a suspicious bad column -
%                      typically several times larger than the width of the
%                      PSF, and of the width of possible extended objects
%                      [default, max(size(AstroImg.Image))/20 pixels]
%        HighFraction: fraction of the segment pixels over or below VarLevel,
%                       for a segment to be considered bad [default 0.8]
%        ThetaResolution: [default 0.25 degrees]
%        RhoResolution: [default 0.5] (should be commensurate to the width of
%                       the PSF)
%        FillGap:      Join collinear segments separated no more than this
%                      distance[default max(size(AstroImg.Image))/20 pixels]
%        MaxLines:  maximal number of Hough lines to consider [default 5]
%
% Outputs:
%     AstroImg.Mask is updated for each image of the input array
%     - the last of the masks is also returned as optional output, for
%       debugging
%
% This function may give as false positive: bad lines/columns, blooming
%  stars. Since there may be many the like in bad images, whereas 
%  statistically we seldom see more than a few satellite tracks per image,
%  best results are obtained if these known bad features are first impainted
%  with the background values, or if a high 'MaxStreaks' is used in order
%  to find also the relevant tracks besides the many bad lines.
%
% Author: Enrico Segre, August 2023
    arguments
        AstroImg AstroImage = [];
        Args.VarLevel = 2.5;
        Args.MinLineLength = [];
        Args.HighFraction = 0.8;
        Args.ThetaResolution = 0.25;
        Args.RhoResolution = 0.5;
        Args.FillGap = [];
        Args.MaxLines = 5;
    end

    function bwmask=pixelline(orig_image,point1,point2)
        % ancillary to draw a pixellated line from point1 on point2,
        % and to add it to the input binary matrix
        [sx,sy]=size(orig_image);
        %  first clip the coordinates to size(orig_image)+1 to avoid unbounded
        %   vectors for wrong points
        x1=max(min(point1(1),sx+1),0);
        x2=max(min(point2(1),sx+1),0);
        y1=max(min(point1(2),sy+1),0);
        y2=max(min(point2(2),sy+1),0);
        % choose increment direction
        if abs(x2-x1)>abs(y2-y1)
            if x1<x2
                x=x1:x2;
            else
                x=x2:x1;
            end
            y=y1+round((x-x1)*(y2-y1)/(x2-x1));
        else
            if y1<y2
                y=y1:y2;
            else
                y=y2:y1;
            end
            x=x1+round((y-y1)*(x2-x1)/(y2-y1));
        end

        % clip the pixel indices to what falls really on the image
        q= x>=1 & x<=sx & y>=1 & y<=sy;
        x=x(q);
        y=y(q);

        % set the line pixels
        np=numel(x);
        bwmask=orig_image;
        for i=1:np
            bwmask(y(i),x(i))=true;
        end
    end

    % for each AstroImage, find the streaks with the Hough transform
    for k=1:numel(AstroImg)
        % give default, image-size dependent values to all arguments whih
        %  are undefined
        if isempty(Args.MinLineLength)
            MinLength=min(size(AstroImg(k).Image))/20;
        else
            MinLength=Args.MinLineLength;
        end
        if isempty(Args.FillGap)
            FillGap=min(size(AstroImg(k).Image))/20;
        else
            FillGap=Args.FillGap;
        end
        HighPix = (AstroImg(k).Image - AstroImg(k).Back) > ...
                   Args.VarLevel*sqrt(AstroImg(k).Var);
        [H,T,R] = hough(HighPix,...
            'Theta',-90:Args.ThetaResolution:90-Args.ThetaResolution,...
            'RhoResolution',Args.RhoResolution);
        % HighPix is binary, therefore the value of H represents the number
        %  of active pixels alog the given cutline
        % Search only up to MaxLines peaks
        P  = houghpeaks(H,Args.MaxLines,'theta',T,...
                        'threshold',Args.HighFraction*MinLength);
        % the Hough transform only finds unbounded lines. To mask
        %  specifically only segments corresponding to high pixels along
        %  the found direction, we use houghlines
        lines = houghlines(HighPix,T,R,P,'FillGap',FillGap,'MinLength',MinLength);
        %L1=vertcat(lines.point1);
        %L2=vertcat(lines.point2);
        % plot([L1(:,1),L2(:,1)]',[L1(:,2),L2(:,2)]')

        % mask: draw all the pixellated lines and then dilate them
        % approximately as much as the PSF is wide
        bwmask=false(size(HighPix));
        for j=1:numel(lines)
            bwmask=pixelline(bwmask,lines(j).point1,lines(j).point2);
        end
        [wx,wy]=imUtil.psf.pseudoFWHM(AstroImg(k).PSF);
        SE=strel("rectangle",[wx,wy]);
        bwmask=imdilate(bwmask,SE);
        % imagesc(bwmask); axis xy

        % set this into the AstroImage mask
        AstroImg(k).maskSet(bwmask,'Streak');
    end
end