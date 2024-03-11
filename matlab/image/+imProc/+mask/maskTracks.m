function [bwmask,lines]=maskTracks(AstroImg,Args)
    % Detect satellite/airplane tracks as lines in the image with the Matlab
    %   stock Hough transform, and update the image mask.
    %   This function may give as false positive: bad lines/columns, blooming
    %   stars. Since there may be many the like in bad images, whereas 
    %   statistically we seldom see more than a few satellite tracks per image,
    %   best results are obtained if these known bad features are first impainted
    %   with the background values, or if a high 'MaxStreaks' is used in order
    %   to find also the relevant tracks besides the many bad lines.
    % Input  : - (AstroImg) an AstroImage, or an array of AstroImages, with .Back and 
    %              .Var already populated
    %          * ...,Key,Val,...
    %            'VarLevel' - threshold level for suspected bad line [default 2.5]
    %            'MinLineLength' - minimal length of a suspicious bad column -
    %                      typically several times larger than the width of the
    %                      PSF, and of the width of possible extended objects
    %                      [default, max(size(AstroImg.Image))/20 pixels]
    %            'HighFraction' - fraction of the segment pixels over or below VarLevel,
    %                       for a segment to be considered bad [default 0.8]
    %            'ThetaResolution' - default 0.25 degrees.
    %            'RhoResolution' - Default 0.5. (should be commensurate to the width of
    %                       the PSF)
    %            'FillGap' - Join collinear segments separated no more than this
    %                      distance[default max(size(AstroImg.Image))/20 pixels]
    %            'MaxLines' - maximal number of Hough lines to consider [default 5]
    %            'MaskedTrackWidth' - width in pmask.
    %                              mask. If not provided, computed by imUtil.psf.pseudoFWHM
    %
    % Output: - An AstroImage in which the MaskData property is updated for each image of the input array
    %         - The last of the masks is also returned as optional output, for
    %           debugging
    % Author : Enrico Segre (Aug 2023)
    %
    % Example:
    %
    %   AI=AstroImage();
    %   AI.Image=20*imUtil.art.createSegments([650,700],...
    %                    [322,233;98,0],[54,11;145,211],'width',0.5) + ...
    %               rand(650,700);
    %   imProc.background.background(AI);
    %   imProc.mask.maskTracks(AI);


    arguments
        AstroImg AstroImage
        Args.VarLevel              = 2.5;
        Args.MinLineLength         = [];
        Args.HighFraction          = 0.8;
        Args.ThetaResolution       = 0.25;
        Args.RhoResolution         = 0.5;
        Args.FillGap               = [];
        Args.MaxLines              = 5;
        Args.BitName_Streak        = 'Streak';
        Args.MaskedTrackWidth      = [];
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
        if ~isempty(P)
            lines = houghlines(HighPix,T,R,P,'FillGap',FillGap,'MinLength',MinLength);
            L1=vertcat(lines.point1);
            L2=vertcat(lines.point2);
            % plot([L1(:,1),L2(:,1)]',[L1(:,2),L2(:,2)]')
        else
            L1=[];
            L2=[];
        end

        % mask: draw all the pixellated lines and then dilate them
        % approximately as much as the PSF is wide
        if isempty(Args.MaskedTrackWidth)
            [wx,wy]=imUtil.psf.pseudoFWHM(AstroImg(k).PSF);
            w=max(wx,wy);
        else
            w=Args.MaskedTrackWidth;
        end
        bwmask=imUtil.art.createSegments(size(HighPix),L1,L2,...
                                        'shape','flat','width',w);

        % imagesc(bwmask); axis xy

        % set this into the AstroImage mask
        AstroImg(k).maskSet(bwmask>0, Args.BitName_Streak);
    end
end