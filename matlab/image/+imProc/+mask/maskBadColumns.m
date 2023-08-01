function mask=maskBadColumns(AstroImg,Args)
% Mask pixels which belong to bad columns, by applying a sliding block
%  filter over column segments
% Bad column segments are defined as those which have more than HighFraction
%  of their pixels values either larger or smaller than VarLevel*sqrt(Var) 
%  from the local background value.
% Consistently with our mask bits dictionary, bad pixels are flagged with
%  either bit 21 (Bad column low values) or 22 (Bad column high values) on.
% Blooming sources also cause part of the column containing them to be
%  marked.
%
% Inputs:
%  - AstroImg: an AstroImage, or an array of AstroImages, with .Back and 
%              .Var already populated
%  - Key,Val arguments:
%        VarLevel:     threshold level for suspected bad line [default 2]
%        MinLineLength: minimal length of a suspicious bad column -
%                      typically several times larger than the width of the
%                      PSF, and of the width of possible extended objects
%                      [default, size(AstroImg.Image,1)/20]
%        HighFraction: fraction of the segment pixels over or below VarLevel,
%                       for a segment to be considered bad [default 0.5]
%        ColumnDim:    1 (default) to mark columns, 2 to scan rows
% Outputs:
%     AstroImg.Mask is updated for each image of the input array
%     - the last of the masks is also returned as optional output, for
%       debugging

    arguments
        AstroImg AstroImage = [];
        Args.VarLevel = 2;
        Args.MinLineLength= [];
        Args.HighFraction = 0.5;
        Args.ColumnDim = 1;
    end

    if Args.ColumnDim == 1
        coldim=1;
    else
        coldim=2;
    end
    
    bd=BitDictionary;
    bdl=2^bd.name2bit('ColumnLow');
    bdh=2^bd.name2bit('ColumnHigh');
    
    for k=1:numel(AstroImg)
        if isempty(Args.MinLineLength)
            nl = ceil(size(AstroImg(k).Image,coldim)/20);
        else
            nl=Args.MinLineLength;
        end
        HighPix = (AstroImg(k).Image - AstroImg(k).Back) > ...
                      Args.VarLevel*sqrt(AstroImg(k).Var);
        LowPix = (AstroImg(k).Back - AstroImg(k).Image) > ...
                      Args.VarLevel*sqrt(AstroImg(k).Var);

        %mask=nlfilter(HighPix,[Args.MinLineLength,1],@(x) sum(x)/numel(x)>0.5);
        % nlfilter displays a waitbar, which is ugly, and has obsolesced
        % the syntax for passing function parameters (here, HighFraction)

        % This code, trimmed from nlfilter.m, is more effective
        [mx,my] = size(HighPix);
        if coldim==1
            aa = false(mx+nl-1,my);
        else
            aa = false(mx,my+nl-1);
        end
        bb=aa;
        if coldim==1
            aa(floor((nl-1)/2)+(1:mx),:) = HighPix;
            bb(floor((nl-1)/2)+(1:mx),:) = LowPix;
        else
            aa(:,floor((nl-1)/2)+(1:my)) = HighPix;
            bb(:,floor((nl-1)/2)+(1:my)) = LowPix;
        end

        % Find out what output type to make.
        rows = 0:(nl-1);
        maskhigh = false(mx,my);
        masklow = maskhigh;
        % Apply fun to each neighborhood of a
        if coldim==1
            for i=1:mx
                x = aa(i+rows,:);
                y = bb(i+rows,:);
                maskhigh(i,:) = mean(x,1) > Args.HighFraction;
                masklow(i,:) = mean(y,1) > Args.HighFraction;
            end
        else
            for i=1:my
                x = aa(:,i+rows);
                y = bb(:,i+rows);
                maskhigh(:,i) = mean(x,2) > Args.HighFraction;
                masklow(:,i) = mean(y,2) > Args.HighFraction;
            end
        end

        % then we should still imdilate the mask in the column direction, of
        %  MinLineLength*HighFraction in both directions, because edges of the
        %  bad columns fell off the voting
        if coldim==1
            extend=ones(floor(nl*Args.HighFraction),1);
        else
            extend=ones(1,floor(nl*Args.HighFraction));
        end
        mask=uint32(bdl*imdilate(masklow,extend) + ...
                    bdh*imdilate(maskhigh,extend) );

        % turn on these bits in the (existing) AI.Mask
        AstroImg(k).Mask=bitor(mask,AstroImg(k).Mask);
    end