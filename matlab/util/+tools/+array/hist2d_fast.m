function [Mat, VecX, VecY, BinX, BinY] = hist2d_fast(Xv,Yv,Xedges, Yedges, OutType)
    % A fast version of histcounts2 (without all the overheads)
    %       This is faster than imUtil.patternMatch.hist2d
    % Input  : - A vector of X coordinates.
    %          - A vector of Y coordinates.
    %          - A vector of X edges.
    %          - A vector of Y edges.
    %          - Flag indicating if i/j or x/y output:
    %            true (default) - i,j output (like histcounts2)
    %               X axis is along i-dimension...
    %            false - output like imUtil.patternMatch.hist2.
    % Output : - A matrix of 2-D histogram (X vs Y).
    %          - Vector of X coordinate of center of X bins.
    %          - Vector of Y coordinate of center of Y bins.
    % Author : Eran Ofek (Sep 2021)
    % Example: Xv = rand(1e4, 1); Yv=rand(1e4, 1);
    %          RangeX = [0 1]; RangeY = [0 1.1]; StepX = 0.001; StepY = 0.001;
    %          Xed = (0:0.001:1); Yed = (0:0.001:1.1);
    %          Mat = tools.array.hist2d_fast(Xv,Yv, Xed, Yed); 
    %
    %          Mat1 = imUtil.patternMatch.hist2d(Xv,Yv,RangeX, RangeY,
    %          StepX, StepY);  % transposed!
    %          Mat1 = histcounts2(Xv,Yv, Xed, Yed); % like Mat
    %
    % Xv = 0.7; Yv =0.4; Xed=(0:0.333:1); Yed=(0:0.333:1.333);
    %      Mat = tools.array.hist2d_fast(Xv,Yv, Xed, Yed)
    
    arguments
        Xv
        Yv
        Xedges
        Yedges
        OutType logical   = true;
    end
    
    if OutType
        % switching X and Y to avoid transpose at the end
        [~,BinX] = matlab.internal.math.histcounts(Xv, Xedges);
        [~,BinY] = matlab.internal.math.histcounts(Yv, Yedges);

        CountslenX = length(Xedges)-1;
        CountslenY = length(Yedges)-1;
        % Filter out NaNs and out-of-range data
        Subs = [BinX(:) BinY(:)];
        Subs(any(Subs==0, 2),:) = [];
        Mat = accumarray(Subs,ones(size(Subs,1),1),[CountslenX, CountslenY]);
    else
        % switching X and Y to avoid transpose at the end
        [~,BinY] = matlab.internal.math.histcounts(Yv, Yedges);
        [~,BinX] = matlab.internal.math.histcounts(Xv, Xedges);

        CountslenX = length(Yedges)-1;
        CountslenY = length(Xedges)-1;
        % Filter out NaNs and out-of-range data
        Subs = [BinY(:) BinX(:)];
        Subs(any(Subs==0, 2),:) = [];
        Mat = accumarray(Subs,ones(size(Subs,1),1),[CountslenX, CountslenY]);
    end
    
    if nargout>1
        VecX = (Xedges(1:end-1) + Xedges(2:end)).*0.5;
        VecY = (Yedges(1:end-1) + Yedges(2:end)).*0.5;
        VecX = VecX(:);
        VecY = VecY(:);
    end
end