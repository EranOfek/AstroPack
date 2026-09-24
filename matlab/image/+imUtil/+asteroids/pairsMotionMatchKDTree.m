function Cand = pairsMotionMatchKDTree(Time,X,Y,Args)
    % Given a list of [Time, X, Y] search for points which belong to the
    %       same constant-rate moving object.
    %   The algorithm run time scales like N^2. First, all pairs which have
    %   unique observing times are selected. The 4 motion parameters (X0,
    %   dotX, Y0, dotY) are calcaulated for these pairs.
    %   The parameters are stored in a k-d tree, and then for each pair in
    %   the list, other pairs with similar parameter values are searched
    %   using the k-d tree.
    % Requires: the KD tree package from: https://github.com/taiya/kdtree
    % Input  : - Vector of times. These are the times at which the X and Y
    %            positions were measured.
    %            The positions and times corresponds to all the orphan
    %            candidates for which a linear motion will be searched and
    %            fitted.
    %          - Vector of measured X positions.
    %          - Vector of measured Y positions.
    %          * ...,key,val,...
    %            'HalfRangeVec' - A 4 elemene vector of half range in which
    %                   to search for matching pairs in position-velocity
    %                   space. These are the thresholds in X position, X
    %                   velocity, Y position, and Y velocity.
    %                   Default is ones(1,4).*10./3600.
    %            'MinNpairs' - Minimum number of pairs required in order to
    %                   declare a match. Default is 2.
    %            'MinNprev' - Sources pairs for which at least MinNprev
    %                   events has the same indices as previously matched
    %                   pairs, will be decalred as a possible repeat.
    %                   Default is 2.
    %            'MinDist' - Minimum distance between predicted and
    %                   observed positions to search for all the apperances
    %                   of the source in the catalog.
    % Output : - A structure array with element per candidate.
    %            The following fields are available:
    %            .Ipair - Index of pair.
    %            .Ind - Indices of possible matches to Ipair.
    %            .Par - [X0, dotX, Y0, dotY] for all pairs.
    %            .MeanPar - Mean [X0 dotX Y0 dotY]
    %            .Nprev - Number of previous candidates with similar
    %                   matches. NaN, if this is the first.
    %            .ResidX - All X residuals.
    %            .ResidY - All Y residuals.
    %            .DistAll - All Distances from predicted position.
    %            .FlagGood - Flag of sources with Dist smaller than
    %                   the threshold.
    %            .StdX - Std in X residuals for FlagGood. 
    %            .StdY - Std in Y residuals for FlagGood. 
    %            .GoodTXY - [Time, X, Y] for FlagGood.
    % Author : Eran Ofek (Nov 2021)
    % Example: Time = [1 2 3 4 5 6 7 8 9 10 9 2 3 3 1 2 1 1 2 3 5 2, randi(100,1,1000)].'./1440;
    %          Nt   = numel(Time); X=rand(Nt,1).*3600; Y    = rand(Nt,1).*3600;
    %          X(1)=0; Y(1)=0; X(2)=10; Y(2)=5; X(3)=20; Y(3)=10; X(5)=40; Y(5)=20;
    %          Cand = imUtil.asteroids.pairsMotionMatchKDTree(Time,X,Y)
    
    arguments
        Time
        X
        Y
        Args.HalfRangeVec = ones(1,4).*10./3600;  % [X Y, dx/dt, dy/dt]
        Args.MinNpairs    = 2;
        Args.MinNprev     = 2;
        Args.MinDist      = 3;
        Args.UseVChooshK  = true;
    end
    
    Args.HalfRangeVec = Args.HalfRangeVec(:);
    
    % switch to mean time reference frame
    MeanTime = mean(Time);
    Time = Time(:) - MeanTime;
    X    = X(:);
    Y    = Y(:);

    N = numel(Time);
    % choose all pairs permutations:
    if Args.UseVChooshK
        IndAllPairs = VChooseK((1:1:N),2).';
    else
        IndAllPairs = nchoosek((1:1:N),2).';
    end
    
    
    TimePairs = Time(IndAllPairs);
    
    Flag        = TimePairs(1,:)~=TimePairs(2,:);
    IndAllPairs = IndAllPairs(:,Flag);
    TimePairs   = TimePairs(:,Flag);    % BUG? should use this?
    
    Npairs      = size(IndAllPairs,2);
    H           = ones(2,2);
    
    Time_AllPairs = Time(IndAllPairs);
    X_AllPairs    = X(IndAllPairs);
    Y_AllPairs    = Y(IndAllPairs);
    
    DT_AllPairs   = diff(Time_AllPairs,1,1);
    DX_AllPairs   = diff(X_AllPairs,1,1);
    DY_AllPairs   = diff(Y_AllPairs,1,1);
    
    RateX         = DX_AllPairs./DT_AllPairs;
    RateY         = DY_AllPairs./DT_AllPairs;
    Par           = [[X_AllPairs(1,:) - RateX.*Time_AllPairs(1,:)].', RateX.',...
                     [Y_AllPairs(1,:) - RateY.*Time_AllPairs(1,:)].', RateY.'];
    
% OLD: slow
%     for Ipairs=1:1:Npairs
%         %Ipairs
%         % FFU: compute directly instead of 
%         H(:,2) = Time(IndAllPairs(:,Ipairs));
%         ParX   = H\X(IndAllPairs(:,Ipairs));
%         ParY   = H\Y(IndAllPairs(:,Ipairs));
%         Par(Ipairs,1:4) = [ParX.', ParY.'];
%     end
        
    % Par contains:
    % [Xintersect, Xslope Yintersect, Yslope]
    % Build a k-d tree
    Tree = kdtree_build(Par);
    
    % check number of matches:
    Icand = 0;
    Cand  = [];
    for Ipairs=1:1:Npairs
        [Idxs] = kdtree_range_query(Tree, [Par(Ipairs,:).'-Args.HalfRangeVec, Par(Ipairs,:).'+Args.HalfRangeVec]);
        % FFU: alternatively use rangesearch.m and createns.m
        % to simulatnaoulsy search for all pairs.
        % Iprove performences only for large number of point searches by up to factor of
        % two.
        
        if numel(Idxs)>=Args.MinNpairs
            % possible candidate
            Icand = Icand + 1;
            Cand(Icand).Ipair   = Ipairs;
            Cand(Icand).Ind     = Idxs;
            Cand(Icand).Par     = Par(Idxs,:);
            Cand(Icand).MeanPar = mean(Par(Idxs,:));
            Cand(Icand).Nprev   = NaN;
            % compare to previous solutions
            for Iprev=1:1:Icand-1
                Cand(Icand).Nprev = sum(ismember(Cand(Icand).Ind, Cand(Iprev).Ind));
            end
            if isnan(Cand(Icand).Nprev) || Cand(Icand).Nprev<Args.MinNprev
                % search all points that pass near the mean line defined by
                % Par
                Xpred = Cand(Icand).MeanPar(1) + Cand(Icand).MeanPar(2).*Time;
                Ypred = Cand(Icand).MeanPar(3) + Cand(Icand).MeanPar(4).*Time;
                
                Cand(Icand).ResidX = X - Xpred;
                Cand(Icand).ResidY = Y - Ypred;
                Dist  = sqrt(Cand(Icand).ResidX.^2 + Cand(Icand).ResidY.^2);
                Cand(Icand).DistAll  = Dist;
                Cand(Icand).FlagGood = Dist<Args.MinDist; 
                % std for good points
                Cand(Icand).StdX     = std(Cand(Icand).ResidX(Cand(Icand).FlagGood));
                Cand(Icand).StdY     = std(Cand(Icand).ResidY(Cand(Icand).FlagGood));
                Cand(Icand).GoodTXY  = [Time(Cand(Icand).FlagGood) + MeanTime, X(Cand(Icand).FlagGood), Y(Cand(Icand).FlagGood)];
            end
        end
        
    end
 end