function [Result, ResultRS] = fitFastMotion(VecJD, MatRA, MatDec, Args)
    % Search fast moving asteroids in RA/Dec matrices of epochs vs. src.
    %     This function is designed to search for fast moving asteroids in
    %     the MatchedSources data product. 
    %     Given a matrix of [epoch x src] of RA and Dec, and a vector of JD
    %     per epoch. Search for sources that have less than Args.MaxNdet
    %     detections and that are obtained within a time range of less than
    %     Args.MaxTimeDiff days.
    %     This candidates are called orphans. Orphans are linked into a
    %     group when each point lies within MaxDist of some other point in
    %     the group, so a track may be longer than MaxDist. For each group
    %     perform a RANSAC fit to RA and Dec as a function of time.
    %
    % Input  : - Vec of JD.
    %          - Array of RA (Units are specified in CooUnits).
    %          - Array of Dec (Units are specified in CooUnits).
    %          * ...,key,val,... 
    %            'DimEpoch' - Dimension of epoch. Default is 1.
    %            'FlagGood' - Detections to keep. Either a per-source logical
    %                   vector, or a logical array the size of RA/Dec
    %                   (false entries are removed before the search).
    %                   If [], then use all finite sources.
    %            'MaxNdet'- Max. number of detections of source "in the smae position"
    %                   over all epochs. Default is 7.
    %            'MaxTimeDiff' - Max. time range for MaxNdet (days).
    %                   Default is 7.*20./86400.
    %            'MaxDist' - Maximum distance between points to fit linear
    %                   motion (units are in CooUnits').
    %                   Default is 0.03.
    %            'CooUnits' - RA, Dec, MaxDist units. Default is 'deg'.
    %            'NptFit' - Number of points to fit in each RANSAC
    %                   simulation. Default is 3.
    %            'MinNpt' - Min. Number of points in the best fit solution
    %                   of a moving source.
    %                   Default is 8.
    %            'ThresholdDist' - Threshold distance of points in
    %                   best fit solution from the linear motion.
    %                   Default is 3 arcsec.
    %            'Tag' - Optional tag to add to the output structure.
    % Output : - A structure array, with element per asteroid candidate, of:
    %            .JD - A vector of JD for moving source.
    %            .RA - A vector of RA (units like input units).
    %            .Dec - A vector of Dec (units like input units).
    %            .Ind - A vector of indices of the selected RA/Dec points
    %                   from the initial matrix. This can be used to select
    %                   other properties. E.g., MatMag(R.Ind).
    %            .Npt - Number of points in solution.
    %            .NuniqueJD - Number of unique JD points in solution.
    %                   if NuniqueJD<Npt, then there may be a problem.
    %            .RMS - RMS [arcsec] of the great-circle residuals,
    %                   sqrt(mean(d.^2)). d is celestial.coo.sphere_dist_fast,
    %                   so the cos(Dec) factor is included.
    %            .FitRA - Best Fit RA positions.
    %            .FitDec - Best fit Dec positions.
    %            .ParX - [RA0; dRA/dt] in rad and rad/day, at Epoch.
    %                   dRA/dt is the coordinate rate, not multiplied by cos(Dec).
    %            .ParY - [Dec0; dDec/dt] in rad and rad/day, at Epoch.
    %            .Epoch
    %            .Tag - The tag input argument.
    %            .ProperMotion - Sky rate [deg/day],
    %                   sqrt((dRA/dt*cos(Dec))^2 + (dDec/dt)^2), using the
    %                   median Dec of the inliers. dRA/dt is the RA
    %                   coordinate rate.
    %          - A structure array of RANSAC solutions.
    %            Note that ParX, and ParY contains the linear motion
    %            parameters (in radians, and radians per day) relative to
    %            the Epoch.
    % Author : Eran Ofek (2025 Mar) 
    % Example: [Result] = imUtil.asteroids.fitFastMotion(MS.JD, MS.Data.RA, MS.Data.Dec, 'FlagGood',~MS.searchFlags('UseSrc',true));

    arguments
        VecJD
        MatRA
        MatDec
        Args.DimEpoch          = 1;
        Args.FlagGood          = [];
        Args.MaxNdet           = 7;
        Args.MaxTimeDiff       = 7.*20./86400;  % days
        Args.MaxDist           = 0.03;  % deg
        Args.CooUnits          = 'deg';
        Args.NptFit            = 3;
        Args.MinNpt            = 8;
        Args.ThresholdDist     = 3;  % arcsec
        Args.Tag               = [];
    end
    RAD = 180./pi;
    ARCSEC_DEG = 3600;
    
    % convert RA/Dec to radians:
    AngFactor    = convert.angular(Args.CooUnits, 'rad');
    MatRA        = MatRA.*AngFactor;
    MatDec       = MatDec.*AngFactor;
    Args.MaxDist = Args.MaxDist.*AngFactor;  % rad
    
    % MatInd contains the indices of entries
    MatInd       = reshape((1:1:numel(MatRA)), size(MatRA));
    
    ThresholdDist = Args.ThresholdDist./(RAD.*ARCSEC_DEG);
    
    VecJD    = VecJD(:);
    RefEpoch = (VecJD(1)+VecJD(end)).*0.5;
    
    % make epoch along dim=1
    if Args.DimEpoch==2
        MatRA  = MatRA.';
        MatDec = MatDec.';
        MatInd = MatInd.';
        if ~isempty(Args.FlagGood) && ~isvector(Args.FlagGood)
            Args.FlagGood = Args.FlagGood.';
        end
    end
    
    %plot(nanmedian(MatRA),nanmedian(MatDec), '.');
    
    [Nepoch, Nsrc] = size(MatRA);
    % A same-size mask removes individual detections. A vector keeps or
    % drops a whole source.
    GoodSrc = [];
    if ~isempty(Args.FlagGood)
        if isequal(size(Args.FlagGood), [Nepoch, Nsrc])
            MatRA(~Args.FlagGood) = NaN;
            MatDec(~Args.FlagGood) = NaN;
        elseif numel(Args.FlagGood)==Nsrc
            GoodSrc = logical(Args.FlagGood(:).');
        else
            error('FlagGood must be a per-source vector or an array the size of RA');
        end
    end

    MatJD          = repmat(VecJD, 1, Nsrc);
    % MatJD contains NaN where MatRA contains NaN:
    MatJD(isnan(MatRA))  = NaN;
    
    % count not nan
    Nnotnan  = sum(~isnan(MatRA));
    % select candidates
    FlagCand = Nnotnan<Args.MaxNdet;
    if ~isempty(GoodSrc)
        FlagCand = FlagCand & GoodSrc;
    end
    IndCand  = find(FlagCand);
    Ncand    = numel(IndCand);
    
    MatJD    = MatJD(:,IndCand);
    % Finite detections only. range() without 'omitnan' returns NaN for a
    % column that contains any missed epoch.
    RangeJD  = max(MatJD, [], 1, 'omitnan') - min(MatJD, [], 1, 'omitnan');
    % select sources which time range is smaller than MaxTimeDist
    IndGood  = find(RangeJD<Args.MaxTimeDiff);
    MatJD    = MatJD(:,IndGood);
    MatRA    = MatRA(:,IndCand(IndGood));
    MatDec   = MatDec(:,IndCand(IndGood));
    MatInd   = MatInd(:,IndCand(IndGood));
        
    % select all entries of candidates which are not NaN
    Inn      = find(~isnan(MatRA));
    CandsRA  = MatRA(Inn);
    CandsDec = MatDec(Inn);
    CandsJD  = MatJD(Inn);
    CandsInd = MatInd(Inn);
    
    %hold on;
    %plot(CandsRA, CandsDec, 'bo')
    %plot(180.49576./RAD, 14.72892./RAD,'r^')
    
    Npt        = numel(CandsJD);
    K          = 0;
    Result     = struct('JD',cell(0,1), 'RA',cell(0,1), 'Dec',cell(0,1), 'Ind',cell(0,1),...
                        'RMS',cell(0,1), 'Npt',cell(0,1), 'NuniqueJD',cell(0,1),...
                        'ParX',cell(0,1), 'ParY',cell(0,1), 'Epoch',cell(0,1), 'Tag',cell(0,1),...
                        'ProperMotion',cell(0,1));
    ResultRS   = [];

    if Npt>=Args.MinNpt
        % Single-linkage groups: an edge exists when two detections are
        % closer than MaxDist. A track can then be longer than MaxDist.
        Parent = (1:Npt).';
        for Ipt=1:1:Npt-1
            Dist = celestial.coo.sphere_dist_fast(CandsRA(Ipt), CandsDec(Ipt), CandsRA(Ipt+1:end), CandsDec(Ipt+1:end));
            Neigh = find(Dist<Args.MaxDist);
            for Inei=1:1:numel(Neigh)
                Parent = linkUnion(Parent, Ipt, Ipt+Neigh(Inei));
            end
        end
        Root = zeros(Npt,1);
        for Ipt=1:1:Npt
            [Parent, Root(Ipt)] = linkRoot(Parent, Ipt);
        end

        GroupId = unique(Root);
        for Igroup=1:1:numel(GroupId)
            Pool = find(Root==GroupId(Igroup));
            while numel(Pool)>=Args.MinNpt
                R = tools.math.fit.ransacLinear2d([CandsRA(Pool), CandsDec(Pool)], CandsJD(Pool),...
                                'Epoch',RefEpoch,...
                                'DistFun',@celestial.coo.sphere_dist_fast,...
                                'ThresholdDist',ThresholdDist,...
                                'NptFit',Args.NptFit,...
                                'MinNpt',Args.MinNpt,...
                                'StopWhenFound',false);

                if ~R.Found
                    break
                end

                IndF = Pool(R.Flag);
                Nunique = numel(unique(CandsJD(IndF)));
                % One detection per epoch. Otherwise drop these points and
                % keep searching the rest of the group.
                if Nunique~=numel(IndF) || Nunique<Args.MinNpt
                    Pool = Pool(~R.Flag);
                    continue
                end

                K = K + 1;
                if isempty(ResultRS)
                    ResultRS = R;
                else
                    ResultRS(K) = R;
                end

                Result(K).JD  = CandsJD(IndF);
                Result(K).RA  = CandsRA(IndF)./AngFactor;
                Result(K).Dec = CandsDec(IndF)./AngFactor;
                Result(K).Ind = CandsInd(IndF);
                Result(K).RMS = ResultRS(K).FlagRMS.*RAD.*ARCSEC_DEG;

                Result(K).ParX  = ResultRS(K).ParX;
                Result(K).ParY  = ResultRS(K).ParY;
                Result(K).Epoch = ResultRS(K).Epoch;
                Result(K).Tag   = Args.Tag;

                RelTime = Result(K).JD - Result(K).Epoch;
                Result(K).FitRA  = polyval(flipud(Result(K).ParX)./AngFactor, RelTime);
                Result(K).FitDec = polyval(flipud(Result(K).ParY)./AngFactor, RelTime);

                Result(K).Npt = numel(IndF);
                Result(K).NuniqueJD = Nunique;

                % RA coordinate rate times cos(Dec) is the eastward angle.
                DecRad = median(CandsDec(IndF));
                Result(K).ProperMotion = hypot(Result(K).ParX(2).*cos(DecRad), Result(K).ParY(2))./AngFactor;

                Pool = Pool(~R.Flag);
            end
        end
    end

end

function [Parent, Root] = linkRoot(Parent, Index)
    % Find with path compression.
    Root = Index;
    while Parent(Root)~=Root
        Root = Parent(Root);
    end
    while Parent(Index)~=Root
        Next = Parent(Index);
        Parent(Index) = Root;
        Index = Next;
    end
end

function Parent = linkUnion(Parent, A, B)
    [Parent, Ra] = linkRoot(Parent, A);
    [Parent, Rb] = linkRoot(Parent, B);
    if Ra~=Rb
        Parent(Rb) = Ra;
    end
end
