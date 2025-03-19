function [Result, ResultRS] = fitFastMotion(VecJD, MatRA, MatDec, Args)
    % Search fast moving asteroids in RA/Dec matrices of epochs vs. src.
    %     This function is designed to search for fast moving asteroids in
    %     the MatchedSources data product. 
    %     Given a matrix of [epoch x src] of RA and Dec, and a vector of JD
    %     per epoch. Search for sources that have less than Args.MaxNdet
    %     detections and that are obtained within a time range of less than
    %     Args.MaxTimeDiff days.
    %     This candidates are called orphans. For each orphan, look for all
    %     other orphans within MaxDist deg. For each such group perform
    %     RANSAC fit to their RA and Dec as a function of time.
    %
    % Input  : - Vec of JD.
    %          - Array of RA (Units are specified in CooUnits).
    %          - Array of Dec (Units are specified in CooUnits).
    %          * ...,key,val,... 
    %            'DimEpoch' - Dimension of epoch. Default is 1.
    %            'FlagGood' - Optional vector of flags (per source) that
    %                   indicating that the source should be used.
    %                   If [], then use all sources.
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
    %            .RMS - RMS [arcsec] of the best fit solution.
    %            .ParX
    %            .PatY
    %            .Epoch
    %            .Tag - The tag input argument.
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
    RefEpoch = (VecJD(1)+VecJD(2)).*0.5;
    
    % make epoch along dim=1
    if Args.DimEpoch==2
        MatRA  = MatRA.';
        MatDec = MatDec.';
        MatInd = MatInd.';
    end
    
    %plot(nanmedian(MatRA),nanmedian(MatDec), '.');
    
    [Nepoch, Nsrc] = size(MatRA);
    MatJD          = repmat(VecJD, 1, Nsrc);
    % MatJD contains NaN where MatRA contains NaN:
    MatJD(isnan(MatRA))  = NaN;
    
    % count not nan
    Nnotnan  = sum(~isnan(MatRA));
    % select candidates
    FlagCand = Nnotnan<Args.MaxNdet;
    
    % remove bad sources
    if ~isempty(Args.FlagGood)
        FlagCand = FlagCand & Args.FlagGood(:).';
    end
    IndCand  = find(FlagCand);
    Ncand    = numel(IndCand);
    
    MatJD    = MatJD(:,IndCand);
    RangeJD  = range(MatJD, 1);
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
    PointFound = false(Npt,1);
    K          = 0;
    Cont       = true;
    Result     = struct('JD',cell(0,1), 'RA',cell(0,1), 'Dec',cell(0,1), 'Ind',cell(0,1),...
                        'RMS',cell(0,1), 'Npt',cell(0,1), 'NuniqueJD',cell(0,1),...
                        'ParX',cell(0,1), 'ParY',cell(0,1), 'Epoch',cell(0,1), 'Tag',cell(0,1));
    while Cont
        Ipt = find(~PointFound, 1, 'first');
        Dist = celestial.coo.sphere_dist_fast(CandsRA(Ipt), CandsDec(Ipt), CandsRA, CandsDec);
        
        Ind  = find(Dist<Args.MaxDist & ~PointFound);
        PointFound(Ipt) = true;  % not found, but already tested
        if numel(Ind)>=Args.MinNpt
            % fit points using RANSAC
            
            
            
            R = tools.math.fit.ransacLinear2d([CandsRA(Ind), CandsDec(Ind)], CandsJD(Ind),...
                            'Epoch',RefEpoch,...
                            'DistFun',@celestial.coo.sphere_dist_fast,...
                            'ThresholdDist',ThresholdDist,...
                            'NptFit',Args.NptFit,...
                            'MinNpt',Args.MinNpt);
            
            if R.Found
                K = K + 1;
                ResultRS(K) = R;
                % if found - remove data points from list
                PointFound(Ind(R.Flag)) = true;
                
                % store data: output units like input
                IndF = Ind(R.Flag);
                Result(K).JD  = CandsJD(IndF);
                Result(K).RA  = CandsRA(IndF)./AngFactor;
                Result(K).Dec = CandsDec(IndF)./AngFactor;
                Result(K).Ind = CandsInd(IndF);
                Result(K).RMS = ResultRS(K).FlagRMS.*RAD.*ARCSEC_DEG;
                
                % check consistency of detection.
                % The source should appear only once at each epoch
                Result(K).Npt = numel(IndF);
                Result(K).NuniqueJD = numel(unique(Result(K).JD));
                
                Result(K).ParX  = ResultRS(K).ParX;
                Result(K).ParY  = ResultRS(K).ParY;
                Result(K).Epoch = ResultRS(K).Epoch;
                Result(K).Tag   = Args.Tag;
            end
           
        end
        if sum(~PointFound)<Args.MinNpt
            Cont = false;
        end
    end
       
end
