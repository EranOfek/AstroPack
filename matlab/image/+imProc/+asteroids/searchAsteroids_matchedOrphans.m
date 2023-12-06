function Result = searchAsteroids_matchedOrphans(MatchedS, Args)
    %
    
    arguments
        MatchedS
        Args.BitDict BitDictionary    = [];   % Coadd(1).MaskData.Dict
        Args.MaxNepochs               = 3;
    end
    
    
    % find orphans in MatchedSources object
    
    [OrphansList,CleanOrphansList,Norphans] = lcUtil.findOrphansClean(MatchedS, 'BitDict',Args.BitDict, 'MaxNepochs',Args.MaxNepochs);
    
    %go over all Sub images
    Nsub = numel(CleanOrphansList);
    for Isub=1:1:Nsub
        % Orphans with a single detection
        Flag1 = [CleanOrphansList(Isub).Src.Ndet]==1;

        % select [JD, RA, Dec]
        Data = [[CleanOrphansList(Isub).Src(Flag1).MeanJD].',  [CleanOrphansList(Isub).Src(Flag1).MeanRA].', [CleanOrphansList(Isub).Src(Flag1).MeanDec].'];
    
        Result = tools.math.fit.ransacLinear(Data(:,1:2), 'MinRMS',1./3600, 'ThresholdDist', 1./3600);
        if Result.Npt > numel(unique(Data(:,1)))
            % multiple apperances on a staright line in the same epoch
            % likely a satellite streak
        end

        % look for remaining orphans
        FlagRealOrphans = ~Result.FlagGoodPt;
        NnotStreak = numel(CleanOrphansList(Isub).Src(FlagRealOrphans));

        if NnotStreak>3
            % try to match remaining orphans
            
        end
    end


end