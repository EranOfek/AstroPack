function [Result] = orphansOnStreak(JD, X, Y, Args)
    % Given a list of orphans, search for events with the same epoch that are on a straight line.
    %   Given a list [JD, X, Y], this function is useful in order to find
    %   in each unique epoch, points that are seating on a straight line.
    %   For example, this could be points belongs to a satellite streak
    %   that was detected as multiple points in a single epoch.
    % Input  : - Vector of JD. If empty, then assume all 1.
    %            If 'CooUnits' is not 'pix', then the X/Y coordinates will
    %            be projected (using Gnomonic projection) on a plane,
    %            cenetered at the mean spherical position of the points.
    %          - Vector of X/long coordinate.
    %          - Vector of Y/lat coordinate.
    %          * ...,key,val,... 
    %            'CooUnits' - Coordinates units: 'pix'|'deg'|'rad'.
    %                   Default is 'deg'.
    %            'FitUnits' - Units in which 'ThresholdDist' and 'MinRMS'
    %                   are specified. If 'CooUnits'='pix', then this
    %                   argument is ignored, and everything is in pixel
    %                   units.
    %                   Default is 'arcsec'.
    %            'NminEpochLines' -  Number of points to fit per epoch must
    %                   be larger than this value.
    %                   Default is 3.
    %            'NptFit' - Number of points to fit. Should be
    %                   NminEpochLines+1
    %                   Default is 4.
    %            'MinRMS' - Minimum rms for declaring a good fit.
    %                   Default is 0.5.
    %            'ThresholdDist' - Threshold on residuals. If Residuals are
    %                   smaller than this value then they will be included
    %                   in the selected line fit.
    %                   Default is 2.
    %            'UseSrc' - A vector of logicals of sources to use.
    %                   If empty, use all.
    % Output : - A structure containing the following fields:
    %            .StreakInd - A Vector with the same length as the input X
    %                   vector. The value of each element indicate to which
    %                   streak number it belongs. If 0, then no streak.
    %            .ResFit - As tructure array, with element for each streak counter
    %                   in the StreakInd vector. The structure contains the
    %                   output for the best fit returned by tools.math.fit.ransacLinear
    % Author : Eran Ofek (2024 Mar) 
    % Example: R=imUtil.asteroids.orphansOnStreak(RR.JD, RR.RA, RR.Dec);


    arguments
        JD
        X
        Y
        
        Args.CooUnits          = 'deg';
        Args.FitUnits          = 'arcsec';
        Args.NminEpochLine     = 3;
        Args.NptFit            = 4;
        Args.MinRMS            = 0.5;
        Args.ThresholdDist     = 2;
        Args.UseSrc            = [];
    end

    X  = X(:);
    Y  = Y(:);
    Nx = numel(X);
    if isempty(Args.UseSrc)
        Args.UseSrc = repmat(true, Nx, 1);
    else
        Args.UseSrc = Args.UseSrc(:);
    end

    if isempty(JD)
        JD = ones(Nx,1);
    else
        JD = JD(:);
    end

    Result.StreakInd = zeros(Nx,1);
    

    switch lower(Args.CooUnits)
        case 'pix'
            % X/Y in pixel units - no conversion
        otherwise
            % assume input is spherical coordinates
            % get mean RA/Dec
            [Lon0, Lat0] = celestial.coo.funOnCosDir(X, Y, @mean, 'FunArgs',{1,'omitnan'}, 'InUnits',Args.CooUnits, 'OutUnits','rad');
            Conv = convert.angular(Args.CooUnits, 'rad');
            X    = X.*Conv;
            Y    = Y.*Conv;

            % project coordinates on plane
            Scale  = convert.angular('rad', Args.FitUnits);
            [X, Y] = celestial.proj.pr_gnomonic(X, Y, Scale, [Lon0, Lat0]);
    end



    % return number of elements in each unique group
    [UnVal,Count] = tools.array.unique_count(JD);
    % index of unique group for which to test for Epoch Line...
    IndEpochLine  = find(Count>Args.NminEpochLine);
    Nel           = numel(IndEpochLine);

    StreakCounter = 0;
    for Iel=1:1:Nel
        % indicess of all epochs in unique group
        IndInEpoch = find(UnVal(IndEpochLine(Iel))==JD & Args.UseSrc);

        % all points in the same epoch - so just fit a linear model:
        ResFit = tools.math.fit.ransacLinear([X(IndInEpoch), Y(IndInEpoch)],...
                                             "MinNpt",Args.NminEpochLine,...
                                             "Ntrial",20,...
                                             "MinRMS",Args.MinRMS,...
                                             "ThresholdDist",Args.ThresholdDist,...
                                             "NptFit",Args.NptFit);
        if ResFit.Found
            StreakCounter = StreakCounter + 1;
            Result.StreakInd(IndInEpoch(ResFit.FlagGoodPt)) = StreakCounter;
            Result.ResFit(StreakCounter) = ResFit;
        
        end
    end
    if StreakCounter==0
        % if no streaks populate with empty.
        Result.ResFit = [];
    end

end
