function Result = ransacLinear2d(Data, Time, Args)
    % RANSAC fitting of 2D data to a 2D linear function
    %   Give a time dependent 2-D data [T, X, Y], with large number of
    %   outliers, attempt fitting a model of the form:
    %   X = x0 + mu_x *T, Y = y0 + mu_y*T
    %   After a solution is found the function stops.
    % Input  : - A two column matrix [X, Y] of data.
    %          - [Time] vector with the same number of elements as the
    %            data.
    %          * ...,key,val,...
    %            'Epoch' - Reference epoch. Default is 0.
    %            'DistFun' - Function handle with diatance function.
    %                   e.g., @celestial.coo.sphare_dist_fast (in this case
    %                   input must be in radians).
    %                   Default is @tools.math.geometry.plane_dist
    %            'NptFit' - Number of points required for the fit.
    %                   (i.e., in the first iteration random fit).
    %                   Default is 3.
    %            'Ntrial' - Number of trials in each time selection trial.
    %                   Default is 10.
    %            'NtrialT' - Number of time selection trials.
    %                   Default is 20.
    %            'ThresholdDist' - Threshold distance.
    %                   Default is 0.5.
    %            'MinNpt' - Minimum number of points in the solution.
    %                   Default is 5.
    % Output : - A structure with the first found solution.
    %            The following fields are available:
    %            .Found - A logical indicating if a solution was found.
    %                   This is the only field that is available if no solution
    %                   found.
    %            .Epoch - The time epoch to which the fit x0, y0 refers to.
    %            .FlagDataDist - Distance between the model and the selected data
    %                   points.
    %            .FlagRMS - RMS of the points particpatited in the final
    %               fit.
    %            .FlagResidX - X Residuals of good points.
    %            .FlagResidY - Y Residuals of good points.
    %            .FlagN - Number of points particpating in the fit.
    %            .Flag - A vector of logicals, per data point,
    %               indicating if the point was within the treshold distance
    %               from the first iteration fitted line.
    %            .ParX - [x0, mu_x]
    %            .ParY - [y0, mu_y]
    % Author : Eran Ofek (Nov 2021)
    % Example: TXY = [1 11 11; 1 20 16; 2 12 12; 3 13 13; 6 16 16; 7 17 17; 2 1 2; 3 1 1; 4 17 15; 5 15 18; 6 15 15; 7 14 17]; 
    %          R = tools.math.fit.ransacLinear2d(TXY(:,2:3),TXY(:,1))
    
    arguments
        Data                    % [X, Y]
        Time                    % [T]
        
        Args.Epoch            = 0;   
        Args.DistFun function_handle = @tools.math.geometry.plane_dist;
        Args.NptFit           = 3;
        Args.Ntrial           = 10;
        Args.NtrialT          = 20;
        Args.ThresholdDist    = 0.5;
        Args.MinNpt           = 5;
    end
   
    Ndata        = size(Data,1);
    Result.Found = false;
    
    if Ndata<Args.NptFit
        % not enough points - can't fit a model
    else
        X            = Data(:,1);
        Y            = Data(:,2);
        Result.Epoch = Args.Epoch;
        Time         = Time - Result.Epoch;
    
        UniqueTime = unique(Time);
        UniqueTime = UniqueTime(:);
        Nut        = numel(UniqueTime);
        TimePtInd  = cell(1,Nut);   % in each element - indices of points at the specific quniue time.
        TimeNpt    = zeros(1,Nut);  % number of points in each unique time
        for Iut=1:1:Nut
            TimePtInd{Iut} = find(Time == UniqueTime(Iut));
            TimeNpt(Iut)   = numel(TimePtInd{Iut});
        end

        % generate a random selection of K out of N
        SimInd = zeros(Args.NptFit, Args.NtrialT);
        Itrial = 0;
        Found  = false;
        while Itrial<Args.Ntrial || ~Found
            Itrial = Itrial + 1;
            % generate NptFit unique times indices
            IndT = randperm(Nut, Args.NptFit);
            
            H = [ones(Args.NptFit,1), UniqueTime(IndT)];
            
            for It=1:1:Args.NptFit
                SimInd(It,:) = TimePtInd{IndT(It)}(randi(TimeNpt(IndT(It)), 1, Args.NtrialT));
            end
            %Time(SimInd)
            Xsim = X(SimInd);
            Ysim = Y(SimInd);
            ParX = H\Xsim;
            ParY = H\Ysim;
            
            ResidX = Xsim - H*ParX;
            ResidY = Ysim - H*ParY;
            
            RMS = sqrt(sum(ResidX.^2 + ResidY.^2, 1));
            %RMS
            
            [~,MinInd] = min(RMS);
            Xmodel = ParX(1,MinInd) + Time.*ParX(2,MinInd);
            Ymodel = ParY(1,MinInd) + Time.*ParY(2,MinInd);
            
            %DataDist = sqrt((X - Xmodel).^2 + (Y - Ymodel).^2);
            DataDist = Args.DistFun(X, Y, Xmodel, Ymodel);
            Flag = DataDist<Args.ThresholdDist;
            NgoodPt = numel(unique(Time(Flag)));
            if NgoodPt>=Args.MinNpt
                % refit with all data points
                Result.FlagN    = sum(Flag);
                
                H = [ones(Result.FlagN,1), Time(Flag)];
                ParX = H\X(Flag);
                ParY = H\Y(Flag);
                ResidX = X(Flag) - H*ParX;
                ResidY = Y(Flag) - H*ParY;
                %FlagDataDist = sqrt(ResidX.^2 + ResidY.^2);
                FlagDataDist = Args.DistFun(X(Flag), Y(Flag), H*ParX, H*ParY);
                
                % solution found
                Found = true;
                Result.Found    = Found;
                Result.FlagDataDist = FlagDataDist;
                Result.FlagRMS  = std(FlagDataDist);
                Result.FlagResidX = ResidX;
                Result.FlagResidY = ResidY;
                
                Result.Flag     = Flag;
                Result.ParX     = ParX;
                Result.ParY     = ParY;
            end
            
        end
        
    end
    
end