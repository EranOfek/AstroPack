function Result = ransacLinear2d(Data, Time, Args)
    % RANSAC fitting of 2D data to a 2D linear function
    
    % TXY = [1 11 11; 1 20 16; 2 12 12; 3 13 13; 6 16 16; 7 17 17; 2 1 2; 3 1 1; 4 17 15; 5 15 18; 6 15 15; 7 14 17]; 
    %      tools.math.fit.ransacLinear2d(TXY(:,2:3),TXY(:,1))
    
    arguments
        Data                    % [X, Y]
        Time                    % [T]
        
        Args.NptFit          = 3;
        Args.Ntrial          = 10;
        Args.NtrialT         = 20;
        Args.ThresholdDist   = 0.5;
        Args.MinNpt          = 5;
        
        
    end
   
    Ndata      = size(Data,1);
    X          = Data(:,1);
    Y          = Data(:,2);
    Result.Found = false;
    
    if Ndata<Args.NptFit
        % not enough points - can't fit a model
        
    else
    
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
        while Itrial<Args.Ntrial || Found
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
            RMS
            
            [~,MinInd] = min(RMS);
            Xmodel = ParX(1,MinInd) + Time.*ParX(2,MinInd);
            Ymodel = ParY(1,MinInd) + Time.*ParY(2,MinInd);
            
            DataDist = sqrt((X - Xmodel).^2 + (Y - Ymodel).^2);
            Flag = DataDist<Args.ThresholdDist;
            NgoodPt = numel(unique(Time(Flag)));
            if NgoodPt>=Args.MinNpt
                % solution found
                Found = true;
                Result.Found = Found;
                Result.Flag  = Flag;
                Result.ParX  = ParX(:,MinInd);
                Result.ParY  = ParY(:,MinInd);
            end
            
        end
        
           
        
    end
    
end