function Result = ransacLinear(Data, Args)
    % Fit a linear model Y=a+bX using RANSAC
    % Input  : - [X, Y] data
    %          * ...,key,val,...
    %            'NptFit' - Number of random points to fit in each
    %                   iteration. Default is 3.
    %            'Ntrial' - Number of trials. Default is 20.
    %            'MinRMS' - Min. RMS of random points fit below to select
    %                   points as a possible solution.
    %                   default is 0.5.
    %            'ThresholdDist' - Threshold distance between model and all
    %                   points. Default is 0.5.
    %            'MinNpt' - Min number of points in solution.
    %                   Default is 5.
    % Output : - A structure with the following fields:
    %            .Found - if false, then this is the only available field.
    %            .FlagGoodPt - Flag of selected points in best fit.
    %            .Npt - Number of points in best fit.
    %            .Par - Parameters of best fit.
    %            .Resid - Residuals of all points.
    %            .RMS - RMS of points in best fit.
    % Author : Eran Ofek (Feb 2022)
    % Example: X = [1 2 3 4 5 6, rand(1,30).*1000]';
    %          Y = [1 2 3 4 5 6, rand(1,30).*1000]';
    %          Result = tools.math.fit.ransacLinear([X,Y]);
    
    arguments
        Data
        Args.NptFit           = 3;
        Args.Ntrial           = 20;
        Args.MinRMS           = 0.5;
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
    
        Hall         = [ones(Ndata,1), X];
        
        % generate a random selection of K out of N
        SimInd = zeros(Args.NptFit, Args.Ntrial);
        Itrial = 0;
        Found  = false;
        while Itrial<Args.Ntrial && ~Found
            Itrial = Itrial + 1;
            % generate NptFit unique times indices
            Ind = randperm(Ndata, Args.NptFit);
            
            H = [ones(Args.NptFit,1), X(Ind)];
            
            Par   = H\Y(Ind);
            Resid = Y(Ind) - H*Par;
            RMS   = std(Resid);
            
            if RMS<Args.MinRMS
                % check all points
                ResidAll = Y - Hall*Par;
                
                FlagGoodPt = abs(ResidAll)<Args.ThresholdDist;
                
                if sum(FlagGoodPt)>Args.MinNpt
                    % min. number of points found
                    
                    % store solution
                    Found             = true;
                    Result.Found      = true;
                    Result.FlagGoodPt = FlagGoodPt;
                    Result.Npt        = sum(FlagGoodPt);
                    Result.Par        = Hall(FlagGoodPt,:)\Y(FlagGoodPt);
                    ResidAll          = Y - Hall*Par;
                    Result.Resid      = ResidAll;
                    Result.RMS        = std(ResidAll(FlagGoodPt));
                    
                end
            end
            
        end
        
    end
    
    
    
end