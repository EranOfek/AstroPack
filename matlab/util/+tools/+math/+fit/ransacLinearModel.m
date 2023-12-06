function [FlagGood, BestPar, BestStd] = ransacLinearModel(H, Y, Args)
    % Fit a general linear model using a simplified RANSAC-like scheme
    % Input  : - Design matrix of linear model [f(x), g(x), h(x),...]
    %          - Vector of observables.
    %          * ...,key,val,...
    %            'Nsim' - Number of simulations. Default is 100.
    %            'FracPoints' - Fraction of points to use in each
    %                   simulation. Default is 0.1.
    %            'NptSim' - If not [], this overrides 'FracPoints'.
    %                   Specify how many points to use in each simulation.
    %                   Default is [].
    %            'CleanNaN' - Clean NaNs before fitting.
    %                   Default is false.
    %            'NsigmaClip' - Sigma clipping in good points selection.
    %                   Default is [3 3].
    % Output : - A vector of logicals indicating good data points.
    %          - Vector of best fit parameters, using all good data points.
    %          - Vector of best std, using all good data points.
    % Author : Eran Ofek (Sep 2022)
    % Example: [FlagGood, BestPar, BestStd] = tools.math.fit.ransacLinearModel(H, Y, Args)
    
    arguments
        H
        Y
        Args.Nsim               = 100;
        Args.FracPoints         = 0.1;
        Args.NptSim             = []
        Args.CleanNaN logical   = false;
        Args.NsigmaClip         = [3 3];
    end
    
    if Args.CleanNaN
        Flag = ~isnan(Y) & all(~isnan(H),2);
        H    = H(Flag,:);
        Y    = Y(Flag);
    end
    
    Npt = numel(Y);
    if isempty(Args.NptSim)
        NptSim = ceil(Npt.*Args.FracPoints);
    else
        NptSim = Args.NptSim;
    end
    
    PrevStd = Inf;
    for Isim=1:1:Args.Nsim
        IndRand = randi([1 Npt], NptSim, 1);
        Par     = H(IndRand,:)\Y(IndRand);
        Resid   = Y(IndRand) - H(IndRand,:)*Par;
        Std     = std(Resid);
        if Std<PrevStd
            PrevStd     = Std;
            BestIndRand = IndRand;
            BestPar     = Par;
            BestStd     = Std;
        end
    end
    
    
    % remove outliers
    Resid   = Y - H*BestPar;
    FlagGood = Resid<(BestStd.*Args.NsigmaClip(2)) & Resid>(-BestStd.*Args.NsigmaClip(1));
    
    BestPar = H(FlagGood,:)\Y(FlagGood);
    Resid   = Y(FlagGood) - H(FlagGood,:)*BestPar;
    BestStd = std(Resid);
    
end
