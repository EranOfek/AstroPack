function asteroidSizeDistribution
    %

    % N(>D) ~ D^-q
    % N(<H) ~ 10^(alpha *H)
    q = 5.*alpha+1;

    Fun = @(D,q) D.^-q;

    Ind = 0;
    % 
    Ind = Ind + 1;
    PL(Ind).MinD = 5;
    PL(Ind).MaxD = 950;
    PL(Ind).Alpha = 0.61;
    Ind = Ind + 1;
    PL(Ind).MinD = 0.8;
    PL(Ind).MaxD = 5;
    PL(Ind).Alpha = 0.37;
    Ind = Ind + 1;
    PL(Ind).MinD = 0.34;
    PL(Ind).MaxD = 0.8;
    PL(Ind).Alpha = 0.23;
    % extrapolated:
    Ind = Ind + 1;
    PL(Ind).MinD = 0.001;
    PL(Ind).MaxD = 0.34;
    PL(Ind).Alpha = 0.23;

    N = numel(PL);
    for I=1:1:N
        PL(I).q = 5.*PL(I).Alpha + 1;
    end

    I = 1;
    PL(I).Norm     = Fun(PL(I).MaxD,PL(I).q)
    PL(I).AncorLow = Fun(PL(I).MinD,PL(I).q)./PL(I).Norm
    for I=2:1:N
        PL(I).Norm    = PL(I-1).NormLow;
        PL(I).NormLow = Fun(PL(I).MinD,PL(I).q)./PL(I).Norm
    end


end