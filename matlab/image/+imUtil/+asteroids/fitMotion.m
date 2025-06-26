function [Result] = fitMotion(JD, RA, Dec, Args)
    % Fit small-angle motion on sky
    % Input  : - A column vector of JD.
    %          - A matrix of RA observations. Column per object.
    %          - A matrix of Dec observations. Column per object.
    %          * ...,key,val,...
    %            'CooUnits' - Units of input coordinates. Default is 'deg'.
    %            'OutCooUnits' - Units of output coordinates.
    %                   Default is 'deg'.
    %            'StdCooUnits' - Units of fit std. Default is 'arcsec'.
    % Output : - A structure with the following fields:
    %            .RefJD  - Reference JD.
    %            .FitRA  - Best Fit RA at RefJD.
    %            .FitDec - Best Fit Dec at RefJD.
    %            .MuRA   - Best fit RA motion.
    %            .MuDec  - Best fit Dec motion.
    %            .StdMuRA  - RMS of RA fit.
    %            .StdMuDec - RMS of Dec fit.
    %            .FunRA    - Function for RA propagation at JD
    %                   .FunRA(Result, JD)
    %            .FunDec   - Function for Dec propagation at JD
    %                   .FunDec(Result, JD)
    % Author : Eran Ofek (2025 Jun) 
    % Example: T=(1:5)';R=imUtil.asteroids.fitMotion(T, 1+0.001.*T+randn(5,1).*1e-4, 1+0.001.*T+randn(5,1).*1e-5)
    %          R=imUtil.asteroids.fitMotion(T, [1 0]+0.001.*T, [1 0]+0.001.*T)

    arguments
        JD
        RA
        Dec
        Args.CooUnits     = 'deg';
        Args.OutCooUnits  = 'deg';
        Args.StdCooUnits  = 'arcsec';
    end

    %JD  = JD(:).';
    %RA  = RA(:).';
    %Dec = Dec(:).';
    N   = numel(JD);

    Factor = convert.angular(Args.CooUnits, 'rad');
    RA     = RA.*Factor;
    Dec    = Dec.*Factor;

    [CD1,CD2,CD3]=celestial.coo.coo2cosined(RA, Dec);

    RefJD = median(JD);
    Time  = JD - RefJD;

    H    = [ones(N,1), Time];
    Par1 = H\CD1;
    Par2 = H\CD2;
    Par3 = H\CD3;

    Resid1 = CD1 - H*Par1;
    Resid2 = CD2 - H*Par2;
    Resid3 = CD3 - H*Par3;

    StdResid1 = std(Resid1);
    StdResid2 = std(Resid2);
    StdResid3 = std(Resid3);

    %[Par1, N1] = polyfit(Time, CD1, 1);
    %[Par2, N2] = polyfit(Time, CD2, 1);
    %[Par3, N3] = polyfit(Time, CD3, 1);
    [MeanRA, MeanDec] = celestial.coo.cosined2coo(Par1(1,:), Par2(1,:), Par3(1,:));

    % Basis vectors
    Nra     = numel(MeanRA);
    E_alpha = [-sin(MeanRA); cos(MeanRA); zeros(1,Nra)];
    E_delta = [-cos(MeanRA).*sin(MeanDec); -sin(MeanRA).*sin(MeanDec); cos(MeanDec)];

    % Motion vector
    Dr = [Par1(2,:); Par2(2,:); Par3(2,:)];
    Derr = [StdResid1; StdResid2; StdResid3];

    % Projections
    Factor = convert.angular('rad',Args.OutCooUnits);
    Result.RefJD    = RefJD;
    Result.FitRA    = MeanRA.*Factor;
    Result.FitDec   = MeanDec.*Factor;
    Result.MuRA     = dot(Dr, E_alpha).*Factor;
    Result.MuDec    = dot(Dr, E_delta).*Factor;
    Factor = convert.angular('rad',Args.StdCooUnits);
    Result.StdMuRA  = dot(Derr, E_alpha).*Factor;
    Result.StdMuDec = dot(Derr, E_delta).*Factor;

    Result.FunRA  = @(Result, JD) Result.FitRA + Result.MuRA.*(JD - Result.RefJD);
    Result.FunDec = @(Result, JD) Result.FitDec + Result.MuDec.*(JD - Result.RefJD);


end
