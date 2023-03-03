function Result=fitPeriod(LC, Freq, Args)
    % fit a light curve with an harmonic (sin/cos) series
    % Input  : - A matrix of light curve, with [t, Mag, Err] columns.
    %          - A vector of frequencies to fit.
    %          * ...,key,val,...
    %            'ColT' - Time column index. Default is 1.
    %            'ColM' - Mag. column index. Default is 2.
    %            'ColE' - Err. column index. If empty, set error to 1.
    %                   Default is 3.
    %            'FitConst' - A logical indicating if to fit a constant.
    %                   Default is true.
    %            'Orders' - List of harmoinc orders to fit.
    %                   Default is [1 2 3].
    %            'RefT0' - Reference time to subtract before fitting.
    %                   Default is 0.
    % Output : - A structure array with entry per frequency and the
    %            following fields:
    %            .Freq - Frequency
    %            .Par  - Fitted parameters [sin(1...), cos(1...), sin(2...),... const] 
    %            .ParErr - Fitted error parameters.
    %            .A      - Amplitudes.
    %            .Phase  - Phases.
    % Author : Eran Ofek (Mar 2023)
    % Example: 
    
    arguments
        LC
        Freq
        Args.ColT               = 1;
        Args.ColM               = 2;
        Args.ColE               = 3;
        Args.FitConst logical   = true;
        Args.Orders             = [1 2 3];
        Args.RefT0              = 0;
    end
    
    T = LC(:,Args.ColT);
    M = LC(:,Args.ColM);
    N = numel(T);

    if isempty(Args.ColE)
        InvV = ones(N,1);
    else
        InvV = 1./(LC(:,Args.ColE).^2);
    end
    
    T = T - Args.RefT0;
    
    T2pi = 2.*pi.*T;
    
    Nord = numel(Args.Orders);
    H    = nan(N, Nord.*2); 
    
    if Args.FitConst
        Npar   = 1;
        Hconst = ones(N,1);
    else
        Npar   = 0;
        Hconst = ones(N,0);
    end
    H = [H, Hconst];
    
    Nfreq = numel(Freq);
    Result = struct('Freq',cell(Nfreq,1), 'Par',cell(Nfreq,1), 'ParErr',cell(Nfreq,1), 'A',cell(Nfreq,1), 'Phase',cell(Nfreq,1));
    for Ifreq=1:1:Nfreq
        for Iord=1:1:Nord
            Icol = (Iord-1).*2 + 1;
            H(:,Icol)   = sin(Args.Orders(Iord).*T2pi.*Freq(Ifreq));
            H(:,Icol+1) = cos(Args.Orders(Iord).*T2pi.*Freq(Ifreq));
        end
        
        [Par, ParErr] = lscov(H, M, InvV);
        
        Result(Ifreq).Freq    = Freq(Ifreq);
        Result(Ifreq).Par     = Par;
        Result(Ifreq).ParErr  = ParErr;
        Result(Ifreq).A       = zeros(Nord,1);
        Result(Ifreq).Phase   = zeros(Nord,1);
        
        for Iord=1:1:Nord
            Icol = (Iord-1).*2 + 1;
            Result(Ifreq).A(Iord)     = sqrt(Par(Icol).^2 + Par(Icol+1).^2);
            Result(Ifreq).Phase(Iord) = atan2(Par(Icol+1), Par(Icol));
        end
    end
    
    
end