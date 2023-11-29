function [PS]=periodFoldedEventsBeaming(T, FreqVec, Args)
    % Search for beaming periodicity in time tagged and energy tagged photon list.
    %   Given time (T) of photons, their energy (Args.Energy) and
    %   background photons energy (Args.BckEnergy), for each trial period
    %   fold the data and fit a sine wave form in energy and phase bins.
    %   The energy bins are weighted according to the expected beamin
    %   formula (e.g., Eq. 3 in Zucker et al. 2007).
    %   Alpha = dLog(Fnu)/dLog(Nu)
    %   Also weight the fitting by the expected flux in the energy bin
    %   divided by the expected flux in the background energy bin.
    % Input  : - A vector of time tagged events
    %          - A vector of trial frequncies to test.
    %          * ...,key,val,...
    %            'Energy' - Vector of energies corresponding to the Time.
    %                   Must provided.
    %                   Default is [].
    %            'BckEnergy' - Vector of energy of background photons.
    %                   Must provided. Default is [].
    %            'EdgesEnergy' - Energy bins edges.
    %                   Default is (200:200:6000).
    %            'BinSize' - Bin size in phase folding. Default is 0.1.
    % Output : - A column matrix of [Freq, Amplitude, Mean, A, B]
    %          - A column matrix of [Freq, Amplitude, Mean, A, B] for the
    %            additional values supplied in 'ExtraVal'.
    % Author : Eran Ofek (Oct 2023)
    % Example: PS=timeSeries.period.periodFoldedEventsBeaming(X.Time, FreqVec, 'Energy',X.Energy, 'BckEnergy',X.BckEnergy);
    
    arguments
        T                    = [];
        FreqVec              = [];
        Args.Energy          = [];
        Args.BckEnergy       = [];
        Args.BinSize         = 0.1;
        Args.EdgesEnergy     = (200:200:6000);
        Args.Verbose logical = false;
    end
    
    if isempty(T)
        % simulation mode

        FreqVec = (0:0.00005:0.1);
        %PS = timeSeries.period.periodPoisson(TT,FreqVec);
    %
        Time     = (1:1:1e5)';
        Lambda0  = 0.1;
        A        = 0.1;
        FreqPeak = 0.01;
        LambdaT  = Lambda0.*(1 + A.*sin(2.*pi.*FreqPeak.*Time));
        Cnt      = poissrnd(LambdaT);
        FlagT    = Cnt>0;
        T       = Time(FlagT);
        %numel(T)    

    end
    
    T = T(:) - min(T);
    FreqVec = FreqVec(:).';
    Nfreq   = numel(FreqVec);


    %Edges   = (0:Args.BinSize:1);
    %MidBin  = (Edges(1:end-1) + Edges(2:end)).*0.5;
    
    %Nbin = numel(MidBin);
    %H = [ones(Nbin,1), sin(2.*pi.*MidBin(:)), cos(2.*pi.*MidBin(:))];

    EdgesPhase  = (0:Args.BinSize:1);
    BinPhase    = ((EdgesPhase(1:end-1) + EdgesPhase(2:end)).*0.5).';
    

    EdgesEnergy = Args.EdgesEnergy(:).';
    BinEnergy   = (EdgesEnergy(1:end-1) +EdgesEnergy(2:end)).*0.5;
    
    BinPhaseMat = repmat(BinPhase,1, numel(BinEnergy));

    % calculate the Alpha weight
    NphInBin         = histcounts(Args.Energy, EdgesEnergy);
    NphBckInBin      = histcounts(Args.BckEnergy, EdgesEnergy);

    %NbckInBin        = histcounts(X.BckEnergy(Flag),Edges);
    EnergyInBin      = NphInBin.*BinEnergy;
    [Alpha] = tools.deriv.numerical_deriv(log(BinEnergy), log(EnergyInBin));
    Nbins   = numel(BinEnergy).*numel(BinPhase);
    AlphaVec = repmat(Alpha,[numel(BinPhase),1]);
    AlphaVec = AlphaVec(:);

    PS      = [FreqVec(:), zeros(Nfreq,7)];
    for Ifreq=1:1:Nfreq
        if Args.Verbose
            if Ifreq./1000==floor(Ifreq./1000)
                [Ifreq, Nfreq]
            end
        end
        TF      = T.*FreqVec(Ifreq);
        % Vector of phases for trial frequency
        PhaseTF = (TF - floor(TF));
       
        % bin
        %N = histcounts(PhaseTF, Edges);
        %N = matlab.internal.math.histcounts(PhaseTF, Edges);
        %PS(Ifreq,2) = std(N)./sqrt(mean(N));
        

        %%

       

        Nph = histcounts2(PhaseTF, Args.Energy, EdgesPhase, EdgesEnergy);
        E   = Nph.*BinEnergy;  % energy per bin

        %NphB = histcounts2(PhaseTF, Args.BckEnergy, EdgesPhase, EdgesEnergy);
        Weight = Nph./NphBckInBin;

        H = [ones(Nbins,1), (3-AlphaVec(:)).*sin(2.*pi.*BinPhaseMat(:)), (3-AlphaVec(:)).*cos(2.*pi.*BinPhaseMat(:))];
        [Par, ParErr] = lscov(H, E(:), Weight(:));
        Resid         = E(:) - H*Par;
        % Plot as a function of phase
        % semilogy(BinPhaseMat(:), E(:),'.')
        % plot(BinPhaseMat(:), Resid(:),'.')

        Amp = sqrt(Par(2).^2 + Par(3).^2);
        PS(Ifreq,2:8) = [Amp,  Par(1), Par(2), Par(3), ParErr(1), ParErr(2), ParErr(3)];
        % Since the amplitude depands on the number of photons this must be
        % normalized by the response function (window function)
        
    end
    
    
    
end
