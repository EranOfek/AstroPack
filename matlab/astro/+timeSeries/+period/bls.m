function [Result,Time,Mag] = bls(Time, Mag, FreqVec, Args)
    % Box-least square fit.
    % Input  : - Vector of times.
    %          - Matrix of magnitude (Epoch X Src).
    %          - Vector of frequencies to test. If empty, then use
    %            timeSeries.period.getFreq to generate frequency vector.
    %            Default is [].
    %          * ...,key,val,...
    %            'MagErr' - Default is []. NOT OPERATIONAL.
    %            'EclipseTimeVec' - Vector of eclipse time length to test.
    %                   Default is logspace(0,log10(600), ceil(log10(600).*4))./1440;
    %            'OverPhase' - Factor of oversampling in phase (i.e., the
    %                   phase step is the eclipse length divided by OverPhase).
    %                   Default is 3.
    %            'MinNinEc' - Minumum number of events in eclipse, in order
    %                   to calculate statistics.
    %                   Default is 3.
    %            'freqVecArgs' - A cell array of additional arguments to
    %                   pass to timeSeries.period.getFreq
    %                   Default is {'MinFreq',0.1, 'MaxFreq',1}
    %            'DimEpoch' - Dim of epoch. Default is 1.
    %            'UseRStd' - Use RStd for eclipse std calculation.
    %                   Default is false.
    %            'Verbose' - Default is false.
    %            
    % Output : - A structure containing the following fields.
    %            Cubes has: Nsrc X Nfreq X Neclipse_length size:
    %            .DeltaChi2 - Chi2_0 - BestChi2
    %            .BestChi2 - Matrix with Chi2 for each fit.
    %            .BestZ -
    %            .BestPh
    %            .BestN
    %            .Amplitude
    %            
    %            Matrices:
    %            .Nphase - Number of phases tested for aech frequency and each eclipse-length. 
    %
    %            Vectors:
    %            .FreqVec - The input FreqVec.
    %            .EclipseTimeVec - The input EclipseTimeVec
    %            .Chi2_0 - Vector of null hypothesis \chi^2 for each src.
    %            .MaxDeltaChi2 - Max. delta chi^2 per source.
    %
    %            Structure:
    %            .EclipsePhaseLength - struct array of eclipse phase length
    %                   for each seached frequency.
    %
    %            Functions:
    %            plot(Result, SrcInd)
    %            plotFold(Time, Mag, SrcInd, Period)
    %          - The input Time vector.
    %          - The input magnitude matrix.
    %
    % Author : Eran Ofek (2025 May) 
    % Example: [R,Time,Mag] = timeSeries.period.bls();
    %          plot(R.FreqVec, max(R.Chi2_0(1)-squeeze(R.BestChi2(1,:,:)),[],2) )
    %          plot(R.FreqVec, max(R.Chi2_0(1)-squeeze(R.BestChi2(1,:,:)),[],2)./sqrt(sum(squeeze(R.BestN(1,:,:)),2)) )
    %          or
    %          R.plot(R, 1); % where 1 is src index
    %          R.plotFold(Time, Mag, 1, 3.1)

    arguments
        Time                   = [];
        Mag                    = [];
        FreqVec                = []; %1./3.1; %[];
        Args.MagErr            = [];
        Args.EclipseTimeVec    = logspace(0,log10(600), ceil(log10(600).*4))./1440;  % eclipse length [ossibilities
        Args.OverPhase         = 3;
        Args.MinNinEc          = 3;
        Args.freqVecArgs       = {'MinFreq',0.1, 'MaxFreq',1};
        Args.DimEpoch          = 1;
        Args.UseRStd           = false;
        
        Args.Verbose           = false;
    end


    if isempty(Time)
        % simulation mode
        Npt = 3e3;
        Time = rand(Npt,1).*100;
        Mag  = randn(size(Time)).*0.01;
        Period = 3.1;
        TimeFreq = Time./Period;
        Phase    = TimeFreq - floor(TimeFreq);
        Flag     = Phase>0.1 & Phase<0.12;
        Mag(Flag) = Mag(Flag) + 0.1;

        Mag       = [Mag, Mag]; %, Mag, Mag, Mag];
    end

    [~  , Nsrc] = size(Mag);

    Nec = numel(Args.EclipseTimeVec);  % Number of eclipse length to test
    %Nph = ceil(Nec.*Args.OverPhase);   % Number of phase positions to test
    %PhasePos = (0:1./Nph:1-1./(Args.OverPhase.*Nph));

    if isempty(FreqVec)
        FreqVec = timeSeries.period.getFreq(Time, Args.freqVecArgs{:});
    end

    if Args.DimEpoch==2
        Mag = Mag.';
        Args.MagErr = Args.MagErr.';
    end
    Time = Time(:);

    MedMag = median(Mag, 1, 'omitnan');
    StdMag = tools.math.stat.rstd(Mag, 1);
    Chi2_0 = sum(((Mag - MedMag)./StdMag).^2, 1, 'omitnan');

    Period = 1./FreqVec;
    if isempty(Args.MagErr)
        MagErr = StdMag;
    else
        if size(Mag,1)>1 && size(Mag,2)>1
            error('Vector MagErr is possible only for a single light curve')
        end
        MagErr = Args.MagErr;
    end

    Nfreq = numel(FreqVec);
    %Result.BestZ     = nan(Nsrc, Nfreq, Nec);
    Result.BestPh    = nan(Nsrc, Nfreq, Nec);
    Result.BestN     = nan(Nsrc, Nfreq, Nec);
    Result.BestChi2  = nan(Nsrc, Nfreq, Nec);
    Result.Amplitude = nan(Nsrc, Nfreq, Nec);
    Result.Nphase    = nan(Nfreq, Nec);
    Result.FreqVec   = FreqVec;
    Result.EclipseTimeVec = Args.EclipseTimeVec;
    Result.Chi2_0    = Chi2_0;
    Result.plot      = @(R, Ind) plot(R.FreqVec, max(R.Chi2_0(Ind)-squeeze(R.BestChi2(Ind,:,:)),[],2),'k-');
    Result.plotFold  = @(Time, Mag, Ind, Period) plot( Time./Period - floor(Time./Period), Mag(:,Ind), '.');

    for Ifreq=1:1:Nfreq
        if Args.Verbose
            [Ifreq, Nfreq]
        end

        TimeF = Time.*FreqVec(Ifreq);
        Phase = TimeF - floor(TimeF); 

        EclipsePhaseLength = Args.EclipseTimeVec.*FreqVec(Ifreq);

        Result.EclipsePhaseLength(Ifreq).Length = EclipsePhaseLength;
        for Iec=1:1:Nec
            if EclipsePhaseLength(Iec)>1
                % skip
            else
                PhaseStep   = EclipsePhaseLength(Iec)./Args.OverPhase;
                MidPhaseVec = (0:PhaseStep:1-PhaseStep);
                Nph         = numel(MidPhaseVec);

                BestZ       = Inf.*ones(1,Nsrc);
                BestChi2    = Inf.*ones(1,Nsrc);
                BestPh      = NaN.*ones(1,Nsrc);
                NinEc       = NaN.*ones(1,Nsrc);
                Amp         = NaN.*ones(1,Nsrc);
                for Iph=1:1:Nph
                   
                    Ph1 = MidPhaseVec(Iph) - EclipsePhaseLength(Iec).*0.5;
                    Ph2 = MidPhaseVec(Iph) + EclipsePhaseLength(Iec).*0.5;

                    if Ph1>=0 && Ph2<=1
                        Flag = Phase>Ph1 & Phase<=Ph2;
                    elseif Ph1>=0 && Ph2>1
                        Flag = Phase>Ph1 | Phase<=(Ph2-1);
                    elseif Ph1<0 && Ph2<=1
                        Flag = Phase<Ph2 | Phase>=(1+Ph1);
                    else
                        error('Phase option problem');
                    end

                    MagFlag = Mag(Flag,:);
                    EclipseMedMag = median(MagFlag, 1, 'omitnan');
                    
                    if Args.UseRStd
                        EclipseStdMag = tools.math.stat.rstd(MagFlag, 1);                  
                    else
                        EclipseStdMag = std(MagFlag, [], 1, 'omitnan');                  
                    end

                    %CurrZ = (EclipseMedMag - MedMag)./sqrt(StdMag.^2+EclipseStdMag.^2);

                    %Chi2  = sum(((Mag(~Flag,:) - MedMag)./StdMag).^2, 1, 'omitnan') + sum(((MagFlag - EclipseMedMag)./EclipseStdMag).^2, 1, 'omitnan'); 
                    EclipseStdMag = max(EclipseStdMag, StdMag);
                    Chi2  = sum(((Mag(~Flag,:) - MedMag)./StdMag).^2, 1, 'omitnan') + sum(((MagFlag - EclipseMedMag)./EclipseStdMag).^2, 1, 'omitnan'); 

                    Nin   = sum(~isnan(MagFlag), 1);
                    for Isrc=1:1:Nsrc
                        if Chi2(Isrc)<BestChi2(Isrc) && Nin(Isrc)>=Args.MinNinEc
                            BestChi2(Isrc) = Chi2(Isrc);
                            %BestZ(Isrc)    = CurrZ(Isrc);
                            BestPh(Isrc)   = MidPhaseVec(Iph);
                            NinEc(Isrc)    = Nin(Isrc);
                            Amp(Isrc)      = MedMag(Isrc) - EclipseMedMag(Isrc);
                        end
                    end

                end

                %Result.BestZ(:, Ifreq, Iec)     = BestZ;
                Result.BestPh(:, Ifreq, Iec)    = BestPh;
                Result.BestN(:, Ifreq, Iec)     = NinEc;
                Result.BestChi2(:, Ifreq, Iec)  = BestChi2;
                Result.Amplitude(:, Ifreq, Iec) = Amp;
                Result.Nphase(Ifreq, Iec)       = Nph;

            end
        end
         
    end

    Result.DeltaChi2 = reshape(Result.Chi2_0, [Nsrc 1 1]) - Result.BestChi2;
    Result.MaxDeltChi2 = max(Result.DeltaChi2,[],[2 3]);  % max DeltaChi2 per source

end
