function [Result] = bls(Time, Mag, FreqVec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 May) 
    % Example: timeSeries.period.bls();

    arguments
        Time                   = [];
        Mag                    = [];
        FreqVec                = 1./3.1; %[];
        Args.MagErr            = [];
        Args.EclipseTimeVec    = logspace(0,log10(400), ceil(log10(400).*4))./1440;  % eclipse length [ossibilities
        Args.OverPhase         = 3;
        Args.MinNinEc          = 3;
        Args.freqVecArgs       = {};
        Args.DimEpoch          = 1;
        

    end


    if isempty(Time)
        % simulation mode
        Time = rand(1000,1).*100;
        Mag  = randn(size(Time)).*0.01;
        Period = 3.1;
        TimeFreq = Time./Period;
        Phase    = TimeFreq - floor(TimeFreq);
        Flag     = Phase>0.1 & Phase<0.15;
        Mag(Flag) = Mag(Flag) + 0.1;
    end

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
    Result.BestZ    = zeros(Nfreq, Nec);
    Result.BestPh   = zeros(Nfreq, Nec);
    Result.BestN    = zeros(Nfreq, Nec);
    Result.BestChi2 = zeros(Nfreq, Nec);
    Result.FreqVec  = FreqVec;
    Result.Chi2_0   = Chi2_0;
    
    for Ifreq=1:1:Nfreq
        %[Ifreq, Nfreq]
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

                BestZ       = Inf;
                BestChi2    = Inf;
                BestPh      = NaN;
                NinEc       = NaN;
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
                    EclipseStdMag = tools.math.stat.rstd(MagFlag, 1);

                    CurrZ = (EclipseMedMag - MedMag)./sqrt(StdMag.^2+EclipseStdMag.^2);

                    Chi2  = sum(((Mag(~Flag,:) - MedMag)./StdMag).^2, 1, 'omitnan') + sum(((MagFlag - EclipseMedMag)./EclipseStdMag).^2, 1, 'omitnan'); 

                    Nin   = sum(Flag);
                    if Chi2<BestChi2 && Nin>=Args.MinNinEc
                        BestChi2 = Chi2;
                        BestZ    = CurrZ;
                        BestPh   = MidPhaseVec(Iph);
                        NinEc    = Nin;
                    end

                end

                Result.BestZ(Ifreq, Iec)    = BestZ;
                Result.BestPh(Ifreq, Iec)   = BestPh;
                Result.BestN(Ifreq, Iec)    = NinEc;
                Result.BestChi2(Ifreq, Iec) = BestChi2;

            end
        end
         
    end


end
