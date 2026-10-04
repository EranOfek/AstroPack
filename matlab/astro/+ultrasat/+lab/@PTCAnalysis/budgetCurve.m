function S = budgetCurve(Q, In)
    % Per-pixel noise budget and SNR against charge, from plain scalars.
    %   Pure function of the measured quantities, so that noiseBudget and any
    %   later re-evaluation share one formula. All in electrons, 
    %     Qc              = Q - max(T, 0)          collected charge
    %     sigma_eff^2(Q)  = RN^2 + Qc + DC*t
    %                       + [(1-f)*sigma_off]^2  offset fixed pattern
    %                       + [(1-f)*sigma_DC*t]^2 DSNU
    %                       + [(1-f)*PRNU*Qc]^2    photo-response FPN
    %     SNR(Q)          = Qc / sigma_eff(Q)
    %   with f = 1 when the fixed patterns are calibrated out and f = 0 for a
    %   single raw frame.
    %   Only a POSITIVE threshold removes charge: the first T electrons are
    %   lost, so Qc = Q - T. A negative threshold means charge is present at
    %   zero intensity, which is a constant offset taken out by the bias and
    %   dark subtraction, not extra signal -- Qc = Q, and all that survives of
    %   it is its pixel-to-pixel spread, which is already the offset term.
    %   Clamping with max(Q-T,0) instead would let Qc exceed Q and hand the
    %   setups with a negative threshold an SNR they do not have.
    % Input  : - Charge grid [e-].
    %          - Structure with RN_e, DC_e, SigmaT_e, SigmaDC_e, PRNU,
    %            Threshold_e, ExpTime, SNRdet (missing or non-finite fields
    %            are taken as zero, except SNRdet which defaults to 5).
    % Output : - Structure with Q, Qc, Terms, SigmaEff_cal, SigmaEff_raw,
    %            SNR_cal, SNR_raw, Qlim_cal, Qlim_raw and the inputs used.
    % Example: S = ultrasat.lab.PTCAnalysis.budgetCurve(logspace(0,3,61), ...
    %                  struct('RN_e',1.8, 'DC_e',0.26, 'Threshold_e',30))
    arguments
        Q (1,:) double
        In struct
    end
    Get = @(N, D) local_get(In, N, D);
    RNe  = Get('RN_e', 0);
    DCe  = Get('DC_e', 0);
    Se   = Get('SigmaT_e', 0);
    SDCe = Get('SigmaDC_e', 0);
    PRNU = Get('PRNU', 0);
    Te   = Get('Threshold_e', 0);
    Tt   = Get('ExpTime', 0);
    Sdet = Get('SNRdet', 5);

    Qc = max(Q - max(Te, 0), 0);
    S  = struct('Q',Q, 'Qc',Qc, 'RN_e',RNe, 'DC_e',DCe, 'SigmaT_e',Se, ...
                'SigmaDC_e',SDCe, 'PRNU',PRNU, 'Threshold_e',Te, 'ExpTime',Tt, 'SNRdet',Sdet);
    S.Terms = struct('RN',RNe.^2 + 0.*Q, 'Shot',Qc, 'DarkShot',DCe.*Tt + 0.*Q, ...
                     'OffsetFPN',Se.^2 + 0.*Q, 'DSNU',(SDCe.*Tt).^2 + 0.*Q, 'PRNU',(PRNU.*Qc).^2);
    Base = S.Terms.RN + S.Terms.Shot + S.Terms.DarkShot;
    S.SigmaEff_cal = sqrt(Base);
    S.SigmaEff_raw = sqrt(Base + S.Terms.OffsetFPN + S.Terms.DSNU + S.Terms.PRNU);
    S.SNR_cal = Qc./S.SigmaEff_cal;
    S.SNR_raw = Qc./S.SigmaEff_raw;
    S.Qlim_cal = local_lim(S.SNR_cal, Q, Sdet);
    S.Qlim_raw = local_lim(S.SNR_raw, Q, Sdet);
end

function V = local_get(In, Name, Default)
    V = Default;
    if isfield(In, Name) && ~isempty(In.(Name)) && isfinite(In.(Name))
        V = In.(Name);
    end
end

function Ql = local_lim(Snr, Q, Sdet)
    % lowest Q at which the SNR crosses the detection level (log interpolation)
    Ql = NaN;
    Ix = find(Snr>=Sdet, 1, 'first');
    if isempty(Ix)
        return
    end
    if Ix==1
        Ql = Q(1);
    else
        Ql = exp(interp1(Snr([Ix-1 Ix]), log(Q([Ix-1 Ix])), Sdet, 'linear'));
    end
end
