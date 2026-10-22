function S = lowCountNonLin(S, Par)
    % Apply a low-count detector non-linearity to a charge image
    % Description: At low signal the ULTRASAT lab devices read out less charge
    %              than they collect, and the shortfall is a roughly constant
    %              number of electrons once the signal is well above it. This
    %              function maps the collected charge per pixel per frame to the
    %              charge that is read out:
    %
    %                  Q_out = Q * exp(-D/Q)
    %
    %              a single parameter D [e-/pix/frame]. Its two limits are what
    %              make it a fair description of the measurements: at large Q it
    %              tends to Q - D, the constant deficit that a straight line
    %              through the photon-transfer curve reports as its intercept
    %              (the intercept in ADU is g*D), and below Q ~ D it rolls off
    %              smoothly to zero rather than clipping. Because the map is
    %              applied to the already-noisy charge, the variance follows
    %              automatically: Var_out = (dQ_out/dQ)^2 * Var_in, which is
    %              what bends the photon-transfer curve at low signal.
    %
    %              MEASURED VALUES (DESY campaign, die LT112181 W14 D1, 2026).
    %              D was fitted to the measured photon-transfer curve of eleven
    %              units over their whole bright ladder and reproduces it to
    %              3-9 % rms over 10-200 ADU. The seven shutter-stepped run
    %              pairs give D = 7-27 e-, median 15, which is the default
    %              here. The four INTERACTIVE runs of the same die at the same
    %              settings give D = 0.9-1.5 e-. The two disagree by an order of
    %              magnitude and the campaign does not say which is the device:
    %              the two kinds of run differ in what their bright ladder is
    %              referenced to and in how long the sensor integrates. A
    %              simulation should therefore state the D it assumed rather
    %              than treat it as a property of the detector, and is well
    %              advised to run both ends of that range.
    %
    %              Meant to be passed to ultrasat.usim as
    %              'LowCountNonLin', @ultrasat.lowCountNonLin with
    %              'LowCountNonLinPar', D.
    % Input  : - S: an array of collected charge [e-/pix in one frame]
    %          - Par: the deficit D [e-/pix/frame], a scalar. Default is 15,
    %            the median of the seven run pairs. Par = [] or 0 leaves the
    %            charge unchanged.
    % Output : - S: the read-out charge [e-/pix in one frame], same size as the
    %            input. Non-positive input charge is returned as 0: the map is
    %            not defined there and no charge is read out.
    % Author : A.M. Krassilchtchikov (Oct 2026)
    % Example: S = ultrasat.lowCountNonLin(100.*ones(10), 15);
    %          % the deficit at 100 e- is about 14 e-, at 20 e- about 10.5
    arguments
        S
        Par = 15;    % [e-] the measured median of the DESY run pairs
    end

    if isempty(Par)
        return;
    end
    D = Par(1);
    if ~isfinite(D) || D <= 0
        return;                       % D = 0 is the linear device
    end

    Pos    = S > 0;
    S(~Pos) = 0;
    S(Pos)  = S(Pos) .* exp(-D ./ S(Pos));
end
