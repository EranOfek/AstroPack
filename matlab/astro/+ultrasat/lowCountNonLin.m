function S = lowCountNonLin(S, Par)
    % Apply a low-count detector non-linearity (charge trapping) to a charge image
    % Description: At low signal levels a fraction of the collected electrons is
    %              trapped and not read out. This function maps the collected charge
    %              per pixel per frame to the charge that is read out. It is meant to
    %              be passed to ultrasat.usim as 'LowCountNonLin', @ultrasat.lowCountNonLin
    %              with the model parameters in 'LowCountNonLinPar'.
    %              PLACEHOLDER: the model form is not chosen yet, so the charge is
    %              returned unchanged.
    % Input  : - S: an array of collected charge [e-/pix in one frame]
    %          - Par: a vector of model parameters. Not used by the placeholder.
    %            Default is [].
    % Output : - S: the read-out charge [e-/pix in one frame], same size as the input
    % Author : A.M. Krassilchtchikov (Oct 2026)
    % Example: S = ultrasat.lowCountNonLin(100.*ones(10), []);
    arguments
        S
        Par = [];  %#ok<INUSA> the placeholder ignores the parameters
    end
end
