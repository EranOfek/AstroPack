function MdotCgs = accDisk_convertMdot(Mcgs, Mdot, MdotUnits, Eta)
    % Convert a mass accretion rate to cgs units [g/s]. Vectorized.
    % Package: astro.accretion
    % Description: Internal utility converting an accretion rate given
    %              in one of several common conventions into g/s. All
    %              numeric inputs may be scalars, vectors, matrices or
    %              N-D arrays; they are combined by ordinary implicit
    %              expansion (broadcasting), so e.g. an Nx1 Mcgs and a
    %              1xM Mdot return an NxM result.
    % Input  : - Mcgs      : BH mass [g].
    %          - Mdot      : Accretion rate, in the units given by
    %                        MdotUnits.
    %          - MdotUnits : One of "gs" | "MsunYr" | "Edd" (scalar -
    %                        applies uniformly to every element).
    %          - Eta       : Radiative efficiency (used only if
    %                        MdotUnits == "Edd"; Mdot is then the
    %                        Eddington ratio, dimensionless). Default is 0.1.
    % Output : - MdotCgs : Accretion rate [g/s], broadcast size of
    %                      Mcgs, Mdot and Eta.
    % Author : (fill in your name) (Sep 2026)
    % Example: MdotCgs = astro.accretion.accDisk_convertMdot(1e8*1.98847e33, 0.1, "Edd", 0.1);
    %          MdotCgs = astro.accretion.accDisk_convertMdot([1e7;1e8;1e9]*1.98847e33, [0.01 0.1 1], "Edd", 0.1); % 3x3 grid

    arguments
        Mcgs double {mustBePositive}
        Mdot double {mustBePositive}
        MdotUnits (1,1) string {mustBeMember(MdotUnits, ["gs","MsunYr","Edd"])}
        Eta double {mustBePositive} = 0.1
    end

    Const = astro.accretion.accDisk_physConst();

    switch MdotUnits
        case "gs"
            MdotCgs = Mdot;
        case "MsunYr"
            MdotCgs = Mdot .* Const.SunM ./ Const.Yr;
        case "Edd"
            LEdd    = 4.*pi.*Const.G.*Mcgs.*Const.mp.*Const.c ./ Const.sigmaT;
            MdotEdd = LEdd ./ (Eta.*Const.c.^2);
            MdotCgs = Mdot .* MdotEdd;
    end
end
