function T = accDisk_diskTemperature(M_BH, Mdot, R, Args)
    % Shakura & Sunyaev (1973) thin-disk effective temperature profile. Vectorized.
    % Package: astro.accretion
    % Description: Computes the local effective temperature T(R) of a
    %              geometrically thin, optically thick, steady-state
    %              accretion disk:
    %                 T(R) = [3 G M Mdot / (8 pi sigma R^3) *
    %                         (1 - sqrt(Rin/R))]^(1/4)
    %              T is set to 0 for R <= Rin.
    %
    %              Fully vectorized: M_BH, Mdot, R, 'Rin' and 'Eta' may
    %              each be a scalar, vector, matrix or N-D array. They
    %              are combined by ordinary implicit expansion
    %              (broadcasting) - e.g. an Nx1 M_BH together with a
    %              1xM R returns an NxM grid of temperatures, one per
    %              (mass, radius) pair, with no loop required.
    % Input  : - M_BH : BH mass [Solar masses].
    %          - Mdot : Accretion rate [units set by 'MdotUnits'].
    %          - R    : Radius (radii) at which to evaluate T. Units set
    %                   by 'Units' (default: cm).
    %          * ...,key,val,...
    %            'MdotUnits' - "gs" | "MsunYr" | "Edd" (scalar). Default is "MsunYr".
    %            'Eta'       - Radiative efficiency (used if MdotUnits=="Edd").
    %                          Scalar or array. Default is 0.1.
    %            'Rin'       - Inner disk radius, in units of the
    %                          gravitational radius Rg = G*M/c^2.
    %                          Scalar or array. Default is 6 (Schwarzschild ISCO).
    %            'Units'     - "cm" | "Rg" - units of the input R (scalar).
    %                          Default is "cm".
    % Output : - T : Local effective temperature [K]. Size is the common
    %                broadcast size of M_BH, Mdot, R, 'Rin' and 'Eta'.
    % Author : (fill in your name) (Sep 2026)
    % Example: T = astro.accretion.accDisk_diskTemperature(1e8, 0.1, logspace(13,17,50), 'MdotUnits','Edd');
    %          % Grid over BH mass (3x1) and radius (1x50) -> 3x50 matrix:
    %          T = astro.accretion.accDisk_diskTemperature([1e7;1e8;1e9], 0.1, logspace(13,17,50), 'MdotUnits','Edd');

    arguments
        M_BH double {mustBePositive}
        Mdot double {mustBePositive}
        R double {mustBePositive}
        Args.MdotUnits (1,1) string {mustBeMember(Args.MdotUnits, ["gs","MsunYr","Edd"])} = "MsunYr"
        Args.Eta double {mustBePositive} = 0.1
        Args.Rin double {mustBePositive} = 6
        Args.Units (1,1) string {mustBeMember(Args.Units, ["cm","Rg"])} = "cm"
    end

    Const   = astro.accretion.accDisk_physConst();
    Mcgs    = M_BH .* Const.SunM;
    MdotCgs = astro.accretion.accDisk_convertMdot(Mcgs, Mdot, Args.MdotUnits, Args.Eta);
    Rg      = Const.G .* Mcgs ./ Const.c.^2;
    RinCgs  = Args.Rin .* Rg;

    if Args.Units == "Rg"
        Rcgs = R .* Rg;
    else
        Rcgs = R;
    end

    Prefactor = 3.*Const.G.*Mcgs.*MdotCgs ./ (8.*pi.*Const.sigma);

    % Broadcast Rcgs, RinCgs and Prefactor to a common grid size before
    % the masked assignment below, so that logical indexing lines up
    % between them even when e.g. R and M_BH arrived with different
    % (but mutually broadcastable) shapes.
    [SzOut, RcgsF, RinCgsF, PrefactorF] = astro.accretion.accDisk_broadcast(Rcgs, RinCgs, Prefactor);

    T    = zeros(SzOut);
    Mask = RcgsF > RinCgsF;
    T(Mask) = (PrefactorF(Mask) ./ RcgsF(Mask).^3 .* (1 - sqrt(RinCgsF(Mask)./RcgsF(Mask)))).^(1/4);
end
