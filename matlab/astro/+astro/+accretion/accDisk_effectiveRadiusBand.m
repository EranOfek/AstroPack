function Result = accDisk_effectiveRadiusBand(M_BH, Mdot, Lambda1, Lambda2, z, Args)
    % Flux-weighted effective disk radius for a filter passband. Vectorized.
    % Package: astro.accretion
    % Description: Computes the luminosity-weighted mean disk radius
    %              contributing to the observed flux in the passband
    %              [Lambda1, Lambda2] (Angstrom, observed frame):
    %                 <R> = Int R * dL_nu(R) dR / Int dL_nu(R) dR
    %              with
    %                 dL_nu(R) = 2*pi*R * Int_{nu1}^{nu2}
    %                            FilterFunc(lambda_obs) * B_nu(T(R)) dnu
    %              and [nu1,nu2] the rest-frame frequencies corresponding
    %              to [Lambda2,Lambda1] once redshifted by (1+z). Also
    %              returns the bracketing single-wavelength radii at
    %              Lambda1 and Lambda2, and a pivot-wavelength
    %              (Lambda_p = sqrt(Lambda1*Lambda2)) estimate.
    %
    %              Fully vectorized: M_BH, Mdot, Lambda1, Lambda2, z,
    %              'Rin' and 'Eta' may each be a scalar, vector, matrix
    %              or N-D array, combined by ordinary implicit expansion
    %              (broadcasting) into a common grid. Since every grid
    %              point requires its own numerical radial integration,
    %              this function loops internally over the (broadcast)
    %              grid - unlike accDisk_diskTemperature, which is
    %              loop-free, this one cannot avoid the loop, but the
    %              calling convention still accepts array inputs of any
    %              shape and returns array outputs of the same grid shape.
    % Input  : - M_BH    : BH mass [Solar masses].
    %          - Mdot    : Accretion rate [units set by 'MdotUnits'].
    %          - Lambda1 : Blue edge of the observed passband [Angstrom].
    %          - Lambda2 : Red edge of the observed passband [Angstrom].
    %          - z       : Redshift. Default is 0.
    %          * ...,key,val,...
    %            'MdotUnits'  - "gs" | "MsunYr" | "Edd" (scalar). Default is "MsunYr".
    %            'Eta'        - Radiative efficiency (if MdotUnits=="Edd").
    %                           Scalar or array. Default is 0.1.
    %            'Rin'        - Inner radius [Rg]. Scalar or array. Default is 6.
    %            'FilterFunc' - Function handle @(lambda_obs_Ang) -> [0,1]
    %                           giving the filter transmission at an
    %                           observed wavelength [Angstrom], applied
    %                           identically at every grid point. Default
    %                           is [] (top-hat over [Lambda1,Lambda2] at
    %                           each grid point).
    %            'Nr'         - Number of log-spaced radial grid points
    %                           per grid point (scalar). Default is 400.
    % Output : - Result : Struct, each field an array with the common
    %                     broadcast size of M_BH, Mdot, Lambda1, Lambda2,
    %                     z, 'Rin' and 'Eta':
    %              Reff_cm, Reff_Rg     - flux-weighted effective radius,
    %              R1_cm, R2_cm         - single-wavelength (Wien) radii
    %                                     at Lambda1, Lambda2,
    %              LambdaPivot          - sqrt(Lambda1*Lambda2),
    %              RPivot_cm, RPivot_Rg - radius at LambdaPivot,
    %              Rg, Rin_cm.
    % Author : (fill in your name) (Sep 2026)
    % Example: Result = astro.accretion.accDisk_effectiveRadiusBand(1e8, 0.1, 4000, 5500, 0.5, 'MdotUnits','Edd');
    %          % Grid over redshift (1x3) at fixed mass/Mdot -> 1x3 outputs:
    %          Result = astro.accretion.accDisk_effectiveRadiusBand(1e8, 0.1, 4000, 5500, [0 0.5 1], 'MdotUnits','Edd');

    arguments
        M_BH double {mustBePositive}
        Mdot double {mustBePositive}
        Lambda1 double {mustBePositive}
        Lambda2 double {mustBePositive}
        z double {mustBeNonnegative} = 0
        Args.MdotUnits (1,1) string {mustBeMember(Args.MdotUnits, ["gs","MsunYr","Edd"])} = "MsunYr"
        Args.Eta double {mustBePositive} = 0.1
        Args.Rin double {mustBePositive} = 6
        Args.FilterFunc = []
        Args.Nr (1,1) double {mustBePositive, mustBeInteger} = 400
    end

    Const = astro.accretion.accDisk_physConst();

    [SzOut, M_BHf, Mdotf, Lambda1f, Lambda2f, zf, Rinf, Etaf] = ...
        astro.accretion.accDisk_broadcast(M_BH, Mdot, Lambda1, Lambda2, z, Args.Rin, Args.Eta);

    if any(Lambda2f(:) <= Lambda1f(:))
        error('accDisk_effectiveRadiusBand:badRange', ...
            'Lambda2 must be greater than Lambda1 at every grid point.');
    end

    Reff_cm     = zeros(SzOut);
    Reff_Rg     = zeros(SzOut);
    R1_cm       = zeros(SzOut);
    R2_cm       = zeros(SzOut);
    LambdaPivot = zeros(SzOut);
    RPivot_cm   = zeros(SzOut);
    RPivot_Rg   = zeros(SzOut);
    Rg_out      = zeros(SzOut);
    Rin_cm_out  = zeros(SzOut);

    NPts = numel(Reff_cm);
    for Ik = 1:NPts
        Mk   = M_BHf(Ik);
        Mdk  = Mdotf(Ik);
        L1k  = Lambda1f(Ik);
        L2k  = Lambda2f(Ik);
        zk   = zf(Ik);
        Rink = Rinf(Ik);
        Etak = Etaf(Ik);

        if isempty(Args.FilterFunc)
            FilterFunck = @(L) double(L >= L1k & L <= L2k);
        else
            FilterFunck = Args.FilterFunc;
        end

        Mcgs_k   = Mk .* Const.SunM;
        Rg_k     = Const.G .* Mcgs_k ./ Const.c.^2;
        RinCgs_k = Rink .* Rg_k;

        NuLo = Const.c.*(1+zk) ./ (L2k.*1e-8);
        NuHi = Const.c.*(1+zk) ./ (L1k.*1e-8);

        R1cm_k = astro.accretion.accDisk_effectiveRadius(Mk, Mdk, L1k, zk, ...
            'MdotUnits',Args.MdotUnits, 'Eta',Etak, 'Rin',Rink);
        R2cm_k = astro.accretion.accDisk_effectiveRadius(Mk, Mdk, L2k, zk, ...
            'MdotUnits',Args.MdotUnits, 'Eta',Etak, 'Rin',Rink);

        Rmin_k  = max(RinCgs_k.*1.001, 0.1.*R1cm_k);
        Rmax_k  = 10.*R2cm_k;
        Rgrid_k = logspace(log10(Rmin_k), log10(Rmax_k), Args.Nr);
        Tgrid_k = astro.accretion.accDisk_diskTemperature(Mk, Mdk, Rgrid_k, ...
            'MdotUnits',Args.MdotUnits, 'Eta',Etak, 'Rin',Rink, 'Units','cm');

        DenR = zeros(size(Rgrid_k));
        for Ir = 1:numel(Rgrid_k)
            if Tgrid_k(Ir) <= 0
                continue
            end
            Ti  = Tgrid_k(Ir);
            Bnu = @(Nu) (2.*Const.h.*Nu.^3./Const.c.^2) ./ (exp(Const.h.*Nu./(Const.kB.*Ti)) - 1);
            Integrand = @(Nu) Bnu(Nu) .* FilterFunck((Const.c.*(1+zk)./Nu).*1e8);
            Flux = integral(Integrand, NuLo, NuHi, 'ArrayValued', true, 'RelTol', 1e-6);
            DenR(Ir) = 2.*pi.*Rgrid_k(Ir) .* Flux;
        end
        NumR = Rgrid_k .* DenR;

        Reff_cm(Ik)     = trapz(Rgrid_k, NumR) ./ trapz(Rgrid_k, DenR);
        Reff_Rg(Ik)     = Reff_cm(Ik) ./ Rg_k;
        R1_cm(Ik)       = R1cm_k;
        R2_cm(Ik)       = R2cm_k;
        LambdaPivot(Ik) = sqrt(L1k.*L2k);
        RPivot_cm(Ik)   = astro.accretion.accDisk_effectiveRadius(Mk, Mdk, LambdaPivot(Ik), zk, ...
            'MdotUnits',Args.MdotUnits, 'Eta',Etak, 'Rin',Rink);
        RPivot_Rg(Ik)   = RPivot_cm(Ik) ./ Rg_k;
        Rg_out(Ik)      = Rg_k;
        Rin_cm_out(Ik)  = RinCgs_k;
    end

    Result.Reff_cm     = Reff_cm;
    Result.Reff_Rg     = Reff_Rg;
    Result.R1_cm       = R1_cm;
    Result.R2_cm       = R2_cm;
    Result.LambdaPivot = LambdaPivot;
    Result.RPivot_cm   = RPivot_cm;
    Result.RPivot_Rg   = RPivot_Rg;
    Result.Rg          = Rg_out;
    Result.Rin_cm      = Rin_cm_out;
end
