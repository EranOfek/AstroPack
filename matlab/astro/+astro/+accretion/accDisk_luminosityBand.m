function [Lband, Result] = accDisk_luminosityBand(M_BH, Mdot, Lambda1, Lambda2, z, Args)
    % Disk luminosity radiated into a filter passband. Vectorized.
    % Package: astro.accretion
    % Description: Computes the intrinsic (rest-frame) luminosity a
    %              Shakura-Sunyaev thin disk radiates into the
    %              rest-frame frequency band [nu1,nu2] that corresponds,
    %              once redshifted, to the OBSERVED passband
    %              [Lambda1, Lambda2] (Angstrom):
    %                 L_band = Int_{Rin}^{inf} 2 * [Int_{nu1}^{nu2}
    %                          FilterFunc(lambda_obs) * pi*B_nu(T(R)) dnu]
    %                          * 2*pi*R dR
    %              with nu1 = c*(1+z)/Lambda2, nu2 = c*(1+z)/Lambda1 (the
    %              factor 2 is for the disk's two faces, pi*B_nu(T) is
    %              the standard blackbody flux per unit area). This
    %              normalization is exactly consistent with
    %              accDisk_bolometricLuminosity: taking the passband to
    %              cover all frequencies (Lambda1->0, Lambda2->inf)
    %              recovers L_band -> L_bol = G*M*Mdot/(2*Rin) (see the
    %              validation note in that function).
    %
    %              NOTE: this is the disk's intrinsic rest-frame band
    %              luminosity [erg/s], not the flux an observer would
    %              measure - to get an observed flux you still need a
    %              luminosity distance, F = L_band / (4*pi*D_L^2), which
    %              this package does not compute (it has no cosmology
    %              dependence).
    %
    %              The radial integral (Rin -> infinity) is done exactly
    %              via the substitution u = Rin/R in (0,1], the same
    %              trick used by accDisk_bolometricLuminosity's
    %              'Numeric' method, so there is no arbitrary outer
    %              truncation radius. Because the passband is finite,
    %              both R->Rin and R->infinity contribute a vanishing,
    %              smooth (Wien-suppressed) integrand, unlike the
    %              bolometric (all-frequencies) integral, which is why a
    %              single robust adaptive-quadrature implementation is
    %              provided here (no separate 'Analytic' fast path
    %              exists for an arbitrary band).
    %
    %              Fully vectorized: M_BH, Mdot, Lambda1, Lambda2, z,
    %              'Rin' and 'Eta' may each be a scalar, vector, matrix
    %              or N-D array, combined by ordinary implicit expansion
    %              (broadcasting) into a common grid. Every grid point
    %              needs its own nested numerical integration, so (like
    %              accDisk_effectiveRadiusBand) this function loops
    %              internally over the broadcast grid.
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
    %            'RelTol'     - Relative tolerance passed to integral()
    %                           (both the radial and frequency
    %                           integrals). Default is 1e-6.
    % Output : - Lband  : Band luminosity [erg/s]. Size is the common
    %                     broadcast size of M_BH, Mdot, Lambda1, Lambda2,
    %                     z, 'Rin' and 'Eta'.
    %          - Result : Struct (same size as Lband) with fields:
    %              LEdd     - Eddington luminosity [erg/s],
    %              LonLEdd  - Lband / LEdd,
    %              Lbol     - bolometric luminosity (accDisk_bolometricLuminosity,
    %                         'Analytic' method) at the same grid point,
    %              LonLbol  - Lband / Lbol, the band's share of the
    %                         bolometric output (-> 1 as the band widens
    %                         to cover all frequencies),
    %              Rg, Rin_cm, MdotCgs.
    % Author : (fill in your name) (Sep 2026)
    % Example: L = astro.accretion.accDisk_luminosityBand(1e8, 0.1, 4000, 5500, 0.5, 'MdotUnits','Edd');
    %          [L, Result] = astro.accretion.accDisk_luminosityBand(1e8, 0.1, 4000, 5500, 0.5, 'MdotUnits','Edd');
    %          fprintf('L_band = %.3e erg/s (%.2f%% of bolometric)\n', L, 100*Result.LonLbol);
    %          % Grid over BH mass (3x1) and redshift (1x3):
    %          L = astro.accretion.accDisk_luminosityBand([1e7;1e8;1e9], 0.1, 4000, 5500, [0 0.5 1], 'MdotUnits','Edd');

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
        Args.RelTol (1,1) double {mustBePositive} = 1e-6
    end

    Const = astro.accretion.accDisk_physConst();

    [SzOut, M_BHf, Mdotf, Lambda1f, Lambda2f, zf, Rinf, Etaf] = ...
        astro.accretion.accDisk_broadcast(M_BH, Mdot, Lambda1, Lambda2, z, Args.Rin, Args.Eta);

    if any(Lambda2f(:) <= Lambda1f(:))
        error('accDisk_luminosityBand:badRange', ...
            'Lambda2 must be greater than Lambda1 at every grid point.');
    end

    Lband      = zeros(SzOut);
    LEdd       = zeros(SzOut);
    Rg_out     = zeros(SzOut);
    Rin_cm_out = zeros(SzOut);
    MdotCgs_out= zeros(SzOut);

    for Ik = 1:numel(Lband)
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

        Mcgs_k    = Mk .* Const.SunM;
        MdotCgs_k = astro.accretion.accDisk_convertMdot(Mcgs_k, Mdk, Args.MdotUnits, Etak);
        Rg_k      = Const.G .* Mcgs_k ./ Const.c.^2;
        RinCgs_k  = Rink .* Rg_k;
        Prefactor_k = 3.*Const.G.*Mcgs_k.*MdotCgs_k ./ (8.*pi.*Const.sigma);

        NuLo = Const.c.*(1+zk) ./ (L2k.*1e-8);
        NuHi = Const.c.*(1+zk) ./ (L1k.*1e-8);

        % T(R)^4, written out directly (no ^(1/4)-then-^4 round trip);
        % floored at 0 to stay real for R slightly below Rin from
        % floating-point roundoff.
        T4Fun = @(R) Prefactor_k./R.^3 .* max(1 - sqrt(RinCgs_k./R), 0);

        InnerNuInt = @(R) integral( ...
            @(Nu) pi .* (2.*Const.h.*Nu.^3./Const.c.^2) ./ ...
                  (exp(Const.h.*Nu ./ (Const.kB.*max(T4Fun(R),0).^0.25 + realmin)) - 1) .* ...
                  FilterFunck((Const.c.*(1+zk)./Nu).*1e8), ...
            NuLo, NuHi, 'ArrayValued', true, 'RelTol', Args.RelTol);

        % Radial integral Rin -> infinity via u = Rin/R in (0,1]:
        %   Int_Rin^inf 2*InnerNuInt(R)*2*pi*R dR
        %     = Int_0^1 4*pi*(Rin/u)^2/u * InnerNuInt(Rin/u) du
        IntegrandU = @(u) arrayfun(@(uu) 4.*pi.*(RinCgs_k./uu).^2 ./ uu .* InnerNuInt(RinCgs_k./uu), u);

        Lband(Ik) = integral(IntegrandU, 0, 1, 'RelTol', Args.RelTol);

        LEdd(Ik)        = 4.*pi.*Const.G.*Mcgs_k.*Const.mp.*Const.c ./ Const.sigmaT;
        Rg_out(Ik)      = Rg_k;
        Rin_cm_out(Ik)  = RinCgs_k;
        MdotCgs_out(Ik) = MdotCgs_k;
    end

    % MdotCgs_out already holds the converted accretion rate [g/s] for
    % every grid point, so pass it straight through as 'gs' rather than
    % re-converting via the original MdotUnits/Eta.
    Lbol = astro.accretion.accDisk_bolometricLuminosity(M_BHf, MdotCgs_out, 'MdotUnits','gs', 'Rin',Rinf);

    Result.LEdd     = LEdd;
    Result.LonLEdd  = Lband ./ LEdd;
    Result.Lbol     = Lbol;
    Result.LonLbol  = Lband ./ Lbol;
    Result.Rg       = Rg_out;
    Result.Rin_cm   = Rin_cm_out;
    Result.MdotCgs  = MdotCgs_out;
end
