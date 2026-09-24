function [L, Result] = accDisk_bolometricLuminosity(M_BH, Mdot, Args)
    % Bolometric luminosity of a Shakura & Sunyaev (1973) thin disk. Vectorized.
    % Package: astro.accretion
    % Description: Computes the total (both faces, all radii) radiated
    %              power of a geometrically thin, optically thick,
    %              steady-state accretion disk. Integrating the local
    %              flux 2*sigma*T(R)^4 (factor 2 for the two faces) over
    %              the whole disk area, from the inner radius Rin to
    %              infinity, has the closed form
    %                 L = G * M * Mdot / (2 * Rin)
    %              i.e. exactly half of the Newtonian accretion power
    %              G*M*Mdot/Rin released down to Rin is radiated by the
    %              disk itself (the standard SS73 zero-torque-at-Rin
    %              result; see e.g. Frank, King & Raine, "Accretion
    %              Power in Astrophysics"). This is the SAME idealized,
    %              Newtonian, razor-thin-disk model used by
    %              accDisk_diskTemperature/accDisk_effectiveRadius*
    %              elsewhere in this package, so the implied disk
    %              radiative efficiency
    %                 EtaDisk = G*M/(2*Rin*c^2) = Rg/(2*Rin)
    %              (returned in Result.EtaDisk) is the Newtonian
    %              approximation, NOT the general-relativistic
    %              Novikov-Thorne efficiency (e.g. for Rin=6*Rg,
    %              EtaDisk=1/12=0.0833, vs. the true Schwarzschild-ISCO
    %              value of ~0.057) - it is kept consistent with the
    %              rest of this package rather than "corrected", so
    %              that L, T(R) and R_eff(lambda) all describe the same
    %              underlying disk model.
    %
    %              NOTE: If Mdot is supplied via 'MdotUnits'=="Edd" (an
    %              Eddington ratio) together with 'Eta', that 'Eta' is
    %              only used to convert the ratio into a physical Mdot
    %              (Mdot_cgs = Ratio*LEdd/(Eta*c^2)); the disk's OWN
    %              output efficiency is EtaDisk above, which need not
    %              equal the input 'Eta'. Result.LonLEdd will then equal
    %              Ratio*(EtaDisk/Eta), not Ratio itself, unless 'Eta'
    %              is deliberately set equal to EtaDisk.
    %
    %              Fully vectorized: M_BH, Mdot, 'Rin' and 'Eta' may
    %              each be a scalar, vector, matrix or N-D array,
    %              combined by ordinary implicit expansion
    %              (broadcasting). The default 'Analytic' method is a
    %              closed-form elementwise expression (no loop, any
    %              grid size). The optional 'Numeric' method integrates
    %              the local flux profile radius-by-radius instead (as
    %              a cross-check, or as a template for a non-standard
    %              temperature profile); it loops internally over the
    %              broadcast grid, like accDisk_effectiveRadiusBand. The
    %              radial integral is done under the substitution
    %              u = Rin/R in [0,1], so the semi-infinite outer disk
    %              (R: Rin -> infinity) is integrated exactly, with no
    %              arbitrary outer truncation radius.
    % Input  : - M_BH : BH mass [Solar masses].
    %          - Mdot : Accretion rate [units set by 'MdotUnits'].
    %          * ...,key,val,...
    %            'MdotUnits'  - "gs" | "MsunYr" | "Edd" (scalar). Default is "MsunYr".
    %            'Eta'        - Radiative efficiency (if MdotUnits=="Edd";
    %                           see the NOTE above). Scalar or array.
    %                           Default is 0.1.
    %            'Rin'        - Inner disk radius [Rg]. Scalar or array.
    %                           Default is 6 (Schwarzschild ISCO).
    %            'Method'     - "Analytic" | "Numeric" (scalar). Default is "Analytic".
    %            'RelTol'     - For 'Numeric' only: relative tolerance
    %                           passed to integral(). Default is 1e-8.
    % Output : - L      : Bolometric luminosity [erg/s]. Size is the
    %                     common broadcast size of M_BH, Mdot, 'Rin' and 'Eta'.
    %          - Result : Struct (same size as L) with fields:
    %              LEdd     - Eddington luminosity [erg/s],
    %              LonLEdd  - L / LEdd (the disk's actual Eddington ratio;
    %                         see the NOTE above re. 'MdotUnits'=="Edd"),
    %              EtaDisk  - G*M/(2*Rin*c^2), this model's implied
    %                         radiative efficiency (L = EtaDisk*Mdot*c^2,
    %                         exactly, for either Method),
    %              Rg, Rin_cm, MdotCgs.
    % Author : (fill in your name) (Sep 2026)
    % Example: L = astro.accretion.accDisk_bolometricLuminosity(1e8, 0.1, 'MdotUnits','Edd');
    %          [L, Result] = astro.accretion.accDisk_bolometricLuminosity(1e8, 0.1, 'MdotUnits','Edd');
    %          fprintf('L = %.3e erg/s = %.3f L_Edd\n', L, Result.LonLEdd);
    %          % Grid over BH mass (3x1) and Eddington ratio (1x4) -> 3x4 matrix:
    %          L = astro.accretion.accDisk_bolometricLuminosity([1e7;1e8;1e9], [0.01 0.05 0.1 0.5], 'MdotUnits','Edd');
    %          % Cross-check the closed form against direct numerical integration:
    %          Lnum = astro.accretion.accDisk_bolometricLuminosity(1e8, 0.1, 'MdotUnits','Edd', 'Method','Numeric');

    arguments
        M_BH double {mustBePositive}
        Mdot double {mustBePositive}
        Args.MdotUnits (1,1) string {mustBeMember(Args.MdotUnits, ["gs","MsunYr","Edd"])} = "MsunYr"
        Args.Eta double {mustBePositive} = 0.1
        Args.Rin double {mustBePositive} = 6
        Args.Method (1,1) string {mustBeMember(Args.Method, ["Analytic","Numeric"])} = "Analytic"
        Args.RelTol (1,1) double {mustBePositive} = 1e-8
    end

    Const   = astro.accretion.accDisk_physConst();
    Mcgs    = M_BH .* Const.SunM;
    MdotCgs = astro.accretion.accDisk_convertMdot(Mcgs, Mdot, Args.MdotUnits, Args.Eta);
    Rg      = Const.G .* Mcgs ./ Const.c.^2;
    RinCgs  = Args.Rin .* Rg;

    % Broadcast every quantity to the common output grid size up front,
    % so both branches below (and the diagnostics) operate on
    % consistently-shaped arrays.
    [SzOut, McgsF, MdotCgsF, RgF, RinCgsF] = ...
        astro.accretion.accDisk_broadcast(Mcgs, MdotCgs, Rg, RinCgs);

    switch Args.Method
        case "Analytic"
            % Exact closed-form integral of 2*sigma*T(R)^4 over the disk
            % area (both faces), Rin -> infinity: L = G*M*Mdot/(2*Rin).
            L = Const.G .* McgsF .* MdotCgsF ./ (2.*RinCgsF);
        case "Numeric"
            L = zeros(SzOut);
            for Ik = 1:numel(L)
                Prefactor_k = 3.*Const.G.*McgsF(Ik).*MdotCgsF(Ik) ./ (8.*pi.*Const.sigma);
                Rink = RinCgsF(Ik);
                % Integrand = 2 (faces) * sigma*T(R)^4 * 2*pi*R, with
                % sigma*T(R)^4 = sigma * Prefactor/R^3 * (1-sqrt(Rin/R))
                % written out directly (avoids an unnecessary ^(1/4)
                % then ^4 round trip). Substituting u = Rin/R maps the
                % semi-infinite integral R in [Rin, inf) onto the finite
                % range u in [0, 1] exactly:
                %   Int_Rin^inf 4*pi*sigma*Prefactor*(1-sqrt(Rin/R))/R^2 dR
                %     = Int_0^1 (4*pi*sigma*Prefactor/Rin) * (1-sqrt(u)) du
                % which removes any dependence on an arbitrary outer
                % truncation radius (there is none left to choose).
                Integrand = @(u) 4.*pi.*Const.sigma.*Prefactor_k./Rink .* (1 - sqrt(u));
                L(Ik) = integral(Integrand, 0, 1, 'RelTol', Args.RelTol);
            end
    end

    LEdd    = 4.*pi.*Const.G.*McgsF.*Const.mp.*Const.c ./ Const.sigmaT;
    EtaDisk = Const.G.*McgsF ./ (2.*RinCgsF.*Const.c.^2);   % = Rg/(2*Rin); L = EtaDisk*Mdot*c^2 exactly

    Result.LEdd    = LEdd;
    Result.LonLEdd = L ./ LEdd;
    Result.EtaDisk = EtaDisk;
    Result.Rg      = RgF;
    Result.Rin_cm  = RinCgsF;
    Result.MdotCgs = MdotCgsF;
end
