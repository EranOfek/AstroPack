function [Rcm, Rrg, Result] = accDisk_effectiveRadius(M_BH, Mdot, Lambda, z, Args)
    % Effective disk radius emitting a given OBSERVED wavelength. Vectorized.
    % Package: astro.accretion
    % Description: For a Shakura-Sunyaev thin disk, returns the radius
    %              R_eff(Lambda) of the annulus whose local temperature
    %              equals the characteristic "color temperature" of the
    %              observed wavelength Lambda, i.e. the radius R solving
    %                 h*nu_rest = X * kB * T(R),
    %                 nu_rest = c*(1+z)/Lambda
    %              Two conventions for the dimensionless constant X are
    %              supported ('Definition'):
    %                'Wien'          X = 2.8214 (peak of the local
    %                                Planck function nu*B_nu(T) at
    %                                fixed T - the radius whose own
    %                                local spectrum peaks at nu_rest).
    %                'FluxWeighted'  X = 1.1319 (the radius that
    %                                maximizes the disk's contribution
    %                                to L_nu per unit ln(R), i.e. the
    %                                peak of R*B_nu(T(R)); this radius
    %                                is systematically smaller/hotter
    %                                than the 'Wien' radius).
    %              T(R) has an interior maximum at Rpeak = (49/36)*Rin
    %              (exact result for the Shakura-Sunyaev profile), so
    %              the equation above generically has two roots: one on
    %              the rising branch (Rin < R < Rpeak) and one on the
    %              declining, R >> Rin branch. This function always
    %              returns the physically relevant outer (declining-
    %              branch) root, found by bracketed root-finding (fzero)
    %              starting just outside Rpeak. If a requested wavelength
    %              is too blue for any disk annulus to reach the
    %              required temperature (target T >= Tmax) at that grid
    %              point, an error identifying the grid point is raised.
    %
    %              Fully vectorized: M_BH, Mdot, Lambda, z, 'Rin', 'Eta'
    %              and 'X' may each be a scalar, vector, matrix or N-D
    %              array. They are combined by ordinary implicit
    %              expansion (broadcasting) into a common grid, e.g. an
    %              Nx1 M_BH together with a 1xM Lambda returns an NxM
    %              grid of radii, one per (mass, wavelength) pair. Since
    %              each grid point requires its own root-find, this
    %              function loops internally over the (broadcast) grid;
    %              only accDisk_diskTemperature is loop-free.
    % Input  : - M_BH   : BH mass [Solar masses].
    %          - Mdot   : Accretion rate [units set by 'MdotUnits'].
    %          - Lambda : Observed wavelength(s) [Angstrom].
    %          - z      : Redshift. Default is 0.
    %          * ...,key,val,...
    %            'MdotUnits'    - "gs" | "MsunYr" | "Edd" (scalar). Default is "MsunYr".
    %            'Eta'          - Radiative efficiency (if MdotUnits=="Edd").
    %                             Scalar or array. Default is 0.1.
    %            'Rin'          - Inner radius [Rg]. Scalar or array. Default is 6.
    %            'Definition'   - "Wien" | "FluxWeighted" (scalar). Default is "Wien".
    %            'X'            - Override the dimensionless constant X
    %                             (if non-empty, takes precedence over
    %                             'Definition'). Scalar or array. Default is [].
    %            'SolveBoundary'- If true (default), solve the full
    %                             equation including the inner-boundary
    %                             term (1-sqrt(Rin/R)) by root-finding.
    %                             If false, use the closed-form,
    %                             no-boundary expression (valid for
    %                             R_eff >> Rin; ~10% too large close to
    %                             Rin) - vectorized with no loop.
    % Output : - Rcm    : Effective radius [cm]. Size is the common
    %                     broadcast size of M_BH, Mdot, Lambda, z, 'Rin',
    %                     'Eta' and 'X'.
    %          - Rrg    : Effective radius [units of Rg = G*M/c^2], same size.
    %          - Result : Struct with fields X, Rg, Rin, Rpeak, Tmax, each
    %                     broadcast to the same output size.
    % Author : (fill in your name) (Sep 2026)
    % Example: [Rcm,Rrg] = astro.accretion.accDisk_effectiveRadius(1e8, 0.1, [4000 5500], 0.5, 'MdotUnits','Edd');
    %          % Grid over BH mass (3x1) and wavelength (1x4) -> 3x4 matrix:
    %          Rrg = astro.accretion.accDisk_effectiveRadius([1e7;1e8;1e9], 0.1, [3000 4000 5000 6000], 0.5, 'MdotUnits','Edd');

    arguments
        M_BH double {mustBePositive}
        Mdot double {mustBePositive}
        Lambda double {mustBePositive}
        z double {mustBeNonnegative} = 0
        Args.MdotUnits (1,1) string {mustBeMember(Args.MdotUnits, ["gs","MsunYr","Edd"])} = "MsunYr"
        Args.Eta double {mustBePositive} = 0.1
        Args.Rin double {mustBePositive} = 6
        Args.Definition (1,1) string {mustBeMember(Args.Definition, ["Wien","FluxWeighted"])} = "Wien"
        Args.X double = []
        Args.SolveBoundary (1,1) logical = true
    end

    Const   = astro.accretion.accDisk_physConst();
    Mcgs    = M_BH .* Const.SunM;
    MdotCgs = astro.accretion.accDisk_convertMdot(Mcgs, Mdot, Args.MdotUnits, Args.Eta);
    Rg      = Const.G .* Mcgs ./ Const.c.^2;
    RinCgs  = Args.Rin .* Rg;
    RpeakCgs= (49/36) .* RinCgs;   % exact location of the T(R) maximum

    if isempty(Args.X)
        switch Args.Definition
            case "Wien"
                X = 2.821439372122079;
            case "FluxWeighted"
                X = 1.13187;
        end
    else
        X = Args.X;
    end

    Prefactor = 3.*Const.G.*Mcgs.*MdotCgs ./ (8.*pi.*Const.sigma);
    NuRest    = Const.c.*(1+z) ./ (Lambda.*1e-8);          % rest-frame frequency
    Target    = Const.h.*NuRest./Const.kB;                  % required X*T(R)
    RGuessNoB = (Prefactor .* (X.*Const.kB./(Const.h.*NuRest)).^4).^(1/3);

    % Broadcast every quantity needed at each grid point to the common
    % output grid size (union of the shapes of M_BH, Mdot, Lambda, z,
    % 'Rin', 'Eta', 'X').
    [SzOut, PrefactorF, RinCgsF, RpeakCgsF, TargetF, RGuessNoBF, XF] = ...
        astro.accretion.accDisk_broadcast(Prefactor, RinCgs, RpeakCgs, Target, RGuessNoB, X);

    % Tmax(R=Rpeak) is a closed-form elementwise expression (no root-find
    % needed): since Rpeak = (49/36)*Rin exactly, (1-sqrt(Rin/Rpeak)) = 1/7
    % identically, but we keep the general sqrt(.) form for clarity.
    TmaxF = (PrefactorF./RpeakCgsF.^3 .* (1 - sqrt(RinCgsF./RpeakCgsF))).^(1/4);

    if ~Args.SolveBoundary
        Rcm = RGuessNoBF;
    else
        Rcm  = zeros(SzOut);
        NPts = numel(Rcm);
        for Ik = 1:NPts
            Pk    = PrefactorF(Ik);
            Rink  = RinCgsF(Ik);
            Rpk   = RpeakCgsF(Ik);
            Tk    = TargetF(Ik);
            Xk    = XF(Ik);
            RGk   = RGuessNoBF(Ik);
            Tmaxk = TmaxF(Ik);

            if Tk./Xk >= Tmaxk
                SubIdx = cell(1, max(numel(SzOut),2));
                [SubIdx{:}] = ind2sub(SzOut, Ik);
                error('accDisk_effectiveRadius:noSolution', ...
                    ['Grid point [%s] (linear index %d) requires T >= Tmax = %.3e K; ', ...
                     'no annulus of this disk is that hot (thin-disk approximation ', ...
                     'breaks down / this band is not disk-dominated at that point).'], ...
                    strtrim(sprintf('%d ', [SubIdx{:}])), Ik, Tmaxk);
            end

            TFun = @(R) (Pk./R.^3 .* (1-sqrt(Rink./R))).^(1/4);
            Fun  = @(R) Tk - Xk.*TFun(R);
            % Fun(Rpeak) < 0 and Fun(R->inf) -> Target > 0: bracket the
            % OUTER (declining, R > Rpeak) branch - the physically
            % relevant R >> Rin solution (ignores the inner, rising-
            % branch root, which is not of interest here).
            Rhi = max(RGk.*10, Rpk.*10);
            while Fun(Rhi) < 0
                Rhi = Rhi.*10;
            end
            Rcm(Ik) = fzero(Fun, [Rpk.*(1+1e-9), Rhi]);
        end
    end

    Rg_full = Rg + zeros(SzOut);
    Rrg = Rcm ./ Rg_full;

    Result.X     = XF;
    Result.Rg    = Rg_full;
    Result.Rin   = RinCgsF;
    Result.Rpeak = RpeakCgsF;
    Result.Tmax  = TmaxF;
end
