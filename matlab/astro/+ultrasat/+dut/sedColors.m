function [UmG, UmNUV] = sedColors(SED, GABminusVega)
    % U - G(Vega) and U - NUV of a source SED (U: AB, the r = 0 ULTRASAT filter)
    %   Synthetic photometry of the SED through the ULTRASAT filter at the
    %   field center, Gaia G and GALEX NUV.
    % Input  : - SED: 'BB<Teff>' (a blackbody, e.g. 'BB20000') or a Pickles
    %               template name (e.g. 'G2V').
    %          - GABminusVega: G(AB) - G(Vega). Default is 0.1136.
    % Output : - UmG: U - G(Vega).
    %          - UmNUV: U - NUV.
    % Author : D. Kovaleva (Oct 2026)
    % Example: [UmG, UmNUV] = ultrasat.dut.sedColors('G2V');

    arguments
        SED (1,:) char
        GABminusVega (1,1) double = 0.1136
    end
    I = Installer;
    UP = io.files.load1(fullfile(I.getDataDir('ULTRASAT_Properties'), 'P90_UP_test_60_ZP_Var_Cern_21.mat'), 'UP');
    if startsWith(upper(SED), 'BB')
        Teff = str2double(SED(3:end));
        W = (1000:5:11000)';
        S = [W reshape(astro.spec.black_body(Teff, W), [], 1)];
    else
        S = io.files.load2(fullfile(I.getDataDir('PicklesStellarSpec'), ['uk' lower(SED) '.mat']));
        S = S(:,1:2);
    end
    U     = astro.spec.synthetic_phot(S, UP.U_AstFilt(1), 'R1', 'AB');
    UmG   = U - astro.spec.synthetic_phot(S, 'GAIA', 'G', 'AB') + GABminusVega;
    UmNUV = U - astro.spec.synthetic_phot(S, 'GALEX', 'NUV', 'AB');
end
