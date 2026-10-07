function Var = tessPixelVar(AI, Back)
    %{
    Per-pixel noise variance of a TESS FFI tile in (e-/s)^2: sky photons
    Back/t plus read noise NREADOUT*RN^2/t^2, with RN of the CCD output
    (512 columns each) of every column, located through the tile CCDSEC.
    
    Input   : - AI. AstroImage of a TESS FFI tile in e-/s, whose header holds
                EXPOSURE (d), NREADOUT, READNOIA-READNOID (e-) and CCDSEC.
              - Back. Background of the tile (e-/s), a map or a scalar.
    
    Output  : - Var. Variance map (e-/s)^2, of the tile size.
    
    Author  : Ruslan Konno (Oct 2026)
    Example : AI.Var = pipeline.tess.reduction.tessPixelVar(AI, AI.Back);
    %}

    arguments
        AI
        Back
    end

    H  = AI.HeaderData;
    t  = H.getVal('EXPOSURE')*86400;
    NR = H.getVal('NREADOUT');
    RN = [H.getVal('READNOIA') H.getVal('READNOIB') H.getVal('READNOIC') H.getVal('READNOID')];
    CCDSEC = H.getVal('CCDSEC');
    if ischar(CCDSEC) || isstring(CCDSEC)
        CCDSEC = str2num(CCDSEC); %#ok<ST2NM>
    end
    [Ny, Nx] = size(AI.Image);
    XFFI   = CCDSEC(1) - 1 + (1:Nx);
    Output = min(4, max(1, ceil(XFFI./512)));
    Var = double(Back)./t + repmat(NR.*RN(Output).^2./t.^2, Ny, 1);
end
