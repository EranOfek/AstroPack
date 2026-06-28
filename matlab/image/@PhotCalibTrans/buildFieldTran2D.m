function FieldTran2DObj = buildFieldTran2D(AI_array, Args)
    % Construct a field-frame Tran2D object covering the union of all crops.
    % Reads ORIGSEC (CCDSEC) from each AstroImage header, computes the
    % bounding box across all crops, and sets ParNX/ParNY to (FieldCenter,
    % FieldHalfRange) so the basis polynomials normalize correctly over
    % the entire mosaic frame. ParX/ParY are zero-initialised — caller
    % (fitJointVisit) overwrites ParX with the fitted coefficients.
    % Input  : - AI_array - 1xN AstroImage array. Each AI's HeaderData must
    %                       carry the ORIGSEC keyword (CCDSEC string
    %                       parseable by imUtil.ccdsec.ccdsecStr2num).
    %          * Args - struct or key/val with:
    %             .Tran2DType - char; Tran2D selected_trans name. Default
    %                           'cheby1_4_xt' (LAST joint-fit standard).
    %             .KeyCCDSEC  - HeaderData key. Default 'ORIGSEC'.
    %             .Verbose    - logical, default false.
    % Output : - FieldTran2DObj - Tran2D object with:
    %             ParNX = [(Xmin+Xmax)/2, (Xmax-Xmin)/2]
    %             ParNY = [(Ymin+Ymax)/2, (Ymax-Ymin)/2]
    %             ParX  = zeros(1, Nparam)
    %             ParY  = zeros(1, NparamY)
    % Author : D. Kovaleva (April 2026)
    % Example: T2D = PhotCalibTrans.buildFieldTran2D(AI);
    %          % For LAST 4x6 mosaic of 1726-px crops: ParNX = [3452, 3452],
    %          % ParNY = [5178, 5178].

    arguments
        AI_array
        Args.Tran2DType char = 'cheby1_4_xt'
        Args.KeyCCDSEC  char = 'ORIGSEC'
        Args.Verbose logical = false
    end

    Ncrops = numel(AI_array);
    Xmin =  inf;  Xmax = -inf;
    Ymin =  inf;  Ymax = -inf;

    for I = 1:Ncrops
        CCDSECStr = AI_array(I).HeaderData.getVal(Args.KeyCCDSEC, 'UseDict', false);
        CCDSEC    = imUtil.ccdsec.ccdsecStr2num(CCDSECStr);   % [Xmin, Xmax, Ymin, Ymax]
        Xmin = min(Xmin, CCDSEC(1));
        Xmax = max(Xmax, CCDSEC(2));
        Ymin = min(Ymin, CCDSEC(3));
        Ymax = max(Ymax, CCDSEC(4));
    end

    if ~isfinite(Xmin) || ~isfinite(Xmax) || ~isfinite(Ymin) || ~isfinite(Ymax)
        error('PhotCalibTrans:buildFieldTran2D:NoCCDSEC', ...
              'Could not extract CCDSEC from any AI HeaderData under key %s.', ...
              Args.KeyCCDSEC);
    end

    FieldTran2DObj = Tran2D(Args.Tran2DType);
    FieldTran2DObj.ParNX = [(Xmin + Xmax)/2, (Xmax - Xmin)/2];
    FieldTran2DObj.ParNY = [(Ymin + Ymax)/2, (Ymax - Ymin)/2];

    [NfunX, NfunY] = nfuns(FieldTran2DObj);
    FieldTran2DObj.ParX = zeros(1, NfunX);
    FieldTran2DObj.ParY = zeros(1, NfunY);

    if Args.Verbose
        fprintf('  Field Tran2D: type=%s, ParNX=[%.1f, %.1f], ParNY=[%.1f, %.1f]\n', ...
                Args.Tran2DType, FieldTran2DObj.ParNX, FieldTran2DObj.ParNY);
    end
end
