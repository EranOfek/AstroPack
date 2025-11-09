function [Result, Iim, Iref] = remapWCS(CCDSEC, OtherAW, OtherCCDSEC, Args)
    % Remap WCS from one CCDSEC to another CCDSEC in the "same image"
    %     This function can be used to generate an initial guess WCS (for
    %     use with astrometryRefine) for a single subimage that belongs to
    %     an image in which the WCS of other subimages is known.
    %     Given a list of of CCDSEC of subimage candidates for which a WCS
    %     is needed, this function will search for the most adequate CCDSEC
    %     to solve next (i.e., with nearest existing WCS).
    % Input  : - (CCDSEC) A 4 column matrix of CCDSEC of images for an AstroWCS object
    %            is required. The function whill look for the line in
    %            CCDSEC which is neareast to OtherCCDSEC with known WCS,
    %            and remap the OtherWCS to that of the selected CCDSEC.
    %          - (OtherAW) An array of AstroWCS of other CCDSECs in the same image.
    %            Alternatively, an array of AstroImage object containing
    %            AstroWCS.
    %          - (OtherCCDSEC) A 4 colum matrix of CCDSECs (one per line) corresponding
    %            to the AstroWCS in the second input argument.
    %          * ...,key,val,... 
    %            'OtherSubCenter' - An optional two column matrix of [X, Y]
    %                   centers of each one of the OtherCCDSEC.
    %                   If empty, then will calculate from OtherCCDSEC.
    %                   Default is [].
    %            'SubCenter' - An optional two column matrix of [X, Y]
    %                   centers of CCDSEC.
    %                   If empty, then will calculate from CCDSEC.
    %                   Default is [].
    %            'SucessAW' - A vector of logicals indicating which
    %                   OtherWCS is good to use. If empty, then this vector
    %                   will be generated from the Sucess property in Other
    %                   WCS (AstroWCS object).
    %                   Default is [].
    %            'ThresholdIdenticalWCS' - If nearest distance between CCDSEC and
    %                   OtherCCDSEC is smaller than this threshold
    %                   distance, then will use nearest OtherWCS as is,
    %                   without transforming.
    %                   Default is 0.5 pix.
    %            'JD' - Scalar JD, corresponding to the epoch of the main image
    %                   (i.e., for which CCDSEC is specified).
    %                   If not empty, then will choose from OtherAW only
    %                   EPOCHs which are nearest to this JD.
    %                   Default is [].
    %
    % Output : - An "initial guess" AstroWCS objet for the CCDSEC, which is
    %            nearest to one of the OtherWCS.
    %            This is a new deep copy of a WCS.
    %          - An index of the CCDSEC (line in CCDSEC input) for which
    %            the WCS was calculated/transformed.
    %          - An index of the line in OtherCCDSEC from which the CCDSEC
    %            was transformed.
    % Author : Eran Ofek (2025 Nov) 
    % Example: [RefWCS, Iccdsec]=imProc.astrometry.remapWCS(CCDSEC, AI, AI_CCDSEC);

    arguments
        CCDSEC
        OtherAW                        % AstroWCS or AstroImage
        OtherCCDSEC
        Args.OtherSubCenter         = [];
        Args.SubCenter              = [];
        Args.SucessAW               = [];
        Args.ThresholdIdenticalWCS  = 0.5;  % pix
        Args.JD                     = [];
    end

    if ~isempty(Args.JD)
        OtherJD = [OtherAW.EPOCH].';
        UniqueOtherJD = unique(OtherJD);
        [~,IminJD] = min(abs(UniqueOtherJD - Args.JD));

        % Select only 'Other' which are nearest in EPOCH to JD:
        Flag        = abs(OtherJD(IminJD) - OtherJD)<(100.*eps);
        OtherAW     = OtherAW(Flag);
        OtherCCDSEC = OtherCCDSEC(Flag,:);
    end


    Naw = numel(OtherAW);
    if isempty(Args.OtherSubCenter)
        Args.OtherSubCenter = [(OtherCCDSEC(:,1)+OtherCCDSEC(:,2)), (OtherCCDSEC(:,3)+OtherCCDSEC(:,4))].*0.5;
    end
    if isempty(Args.SubCenter)
        Args.SubCenter = [(CCDSEC(:,1)+CCDSEC(:,2)), (CCDSEC(:,3)+CCDSEC(:,4))].*0.5;
    end

    if ~isa(OtherAW, 'AstroWCS')
        % treat AstroImage, AstroZOGY, etc.
        % copy WCS in AstroImage to AstroWCS object.
        AW = AstroWCS(size(OtherAW));
        for Iaw=1:1:Naw
            AW(Iaw) = OtherAW(Iaw).WCS;
        end
    else
        AW = OtherAW;
    end

    if isempty(Args.SucessAW)
        Args.SucessAW = false(Naw,1);
        for Iaw=1:1:Naw
            Args.SucessAW(Iaw) = AW(Iaw).Success;
        end
    end
    IsucessAW = find(Args.SucessAW); % FS

    Dist2SubCenter    = (Args.OtherSubCenter(IsucessAW,1) - Args.SubCenter(:,1).').^2 + (Args.OtherSubCenter(IsucessAW,2) - Args.SubCenter(:,2).').^2;
    %Dist2SubCenter(triu(Dist2SubCenter,1)==0)=Inf;
    [MinDist2,IndMin] = min(Dist2SubCenter,[],'all','linear');
    [MinI,MinJ]       = imUtil.image.ind2sub_fast(size(Dist2SubCenter), IndMin);
    %FS                = find(Sucess);   % FS
    InotSucessAW      = find(~Args.SucessAW);  % FNS
    % index of image from which to take the WCS solution
    Iref    = IsucessAW(MinI); 
    % Index of image to solve
    Iim     = InotSucessAW(MinJ);  % looks like a BUG
    %Iim = MinJ;

    % new copy of WCS 
    Result = AW(Iref).copy;
    % check if need to shift solution, or can we use existing solution as is
    if MinDist2>(Args.ThresholdIdenticalWCS.^2)
        % transform WCS:
        % If NOT: use existing WCS as is
        
        % the shift in CRPIX between the image and the ref
        ShiftX = CCDSEC(Iim,1) - OtherCCDSEC(Iref,1);
        ShiftY = CCDSEC(Iim,3) - OtherCCDSEC(Iref,3);
        % add shift to CRPIX - why??????
        Result.CRPIX = AW(Iref).CRPIX - [ShiftX, ShiftY];
    end


end
