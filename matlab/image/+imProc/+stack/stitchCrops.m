function [Result, AstrometricCat, PhotCat] = stitchCrops(AI, Args)
    % Stitch together several crops originating from the same image
    %     Optional detailed description
    % Input  : - a stack of AstroImages (containing individual crops)
    %         * ...,key,val,...
    %         'UpdateWCS' - whether to build a WCS for the stitched image from the merged catalog
    %         'UpdateZP'  - whether to calculate a new photometric ZP for the stitched image
    %         'AstrometricCat' - an AstroCatalog to pass to astrometryRefine as CatName,
    %                    skipping the catsHTM query. If empty, the catalog is fetched.
    %                    Default is [].
    %         'PhotCat'   - an AstroCatalog to pass to photometricZP as CatName,
    %                    skipping the catsHTM query. If empty, the catalog is fetched.
    %                    Default is [].
    %         'PhotZPMethod' - method to determine the photometric ZP when UpdateZP is true:
    %                    'photometricZP' - call imProc.calib.photometricZP (default).
    %                    'header' - read KeyPH_ZP from input crop headers and take the mean.
    %         'KeyZP' - cell array of header keyword synonyms for the ZP.
    %                    With PhotZPMethod 'header' these are the keywords read from the
    %                    input crops. In both methods the resulting ZP is written back to
    %                    every keyword in the list, so that a consumer looking the ZP up
    %                    under any one of the synonyms finds it. Default is {'PH_ZP','PT_ZP'}.
    %         'BitDict'   - a BitDictionary to use for the mask bit operations, allowing the
    %                    caller to build it once and reuse it across many calls.
    %                    If empty, a default BitDictionary is built here. Default is [].
    %         'PSFMethod' - how to propagate the PSF of the crops to the stitched image:
    %                    'none'    - leave the PSF of the stitched image empty (default).
    %                    'central' - copy the PSF of the crop closest to the centre of the
    %                                stitched image (the whole AstroPSF object is copied).
    %                    'wmean'   - a weighted mean of the crop PSFs, each crop weighted
    %                                by the area it contributes to the stitched image.
    %                                Only the PSF stamp, its variance, Nstars and the
    %                                stamp grid are populated; the derived quantities
    %                                (FWHM, FluxContainmentRadius, SuppressRad, the
    %                                extended PSF) are left empty.
    %                    Crops with an empty or non-finite PSF are ignored. If no crop
    %                    carries a usable PSF, the stitched PSF is left empty.
    % Output : - a stitched AstroImage with a merged catalog and updated WCS
    %          - AstroCatalog used for astrometry ([] if UpdateWCS is false)
    %          - AstroCatalog used for photometry ([] if UpdateZP is false or PhotZPMethod is 'header')
    % Author : A.M. Krassilchtchikov (2026 Jan)
    % Example: [AIs, AstCat, PhCat] = imProc.stack.stitchCrops(AI,'UpdateWCS',true,'UpdateZP',true)
    %          [AIs] = imProc.stack.stitchCrops(AI,'UpdateZP',true,'PhotZPMethod','header')
    %          [AIs] = imProc.stack.stitchCrops(AI,'PSFMethod','wmean')
    %
    arguments
        AI
        Args.CCDSEC                  = 'CCDSEC';
        Args.ORIGUSEC                = 'ORIGUSEC';
        Args.ORIGSEC                 = 'ORIGSEC';
        Args.UpdateWCS               = false;
        Args.UpdateZP                = false;
        Args.AstrometricCat          = [];
        Args.PhotCat                 = [];
        Args.PhotZPMethod            = 'photometricZP';  % 'photometricZP'|'header'
        Args.KeyZP                   = {'PH_ZP','PT_ZP'};
        Args.BitDict                 = [];
        Args.PSFMethod               = 'none';  % 'none'|'central'|'wmean'

        Args.MatchMethod             = 'mex';  % 'mex'|'old'
    end

    if isempty(Args.BitDict)
        Args.BitDict = BitDictionary('BitMask.Image.Default');
    end

    AstrometricCat = [];
    PhotCat        = [];

    Ncrop = numel(AI);
    MCat  = repmat(AstroCatalog,1,Ncrop);
    Xmin  = zeros(Ncrop,1); Xmax  = zeros(Ncrop,1);
    Ymin  = zeros(Ncrop,1); Ymax  = zeros(Ncrop,1);
    CCDSEC= zeros(Ncrop,4);
    OrigU = zeros(Ncrop,4);
    % the area and the [Xmin Xmax Ymin Ymax] region each crop contributes to
    % the stitched image, in the pixel coordinates of the stitched image
    CropArea = zeros(Ncrop,1);
    CropReg  = zeros(Ncrop,4);

    % get the table indices of the pixel columns
    IndX = AI(1).CatData.colname2ind({'XPEAK','X1','X'});
    IndY = AI(1).CatData.colname2ind({'YPEAK','Y1','Y'});

    % read the sizes and locations, determine the overlaps
    for Icrop = 1:Ncrop
        CCDSEC(Icrop,:) = AI(Icrop).HeaderData.getVal(Args.CCDSEC,'ReadCCDSEC',true);
        OrigU(Icrop,:)  = AI(Icrop).HeaderData.getVal(Args.ORIGUSEC,'ReadCCDSEC',true);
        Orig            = AI(Icrop).HeaderData.getVal(Args.ORIGSEC,'ReadCCDSEC',true);
        [Xmin(Icrop), Xmax(Icrop), Ymin(Icrop), Ymax(Icrop)] = deal(Orig(1),Orig(2),Orig(3),Orig(4));
    end

    O = tools.math.geometry.overlapRectangles(Xmin, Xmax, Ymin, Ymax);

    % find the lower left corner of the stitch on the whole image
    X0 = min(Xmin);
    Y0 = min(Ymin);

    % find a major shift of pixel coordinates in the catalogs
    CatShiftX = Xmin-X0;
    CatShiftY = Ymin-Y0;

    % accumulate the stitched image/mask in plain arrays: a partial
    % assignment into an AstroImage/MaskImage Data property (which has a
    % custom setter) forces a full Ny-by-Nx copy on every crop, so we
    % accumulate locally and build the Result object once at the end
    Nx = max(Xmax)-X0+1;
    Ny = max(Ymax)-Y0+1;
    ImgAccum  = nan(Ny,Nx,'single');
    MaskAccum = zeros(Ny,Nx,'uint32');

    % fill the new image with chopped crops, shift the catalog pixels
    for Icrop = 1:Ncrop
        if O.hasLeft(Icrop)
            XUmin = OrigU(Icrop,1)-Xmin(Icrop);
            ImaShiftX = OrigU(Icrop,1)-X0;
        else
            XUmin = CCDSEC(Icrop,1);
            ImaShiftX = Xmin(Icrop)-X0 + XUmin-1;
        end
        if O.hasRight(Icrop)
            XUmax = CCDSEC(Icrop,2)-(Xmax(Icrop)-OrigU(Icrop,2));
        else
            XUmax = CCDSEC(Icrop,2);
        end
        if O.hasBottom(Icrop)
            YUmin = OrigU(Icrop,3)-Ymin(Icrop);
            ImaShiftY = OrigU(Icrop,3)-Y0;
        else
            YUmin = CCDSEC(Icrop,3);
            ImaShiftY = Ymin(Icrop)-Y0 + YUmin-1;
        end
        if O.hasTop(Icrop)
            YUmax = CCDSEC(Icrop,4)-(Ymax(Icrop)-OrigU(Icrop,4));
        else
            YUmax = CCDSEC(Icrop,4);
        end

        AIc = crop(AI(Icrop),[XUmin XUmax YUmin YUmax],'UpdateCat',true,'CreateNewObj',true);
        MCat(Icrop) = AIc.CatData;

        if O.hasLeft(Icrop)
            MCat(Icrop).Catalog(:,IndX) = MCat(Icrop).Catalog(:,IndX) + CatShiftX(Icrop) + XUmin;
        else
            MCat(Icrop).Catalog(:,IndX) = MCat(Icrop).Catalog(:,IndX) + CatShiftX(Icrop) + XUmin - 1;
        end
        if O.hasBottom(Icrop)
            MCat(Icrop).Catalog(:,IndY) = MCat(Icrop).Catalog(:,IndY) + CatShiftY(Icrop) + YUmin;
        else
            MCat(Icrop).Catalog(:,IndY) = MCat(Icrop).Catalog(:,IndY) + CatShiftY(Icrop) + YUmin - 1;
        end

        ImgAccum(ImaShiftY+1:ImaShiftY+YUmax-YUmin+1, ImaShiftX+1:ImaShiftX+XUmax-XUmin+1)  = AIc.ImageData.Data;
        MaskAccum(ImaShiftY+1:ImaShiftY+YUmax-YUmin+1, ImaShiftX+1:ImaShiftX+XUmax-XUmin+1) = AIc.MaskData.Data;

        CropArea(Icrop)  = (XUmax-XUmin+1).*(YUmax-YUmin+1);
        CropReg(Icrop,:) = [ImaShiftX+1, ImaShiftX+XUmax-XUmin+1, ImaShiftY+1, ImaShiftY+YUmax-YUmin+1];
    end

    % assemble the stitched AstroImage from the accumulated arrays
    Result = AstroImage({ImgAccum},'Mask',{MaskAccum});

    % the crop NearEdge and Overlap flags are meaningless after stitching
    %AllPix = true(Ny,Nx);
    FFne = Result.MaskData.findBit('NearEdge');
    FFov = Result.MaskData.findBit('Overlap');
    Result.MaskData = Result.MaskData.maskSet({FFne,FFov}, {'NearEdge','Overlap'}, [0 0], 'DefBitDict',Args.BitDict);
    %Result.MaskData = Result.MaskData.maskSet(AllPix, 'NearEdge', 0);
    %Result.MaskData = Result.MaskData.maskSet(AllPix, 'Overlap',  0);

    % propagate the PSF of the crops
    if ~strcmpi(Args.PSFMethod,'none')
        Result.PSFData = stitchPSF(AI, CropArea, CropReg, [Nx Ny], Args.PSFMethod);
    end

    % merge the catalogs:
    Result.CatData = merge(MCat);
    Result.CatData.JD = MCat(1).julday;
    RA0  = mean(Result.CatData.getCol('RA'));
    Dec0 = mean(Result.CatData.getCol('Dec'));

    % build WCS from the merged catalog
    if Args.UpdateWCS
        if isempty(Args.AstrometricCat)
            AstCatArg = {};
        else
            AstCatArg = {'CatName', Args.AstrometricCat};
        end
        [FitRes, Result.CatData, AstrometricCat] = imProc.astrometry.astrometryRefine(...
                                                        Result.CatData, 'RA',RA0, 'Dec',Dec0,...
                                                        'MatchMethod',Args.MatchMethod,...
                                                        AstCatArg{:});
        % When the refinement fails (e.g. too few reference sources) FitRes
        % carries an empty Tran/ResFit, and assigning those to the AstroWCS
        % errors. Keep the failure as a false Success instead of crashing.
        Result.WCS         = FitRes.WCS;
        if ~isempty(FitRes.Tran)
            Result.WCS.Tran2D = FitRes.Tran;
        end
        if ~isempty(FitRes.ResFit)
            Result.WCS.ResFit = FitRes.ResFit;
        end
        if isempty(FitRes.Success)
            Result.WCS.Success = false;
        else
            Result.WCS.Success = FitRes.Success;
        end
        if Result.WCS.Success
            Result.propagateWCS('UpdateCat',false);
        end
    end

    % calculate a new photometric ZP
    % skip it if the astrometry failed - the ZP would be meaningless and
    % photometricZP would in any case refuse to work on an unsuccessful WCS
    if Args.UpdateZP && (~Args.UpdateWCS || Result.WCS.Success)
        switch lower(Args.PhotZPMethod)
            case 'photometriczp'
                if isempty(Args.PhotCat)
                    PhCatArg = {};
                else
                    PhCatArg = {'CatName', Args.PhotCat};
                end
                [Result, ZPfit, PhotCat] = imProc.calib.photometricZP(Result, PhCatArg{:});
                % photometricZP writes the ZP under its own keyword only.
                % Downstream consumers may look it up under a different
                % synonym, so take the fitted value and write all of them.
                MeanZP = [];
                if ~isempty(ZPfit) && isfield(ZPfit,'ZP') && ~isempty(ZPfit(1).ZP) && isfinite(ZPfit(1).ZP)
                    MeanZP = ZPfit(1).ZP;
                end
            case 'header'
                ZP_vals = NaN(Ncrop, 1);
                for Icrop = 1:Ncrop
                    ZP_vals(Icrop) = AI(Icrop).HeaderData.getVal(Args.KeyZP);
                end
                MeanZP = mean(ZP_vals, 'omitnan');
            otherwise
                error('Unknown PhotZPMethod: %s', Args.PhotZPMethod);
        end

        % write the ZP under every synonym in KeyZP
        if ~isempty(MeanZP) && isfinite(MeanZP)
            for Ikey=1:1:numel(Args.KeyZP)
                Result.HeaderData = replaceVal(Result.HeaderData, Args.KeyZP{Ikey}, MeanZP);
            end
        end
    end

    % add a mean JD (should be the same, but still):
    MeanJD = mean(julday(AI));
    Result.HeaderData = replaceVal(Result.HeaderData, 'MIDJD', MeanJD);

    % add a mean EXPTIME:
    MeanExp = mean([AI.getStructKey('EXPTIME').EXPTIME]);
    Result.HeaderData = replaceVal(Result.HeaderData, 'EXPTIME', MeanExp);

    % add some keywords from the first crop:
    Keys = {'NODENUMB','MOUNTNUM','CAMNUM','IMTYPE','NCOADD'};
    Vals = cellfun(@(k) AI(1).HeaderData.getVal(k), Keys, 'UniformOutput', false);
    Result.HeaderData = replaceVal(Result.HeaderData, Keys, Vals);
end

function Result = stitchPSF(AI, CropArea, CropReg, StitchSize, Method)
    % Build the PSF of a stitched image out of the PSFs of its crops
    % Input  : - the array of crop AstroImages.
    %          - a vector with the area each crop contributes to the stitch.
    %          - an [Ncrop, 4] matrix with the [Xmin Xmax Ymin Ymax] region each
    %            crop contributes, in the pixel coordinates of the stitched image.
    %          - the [Nx, Ny] size of the stitched image.
    %          - 'central' | 'wmean', see the PSFMethod argument of stitchCrops.
    % Output : - an AstroPSF object (empty if no crop carries a usable PSF).
    % Author : A.M. Krassilchtchikov (Aug 2026)

    Result = AstroPSF;

    % use only the crops carrying a usable PSF
    Ncrop  = numel(AI);
    Good   = false(Ncrop,1);
    Nstars = NaN(Ncrop,1);
    for Icrop = 1:Ncrop
        Data = AI(Icrop).PSFData.DataPSF;
        % a non-positive stamp sum would turn the normalization into NaN/Inf
        Good(Icrop)   = ~isempty(Data) && all(isfinite(Data),'all') && all(sum(Data,[1 2])>0,'all');
        Nstars(Icrop) = AI(Icrop).PSFData.Nstars;
    end
    if ~any(Good)
        return
    end
    Ind = find(Good);

    % the crop closest to the centre of the stitched image: the distance to the
    % contributed region (0 when the centre falls inside it), with the distance
    % to the centre of the region as a tie breaker
    Xc      = (1+StitchSize(1))./2;
    Yc      = (1+StitchSize(2))./2;
    DistReg = hypot( max(0, max(CropReg(:,1)-Xc, Xc-CropReg(:,2))), ...
                     max(0, max(CropReg(:,3)-Yc, Yc-CropReg(:,4))) );
    DistCen = hypot( (CropReg(:,1)+CropReg(:,2))./2-Xc, (CropReg(:,3)+CropReg(:,4))./2-Yc );
    DistReg(~Good) = Inf;
    DistCen(~Good) = Inf;
    [~, Ord] = sortrows([DistReg, DistCen]);
    Icen     = Ord(1);
    Ref      = AI(Icen).PSFData;

    switch lower(Method)
        case 'central'
            % AstroPSF is a handle object, so copy it not to alias the input
            Result = Ref.copy;
        case 'wmean'
            % averaging is meaningful only for stamps sharing a common grid
            Ndim  = max(0, ndims(Ref.DataPSF)-2);
            Sizes = arrayfun(@(X) isequal(size(X.PSFData.DataPSF), size(Ref.DataPSF)), AI(Ind));
            Grids = arrayfun(@(X) isequal(X.PSFData.DimName(1:Ndim), Ref.DimName(1:Ndim)) && ...
                                  isequal(X.PSFData.DimVals(1:Ndim), Ref.DimVals(1:Ndim)), AI(Ind));
            if ~all(Sizes & Grids)
                warning('imProc:stack:stitchCrops:PSFGridMismatch',...
                        'The crop PSFs are not on a common grid, falling back to the PSF of the central crop');
                Result = Ref.copy;
            else
                Weights = CropArea(Ind);
                PsfCell = arrayfun(@(X) X.PSFData.DataPSF, AI(Ind), 'UniformOutput',false);
                Result.DataPSF = imUtil.psf.combinePSF(PsfCell(:).', 'Weights',Weights);

                % the variance refers to the 2D photometry stamp, i.e. to the
                % leading slice of the PSF data, so combine it against that one
                VarCell = arrayfun(@(X) X.PSFData.DataVar, AI(Ind), 'UniformOutput',false);
                SizeVar = size(Ref.DataVar);
                if ~isempty(Ref.DataVar) && all(cellfun(@(X) isequal(size(X), SizeVar), VarCell)) && ...
                        isequal(SizeVar, size(Ref.DataPSF,[1 2]))
                    Psf2D = cellfun(@(X) X(:,:,1), PsfCell, 'UniformOutput',false);
                    [~, Result.DataVar] = imUtil.psf.combinePSF(Psf2D(:).', 'Weights',Weights, 'Var',VarCell(:).');
                end

                if ~all(isnan(Nstars(Ind)))
                    Result.Nstars = sum(Nstars(Ind), 'omitnan');
                end
                Result.Scale        = Ref.Scale;
                Result.DimName      = Ref.DimName;
                Result.DimVals      = Ref.DimVals;
                Result.InterpMethod = Ref.InterpMethod;
            end
        otherwise
            error('Unknown PSFMethod: %s', Method);
    end
end
