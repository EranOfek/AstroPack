function Result=findTransients(New, Ref, D, S, Scorr, Z2, S2, F_S, SdN, SdR, Args)
    %
    % Example: imProc.sources.findTransients(AIreg(2), AIreg(1), DD, S, Scorr, Z2, S2)
   

    arguments
        New AstroImage
        Ref AstroImage
        D                          = [];
        S                          = [];
        Scorr                      = [];
        Z2                         = [];
        S2                         = [];
        F_S                        = [];
        SdN                        = [];
        SdR                        = [];

        Args.HalfSizePSF           = 7;
        Args.NormS logical         = true;

        Args.Threshold             = 5;
        Args.findLocalMaxArgs cell = {};
        Args.BitCutHalfSize        = 3;
        % Args.CutHalfSize           = 12; must be like PSF size
        Args.psfPhotCubeArgs cell  = {};

        % selection
        Args.NewMask_BadHard       = {'Overlap','Edge','CR_DeltaHT','NaN'};
        Args.RefMask_BadHard       = {'Saturated','Overlap','Edge','CR_DeltaHT','NaN'};
        Args.NewMask_BadSoft       = {'HighRN', 'DarkHighVal', 'BiasFlaring', 'Hole', 'Interpolated', 'SrcNoiseDominated'};
        Args.RefMask_BadSoft       = {'HighRN', 'DarkHighVal', 'BiasFlaring', 'Hole', 'Interpolated', 'SrcNoiseDominated'};

        Args.Chi2dofLimits         = [0.5 2];
    end

    
    Nobj = numel(New);
    for Iobj=1:1:Nobj
        % for each image

        % find positive and negative sources in S
        [PosLocalMax] = imUtil.sources.findLocalMax(Scorr(Iobj).Image, 'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});
        [NegLocalMax] = imUtil.sources.findLocalMax(-Scorr.Image, 'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});
        % Output *LocalMax contains: [X,Y,SN,ImageIndex,LinaerIndexIn2D]
        % Merge pos/neg lists and add sign to the SN column
        NegLocalMax(:,3) = -NegLocalMax(:,3);
        LocalMax = [PosLocalMax; NegLocalMax];
        Nsrc     = size(LocalMax,1);

        % get Mask values within cutouts around pos/neg transients candidates
        
        NewMaskVal = New(Iobj).MaskData.bitwise_cutouts(LocalMax(:,1:2), 'or', 'HalfSize',Args.BitCutHalfSize);
        RefMaskVal = Ref(Iobj).MaskData.bitwise_cutouts(LocalMax(:,1:2), 'or', 'HalfSize',Args.BitCutHalfSize);
        BD=BitDictionary;

        NewFlagBad  = BD.findBit(NewMaskVal, Args.NewMask_BadHard, 'Method','any');
        RefFlagBad  = BD.findBit(RefMaskVal, Args.RefMask_BadHard, 'Method','any');
        NewFlagSoft = BD.findBit(NewMaskVal, Args.NewMask_BadSoft, 'Method','any');
        RefFlagSoft = BD.findBit(RefMaskVal, Args.RefMask_BadSoft, 'Method','any');

        


        % PSF fit all candidates in the D image
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(D(Iobj).Image, LocalMax(:,1), LocalMax(:,2), Args.HalfSizePSF);
        % Change the sign of negative sources
        Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
        Psf = imUtil.psf.full2stamp(D(Iobj).PSFData.getPSF, 'StampHalfSize',Args.HalfSizePSF.*ones(1,2));
        [ResultD, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF', Psf, Args.psfPhotCubeArgs{:});
    
        Chi2dof = ResultD.Chi2./ResultD.Dof;
        Flag = Chi2dof>0.5 & Chi2dof<2;

        % PSF fit all candidates in the New image
        CutHalfSize = (size(New(Iobj).PSFData.getPSF,1)-1).*0.5;
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(New(Iobj).Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        % Change the sign of negative sources
        Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
        [ResultN, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF', New(Iobj).PSFData.getPSF, Args.psfPhotCubeArgs{:});
    
        % PSF fit all candidates in the Ref image
        CutHalfSize = (size(Ref(Iobj).PSFData.getPSF,1)-1).*0.5;
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Ref(Iobj).Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        % Change the sign of negative sources
        Cube = Cube.*reshape(-sign(LocalMax(:,3)), [1 1 Nsrc]);
        [ResultR, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF', Ref(Iobj).PSFData.getPSF, Args.psfPhotCubeArgs{:});
    
        % value at position
        ValScorr = Scorr(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        ValS     = S(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        if isempty(SdN)
            ValSdN = nan(Nsrc,1);
        else
            [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(SdN(Iobj).Image, LocalMax(:,1), LocalMax(:,2), 2);
            ValSdN   = squeeze(max(Cube,[],[1 2]));
            %ValSdN   = SdN(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        end
        if isempty(SdR)
            ValSdR = nan(Nsrc,1);
        else
            [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(SdR(Iobj).Image, LocalMax(:,1), LocalMax(:,2), 2);
            ValSdR   = squeeze(max(Cube,[],[1 2]));
            %ValSdR   = SdR(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        end
        if isempty(Z2) || isempty(S2)
            ValZ2 = nan(Nsrc,1);
            ValS2 = nan(Nsrc,1);

        else
            ValZ2    = Z2(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
            ValS2    = S2(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        end
        
        %Chi2dof = ResultN.Chi2./ResultN.Dof;
        Result(Iobj).Flag.ThresholdD  = ResultD.SNm>Args.Threshold;
        Result(Iobj).Flag.ThresholdScorr = ValScorr>Args.Threshold;
        Result(Iobj).Flag.ThresholdSfit  = ResultD.SNm>Args.Threshold;

        Result(Iobj).Flag.NotCR          = abs(ValS)>(abs(ValSdN)+1) & abs(ValS)>(abs(ValSdR)+1);

        Result(Iobj).Flag.Chi2        = Chi2dof>Args.Chi2dofLimits(1) & Chi2dof<Args.Chi2dofLimits(2);
        Result(Iobj).Flag.MaskHard    = ~NewFlagBad & ~RefFlagBad;
        Result(Iobj).Flag.MaskSoft    = ~NewFlagSoft & ~RefFlagSoft;

        Result(Iobj).Flag.SummaryHard = Result(Iobj).Flag.ThresholdScorr & Result(Iobj).Flag.Chi2 & Result(Iobj).Flag.ThresholdSfit & Result(Iobj).Flag.MaskHard & Result(Iobj).Flag.NotCR;
        Result(Iobj).FlagSummaryHard  = Result(Iobj).Flag.SummaryHard;
        Result(Iobj).Ntran = sum(Result(Iobj).Flag.SummaryHard);
        
        Result(Iobj).Flux = ResultD.Flux; %.*F_S(Iobj);   % need to multiply by F_S
        Result(Iobj).SNm  = ResultD.SNm;
        Result(Iobj).LocalMax = LocalMax;

        TranTable = AstroCatalog;
        TranTable.ColNames = {'XPEAK',            'YPEAK',       'Scorr',       'PSF_SNm',   'Chi2_D', 'NewMaskVal','RefMaskVal','ValSdN','ValSdR', 'ValZ2', 'ValS2', 'N_SNm',    'N_Chi2dof',               'N_Flux',     'R_SNm',     'R_Chi2dof',               'R_Flux'};
        TranTable.Catalog  = table(LocalMax(:,1), LocalMax(:,2), LocalMax(:,3), ResultD.SNm, Chi2dof,  NewMaskVal,  RefMaskVal,  ValSdN,  ValSdR,   ValZ2,   ValS2,   ResultN.SNm, ResultN.Chi2./ResultN.Dof, ResultN.Flux, ResultR.SNm, ResultR.Chi2./ResultR.Dof, ResultR.Flux );
        TranTable.Catalog.Properties.VariableNames = TranTable.ColNames;
        Result(Iobj).TranTable = TranTable;
        

    end

end