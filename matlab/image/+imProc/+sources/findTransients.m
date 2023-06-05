function Result=findTransients(New, Ref, D, S, Scorr, Z2, F_S, Args)
    %
    % Example: imProc.sources.findTransients(AIreg(2), AIreg(1), D, S, Scorr, Z2)
   

    arguments
        New AstroImage
        Ref AstroImage
        D                          = [];
        S                          = [];
        Scorr                      = [];
        Z2                         = [];
        F_S                        = [];

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
    
        % Scorr value at position
        ValScorr = Scorr(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));

        %Chi2dof = ResultN.Chi2./ResultN.Dof;
        Result(Iobj).Flag.Threshold = ResultD.SNm>Args.Threshold;
        Result(Iobj).Flag.Chi2      = Chi2dof>Args.Chi2dofLimits(1) & Chi2dof<Args.Chi2dofLimits(2);
        Result(Iobj).Flag.MaskHard  = ~NewFlagBad & ~RefFlagBad;
        Result(Iobj).Flag.MaskSoft  = ~NewFlagSoft & ~RefFlagSoft;
        Result(Iobj).Flag.SummaryHard = Result(Iobj).Flag.Threshold & Result(Iobj).Flag.Chi2 & Result(Iobj).Flag.MaskHard;
        Result(Iobj).Ntran = sum(Result(Iobj).Flag.SummaryHard);
        
        Result(Iobj).Flux = ResultD.Flux; %.*F_S(Iobj);   % need to multiply by F_S
        Result(Iobj).SNm  = ResultD.SNm;


    end

end