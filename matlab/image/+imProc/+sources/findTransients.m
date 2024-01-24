function Result=findTransients(New, Ref, D, S, Scorr, Z2, S2, SdN, SdR, Args)
    %{ 
    Performs a transients search on a set of products derived by the proper
    subtraction of a new and a reference image. Finds local maxima in the
    Scorr statistic image above a defined threshold. Derives further values
    such as significances and fluxes for identified transient candidates. 
    Results are then stored in a table summerising the transients values.

    Input : 
        - New (AstroImage) New image object.
        - Ref (AstroImage) Reference image object.
        - D (AstroImage) Proper subtraction image object between 
        New and Ref images.
        - S (AstroImage) Proper subtraction score image object.
        - Scorr (AstroImage) Corrected proper subtraction score imabe 
        object.
        - Z2 (AstroImage) Translient score image object.
        - S2 (AstroImage) Squared proper subtraction image object.
        - SdN (AstroImage) Proper subtraction score image object for a
        delta PSF in New. For e.g. cosmic ray hits.
        - SdR (AstroImage) Proper subtraction score image object for a
        delta PSF in Ref. For e.g. cosmic ray hits.

        * ...,key,val,...
        'HalfSizePSF' - Half size of area on transients positions in 
            image. Actual size will be 1+2*HalfSizePSF. Used to cut out 
            an image area to perform PSF photometry on.
            Default is 7.
        'Threshold' - Threshold in units of std (=sqrt(Variance)). Search
        for local maxima only above this threshold. Default is 5.
        'findLocalMaxArgs' - Args passed into imUtil.sources.findLocalMax()
            when looking for local maxima in Scorr.
            Default is {}.
        'BitCutHalfSize' - Half size of area on transients positions in 
            image bit masks. Actual size will be 1+2*BitCutHalfSize. Used
            to retrieve bit mask values around transient positions.
            Default is 3.
        'psfPhotCubeArgs' - Args passed into imUtil.sources.psfPhotCube when
            performing PSF photometry on New, Ref, and D cut outs.
            Default is {}.
        'NewMask_BadHard' - Hard bit mask criteria for bad pixels in 
            New image. Default is {'Overlap','Edge','CR_DeltaHT','NaN'}.
        'RefMask_BadHard' - Hard bit mask criteria for bad pixels in 
            Ref image. Default is {'Saturated','Overlap','Edge',
            'CR_DeltaHT','NaN'}.
        'NewMask_BadSoft' - Soft bit mask criteria for bad pixels in 
            New Image. Default is {'HighRN', 'DarkHighVal', 'BiasFlaring', 
            'Hole', 'Interpolated', 'SrcNoiseDominated'}.
        'RefMask_BadSoft' - Soft bit mask criteria for bad pixels in 
            Ref image. Default is {'HighRN', 'DarkHighVal', 'BiasFlaring', 
            'Hole', 'Interpolated', 'SrcNoiseDominated'}.
        'Chi2dofLimits' - Chi2 per degrees of limits, used to flag if chi2
            per degrees of freedom is within the limits.
            Default is [0.5 2].
    Output : 
        - Result (struct) Object holding the result of transient search. 
        Transient values are given in Result.TranTable.

    Author : Eran Ofek (2023)
    Example: imProc.sources.findTransients(AIreg(2), AIreg(1), DD, S, Scorr, Z2, S2)
    %}

    arguments
        New AstroImage
        Ref AstroImage
        D                          = [];
        S                          = [];
        Scorr                      = [];
        Z2                         = [];
        S2                         = [];
        SdN                        = [];
        SdR                        = [];

        Args.Threshold             = 5;

        Args.HalfSizePSF           = 7;
        Args.HalfSizeTS            = 5;
        Args.findLocalMaxArgs cell = {};
        Args.BitCutHalfSize        = 3;
        Args.psfPhotCubeArgs cell  = {};

        % selection
        Args.NewMask_BadHard       = {'Overlap','Edge','CR_DeltaHT','NaN'};
        Args.RefMask_BadHard       = {'Saturated','Overlap','Edge','CR_DeltaHT','NaN'};
        Args.NewMask_BadSoft       = {'HighRN', 'DarkHighVal', 'BiasFlaring', 'Hole', 'Interpolated', 'SrcNoiseDominated'};
        Args.RefMask_BadSoft       = {'HighRN', 'DarkHighVal', 'BiasFlaring', 'Hole', 'Interpolated', 'SrcNoiseDominated'};

        Args.Chi2dofLimits         = [0.5 2];
    end

    
    Nobj = numel(New);

    % reverse order to initiate Result array with proper size on first 
    % iteration
    for Iobj=Nobj:-1:1
        % for each image

        % find positive and negative sources in S
        [PosLocalMax] = imUtil.sources.findLocalMax(Scorr(Iobj).Image, ...
            'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});
        [NegLocalMax] = imUtil.sources.findLocalMax(-Scorr.Image, ...
            'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});

        % Output *LocalMax contains: [X,Y,SN,ImageIndex,LinaerIndexIn2D]
        % Merge pos/neg lists and add sign to the SN column
        NegLocalMax(:,3) = -NegLocalMax(:,3);
        LocalMax = [PosLocalMax; NegLocalMax];
        Nsrc     = size(LocalMax,1);

        % get Mask values within cutouts around pos/neg transients candidates
        
        NewMaskVal = New(Iobj).MaskData.bitwise_cutouts(LocalMax(:,1:2), ...
            'or', 'HalfSize',Args.BitCutHalfSize);
        RefMaskVal = Ref(Iobj).MaskData.bitwise_cutouts(LocalMax(:,1:2), ...
            'or', 'HalfSize',Args.BitCutHalfSize);

        % identify pixels with bit mask values matching hard and soft bad
        % pixel criteria
        BD=BitDictionary;
        NewFlagBad  = BD.findBit(NewMaskVal, Args.NewMask_BadHard, 'Method','any');
        RefFlagBad  = BD.findBit(RefMaskVal, Args.RefMask_BadHard, 'Method','any');
        NewFlagSoft = BD.findBit(NewMaskVal, Args.NewMask_BadSoft, 'Method','any');
        RefFlagSoft = BD.findBit(RefMaskVal, Args.RefMask_BadSoft, 'Method','any');

        
        % PSF fit all candidates in the D image
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(D(Iobj).Image, LocalMax(:,1), LocalMax(:,2), Args.HalfSizePSF);
        % Change the sign of negative sources
        Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
        Psf = imUtil.psf.full2stamp(D(Iobj).PSFData.getPSF, 'StampHalfSize',Args.HalfSizePSF.*ones(1,2));
        [ResultD, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', Psf, Args.psfPhotCubeArgs{:});
    
        % PSF fit all candidates in the New image
        CutHalfSize = (size(New(Iobj).PSFData.getPSF,1)-1).*0.5;
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(New(Iobj).Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        % Change the sign of negative sources
        Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
        [ResultN, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', New(Iobj).PSFData.getPSF, Args.psfPhotCubeArgs{:});
    
        % PSF fit all candidates in the Ref image
        CutHalfSize = (size(Ref(Iobj).PSFData.getPSF,1)-1).*0.5;
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(Ref(Iobj).Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        % Change the sign of negative sources
        Cube = Cube.*reshape(-sign(LocalMax(:,3)), [1 1 Nsrc]);
        [ResultR, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', Ref(Iobj).PSFData.getPSF, Args.psfPhotCubeArgs{:});
    
        % value at position
        ValScorr = Scorr(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        ValS     = S(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));

        if isempty(SdN)
            ValSdN = nan(Nsrc,1);
        else
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(SdN(Iobj).Image, LocalMax(:,1), LocalMax(:,2), 2);
            ValSdN   = squeeze(max(Cube,[],[1 2]));
            %ValSdN   = SdN(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        end
        if isempty(SdR)
            ValSdR = nan(Nsrc,1);
        else
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(SdR(Iobj).Image, LocalMax(:,1), LocalMax(:,2), 2);
            ValSdR   = squeeze(max(Cube,[],[1 2]));
            %ValSdR   = SdR(Iobj).getImageVal(LocalMax(:,1),LocalMax(:,2));
        end

        % find and save peak TS and corresponding gaussian significance for
        % S2 and Z2 statistics

        % set the S2 and/or Z2 values to NaN for all transients if the TS
        % maps do not exist or are empty, otherwise run process_TS_map()
        if isempty(S2)
            S2_TS = nan(Nsrc,1);
            S2_sig = nan(Nsrc,1);
        else
            [S2_TS, S2_sig] = process_TS_map(S2(Iobj).Image, ...
                LocalMax(:,1), LocalMax(:,2), Args.HalfSizeTS, 1);
        end

        if isempty(Z2)
            Z2_TS = nan(Nsrc,1);
            Z2_sig = nan(Nsrc,1);
        else
            [Z2_TS, Z2_sig] = process_TS_map(Z2(Iobj).Image, ...
                LocalMax(:,1), LocalMax(:,2), Args.HalfSizeTS, 2);
        end

        % fill result struct
        Result(Iobj).Flag.ThresholdD  = ResultD.SNm>Args.Threshold;
        Result(Iobj).Flag.ThresholdScorr = ValScorr>Args.Threshold;
        Result(Iobj).Flag.ThresholdSfit  = ResultD.SNm>Args.Threshold;

        Result(Iobj).Flag.NotCR          = abs(ValS)>(abs(ValSdN)+1) & abs(ValS)>(abs(ValSdR)+1);

        Chi2dof = ResultD.Chi2./ResultD.Dof;
        Result(Iobj).Flag.Chi2        = Chi2dof>Args.Chi2dofLimits(1) & Chi2dof<Args.Chi2dofLimits(2);
        Result(Iobj).Flag.MaskHard    = ~NewFlagBad & ~RefFlagBad;
        Result(Iobj).Flag.MaskSoft    = ~NewFlagSoft & ~RefFlagSoft;

        Result(Iobj).Flag.SummaryHard = Result(Iobj).Flag.ThresholdScorr & Result(Iobj).Flag.Chi2 & Result(Iobj).Flag.ThresholdSfit & Result(Iobj).Flag.MaskHard & Result(Iobj).Flag.NotCR;
        Result(Iobj).FlagSummaryHard  = Result(Iobj).Flag.SummaryHard;
        Result(Iobj).Ntran = sum(Result(Iobj).Flag.SummaryHard);
        
        Result(Iobj).Flux = ResultD.Flux; %.*F_S(Iobj);   % need to multiply by F_S
        Result(Iobj).SNm  = ResultD.SNm;
        Result(Iobj).LocalMax = LocalMax;

        [RA, Dec] = xy2sky(New.WCS, LocalMax(:,1), LocalMax(:,2),'OutUnits','rad');
        RA = cast(RA,'double');
        Dec = cast(Dec,'double');

        [MidJD, ExpTime] = New.julday();

        col_size = size(RA);
        ExpTime_d = ExpTime/3600/24;
        StartJD = MidJD-ExpTime_d/2;
        EndJD = MidJD+ExpTime_d/2;

        MidJD = MidJD*ones(col_size);
        StartJD = StartJD*ones(col_size);
        EndJD = EndJD*ones(col_size);

        TranTable = AstroCatalog;
       
        TranTable.ColNames = {'XPEAK', 'YPEAK', 'RA', 'Dec',  ...
            'StartJD', 'MidJD', 'EndJD',...
            'PSF_SNm', 'Chi2_D', 'NewMaskVal', 'RefMaskVal',...
            'ValSdN', 'ValSdR', 'Scorr', 'Z2_TS', 'Z2_Sig', 'S2_TS', 'S2_Sig', ...
            'N_SNm', 'N_Chi2dof', 'N_Flux', 'N_Mag', ...
            'R_SNm', 'R_Chi2dof', 'R_Flux', 'R_Mag',...
            };

        TranTable.Catalog  = table(LocalMax(:,1), LocalMax(:,2), RA, Dec, ...
            StartJD, MidJD, EndJD,...
            ResultD.SNm, Chi2dof,  NewMaskVal,  RefMaskVal, ...
            ValSdN,  ValSdR, LocalMax(:,3), Z2_TS, Z2_sig, S2_TS, S2_sig, ...
            ResultN.SNm, ResultN.Chi2./ResultN.Dof, ResultN.Flux, ResultN.Mag, ...
            ResultR.SNm, ResultR.Chi2./ResultR.Dof, ResultR.Flux, ResultR.Mag... 
            );

        TranTable.ColUnits = {'','','','',...
            '','','',...
            '','','','',...
            '','','','','','','',...
            '','','','',...
            '','','','',...
            };


        TranTable.Catalog.Properties.VariableNames = TranTable.ColNames;
        TranTable.Catalog.Properties.VariableUnits = TranTable.ColUnits;

        TranTable = imProc.match.match_catsHTMmerged(TranTable);
        [TranTable, ~, ~, ~] = imProc.match.match_catsHTM(TranTable, ...
            'GLADE','ColDistName','Galaxy_Dist','ColNmatchName','Galaxy_Matches');
        [TranTable, ~, ~, ~] = imProc.match.match_catsHTM(TranTable, ...
            'CRTS_per_var','ColDistName','VarStar_Dist','ColNmatchName','VarStar_Matches');

        Result(Iobj).TranTable = TranTable;
        
    end

end

function [TS, significance] = process_TS_map(TS_map, x_vec, y_vec, dist, dof)
    %{
    For a given set of (x,y) coordinates on a test static map, finds the
    peak values within a square centered on each pair of given coordinates.
    Then converts peak TS values into gaussian significance assuming
    the TS values follow chi2 distribution of a given degrees of freedom.

    Input :
        - TS_map (double matrix) Matrix containing the test statistic.
        - x_vec (double vector) Vector containing the x query coordinates.
        - y_vec (double vector) Vector containing the y query coordinates.
        - dist (int) Distance specifying the length of the search square in 
        which to search for the TS peak in. The side length of the square 
        equals to 2*dist+1.
        - dof (int) Degrees of freedom of the chi2 distribution attributed to
        the test statistic values.

    Output :
        - TS (double vector) Vector containing the TS peak values attributed
        to the (x,y) query coordinates.
        - significance (double vector) Vector containing the gaussian
        significance derived from the TS peak values.
    %}

    % check if TS map exists and is a matrix
    if isempty(TS_map)
        error("Test statistic map does not exist or is empty.");
    elseif ~ismatrix(TS_map)
        error("Test statistic map should be a matrix.");
    end
    
    num_trans = numel(x_vec);
    
    % return empty results if no transients are found
    if num_trans == 0
        TS = [];
        significance = [];
        return;
    end

    % pad TS map with zeros to account for on-edge transient queries
    TS_map = padarray(TS_map, [dist, dist]);

    % construct query indices relative to the search square center position
    % account for zero padded matrix by adding dist
    x_rel = 0:dist*2;
    y_rel = 0:dist*2;

    % construct and fill TS vector
    TS = zeros(num_trans,1);
    for n=1:num_trans
        % query all positions within search square centered on original 
        % (x,y) coordinates
        x_query = x_vec(n)+x_rel;
        y_query = y_vec(n)+y_rel;
        TS0 = TS_map(y_query,x_query);
        % take only the maximum TS value within search square
        TS(n) = max(TS0, [],'all');
    end
    
    % convert TS values to gaussian significance
    p_val = chi2cdf(TS,dof,'upper');
    significance = -norminv(p_val);
    significance(isinf(significance)) = nan;
end

