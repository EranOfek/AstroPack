function [Result, MeanPSF, VarPSF, NimPSF] = constructPSF(Image, Args)
    % Select stars and construct PSF from image or a cube of selected sources.
    %   The function can be used to either select good stars for PSF
    %   construction, or to select stars and construct a mean PSF.
    %   The function can start with a cube of sources, or an image from
    %   which sources will be selected.
    %   The function select good PSF stars by the following criteria:
    %       S/N with some PSF is larger than the S/N of a delta-function
    %       PSF.
    %       Second moment is similar to the typical 2nd moment of stars.
    %       The star does not have neighboors within some radius.
    % Input  : - A 2-D image or a cube of sources.
    %          * ...,key,val,...
    %            'X' - An optional vector of X positions in the 2-D image
    %                   of candidate PSF stars.
    %                   If empty, and 1st input is a 2-D matrix, then will
    %                   use imUtil.sources.findSources to find stars in the
    %                   image.
    %                   Default is [].
    %            'Y' - Like 'X', but for the Y coordinate.
    %                   Default is [].
    %            'SN' - A matrix with one or more columns containing S/N
    %                   per star in each row. The columns corresponds to
    %                   S/N measured with different PSFs.
    %                   Typically, the 1st column is for a delta function
    %                   PSF, and the 2nd column for a stellar-like PSF.
    %                   If empty, then select sources by S/N diff between
    %                   different PSFs will not be used (see also 'SNdiff'
    %                   argument).
    %                   Default is [].
    %            'Back' - An optional background image of the 2-D input
    %                   image. This is needed for imUtil.sources.findSources
    %                   (if relevant).
    %                   If given, it will be also subtracted from the Cube
    %                   of stars. See also 'SubAnnulusBack' argument.
    %                   Default is [].
    %            'Var' - Variance image for the source detection.
    %                   This is required if imUtil.sources.findSources is
    %                   used.
    %                   Default is [].
    %            'SubAnnulusBack' - A logical indicating if to estimate the
    %                   background of each star in an annulus around the star and
    %                   subtract it.
    %                   Note that you can control the
    %                   imUtil.sources.backgroundCube arguments using the
    %                   'backgroundCubeArgs' argument.
    %                   Default is true.
    %            'RadiusPSF' - Radius of PSF to construct.
    %                   Stamp size will be radiusPSF*2 +1.
    %                   Default is 8.
    %            'image2cutoutsArgs' - A cell array of arguments to pass to
    %                   imUtil.cut.image2cutouts
    %                   Default is {}.
    %            'ThresholdPSF' - Threhold for PSF detection.
    %                   Default is 20.
    %            'RangeSN' - range of S/N of sources to use for PSF
    %                   construction.
    %                   Default is [50 1000].
    %            'InitPsf' - A function handle to create a PSF stamp for
    %                   the source detection.
    %                   Default is @imUtil.kernel2.gauss
    %            'InitPsfArgs' - A cell array of arguments to pass to the
    %                   function provided in 'InitPsf'.
    %                   Default is {[0.1;2]}
    %            'Conn' - Local maxima finding connectivity.
    %                   Default is 8.
    %            'CleanSources' - A logical indicating if to clean sources
    %                   using imUtil.sources.findSources
    %                   This will include remove sources near the image
    %                   edge, and using the S/N diff method.
    %                   Default is true.
    %            'cleanSourcesArgs' - A cell array of arguments to pass to
    %                   the imUtil.sources.findSources cleanSources argument.
    %                   Default is {}.
    %            'backgroundCubeArgs' - A cell array of arguments to pass
    %                   to imUtil.sources.backgroundCube
    %                   Default is {}.
    %
    %            'SNdiff' - The mim. diff. between the second S/N column
    %                   and first S/N column to declare a good source.
    %                   Default is 0.
    %            'moment2Args' - A cell array of arguments to pass to
    %                   imUtil.sources.moment2
    %                   Default is {}.
    %            'DeltaSigma' - Maximum differennce between the median 2nd
    %                   moment of sources and the 2nd moment itself.
    %                   If empty, then skip this step.
    %                   Default is 0.5. OBSOLETE.
    %            'SigmaQuantile' - Select only stars wihich 2nd moment
    %                   Sigma is in these quantile rane.
    %                   Default is [0.1 0.7].
    %            'NighRadius' - If a star has a neighboor star within this
    %                   radius, then it will not be used.
    %                   Default is 7.
    %            'MinNumGoodPsf' - Minimum number of good PSF stars needed
    %                   in order to construct a mater PSF.
    %                   Default is 5.
    %            'constructPSF_cutoutsArgs' - A cell array of arguments to
    %                   pass to imUtil.psf.constructPSF_cutouts
    %                   Default is {}.
    %            'SumMethod' - PSF sum method (see
    %                   imUtil.psf.constructPSF_cutouts for options).
    %                   Default is 'median'.
    %
    %            'SmoothWings' - Smooth wings using imUtil.psf.psf_zeroConvergeArgs
    %                   Default is true.
    %            'SuppressWidth' - A width of the PSF zero suppress
    %                   function (see imUtil.psf.suppressEdges).
    %                   Default is 3.
    %            'SuppressFun' - Default is @imUtil.kernel2.cosbell
    %             
    %            'DataType' - if not empty, convert the result to to 'single', or
    %            'double' or other type if needed; Default is empty
    %
    %            'CropByQuantile' - crop the stamp so that it contain only
    %                   the central part of the stamp with flux above 'Quantile' 
    %                   Default is false
    %            'Quantile' - defines the flux level where the stamp should be
    %                   cut if 'CropByQuantile' is true
    %                   Default is 0.999
    %
    % Output : - A structure containing information about the stars used
    %            for the PSF construction.
    %          - Mean constructed PSF.
    %          - PSF variance.
    %          - Number of actial PSF used.
    %
    % Author : Eran Ofek (Jul 2023)
    % Example: AI=AstroImage('LAST.00.01.01_20220303.224914.224_clear__001_001_001_sci_raw_Image_1.fits');
    %          AI.crop([2000 3000 2000 3000]);
    %          AI=imProc.background.background(AI);   
    %          [Result, MeanPSF, VarPSF, NimPSF] = imUtil.psf.constructPSF(AI.Image)
    
    arguments
        Image
        Args.X                      = [];
        Args.Y                      = [];
        Args.SN                     = [];
        Args.Back                   = [];
        Args.Var                    = [];
        Args.SubAnnulusBack logical = true;
       
        Args.RadiusPSF                 = 8;
        Args.Annulus                   = [10 12];
        
        Args.image2cutoutsArgs cell    = {};
        
        %Args.Threshold                 = 5;
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss;
        Args.InitPsfArgs cell          = {[0.1;2]};
        Args.Conn                      = 8;
        Args.CleanSources              = true;
        Args.cleanSourcesArgs cell     = {};
        Args.backgroundCubeArgs cell   = {};
        
        Args.SNdiff                    = 0;  % if empty skip
        Args.moment2Args cell          = {};
        Args.DeltaSigma                = 0.5;   % if empty skip
        Args.SigmaQuantile             = [0.1 0.7];

        Args.NighRadius                = 7;     % if empty skip
        Args.MinNumGoodPsf             = 5;
        
        Args.constructPSF_cutoutsArgs cell = {};
        Args.SumMethod                 = 'median';
        
        Args.SmoothWings logical       = true;
        Args.SuppressFun               = @imUtil.kernel2.cosbell;
        Args.SuppressWidth             = 3;
        
        Args.DataType                  = []; % or '@single', '@double',...
        
        Args.CropByQuantile logical    = false;
        Args.Quantile                  = 0.999;
    end
    
    if ndims(Image)==2
        % input is an image - search sources
        if isempty(Args.Back) || isempty(Args.Var)
            error('For 2-D image input Back and Var must be provided');
        end
        if isempty(Args.X) || isempty(Args.Y) || isempty(Args.SN)
            [FindSrcSt] = imUtil.sources.findSources(Image, 'Threshold',Args.ThresholdPSF,...
                                                              'PsfFun',Args.InitPsf,...
                                                              'PsfFunPar',Args.InitPsfArgs,...
                                                              'ForcedList',[],...
                                                              'OnlyForced',false,...
                                                              'BackIm',Args.Back,...
                                                              'VarIm',Args.Var,...
                                                              'Conn',Args.Conn,...
                                                              'CleanSources',Args.CleanSources,...
                                                              'cleanSourcesArgs',Args.cleanSourcesArgs,...
                                                              'SortByY',true,...
                                                              'OutType','struct',...
                                                              'BackField','Back',...
                                                              'VarField','Var');
    
            % Cube of sources
            Args.X  = FindSrcSt.XPEAK;
            Args.Y  = FindSrcSt.YPEAK;
            Args.SN = FindSrcSt.SN;
        end
        CutoutRadius = max(Args.RadiusPSF, max(Args.Annulus).*(~isempty(Args.DeltaSigma)));
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image, Args.X, Args.Y, CutoutRadius, Args.image2cutoutsArgs{:});
        Xstamp = zeros(size(Args.X)) + (CutoutRadius + 1);
        Ystamp = zeros(size(Args.Y)) + (CutoutRadius + 1);
    else
        % assume Cube was provided
        Cube = Image;
    end
    
    % Select sources for PSF
    % select by SN diff
    Nsrc = size(Cube,3);
    FlagGoodPsf = true(Nsrc,1);
    if ~isempty(Args.SNdiff) && size(Args.SN,2)>1 && ~isempty(Args.SN)
        Flag        = (Args.SN(:,2) - Args.SN(:,1))>Args.SNdiff;
        FlagGoodPsf = FlagGoodPsf & Flag;
    end
    % select by moments
    if ~isempty(Args.DeltaSigma)
        %[M1, M2]    = imUtil.image.moment2(Cube, Args.X, Args.Y, 'Annulus',Args.Annulus, Args.moment2Args{:});
        [M1, M2]    = imUtil.image.moment2(Cube, Xstamp, Ystamp, 'Annulus',Args.Annulus, Args.moment2Args{:});
        Sigma       = sqrt(abs(M2.X2)+abs(M2.Y2));

        

        %MedSigma    = imUtil.background.modeVar_QuantileHist(Sigma);
        %FlagGoodPsf = FlagGoodPsf & (Sigma>(MedSigma - Args.DeltaSigma) & Sigma<(MedSigma + Args.DeltaSigma));

        SigmaRange  = quantile(Sigma, Args.SigmaQuantile);
        FlagGoodPsf = FlagGoodPsf & Sigma>SigmaRange(1) & Sigma<SigmaRange(2);

    else
        M1 = [];
        M2 = [];
    end
    % select by neighboors
    if ~isempty(Args.NighRadius)
        if isempty(Args.X) || isempty(Args.Y)
            error('For cube input X and Y must be provided');
        end
        [MatchedInd] = VO.search.search_sortedY_multi([Args.X, Args.Y], Args.X, Args.Y, Args.NighRadius);
        FlagGoodPsf    = FlagGoodPsf & ([MatchedInd.Nmatch]==1).';
    end
    % select by S/N range
    if ~isempty(Args.SN)
        FlagGoodPsf = FlagGoodPsf & (Args.SN(:,2)>Args.RangeSN(1) & Args.SN(:,2)<Args.RangeSN(2));
    end
            
    % FlagGoodPsf contains the good sources
    NgoodPsf = sum(FlagGoodPsf);

    Result.FlagGoodPsf = FlagGoodPsf;
    Result.NstrasPsf   = sum(FlagGoodPsf);
    Result.CatStarsPSF = [Args.X, Args.Y];
    Result.SN          = Args.SN;
    Result.M1          = M1;
    Result.M2          = M2;
        
    IndGoodPsf = find(FlagGoodPsf);
    
    if NgoodPsf>Args.MinNumGoodPsf && nargout>1
        if ~isempty(Args.Back)
            % read back at X/Y positions
            Ind  = imUtil.image.sub2ind_fast(size(Args.Back), round(Args.Y), round(Args.X));
            Back = Args.Back(Ind(IndGoodPsf));
        else
            Back = Args.Back;
        end
        %XY = [Args.X, Args.Y];
        XY = [Xstamp, Ystamp]; %[Args.X, Args.Y];
        XY = XY(IndGoodPsf,:);
        
        % cut the M1.X and M1.Y values for the selected stars to be
        % employed in imUtil.psf.constructPSF_cutouts below:
        M1.X = M1.X(FlagGoodPsf);
        M1.Y = M1.Y(FlagGoodPsf); 
        
        [MeanPSF, VarPSF, NimPSF, FlagSelected] = imUtil.psf.constructPSF_cutouts(Cube(:,:,IndGoodPsf), XY,...
                                                        'Annulus',Args.Annulus,...
                                                        'ReCenter',true,...
                                                        'Back',Back,...
                                                        'SmoothWings',Args.SmoothWings,...
                                                        'SubAnnulusBack',Args.SubAnnulusBack,...
                                                        'SumMethod',Args.SumMethod,...
                                                        'M1',M1,...
                                                        Args.constructPSF_cutoutsArgs{:});
    else
        warning('PSF construction failed: did not find enough good PSF stars');
        MeanPSF = [];
        VarPSF  = [];
        NimPSF  = 0;
    end
    
    % convert data type 
    if ~isempty(Args.DataType)
        MeanPSF = Args.DataType(MeanPSF);
        VarPSF  = Args.DataType(VarPSF);
    end
    
    % suppress edges
    if ~isempty(Args.SuppressWidth) && ~isempty(MeanPSF) 
         MeanPSF = imUtil.psf.suppressEdges(MeanPSF, 'Fun',Args.SuppressFun, 'FunPars', [Args.RadiusPSF-Args.SuppressWidth, Args.RadiusPSF]);
    end
    
    % cut the stamp and its variance to a given quantile
    if Args.CropByQuantile
        [MeanPSF, VarPSF] = imUtil.psf.cropByQuantile(MeanPSF,Args.Quantile,'Variance',VarPSF);
    end          
          
end
