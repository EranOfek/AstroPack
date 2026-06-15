function [Result, MeanPSF, VarPSF, Nsrc, ExtendedPSF] = buildPSF(Image, Args)
    % Build a master PSF from an image (or a cube of stamps).
    %   Given a 2D image or a 3D cube of stellar stamps, construct a master
    %   PSF by stacking sub-pixel-shifted, normalized cutouts of stars.
    %   When a 2D image is supplied, the function will:
    %     1. Optionally detect sources via imUtil.sources.findSources (if X,
    %        Y, SN are not provided by the caller).
    %     2. Filter sources by their PSF S/N (RangeSN, SNdiff).
    %     3. Cut a stamp around each surviving star.
    %     4. Reject stars with nearby ighbors within NeighRadius.
    %     5. Optionally subtract an annulus background; reject stars whose
    %        annulus background / std lie outside (BackQuantile, StdQuantile).
    %     6. Compute 1st and 2nd moments and reject outliers in shape via
    %        SigmaQuantile on the moment-derived semi-major axis.
    %     7. Shift each stamp to the source 1st-moment center (lanczos3 or
    %        FFT), normalize to unit sum, and stack via the chosen
    %        combination method (median / mean / sigma-clip).
    %     8. Suppress wings of the master PSF using imUtil.psf.suppressWings.
    %   When a 3D cube is supplied, steps 1-3 are skipped; the cube is taken
    %   to already be stamp-centered cutouts, and steps 4-8 proceed.
    % Input  : - A 2D image, or a 3D cube of stellar stamps with the star
    %            index in the 3rd dimension.
    %          * ...,key,val,...
    %            'X' - Vector of star X positions in the image (matrix input)
    %                  or per-stamp source positions (cube input). If empty
    %                  in the matrix case, sources are found by findSources.
    %                  Default is [].
    %            'Y' - Vector of star Y positions. See 'X'. Default is [].
    %            'SN' - [Nsrc x 2] matrix of S/N values: column 1 is the
    %                   delta-function S/N, column 2 is the PSF-filter S/N.
    %                   If empty in the matrix case, computed by findSources.
    %                   Default is [].
    %            'Back' - Image background, forwarded to findSources.
    %                   Default is [].
    %            'Var'  - Image variance, forwarded to findSources.
    %                   Default is [].
    %            'SubAnnulusBack' - If true, subtract per-stamp annulus
    %                   background (annulus_median). Default is true.
    %            'RadiusPSF' - Half-size of the stamps used for the PSF
    %                   (the stamp size is 2*RadiusPSF+1). Default is 12.
    %            'Annulus' - [Rin, Rout] of the background annulus, in
    %                   pixels. Default is [10 12].
    %            'image2cutoutsArgs' - Extra key-val args forwarded to
    %                   imUtil.cut.image2cutouts. Default is {}.
    %            'ThresholdPSF' - Detection S/N threshold used when sources
    %                   are not supplied by the caller. Default is 20.
    %            'RangeSN' - [SNmin, SNmax] PSF-filter S/N window for
    %                   sources used to build the PSF. Default is [50 1000].
    %            'InitPsf' - Function handle generating the initial-guess
    %                   PSF kernels for findSources matched filtering.
    %                   Default is @imUtil.kernel2.gauss.
    %            'InitPsfArgs' - Cell of args to InitPsf. Default is {[0.1;2]}.
    %            'Conn' - Connectivity for findSources. Default is 8.
    %            'CleanSources' - Logical, forwarded to findSources.
    %                   Default is true.
    %            'cleanSourcesArgs' - Cell of key-val args forwarded to
    %                   the source-cleaning step. Default is
    %                   {'MinEdgeDist',13}.
    %            'backgroundCubeArgs' - Reserved; currently unused.
    %                   Default is {}.
    %            'NeighRadius' - Minimum allowed distance to the nearest
    %                   neighbor; sources with a neighbor inside this radius
    %                   are rejected. If empty, neighbor cleaning is skipped.
    %                   Default is 10.
    %            'SNdiff' - Minimum required S/N margin SN(:,2)-SN(:,1).
    %                   Default is 0.
    %            'DeltaSigma' - If non-empty, the cutout half-size is
    %                   enlarged to include the annulus (so background can
    %                   be measured per-stamp). If empty, only RadiusPSF is
    %                   used. Default is 0.5.
    %            'SigmaQuantile' - [Qlow, Qhigh] quantiles on the moment-
    %                   derived semi-major axis A; sources outside this
    %                   range are rejected. If empty, no shape cut is made.
    %                   Default is [0.05 0.8].
    %            'BackQuantile' - [Qlow, Qhigh] quantiles on the per-stamp
    %                   annulus background; sources outside this range are
    %                   rejected. If empty, no back cut. Default is
    %                   [0.01 0.9].
    %            'StdQuantile' - As BackQuantile, applied to the annulus
    %                   StD. Default is [0.01 0.9].
    %            'ShiftMethod' - Sub-pixel shift method used to recenter
    %                   each stamp to its 1st-moment position:
    %                   'lanczos3' (imUtil.trans.mex.shift_lanczos3) or
    %                   'fft' (imUtil.trans.shift_fft).
    %                   Default is 'lanczos3'.
    %            'SumMethod' - Stamp combination method to obtain the
    %                   master PSF. One of:
    %                   'median'      - per-pixel median (default),
    %                   'mean'        - per-pixel mean,
    %                   'sigclip'     - imUtil.image.mean_sigclip,
    %                   'sigclip_mex' - tools.math.stat.mex.sigmaClipCubeN.
    %                   Default is 'median'.
    %            'VarOfMean' - If true, divide the returned VarPSF by Nsrc
    %                   to get variance-of-the-mean. Default is true.
    %            'SigmaClip' - [low, high] sigma-clipping bounds used by
    %                   'sigclip_mex'. Default is [3 3].
    %            'SigmaClipNiter' - Iteration count for 'sigclip_mex'.
    %                   Default is 2.
    %            'Weighted' - If true and 'sigclip_mex' is used, weight
    %                   stamps by 1/max(SN, WeightsMaxSN). Default is true.
    %            'WeightsMaxSN' - SN ceiling used in the weight computation
    %                   above (prevents very bright stars from dominating).
    %                   Default is 100.
    %            'mean_sigclipArgs' - Extra args to imUtil.image.mean_sigclip
    %                   when SumMethod = 'sigclip'. Default is {}.
    %            'SuppressFun' - Window function used by suppressWings to
    %                   taper the master PSF. Default is @imUtil.kernel2.cosbell.
    %            'SuppressThreshold' - Threshold passed to suppressWings.
    %                   Default is 1e-4.
    %            'SuppressFunPars' - Parameters for SuppressFun (e.g. the
    %                   number of pixels from the edge). Default is 3.
    % Output : - Result, a struct with the following fields:
    %            .StartNsrc - Number of sources entering the pipeline
    %                         (after the initial SN cut and stamping).
    %            .Nsrc      - Number of sources that survived all filters
    %                         and contributed to the master PSF.
    %            .SN        - [Nsrc x 2] surviving S/N values.
    %            .X, .Y     - Image-frame positions of the surviving
    %                         sources (empty if X/Y were not provided in
    %                         the cube branch).
    %            .M1        - Struct of 1st-moment fields (.X, .Y) for the
    %                         surviving sources.
    %            .M2        - Struct of 2nd-moment fields (.X2, .Y2, .XY).
    %          - MeanPSF, a 2D array containing the master PSF, normalized
    %            to unit sum and tapered at the wings. Returns [] if no
    %            sources survive.
    %          - VarPSF, a 2D array of per-pixel variances of the stack.
    %            If 'VarOfMean' is true this is divided by Nsrc to give the
    %            variance of the mean. Returns [] if no sources survive.
    %          - Nsrc, the final number of sources contributing to the
    %            master PSF (== Result.Nsrc).
    %          - Extended PSF with power law wings instead of supressed
    %            wings.
    % Author : Eran Ofek (2026 May)
    % Example: imUtil.psf.buildPSF(AI.Image);
    %          [Result, P] = imUtil.psf.buildPSF(AI.Image, 'SumMethod','sigclip_mex');
    %          [Result, P] = imUtil.psf.buildPSF(StampCube, 'X',Xs, 'Y',Ys, 'SN',SN);

    arguments
        Image
        
        Args.X                      = []; % always coordinates in image
        Args.Y                      = [];
        Args.SN                     = [];
        Args.Back                   = [];
        Args.Var                    = [];
        Args.SubAnnulusBack         = true;
       
        Args.RadiusPSF                 = 12;
        Args.Annulus                   = [10 12];
        
        Args.image2cutoutsArgs         = {};
        
        %Args.Threshold                 = 5;
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss;
        Args.InitPsfArgs               = {[0.1;2]};
        Args.Conn                      = 8;
        Args.CleanSources              = true;
        Args.cleanSourcesArgs          = {'MinEdgeDist',13};
        Args.backgroundCubeArgs        = {};
        
        Args.NeighRadius               = 10;  % if [] not clean for neighboors

        Args.SNdiff                    = 0;  % if empty skip
        Args.DeltaSigma                = 0.5;   % if empty skip
        Args.SigmaQuantile             = [0.05 0.8];

        Args.BackQuantile              = [0.01 0.9]; % if empty skip
        Args.StdQuantile               = [0.01 0.9]; % if empty skip

        Args.ShiftMethod               = 'lanczos3'; % 'lanczos3' | 'fft'
        Args.SumMethod                 = 'median'; %'sigclip_mex'; %'median';
        Args.VarOfMean                 = true;
        Args.SigmaClip                 = [3 3];
        Args.SigmaClipNiter            = 2;
        Args.Weighted                  = true;
        Args.WeightsMaxSN              = 100;
        Args.mean_sigclipArgs          = {};

        Args.SuppressFun               = @imUtil.kernel2.cosbell;
        Args.SuppressThreshold         = 1e-4;
        Args.SuppressFunPars           = 3; % or # from edge
        
        Args.ExtendedSize              = [1501 1501];
        Args.Alpha                     = 1;
    end

    

    Result = struct('StartNsrc',0, 'Nsrc',0, 'SN',[], 'X',[], 'Y',[], 'M1',[], 'M2',[], 'SuppressRad',[]);

    if ismatrix(Image)
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
        if size(Args.SN,2)~=2
            error('SN must include two columns (for delta fun and for PSF)');
        end

        % get stamps around stars
        FlagSN  = Args.SN(:,2)>Args.RangeSN(1) & Args.SN(:,2)<Args.RangeSN(2) & Args.SN(:,2)>(Args.SN(:,1)+Args.SNdiff);

        X  = Args.X(FlagSN);
        Y  = Args.Y(FlagSN);
        SN = Args.SN(FlagSN,:);

        % sort by Y
        [Y, SI] = sort(Y);
        X       = X(SI);
        SN      = SN(SI,:);

        if isempty(X)
            Cube = zeros(0,0,0);
        else
            CutoutRadius = max(Args.RadiusPSF, max(Args.Annulus).*(~isempty(Args.DeltaSigma)));
            [Cube, RoundX, RoundY] = imUtil.cut.image2cutouts(Image, X, Y, CutoutRadius, Args.image2cutoutsArgs{:});
            Xstamp = zeros(size(X)) + (CutoutRadius + 1);
            Ystamp = zeros(size(Y)) + (CutoutRadius + 1);
        end
        [SizeY, SizeX, Nsrc] = size(Cube);

    else
        % Image is already cube of PSFs
        Cube = Image;
        [SizeY, SizeX, Nsrc] = size(Cube);
        
        X = Args.X;
        Y = Args.Y;
        Xstamp = zeros(size(X)) + (SizeX+1).*0.5;
        Ystamp = zeros(size(Y)) + (SizeY+1).*0.5;
        if isempty(Args.SN)
            SN = nan(Nsrc,1);
        else
            SN = Args.SN;
        end
       
        if size(SN,2)~=2
            error('SN must include two columns (for delta fun and for PSF)');
        end
    end
   
    Result.StartNsrc = Nsrc;
    

    % screen by neighboors
    % remove sources which have nearby neighboors 
    if ~isempty(Args.NeighRadius) && Nsrc>0
        % sort by Y
        [~, NearestRadius] = imUtil.match.mex.matchSelfCatXY(X, Y, Args.NeighRadius, true, false, false, false);

        IndNeigh = find(isnan(NearestRadius));
        Xstamp    = Xstamp(IndNeigh);
        Ystamp    = Ystamp(IndNeigh);
        X         = X(IndNeigh);
        Y         = Y(IndNeigh);
        SN        = SN(IndNeigh,:);
        Cube      = Cube(:,:,IndNeigh);
        Nsrc      = numel(X);

    end


    if Args.SubAnnulusBack && Nsrc>0
        % subtract annulus background
        [Cube,Back,BackSt,BackNpix] = imUtil.sources.mex.annulus_median(Cube, Args.Annulus, 0);

        if ~isempty(Args.BackQuantile)
            BackQ  = quantile(Back, Args.BackQuantile);
            FlagB  = Back>BackQ(1) & Back<BackQ(2);
        else
            FlagB  = true(Nsrc,1);
        end
        if ~isempty(Args.StdQuantile)
            StdQ  = quantile(BackSt, Args.StdQuantile);
            FlagS  = BackSt>StdQ(1) & BackSt<StdQ(2);
        else
            FlagS  = true(Nsrc,1);
        end
        FlagBS = FlagB & FlagS;

        Cube   = Cube(:,:,FlagBS);
        Xstamp = Xstamp(FlagBS);
        Ystamp = Ystamp(FlagBS);
        SN     = SN(FlagBS,:);
        if numel(X) == numel(FlagBS)
            X = X(FlagBS);
            Y = Y(FlagBS);
        end
        Nsrc   = numel(Xstamp);

    else
        FlagBS = true(Nsrc,1);
    end


    if Nsrc>0
        % 1st and 2nd moments
        [M1, M2] = imUtil.sources.moments(Cube, 'SN',SN(:,2), 'StampX',Xstamp, 'StampY',Ystamp, 'X',0, 'Y',0, 'Annulus',Args.Annulus);
        if ~isempty(Args.SigmaQuantile)
    
            StAB     = imUtil.psf.mom2shape(M2.X2, M2.Y2, M2.XY);
            % remove sources by 2nd moment
            [ValA] = quantile(StAB.A, Args.SigmaQuantile);
            FlagM2 = StAB.A>ValA(1) & StAB.A<ValA(2);
            SN     = SN(FlagM2,:);
            Xstamp = Xstamp(FlagM2);
            Ystamp = Ystamp(FlagM2);
            Cube   = Cube(:,:,FlagM2);
            M1.X   = M1.X(FlagM2);
            M1.Y   = M1.Y(FlagM2);
            M2.X2  = M2.X2(FlagM2);
            M2.Y2  = M2.Y2(FlagM2);
            M2.XY  = M2.XY(FlagM2);
            if numel(X) == numel(FlagM2)
                X = X(FlagM2);
                Y = Y(FlagM2);
            end

            Nsrc   = numel(Xstamp);
    
            % IndM2  = StAB.A>ValA(1) & StAB.A<ValA(2);
            % SN     = SN(IndM2,:);
            % Xstamp = Xstamp(IndM2);
            % Ystamp = Ystamp(IndM2);
            % Cube   = Cube(:,:,IndM2);
            % M1.X   = M1.X(IndM2);
            % M1.Y   = M1.Y(IndM2);
            % M2.X2  = M2.X2(IndM2);
            % M2.Y2  = M2.Y2(IndM2);
            % M2.XY  = M2.XY(IndM2);
        end
    end

    if Nsrc>0

        % shift stamps to 1st moment
        switch Args.ShiftMethod
            case 'lanczos3'
                Cube = imUtil.trans.mex.shift_lanczos3(Cube, -M1.X, -M1.Y);
            case 'fft'
                Cube = imUtil.trans.shift_fft(Cube, -M1.X, -M1.Y);
            otherwise
                error('Unknown ShiftMethod option');
        end
    
        % testing that M1.X, M1.Y distributed around 0
        %[M1, M2] = imUtil.sources.moments(Cube, 'SN',SN(:,2), 'StampX',Xstamp, 'StampY',Ystamp, 'X',0, 'Y',0, 'Annulus',Args.Annulus);
    
        % normalize all PSFs in cube to unity
        Norm = sum(Cube,[1 2], 'omitnan'); 
        Nsrc = numel(Norm);
        InvNorm = reshape(1./Norm, 1, 1, []);
        Cube = Cube.*InvNorm;
    
        switch lower(Args.SumMethod)
            case 'sigclip_mex'
                %MA=mean(A,3,'omitnan'); SA=std(A,[],3,'omitnan'); Z= (A-MA)./SA;
                %Flag=Z<-2 | Z>2; A(Flag)=NaN; MA=mean(A,3,'omitnan'); NN=sum(~isnan(A),3);
                %[MeanPSF,N]=tools.math.stat.mex.sigma_clip_cube(A,[2 2]);
                if Args.Weighted
                    Weights = 1./max(SN(:,2), Args.WeightsMaxSN);
                else
                    Weights = [];
                end
                [MeanPSF, VarPSF, N] = tools.math.stat.mex.sigmaClipCubeN(Cube, Args.SigmaClip, Args.SigmaClipNiter, Weights);
    
            case 'sigclip'
                [MeanPSF,VarPSF,FlagGood,GoodCounter] = imUtil.image.mean_sigclip(Cube, 3, Args.mean_sigclipArgs{:});
            case 'mean'
                MeanPSF = mean(Cube, 3, 'omitnan');
                VarPSF  = var(Cube,1, 3, 'omitnan');
            case 'median'
                MeanPSF = median(Cube, 3, 'omitnan');
                VarPSF  = var(Cube,1, 3, 'omitnan');
            otherwise
                error('Unknown SumMethod option');
        end
    
        if Args.VarOfMean
            VarPSF = VarPSF./Nsrc;
        end
    
        Result.Nsrc = Nsrc;
        Result.SN   = SN;
        Result.X    = X;
        Result.Y    = Y;
        Result.M1   = M1;
        Result.M2   = M2;


        % smooth wings
        if nargout>4
            [MeanPSF, InnerRad,ExtendedPSF] = imUtil.psf.suppressWings(MeanPSF, 'Fun',Args.SuppressFun,...
                                                                'Threshold',Args.SuppressThreshold,...
                                                                'FunPars',Args.SuppressFunPars,...
                                                                'Norm',true,...
                                                                'ExtendedSize',Args.ExtendedSize,...
                                                                'Alpha',Args.Alpha);
        else
            [MeanPSF, InnerRad] = imUtil.psf.suppressWings(MeanPSF, 'Fun',Args.SuppressFun,...
                                                                'Threshold',Args.SuppressThreshold,...
                                                                'FunPars',Args.SuppressFunPars,...
                                                                'Norm',true);

        end
        Result.SuppressRad = InnerRad;

        % fit to analytical function
        % FFU
        

    else
        Result.Nsrc = 0;
        Result.SuppressRad = NaN;
        Result.SN   = SN;
        Result.X    = X;
        Result.Y    = Y;
        MeanPSF = [];
        VarPSF  = [];
    end

end
