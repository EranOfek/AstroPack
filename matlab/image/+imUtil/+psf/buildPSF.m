function [Result, MeanPSF, VarPSF, Nsrc] = buildPSF(Image, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 May) 
    % Example: imUtil.psf.buildPSF(AI.Image);

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
        Args.cleanSourcesArgs          = {};
        Args.backgroundCubeArgs        = {};
        
        Args.NeighRadius               = 10;  % if [] not clean for neighboors

        Args.SNdiff                    = 0;  % if empty skip
        Args.DeltaSigma                = 0.5;   % if empty skip
        Args.SigmaQuantile             = [0.05 0.8];

        Args.BackQuantile              = [0.01 0.9]; % if empty skip
        Args.StdQuantile               = [0.01 0.9]; % if empty skip

        Args.NighRadius                = 7;     % if empty skip
        Args.MinNumGoodPsf             = 5;
        
        Args.SumMethod                 = 'median';
        Args.VarOfMean                 = true;
        Args.SigmaClip                 = [3 3];
        Args.SigmaClipNiter            = 2;
        Args.Weighted                  = true;
        Args.WeightsMaxSN              = 100;

        Args.SuppressFun               = @imUtil.kernel2.cosbell;
        Args.SuppressThrsehold         = 1e-4;
        Args.SuppressFunPars           = 3; % or # from edge
        
        %Args.SmoothWings               = true;  % old: psf_zeroConverge  !! set to false
        %Args.SuppressWings             = false; % suppressWings fun      !! set to true;
        Args.WingsThreshold            = 1e-4;
        Args.SuppressEdges             = true;  % suppressEdges fun      !! set to false
        Args.SuppressWidth             = 3;
        Args.ShiftMethod               = 'fft'; % 'lacczos3' | 'fft'

        Args.DataType                  = []; % or '@single', '@double',...
        
        Args.CropByQuantile logical    = false;
        Args.Quantile                  = 0.999;
    end

    Result = struct('FlagGoodPsf',[], 'NstrasPsf',0, 'CatStarsPSF', zeros(0,2), 'SN',[], 'M1',[], 'M2',[]);

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

        if ~isempty(X)
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
        Xstamp = (SizeX+1).*0.5;
        Ystamp = (SizeY+1).*0.5;
        if isempty(Args.SN)
            SN = nan(Nsrc,1);
        else
            SN = Args.SN;
        end
       
    end
   
    Result.StartNsrc = Nsrc;
    

    % screen by neighboors
    % remove sources which have nearby neighboors 
    if ~isempty(Args.NeighRadius) && ~isempty(X)
        % sort by Y
        [~, NearestRadius] = imUtil.match.mex.matchSelfCatXY(X, Y, Args.NeighRadius, true, false, fale, false);

        IndNeigh = find(isnan(NearestRadius));
        Xstamp    = Xstamp(IndNeigh);
        Ystamp    = Ystamp(IndNeigh);
        X         = X(IndNeigh);
        Y         = Y(IndNeigh);
        SN        = SN(IndNeigh,:);
        Cube      = Cube(:,:,IndNeigh);

    end


    if Args.SubAnnulusBack
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
        
    else
        FlagBS = true(Nsrc,1);
    end

    % 1st and 2nd moments
    [M1, M2] = imUtil.sources.moments(Cube, 'SN',SN(:,2), 'StampX',Xstamp, 'StampY',Ystamp, 'X',0, 'Y',0, 'Annulus',Args.Annulus);
    if ~isempty(Args.SigmaQuantile)

        StAB     = imUtil.psf.mom2shape(M2.X2, M2.Y2, M2.XY);
        % remove sources by 2nd moment
        [ValA] = quantile(StAB.A, Args.SigmaQuantile);
        FlagM2 = StAB.A>ValA(1) & StAB.A<ValA(2);
        %X      = Args.X(FlagM2);
        %Y      = Args.Y(FlagM2);
        SN     = SN(FlagM2,:);
        Xstamp = Xstamp(FlagM2);
        Ystamp = Ystamp(FlagM2);
        Cube   = Cube(:,:,FlagM2);
        M1.X   = M1.X(FlagM2);
        M1.Y   = M1.Y(FlagM2);
        M2.X2  = M2.X2(FlagM2);
        M2.Y2  = M2.Y2(FlagM2);
        M2.XY  = M2.XY(FlagM2);

    end


    % shift stamps to 1st moment
    switch Args.ShiftMethod
        case 'lanczos3'
            Cube = imUtil.trans.mex.shift_lanczos3(Cube, -M1.X, -M1.Y);
        case 'fft'
            Cube = imUtil.trans.mex.shift_fft(Cube, -M1.X, -M1.Y);
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
            [MeanPSF,N]=tools.math.stat.mex.sigma_clip_cube(A,[2 2]);
            if Args.Weighted
                Weights = 1./max(SN(:,2), Args.WeightsMaxSN);
            else
                Weights = [];
            end
            [MeanPSF, N, VarPSF] = tools.math.stat.mex.sigmaClipCubeN(Cube, Args.SigmaClip, Args.SigmaClipNiter, Weights)

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
    

    % smooth wings
    [MeanPSF, InnerRad] = imUtil.psf.suppressWings(MeanPSF, 'Fun',Args.SuppressFun,...
                                                            'Thrsehold',Ags.SupressThreshold,...
                                                            'FunPars',Args.SuppressFunPars,...
                                                            'Norm',true);
    
    % fot to analytical function
    % FFU


end
