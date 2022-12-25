function MatchedS = imagesSequence2LC(List, Args)
    % Generate light curves of stationary sources from a sequence of raw images
    % Input  : - A cell array containing a list of raw images to analyze.
    %            Alternatively a char array containing image name template.
    %            Default is 'LAST*Chiron*.fits'.
    %          * ...,key,val,...
    %            'RA' - J2000.0 RA of main source [deg].
    %            'Dec' - J2000.0 Dec of main source [deg].
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of sub image to
    %                   analyze. If empty, analyze full image.
    %                   Default is [2201 4200 4001 6000].
    %            'CI' - Directory name containing calibration images,
    %                   or a CalibImages object, or empty.
    %                   If empty skip calibration.
    %                   Default is
    %                   '/raid/eran/projects/telescopes/LAST/Images_PipeTest/calib'
    %            'RefRangeMag' - Magnitude range for astrometric reference
    %                   stars. Set to match image depth.
    %                   Default is [8 14].
    %            'SearchRadius' - Search radius [arcsec] for main target.
    %                   Default is 2.
    %            'Plot' - A logical indicating if to plot target LC.
    %                   Default is true.
    %            'argsAstrometry' - A cell array of additional arguments to
    %                   pass to imProc.astrometry.astrometryCore.
    %                   Default is {}.
    %            'argsMerged' - A cell array of additional arguments to
    %                   pass to imProc.match.mergeCatalogs.
    %                   Default is {}.
    % Output : - A MatchedSources object containing the photometry of
    %            soources in the field.
    % Author : Eran Ofek (Dec 2022)
    % Example: [MS]=pipeline.generic.imagesSequence2LC('LAST*Chiron*.fits','CI',CI);

    arguments
        List               = 'LAST*Chiron*.fits';
        Args.RA            = celestial.coo.convertdms('00:39:58.719','SH','d');                                              
        Args.Dec           = celestial.coo.convertdms('+06:16:08.86','SD','d');
        Args.CCDSEC        = [2201 4200 4001 6000]; %[501 2500 2301 4300]; %[2201 4200 4001 6000];
        Args.CI            = '/raid/eran/projects/telescopes/LAST/Images_PipeTest/calib';
        Args.Scale         = 1.25;
        Args.RefRangeMag   = [8 14];
        Args.SearchRadius  = 2;
        Args.Plot logical  = true;
        Args.argsAstrometry = {};
        Args.argsMerged     = {};

    end

    if ischar(List)
        List = io.files.filelist(List);
    end

    if ischar(Args.CI)
        CI = CalibImages.loadFromDir(Args.CI);
    else
        CI = Args.CI;
    end

    RA=celestial.coo.convertdms('00:39:58.719','SH','d');                                              
    Dec=celestial.coo.convertdms('+06:16:08.86','SD','d');

    Nl = numel(List);

    for Il=1:1:Nl
        [Il Nl]
        AI(Il)=AstroImage(List{Il});
        AI(Il).HeaderData.insertKey({'FILTER','clear'});
        
        if ~isempty(Args.CI)
            AI(Il)=CI.processImages(AI(Il),'SubtractOverscan',false);
        end
        AI(Il)=AI(Il).crop(Args.CCDSEC);
    
        AI(Il) = imProc.background.background(AI(Il));
        AI(Il) = imProc.sources.findMeasureSources(AI(Il),'RemoveBadSources',true);

        [~,AI(Il)]=imProc.astrometry.astrometryCore(AI(Il),'RA',Args.RA,'Dec',Args.Dec,'Scale',Args.Scale,'RefRangeMag',Args.RefRangeMag, Args.argsAstrometry{:});

        %AI(Il) = imProc.astrometry.addCoordinates2catalog(AI(Il), 'UpdateCoo',true);
        %AI(Il).CatData.sortrows('Dec');  % <<--- make sure we sort catalogs in pipeline!!!

    end

    [MergedCat, MatchedS, ResultSubIm.ResZP, ResultSubIm.ResVar, ResultSubIm.FitMotion] = imProc.match.mergeCatalogs(AI(:),'Radius',Args.SearchRadius, Args.argsMerged{:});
    JD=AI.julday;
    MatchedS.JD=JD(:);


    [Ind,Flag, Dist] = coneSearch(MatchedS,RA,Dec,8,'CooUnits','deg');
    ResZP = lcUtil.zp_meddiff(MatchedS,'MagField','MAG_APER_3','MagErrField','MAGERR_CONV_3');
    [MatchedS ,ApplyToMagField] = applyZP(MatchedS, ResZP.FitZP, 'FieldZP','FitZP', 'ApplyToMagField','MAG_', 'Operator',@minus);

    
    
    if Args.Plot
        JD0 = floor(MatchedS.JD(1));
        plot((MatchedS.JD-JD0).*1440,MatchedS.Data.MAG_APER_3(:,Ind),'.')
        plot.invy
    end
end