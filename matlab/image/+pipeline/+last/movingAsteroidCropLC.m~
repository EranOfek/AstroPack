function CropAI=movingAsteroidCropLC(TimeStart, TimeStop, Args)
    % Analyze a full farme LAST images
    % Example: CropAI=pipeline.last.movingAsteroidCropLC
    
    arguments
  
        TimeStart      = [26 9 2022 23 10 00];
        TimeStop       = [27 9 2022  2 37 55];
        Args.DataNum   = 1;
        Args.CCDSEC    = [300 6000 2600 7000];
        Args.SameField = true;
        Args.ObsCode   = '097';
        Args.AstDesig  = '65803';
        Args.AstSearchRadius   = 10;   % [arcsec]
        Args.HalfCrop          = 200;
        Args.RefRA             = 050.875917639; 
        Args.RefDec            = -33.41918960863;
        Args.RefMag            = 11.233;
        Args.AstRefRadius      = 5;
        Args.AperRadius        = [2 4 6 8 10];
        Args.Annulus           = [14 18];
        Args.CollectMag        = {'MAG_APER_1','MAG_APER_2','MAG_APER_3','MAG_APER_4','MAG_APER_5'};
        Args.AstRefineRadius   = 10;
        
        Args.Plot logical      = true;
        Args.JD0               = celestial.time.julday([26 9 2022 23 15 0]);
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    SEC_DAY    = 86400;
    
    AstArgs = {'RefRangeMag',[8 16],...
                'Scale',1.25};
    
    
            
    DataNum = 1;
    [BasePath, CalibDir, NewFilesDir, ProjName] = pipeline.last.constructArchiveDir('DataNum',Args.DataNum);
    
    CI = CalibImages.loadFromDir(CalibDir);
    CI.crop(Args.CCDSEC);
    
    if numel(TimeStart)==1
        StartJD = TimeStart;
    else
        StartJD = celestial.time.julday(TimeStart);
    end
    
    if numel(TimeStop)==1
        EndJD = TimeStop;
    else
        EndJD = celestial.time.julday(TimeStop);
    end
    
    
    
    [Ephem]=celestial.SolarSys.jpl_horizons('ObjectInd',Args.AstDesig,'StartJD',StartJD-0.5,'StopJD',  StartJD+1.5,'CENTER',Args.ObsCode,'StepSizeUnits','h');
    
    
    Files = io.files.dirSortedByDate('LAST*.fits');
    List  = {Files.name};
    IP = ImagePath.parseFileName(List);
    Ind = find([IP.Time]>StartJD & [IP.Time]<EndJD);
    Nim = numel(Ind);
    IndDebug = 0;
    for Iim=1:1:300,
        %Nim
        tic;
        IndDebug = IndDebug + 1;
        Iim
        Ilist = Ind(Iim);
        AI    = AstroImage(List(Ilist),'CCDSEC',Args.CCDSEC);
        
        AI = CI.processImages(AI, 'SubtractOverscan',false);
        [SizeY, SizeX] = AI.sizeImage;
        Xcenter = SizeX.*0.5;
        Ycenter = SizeY.*0.5;
        
        
        AI = imProc.background.background(AI);
        AI = imProc.sources.findMeasureSources(AI, 'MomPar',{'AperRadius',Args.AperRadius,'Annulus',Args.Annulus});
        
        % interpolate over bad pixels
        SN = AI.CatData.getCol({'SN_1','SN_4'});
        [X,Y] = AI.CatData.getXY;
        FlagCR = (SN(:,1) - SN(:,2))>0 & SN(:,1)>8;
        Y = Y(FlagCR);
        X = X(FlagCR);
        Y = Y(~isnan(Y));
        X = X(~isnan(X));
        IndPixCR = imUtil.image.sub2ind_fast(size(AI.Image), Y, X);
        AI = maskSet(AI, IndPixCR, 'CR_DeltaHT');
        AI = imProc.mask.interpOverMaskedPix(AI);
        
        
        JD = AI.julday;
    
        if Iim==200
            'a'
        end
        if IndDebug==1 || ~Args.SameField
            [Result, AI, AstrometricCat] = imProc.astrometry.astrometryCore(AI,AstArgs{:});
            LastWCS = AI.WCS;
            
            [CenterRA, CenterDec] = AI.WCS.xy2sky(Xcenter, Ycenter);
            CooArgs = {'RA',CenterRA, 'Dec',CenterDec};
            
        else
            [Result, AI] = imProc.astrometry.astrometryRefine(AI, 'WCS',LastWCS, 'CatName',AstrometricCat, 'SearchRadius',Args.AstRefineRadius, 'MinNmatches',20);
            if isempty(Result.ResFit) || Result.ResFit.Ngood<15
                %if Result is bad
                % run astrometry
                [Result, AI] = imProc.astrometry.astrometryCore(AI, AstArgs{:},...
                    'CatName',AstrometricCat, CooArgs{:});
            end
        end
        
        if ~isempty(Result.ResFit) && Result.ResFit.Ngood>15
        
            % id asteroid in catalog
            [RA,Dec] = getLonLat(AI.CatData,'rad');
            [X,Y]    = getXY(AI.CatData);

            PredRA  = interp1(Ephem.Catalog(:,1), Ephem.Catalog(:,2), JD, 'cubic');
            PredDec = interp1(Ephem.Catalog(:,1), Ephem.Catalog(:,3), JD, 'cubic');


            Dist = celestial.coo.sphere_dist_fast(RA, Dec, PredRA, PredDec);
            IndAstInCatalog = find(Dist<(Args.AstSearchRadius./(RAD.*ARCSEC_DEG)));
                        
            % select highest S/N
            if numel(IndAstInCatalog)>0
                % asteroid found
                if numel(IndAstInCatalog)>1
                    warning('>1 sources found in asteroid position');
                    SN = getCol(AI.CatData,'SN_3');
                    [~,Imax] = max(SN(IndAstInCatalog));
                    IndAstInCatalog = IndAstInCatalog(Imax);
                    ConfusionFlag = false;
                else
                    ConfusionFlag = true;
                end

                AstX = X(IndAstInCatalog);
                AstY = Y(IndAstInCatalog);

                MagAst = getCol(AI.CatData, Args.CollectMag);

                Dist = celestial.coo.sphere_dist_fast(RA, Dec, Args.RefRA./RAD, Args.RefDec./RAD);
                IndRefInCatalog = find(Dist<(Args.AstRefRadius./(RAD.*ARCSEC_DEG)));
                MagAll = getCol(AI.CatData, Args.CollectMag);

                % crop image around PredRA, PredDec
                [PredX, PredY] = AI.WCS.sky2xy(PredRA.*RAD, PredDec.*RAD);

                DistFromPred = sqrt((PredX-AstX).^2 + (PredY-AstY).^2);
                if DistFromPred<1
                    % use observed position instead of predicted position
                    PredX = AstX;
                    PredY = AstY;
                end
                FloorX = floor(PredX)-5;
                FloorY = floor(PredY)-5;
                ShiftX = FloorX - PredX;
                ShiftY = FloorY - PredY;
                
                CropAI(Iim) = AI.crop([FloorX, FloorY, Args.HalfCrop Args.HalfCrop], 'Type','center', 'CreateNewObj',true);
                
                RegisteredImages = imProc.transIm.imwarp(CropAI(Iim), [ShiftX, ShiftY],...
                                                     'TransWCS',false,...
                                                     'FillValues',0,...
                                                     'ReplaceNaN',true,...
                                                     'CreateNewObj',true);

                
                
                
                CropAI(Iim).CatData = AI.CatData;

                CropAI(Iim).UserData.IndAstInCatalog = IndAstInCatalog;
                CropAI(Iim).UserData.IndRefInCatalog = IndRefInCatalog;
                CropAI(Iim).UserData.ZP              = MagAll(IndRefInCatalog,:) - Args.RefMag;  % vector [one per aper]
                CropAI(Iim).UserData.MagAst          = MagAst(IndAstInCatalog,:) - CropAI(Iim).UserData.ZP;
                CropAI(Iim).UserData.JD              = JD;
                CropAI(Iim).UserData.AssymRMS        = Result.ResFit.AssymRMS;
                CropAI(Iim).UserData.Ngood           = Result.ResFit.Ngood;
                CropAI(Iim).UserData.ConfusionFlag   = ConfusionFlag;

                if Args.Plot && ~isempty(CropAI(Iim).UserData.MagAst)
                    plot((JD - Args.JD0).*SEC_DAY, CropAI(Iim).UserData.MagAst(3), 'ko', 'MarkerFaceColor','k')
                    hold on;
                    plot((JD - Args.JD0).*SEC_DAY, CropAI(Iim).UserData.MagAst(5), 'ro', 'MarkerFaceColor','r')
                    plot.invy;
                    drawnow;
                end
            end
        end
        toc
        
        
        
    end
end

    