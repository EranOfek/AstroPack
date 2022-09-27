function movingAsteroidCropLC(TimeStart, Args)
    % Analyze a full farme LAST images
    % Example: Output=pipeline.last.movingAsteroidCropLC
    
    arguments
        TimeStart      = [26 9 2022 23 13 00];
        Args.CCDSEC    = [300 6000 2600 7000];
        Args.SameField = true;
        Args.ObsCode   = '097';
        Args.AstDesig  = '65803';
        Args.AstSearchRadius   = 10;   % [arcsec]
        Args.HalfCrop          = 200;
        Args.RefRA
        Args.RefDec
        Args.RefMag
        Args.AstRefRadius   = 5;
        Args.AperRadius     = [2 4 6 8 10];
        Args.Annulus        = [14 18];
        Args.CollectMag     = {'MAG_APER_1','MAG_APER_2','MAG_APER_3','MAG_APER_4','MAG_APER_5'};
        
        Args.Node      = 1
        Args.DataNum   = 1;
        Args.CropSize  = []; %[100 6000 3400 6200];
        Args.Xpred     = 3644;
        Args.Ypred     = 4042;
        Args.Xref      = 3561;
        Args.Yref      = 3916;
        Args.ZPref     = 11.233;
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    
    AstArgs = {'RefRangeMag',[8 16],...
                'Scale',1.25};
    
    
            
    DataNum = 1;
    [BasePath, CalibDir, NewFilesDir, ProjName] = pipeline.last.constructArchiveDir;
    
    CI = CalibImages.loadFromDir(CalibDir);
    CI.crop(Args.CCDSEC);
    
    StartJD = celestial.time.julday(TimeStart);
    JD0 = celestial.time.julday([26 9 2022 23 15 0]);
    
    [Ephem]=celestial.SolarSys.jpl_horizons('ObjectInd',Args.AstDesig,'StartJD',StartJD-0.5,'StopJD',  StartJD+1.5,'CENTER',Args.ObsCode,'StepSizeUnits','h');
    
    
    Files = io.files.dirSortedByDate('LAST*.fits');
    List  = {Files.name};
    IP = ImagePath.parseFileName(List);
    Ind = find([IP.Time]>StartJD);
    Nim = numel(Ind);
    for Iim=1:1:Nim
        Iim
        Ilist = Ind(Iim);
        AI    = AstroImage(List(Ilist),'CCDSEC',Args.CCDSEC);
        
        AI = CI.processImages(AI, 'SubtractOverscan',false);
        [SizeY, SizeX] = AI.sizeImage;
        Xcenter = SizeX.*0.5;
        Ycenter = SizeY.*0.5;
        
        
        AI = imProc.background.background(AI);
        AI = imProc.sources.findMeasureSources(AI, 'MomPar',{'AperRadius',Args.AperRadius,'Annulus',Args.Annulus});
        JD = AI.julday;
    
        if Iim==1 || ~Args.SameField
            [Result, AI, AstrometricCat] = imProc.astrometry.astrometryCore(AI,AstArgs{:});
            LastWCS = AI.WCS;
            
            [CenterRA, CenterDec] = AI(Iim).WCS.xy2sky(Xcenter, Ycenter);
            CooArgs = {'RA',CenterRA, 'Dec',CenterDec};
        else
            [Result, AI] = imProc.astrometry.astrometryRefine(AI, 'WCS',LastWCS, 'CatName',AstrometricCat);
            if Result is bad
                % run astrometry
                [Result, AI] = imProc.astrometry.astrometryCore(AI(Iim), AstArgs{:},...
                    'CatName',AstrometricCat, CooArgs{:});
            end
        end
        
        % id asteroid in catalog
        [RA,Dec] = getLonLat(AI.CatData,'rad');
        
        PredRA  = interp1(Ephem.Catalog(:,1), Ephem.Cat(:,2), JD, 'cubic');
        PredDec = interp1(Ephem.Catalog(:,1), Ephem.Cat(:,3), JD, 'cubic');
        
        
        Dist = celestial.coo.sphere_dist_fast(RA, Dec, PredRA, PredDec);
        IndAstInCatalog = find(Dist<(Args.AstSearchRadius./(RAD.*ARCSEC_DEG)));
        
        Dist = celestial.coo.sphere_dist_fast(RA, Dec, Args.RefRA, Args.RefDec);
        IndRefInCatalog = find(Dist<(Args.AstRefRadius./(RAD.*ARCSEC_DEG)));
        Mag = getCol(AI.CatData, Args.CollectMag);
        
        % crop image around PredRA, PredDec
        [PredX, PredY] = AI.WCS.coo2xy(PredRA.*RAD, PredDec.*RAD);
        
        CropAI(Iim) = AI.crop([PredX, PredY, Args.HalfCrop Args.HalfCrop], 'Type','center', 'CreateNewObj',true);
        CropAI(Iim).CatData = AI.CatData;
        
        CropAI(Iim).UserData.IndAstInCatalog = IndAstInCatalog;
        CropAI(Iim).UserData.IndRefInCatalog = IndRefInCatalog;
        CropAI(Iim).UserData.ZP              = Mag(IndRefInCatalog,:) - Args.RefMag;  % vector [one per aper]
        
        
    end
end

    
%     
%     
%     SearchRadius = 15;
%     
%     Xpred = Args.Xpred;
%     Ypred = Args.Ypred;
%     
%     Xref = Args.Xref;
%     Yref = Args.Yref;
%     ZPref = Args.ZPref;
%    
%     
%     
%     Iim = 0;
%     Cont = true;
%     Output.Data = nan(1000, 5);
%     while Cont
%         tic;
%         Iim = Iim + 1;
%         Files = io.files.dirSortedByDate('LAST*.fits');
%         List  = {Files.name};
%         IP = ImagePath.parseFileName(List);
%         Ind = find([IP.Time]>StartJD, 1);
% 
%         if isempty(Ind)
%             Cont = false;
%         end
%         
%         try
%             AI = pipeline.last.singleFullImageReduction(List(Ind), 'CropSize',Args.CropSize,'DoAstrom',false);
%             JD = julday(AI);
%             StartJD = JD + 0.5./86400;
% 
%             TT = AI.CatData.toTable;
%             Dist = sqrt((TT.X1 - Xpred).^2 + (TT.Y1 - Ypred).^2);
%             Isrc = find(Dist<SearchRadius);
%             % update star position
%             XsrcN = TT.X1(Isrc);
%             YsrcN = TT.Y1(Isrc);
%             
%             Xpred = mean(XsrcN);
%             Ypred = mean(YsrcN);
%             
%             % crop image
%             CropAI(Iim) = AI.crop([XsrcN, YsrcN, 200, 200],'Type','center','CreateNewObj',true);
%             
% 
% %             if ~isempty(Isrc) && Iim>2 
% %                 DX = XsrcN - Xsrc;
% %                 DY = YsrcN - Ysrc;
% %             else
% %                 DX = 0;
% %                 DY = 0;
% %             end
% %             % prediction for next image
% %             XsrcP = XsrcN;
% %             YsrcP = YsrcN;
% %             Xsrc = XsrcN + DX;
% %             Ysrc = YsrcN + DY;
% 
% 
% 
%             Dist = sqrt((TT.X1 - Xref).^2 + (TT.Y1 - Yref).^2);
%             Iref = find(Dist<SearchRadius);
%             % update ref star position
%             Xref = TT.X1(Iref(1));
%             Yref = TT.Y1(Iref(1));
% 
%             if numel(Isrc)==1
%                 Output.MasterTable(Iim,:) = TT(Isrc,:);
%                 Mag(Iim) = ZPref -2.5.*log10(TT(Isrc,:).FLUX_APER_3 ./ TT(Iref,:).FLUX_APER_3);
%                 Err(Iim) = TT(Isrc,:).FLUXERR_APER_3 ./ TT(Isrc,:).FLUX_APER_3;
% 
%                 Output.Data(Iim,:) = [JD, XsrcN, YsrcN, Mag(Iim), Err(Iim)];
%                 if Iim==1
%                     Output.T = TT;
%                     Output.Iim = Iim;
%                 else
%                     Output.T = [Output.T; TT];
%                     Output.Iim = [Output.Iim; Iim];
%                 end
%                 
%                 plot((JD-JD0).*86400, Mag(Iim),'o');
%                 hold on;
%                 plot.invy;
%                 drawnow
% 
%                 Iim
%                 if Iim==50
%                     'a'
%                 end
% 
%                 if mod(Iim,20)==0
%                     save -v7.3 Output.mat Output CropAI
%                 end
% %                 ds9(CropAI(Iim),3)
% %                 if Iim==1
% %                     ds9(AI,1);
% %                 else
% %                     ds9(AI,2);
% %                 end
% %                 pause(3);
% %                 ds9.plot([XsrcN, YsrcN],'o');
%             else
%                 ['Number of sources in search radius : ', numel(Isrc)]
%             end
%         end
%         toc
%     end
%     
% end