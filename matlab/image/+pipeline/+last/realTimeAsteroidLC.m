function Output = realTimeAsteroidLC(TimeStart, Args)
    % Analyze a full farme LAST images
    % Example: pipeline.last.realTimeAsteroidLC([19 09 2022 22 51 23]);
    
    arguments
        TimeStart
        Args.Node      = 1
        Args.DataNum   = 1;
        Args.CropSize  = []; %[100 6000 3400 6200];
    end

    StartJD = celestial.time.julday(TimeStart);
    
    SearchRadius = 10;
    
    Xpred = 3799
    Ypred = 5401
    
    Xref = 3264;
    Yref = 4903;
    ZPref = 15;
    
    Iim = 0;
    Cont = true;
    Output.Data = nan(1000, 5);
    while Cont
        tic;
        Iim = Iim + 1;
        Files = io.files.dirSortedByDate('LAST*.fits');
        List  = {Files.name};
        IP = ImagePath.parseFileName(List);
        Ind = find([IP.Time]>StartJD, 1);

        if isempty(Ind)
            Cont = false;
        end
        
        AI = pipeline.last.singleFullImageReduction(List(Ind), 'CropSize',Args.CropSize);
        JD = julday(AI);
        StartJD = JD + 0.5./86400;
        
        TT = AI.CatData.toTable;
        Dist = sqrt((TT.X1 - Xpred).^2 + (TT.Y1 - Ypred).^2);
        Isrc = find(Dist<SearchRadius);
        % update star position
        XsrcN = TT.X1(Isrc);
        YsrcN = TT.Y1(Isrc);
        
        if Iim>2
            DX = XsrcN - Xsrc;
            DY = YsrcN - Ysrc;
        else
            DX = 0;
            DY = 0;
        end
        % prediction for next image
        XsrcP = XsrcN;
        YsrcP = YsrcN;
        Xsrc = XsrcN + DX;
        Ysrc = YsrcN + DY;
           
        
        
        Dist = sqrt((TT.X1 - Xref).^2 + (TT.Y1 - Yref).^2);
        Iref = find(Dist<SearchRadius);
        % update ref star position
        Xref = TT.X1(Iref);
        Yref = TT.Y1(Iref);
        
        if numel(Isrc)==1
            Output.MasterTable(Iim,:) = TT(Isrc,:);
            Mag(Iim) = ZPref -2.5.*log10(TT(Isrc,:).FLUX_APER_3 ./ TT(Iref,:).FLUX_APER_3);
            Err(Iim) = TT(Isrc,:).FLUXERR_APER_3 ./ TT(Isrc,:).FLUX_APER_3;
            
            Output.Data(Iim,:) = [JD, XsrcP, YsrcP, Mag(Iim), Err(Iim)];
            
            ds9(AI);
            pause(3);
            ds9.plot([XsrcP, YsrcP],'o');
        else
            ['Number of sources in search radius : ', numel(Isrc)]
        end
        
        toc
    end
    
end