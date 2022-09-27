function [Output,CropAI] = realTimeAsteroidLC(TimeStart, Args)
    % Analyze a full farme LAST images
    % Example: Output=pipeline.last.realTimeAsteroidLC([19 09 2022 22 51 23]);
    
    arguments
        TimeStart      = [26 9 2022 23 13 00];
        Args.Node      = 1
        Args.DataNum   = 1;
        Args.CropSize  = []; %[100 6000 3400 6200];
        Args.Xpred     = 3644;
        Args.Ypred     = 4042;
        Args.Xref      = 3561;
        Args.Yref      = 3916;
        Args.ZPref     = 11.233;
    end

    StartJD = celestial.time.julday(TimeStart);
    JD0 = celestial.time.julday([26 9 2022 23 15 0]);
    
    SearchRadius = 15;
    
    Xpred = Args.Xpred;
    Ypred = Args.Ypred;
    
    Xref = Args.Xref;
    Yref = Args.Yref;
    ZPref = Args.ZPref;
   
    
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
        
        try
            AI = pipeline.last.singleFullImageReduction(List(Ind), 'CropSize',Args.CropSize,'DoAstrom',false);
            JD = julday(AI);
            StartJD = JD + 0.5./86400;

            TT = AI.CatData.toTable;
            Dist = sqrt((TT.X1 - Xpred).^2 + (TT.Y1 - Ypred).^2);
            Isrc = find(Dist<SearchRadius);
            % update star position
            XsrcN = TT.X1(Isrc);
            YsrcN = TT.Y1(Isrc);
            
            Xpred = mean(XsrcN);
            Ypred = mean(YsrcN);
            
            % crop image
            CropAI(Iim) = AI.crop([XsrcN, YsrcN, 200, 200],'Type','center','CreateNewObj',true);
            

%             if ~isempty(Isrc) && Iim>2 
%                 DX = XsrcN - Xsrc;
%                 DY = YsrcN - Ysrc;
%             else
%                 DX = 0;
%                 DY = 0;
%             end
%             % prediction for next image
%             XsrcP = XsrcN;
%             YsrcP = YsrcN;
%             Xsrc = XsrcN + DX;
%             Ysrc = YsrcN + DY;



            Dist = sqrt((TT.X1 - Xref).^2 + (TT.Y1 - Yref).^2);
            Iref = find(Dist<SearchRadius);
            % update ref star position
            Xref = TT.X1(Iref(1));
            Yref = TT.Y1(Iref(1));

            if numel(Isrc)==1
                Output.MasterTable(Iim,:) = TT(Isrc,:);
                Mag(Iim) = ZPref -2.5.*log10(TT(Isrc,:).FLUX_APER_3 ./ TT(Iref,:).FLUX_APER_3);
                Err(Iim) = TT(Isrc,:).FLUXERR_APER_3 ./ TT(Isrc,:).FLUX_APER_3;

                Output.Data(Iim,:) = [JD, XsrcN, YsrcN, Mag(Iim), Err(Iim)];
                if Iim==1
                    Output.T = TT;
                    Output.Iim = Iim;
                else
                    Output.T = [Output.T; TT];
                    Output.Iim = [Output.Iim; Iim];
                end
                
                plot((JD-JD0).*86400, Mag(Iim),'o');
                hold on;
                plot.invy;
                drawnow

                Iim
                if Iim==50
                    'a'
                end

                if mod(Iim,20)==0
                    save -v7.3 Output.mat Output CropAI
                end
%                 ds9(CropAI(Iim),3)
%                 if Iim==1
%                     ds9(AI,1);
%                 else
%                     ds9(AI,2);
%                 end
%                 pause(3);
%                 ds9.plot([XsrcN, YsrcN],'o');
            else
                ['Number of sources in search radius : ', numel(Isrc)]
            end
        end
        toc
    end
    
end