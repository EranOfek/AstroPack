function [Result] = overlapSources(AI, Args)
    % compare source characteristics from overlapping image crops
    %     Optional detailed description
    % Input  : - an AstroImage containing all the crops (proc or coadd)    
    %          * ...,key,val,... 
    %         'MagCut' - a range of MAG_APER_3 magnituds employed for the comparison
    %         'MatchRadius' - match radius in arcsec
    %         'Prop'   - a list of columns to compare
    %         'BadFlags' - a list of bad flags employed to deselect sources
    %         'FilterBad' - whether to use the 'BadFlags' for filtering
    %         'CroppingScheme' - 'old' or 'new'
    %         'Plot' - make a 2-panel plot of MAG_AB_APER_3 and FLUX_APER_3 differences distribution
    % Output : - a struct with statistics for each property 'Prop' and each Crop:
    %          .Diff (a cell array)
    %          .MeanDiff
    %          .StdDiff
    %          .MedianDiff (not very usefull?)
    % Author : A.M. Krassilchtchikov (2026 Feb) 
    % Example: R = pipeline.last.quality.overlapSources(Coadd);
    %
    arguments
        AI      
        Args.MagCut      = [13 15];  
        Args.MatchRadius = 1; % arcsec
        Args.Prop        = {'RA', 'Dec', 'XPEAK', 'YPEAK', 'X1', 'Y1', 'X', 'Y', ...
                            'FLUX_APER_3', 'MAG_APER_3', 'MAG_AB_APER_3', 'MAG_PSF', 'MAG_AB_PSF'};        
        Args.BadFlags    = {'Saturated', 'Negative', 'NaN', 'Spike', 'Hole', 'NearEdge'};   
        Args.FilterBad   = true;
        Args.CroppingScheme = 'new'; 
        Args.Plot        = false;
    end
    BD = BitDictionary;
    IndX = AI(1).CatData.colname2ind({'XPEAK','X1','X'});
    IndY = AI(1).CatData.colname2ind({'YPEAK','Y1','Y'});
    % read the list of overlap interfaces:
    Ind   = LASToverlapsNew('CroppingScheme', Args.CroppingScheme);
    Nvrlp = size(Ind,1);
    % loop over all the possible pairs of crops
    for Ivrlp = 1:Nvrlp
        Cat1 = AI(Ind(Ivrlp,1)).CatData.copy;
        Cat2 = AI(Ind(Ivrlp,2)).CatData.copy;
        % filter bad flags
        if Args.FilterBad
            [BitName1,~,~]=bitdec2name(BD,Cat1.Table.FLAGS);
            [BitName2,~,~]=bitdec2name(BD,Cat2.Table.FLAGS);
            FlagBad1 = cellfun(@(c) any(ismember(c, Args.BadFlags)), BitName1) > 0;
            FlagBad2 = cellfun(@(c) any(ismember(c, Args.BadFlags)), BitName2) > 0;
            Cat1.Catalog = Cat1.Catalog(~FlagBad1,:);
            Cat2.Catalog = Cat2.Catalog(~FlagBad2,:);
        end
        % shift XPEAK, YPEAK, X1, Y1
        ORIGSEC1 = AI(Ind(Ivrlp,1)).HeaderData.getVal('ORIGSEC','ReadCCDSEC',true);
        ORIGSEC2 = AI(Ind(Ivrlp,2)).HeaderData.getVal('ORIGSEC','ReadCCDSEC',true);          
        Cat1.Catalog(:,IndX) = Cat1.Catalog(:,IndX) + ORIGSEC1(1) - 1;
        Cat1.Catalog(:,IndY) = Cat1.Catalog(:,IndY) + ORIGSEC1(3) - 1;
        Cat2.Catalog(:,IndX) = Cat2.Catalog(:,IndX) + ORIGSEC2(1) - 1;
        Cat2.Catalog(:,IndY) = Cat2.Catalog(:,IndY) + ORIGSEC2(3) - 1;        
        
        MS = imProc.match.match(Cat1, Cat2, 'Radius', Args.MatchRadius);             
        
        FlagMag = MS.Table.MAG_APER_3 < Args.MagCut(2) & MS.Table.MAG_APER_3 > Args.MagCut(1);        
                               
        if sum(FlagMag) > 0
            fprintf('%d overlap sources found between crops %d and %d\n',sum(FlagMag),Ind(Ivrlp,1), Ind(Ivrlp,2));
            for Iprop = 1:numel(Args.Prop)
                Prop = Args.Prop{Iprop};
                Val2 = Cat2.Table.(Prop);
                D = MS.Table.(Prop) - Val2;
                Diff = D(FlagMag);
                Result.(Prop).Diff{Ivrlp} = Diff(~isnan(Diff));
                Result.(Prop).MedianDiff(Ivrlp) = median(Diff, 1,'omitnan');
                Result.(Prop).MeanDiff(Ivrlp)   = mean(Diff, 1,'omitnan');
                Result.(Prop).StdDiff(Ivrlp)    = std(Diff,[],1,'omitnan');
                if strcmpi(Prop,'FLUX_APER_3') % add relative diff for the FLUX  
                    Result.(Prop).RelDiff{Ivrlp} = abs(Diff./Val2(FlagMag));  
                    % identify largest flux variations:
                    Noff = sum(Result.(Prop).RelDiff{Ivrlp} > 1e-2);
                    if Noff > 0
                        cprintf('blue','NB: %d case(s) of flux variation > 1%% found between crops: %d %d\n',...
                            Noff,Ind(Ivrlp,1),Ind(Ivrlp,2));
                    end
                end
            end
        else
            fprintf('No overlap sources found between crops %d and %d\n',Ind(Ivrlp,1), Ind(Ivrlp,2));
            for Iprop = 1:numel(Args.Prop)
                Prop = Args.Prop{Iprop};
                Result.(Prop).MedianDiff(Ivrlp) = NaN;
                Result.(Prop).StdDiff(Ivrlp)    = NaN;
            end
        end
        clear Cat1 Cat2
    end
    if Args.Plot
        [c, r] = ind2sub([4 6],Ind);
        X = (c(:,1)+c(:,2))/2;
        Y = (r(:,1)+r(:,2))/2;      
        figure; 
        subplot(2,3,1)
        scatter(X,Y,80, Result.MAG_AB_APER_3.MedianDiff, ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        title 'Median Diff MAG\_AB\_APER\_3'      
        subplot(2,3,2)
        scatter(X,Y,80, Result.MAG_APER_3.MedianDiff, ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        title 'Median Diff MAG\_APER\_3'
        subplot(2,3,3)
        scatter(X,Y,80, Result.MAG_AB_PSF.MedianDiff, ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        Msg = sprintf('filtered by %d < MAG-APER-3 < %d',Args.MagCut(1),Args.MagCut(2));
        xlabel(Msg);
        title 'Median Diff MAG\_AB\_PSF'
        subplot(2,3,4)
        scatter(X,Y,80, sqrt(Result.RA.MedianDiff.^2+Result.Dec.MedianDiff.^2)*3600, ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        title 'sqrt(dRA^2 + dDec^2), arcsec' 
        
        for i=1:24
            F(i) = imUtil.psf.fwhmFromContour(AI(i).PSF); 
            Foffset(i) = sqrt( (F(i).Center(1)-13).^2 + (F(i).Center(2)-13).^2);
        end             
        [Y1, X1] = meshgrid(1:6, 1:4);
        subplot(2,3,5)
        scatter(X1(:),Y1(:),80, [F.Average_fwhm], ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        title 'Average FWHM PSF, arcsec'
        
        subplot(2,3,6)
        scatter(X1(:),Y1(:),80, Foffset, ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        title 'Center offset, pix'
        
        figure;
        dFlux    = vertcat(Result.FLUX_APER_3.Diff{:});       
        dRelFlux = vertcat(Result.FLUX_APER_3.RelDiff{:});
        dMagAB   = vertcat(Result.MAG_AB_APER_3.Diff{:});
        dMagPSF  = vertcat(Result.MAG_PSF.Diff{:});
                
        dX1 = vertcat(Result.X1.Diff{:}); dY1 = vertcat(Result.Y1.Diff{:});
        dR1 = sqrt(dX1.^2+dY1.^2); 
        dX  = vertcat(Result.X.Diff{:}); dY = vertcat(Result.Y.Diff{:});
        dR  = sqrt(dX.^2+dY.^2); 
        dRA = vertcat(Result.RA.Diff{:}); dDec = vertcat(Result.Dec.Diff{:});
        dSky = sqrt(dRA.^2+dDec.^2); 
        
        Ind = [8 11 26 31]; % interfaces between central pixels 10, 11, 14, 15  
        dMAG_PSFCentral = Result.MAG_PSF.Diff(Ind);
        dX_Central      = Result.X.Diff(Ind);
        dY_Central      = Result.Y.Diff(Ind);
        dMagPSFCentral  = vertcat(dMAG_PSFCentral{:}); 
        dRCentral       = sqrt(vertcat(dX_Central{:}).^2+vertcat(dY_Central{:}).^2); 
                        
        subplot(2,3,1)
        loglog(dR1,dRelFlux,"*"); 
        xlabel 'dR1, pix'; ylabel 'dRelFlux'
        subplot(2,3,2)
        loglog(dR1,abs(dFlux),"*"); 
        xlabel 'dR1, pix'; ylabel 'dFlux, e^-'
        subplot(2,3,3)
        semilogy(dMagAB,dRelFlux,"*"); 
        xlabel 'dMagAB\_APER3'; ylabel 'dRelFlux'
        subplot(2,3,4)
        semilogx(dR1,dMagAB,"*"); 
        xlabel 'dR1, pix'; ylabel 'dMagAB\_APER3'
        subplot(2,3,5)
        semilogx(dR,dMagPSF,"*"); 
        hold on
        semilogx(dRCentral,dMagPSFCentral,"o"); 
        xlabel 'dR, pix'; ylabel 'dMag\_PSF'        
        subplot(2,3,6)  
        loglog(dR1,dSky,"*");
        xlabel 'dR1, pix'; ylabel 'dSky, arcsec'
    end
end
%
function Ind = LASToverlapsNew(Args)
    arguments
        Args.CroppingScheme = 'new';
    end
% NB: this is LAST-specific!
    if strcmpi(Args.CroppingScheme,'new')
        Ind = [1 2;   2  3;  3  4; ...
               5 6;   6  7;  7  8; ...
               9 10; 10 11; 11 12; ...
               13 14; 14 15; 15 16; ...
               17 18; 18 19; 19 20; ...
               21 22; 22 23; 23 24; ...
            ...
               1 5; 5  9;  9 13; 13 17; 17 21; ...
               2 6; 6 10; 10 14; 14 18; 18 22; ...
               3 7; 7 11; 11 15; 15 19; 19 23; ...
               4 8; 8 12; 12 16; 16 20; 20 24];
    else
        Ind = [1 7;   7 13; 13 19; ...
               2 8;   8 14; 14 20; ...
               3 9;   9 15; 15 21; ...
               4 10; 10 16; 16 22; ...
               5 11; 11 17; 17 23; ...
               6 12; 12 18; 18 24; ...
            ...
               1 2; 2 3; 3 4; 4 5; 5 6; ...
               7 8; 8 9; 9 10; 10 11; 11 12; ...
               13 14; 14 15; 15 16; 16 17; 17 18; ...
               19 20; 20 21; 21 22; 22 23; 23 24];
    end
end
