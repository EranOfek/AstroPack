function [Result] = overlapSources(AI, Args)
    % compare source characteristics from overlapping image crops
    %     Optional detailed description
    % Input  : - an AstroImage containing all the crops (proc or coadd)    
    %          * ...,key,val,... 
    %         'MagCut' - a limiting magnitude employed for the comparison
    %         'MatchRadius' - match radius in arcsec
    %         'Prop'   - a list of columns to compare
    %         'BadFlags' - a list of bad flags employed to deselect sources
    %         'FilterBad' - whether to use the 'BadFlags' for filtering
    %         'CroppingScheme' - 'old' or 'new'
    %         'Plot' - make a 2-panel plot of MAG_AB_APER_3 and FLUX_APER_3 differences distribution
    % Output : - a struct with statistics for each crop
    % Author : A.M. Krassilchtchikov (2026 Feb) 
    % Example: R = pipeline.last.quality.overlapSources(Coadd);
    %
    arguments
        AI      
        Args.MagCut      = 17;  
        Args.MatchRadius = 3; % arcsec
        Args.Prop        = {'RA', 'Dec', 'FLUX_APER_3', 'MAG_APER_3', 'MAG_PSF', 'MAG_AB_APER_3'};        
        Args.BadFlags    = {'Saturated', 'Negative', 'NaN', 'Spike', 'Hole', 'NearEdge'};   
        Args.FilterBad   = true;
        Args.CroppingScheme = 'new'; 
        Args.Plot        = false;
    end
    BD=BitDictionary;
    % read the list of overlap interfaces:
    Ind   = LASToverlapsNew('CroppingScheme', Args.CroppingScheme);
    Nvrlp = size(Ind,1);
    % loop over all the possible pairs of crops
    for Ivrlp = 1:Nvrlp
        MS = imProc.match.match(AI(Ind(Ivrlp,1)).CatData, AI(Ind(Ivrlp,2)).CatData, 'Radius', Args.MatchRadius);
        
        FlagMag = MS.Table.MAG_APER_3 < Args.MagCut;        
        
        if Args.FilterBad
            Col = MS.colnameDict2ind('FLAGS');
            IsNan = isnan(MS.Table.FLAGS);
            MS.Catalog(IsNan,Col)=0;
            [BitName,~,~]=bitdec2name(BD,MS.Table.FLAGS);
            FlagBad = cellfun(@(c) any(ismember(c, Args.BadFlags)), BitName) > 0;
            
            Flag = FlagMag & ~FlagBad;
        else
            Flag = FlagMag;
        end
                       
        if sum(Flag) > 0
            fprintf('%d overlap sources found between crops %d and %d\n',sum(Flag),Ind(Ivrlp,1), Ind(Ivrlp,2));
            for Iprop = 1:numel(Args.Prop)
                Prop = Args.Prop{Iprop};
                Diff = MS.Table.(Prop) - AI(Ind(Ivrlp,2)).CatData.Table.(Prop);
                Result.(Prop).MedianDiff(Ivrlp) = nanmedian(Diff(Flag), 1);
                Result.(Prop).StdDiff(Ivrlp)    = nanstd(Diff(Flag),[],1);
            end
        else
            fprintf('No overlap sources found between crops %d and %d\n',Ind(Ivrlp,1), Ind(Ivrlp,2));
            for Iprop = 1:numel(Args.Prop)
                Prop = Args.Prop{Iprop};
                Result.(Prop).MedianDiff(Ivrlp) = NaN;
                Result.(Prop).StdDiff(Ivrlp)    = NaN;
            end
        end
    end
    if Args.Plot
        [c, r] = ind2sub([4 6],Ind);
        X = (c(:,1)+c(:,2))/2;
        Y = (r(:,1)+r(:,2))/2;      
        figure; subplot(1,2,1)
        scatter(X,Y,80, Result.MAG_AB_APER_3.MedianDiff, ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        title 'Median Diff MAG\_AB\_APER\_3'
        subplot(1,2,2)
        scatter(X,Y,80, Result.FLUX_APER_3.MedianDiff, ...
           'filled', 'MarkerEdgeColor', 'k', 'LineWidth', 1.5); 
        xlim([0.5 4.5]); ylim([0.5 6.5]); colorbar
        title 'Median Diff FLUX\_APER\_3'
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
