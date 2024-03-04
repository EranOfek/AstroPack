function plotSources(MS,Args)
% 3d plot sources for a MatchedSources object
%  implementing for the moment just what I need now. I future more options,
%  like for instance those in AstroTable.plot could be added. Also,
%  assume for the moment that the keys JD, MAG_PSF, RA, Dec are in the
%  data, without searching for potential vocabulary alternatives
%
% Input:
%      - a MergedSources object
%      * key - value arguments:
%      'MaxMag' any source weaker than this magnitude is plotted with the
%               minimal symbol size. default 24.
%      'SymScale' relative size of the source markers
%      'JoinSources' plot vertical lines connecting the sources found in
%                    adiacent epochs (black solid lines) and nonadjacent
%                    epochs (grey dashes). Clutters the plot, but helps in
%                    visualising correspondances. Default true.
%      'OnlyOrphans' plot only markers for sources appearing in a single
%                    epoch, omitting all the others, and omitting joining lines
%
% Author: Enrico Segre, February 2024

    arguments
        MS MatchedSources
        Args.MaxMag = 24; % reasonable for PTF, excluding artefacts
        Args.SymScale =  3;
        Args.JoinSources = true;
        Args.OnlyOrphans = false;
    end
    
    % code JD incrementally according to order, i.e. epoch number
    JDvalues=unique(MS.Data.JD(~isnan(MS.Data.JD)));
    [~,JDi]=ismember(MS.Data.JD,JDvalues);
    JDi(JDi==0)=NaN;
    
    % normalize magnitudes
    MagSize=min(MS.Data.MAG_PSF, Args.MaxMag);
    MagSize=Args.SymScale*(1+10*((max(MagSize,[],"all","omitnan")-MagSize)/...
             (max(MagSize,[],"all","omitnan")-min(MagSize,[],"all","omitnan"))));
    
    % find first and last epoch of appearance of each source
    JDe=[min(JDi,[],1,'omitnan'); max(JDi,[],1,'omitnan')];
    % beware of pathological JDe=NaN ! https://github.com/EranOfek/AstroPack/issues/408
    q=find(all(~isnan(JDe)));
    orphans=find(JDe(1,:)==JDe(2,:));

    % plot balls of size inversely proportional to magnitude
    if Args.OnlyOrphans
        J=sub2ind(size(MS.Data.RA),JDe(1,orphans),orphans);
        scatter3(MS.Data.RA(J),MS.Data.Dec(J),JDi(J),MagSize(J),JDi(J),'filled')
    else
        scatter3(MS.Data.RA(:),MS.Data.Dec(:),JDi(:),MagSize(:),JDi(:),'filled')
    end

    if Args.JoinSources && ~Args.OnlyOrphans && numel(JDi)>2

        % plot grey lines joining sources detected in all epochs, between first and
        % last detection
        RAe=NaN(2,size(MS.Data.RA,2));
        Dece=RAe;
        % pitfall - JDe points to the unique JD, but there may be several
        %  catalogs with the same JD, https://github.com/EranOfek/AstroPack/issues/409
        si1=sub2ind(size(MS.Data.RA),JDe(1,q),q);
        si2=sub2ind(size(MS.Data.RA),JDe(2,q),q);
        RAe(:,q)=[MS.Data.RA(si1); MS.Data.RA(si2)];
        Dece(:,q)=[MS.Data.Dec(si1); MS.Data.Dec(si2)];

        hold on
        % this could be perhaps improved, not plotting the segments which will
        %  be overplotted by black lines below, but would need more treatment
        plot3(RAe,Dece,JDe,'-.','Color',0.7*[1 1 1])


        % plot black lines joining sources detected in adjacent epochs
        plot3(MS.Data.RA,MS.Data.Dec,JDi,'k')

        hold off
    end
    xlabel('RA'); ylabel('Dec'); zlabel('Epoch')