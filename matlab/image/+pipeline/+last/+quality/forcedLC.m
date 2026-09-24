function [Result] = forcedLC(MS, Args)
    % Return the light curves of a forced phot source and its unforced counterpart.
    % Input  : - A AMtachedSources object.
    %          * ...,key,val,... 
    %            See code.
    % Output : - A structure array of light curves.
    % Author : Eran Ofek (2026 Jun) 
    % Example: pipeline.last.quality.forcedLC(MS);

    arguments
        MS
        
        Args.ColMag            = 'MAG_PSF';
        Args.SearchRadius      = 3;
    end

    MS.addSrcData;

    Nms = numel(MS);
    K = 0;
    for Ims=1:1:Nms
        Iforced = find(MS(Ims).Data.FORCED(1,:)==1);
        Nf = numel(Iforced);
        for If=1:1:Nf
            Isrc = Iforced(If);
            JD  = MS(Ims).JD;
            Mag = MS(Ims).Data.(Args.ColMag)(:,Isrc);
            RA  = MS(Ims).SrcData.RA(Isrc);
            Dec = MS(Ims).SrcData.Dec(Isrc);
            [Res] = coneSearch(MS(Ims), RA, Dec, Args.SearchRadius);
            
            if Res.Nsrc>1
                IndC = Res.Ind(find(~ismember(Res.Ind, Isrc)))
                Mag2 = MS(Ims).Data.(Args.ColMag)(:,IndC);
                if sum(~isnan(Mag2))==20
                    K = K + 1;
                    Result(K).JD  = JD;
                    Result(K).MagF = Mag;
                    Result(K).MagU = Mag2;
                    % plot(Mag,'o')
                    % hold on;
                    % plot(Mag2,'o')
                    % hold off;
                    %     'a'
                end
            end
        end
    end

end
