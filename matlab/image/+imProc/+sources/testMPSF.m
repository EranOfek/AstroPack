function [AI_PSF,mms_PSF,mms_base]=testMPSF(Args)
% This function tests multi-itereation PSF compared to standard pipeline
% products (APER3, PSF) on a set of 13 LAST coadd images of either ctrowded field or 
% uncrowded fields  
%
% Examples: 
%           [AI_PSF,mms_PSF,mms_base]=testMPSF('AI',AI_uncrowded);
%           [AI_PSF,mms_PSF,mms_base]=testMPSF('AI',AI_crowded,'Crowded',true);
%           [AI_PSF,mms_PSF,mms_base]=testMPSF();
%           [AI_PSF,mms_PSF,mms_base]=testMPSF('Crowded',true);
    arguments
        Args.AI   = []; % a vector of AI. If it is empry will try to load AI of crowded field or uncrowded field
        Args.Crowded = false;
        Args.plotShift  = true;
        Args.plotRMS  = true;
        Args.SavePlots = false;
        Args.VerboseMPSF = true;
        Args.mergeBy = 1;
        Args.BadFlags = {'Saturated', 'Negative', 'NaN', 'Spike', 'Hole', 'NearEdge'}; % Change to NaN all data points associated with these flags.
        Args.Det_frac    = 0.85% Allow for 15% no detections per source.
        Args.Radius                  = 1;
    end

    if isempty(Args.AI)
        if Args.Crowded
            load('AI_crowded.mat');
        else
            load('AI_UNcrowded.mat');
        end
    else
        AI = Args.AI;
    end
    
    % Extract results from defult pipeline
    mms_base = merge_n_ZPcoo(AI,Args.mergeBy,Args.BadFlags,Args.Det_frac,Args.Radius);

     % Run Mextractor and extract results
    AI_PSF = imProc.sources.mextractor(AI.createNewObj(1),'Verbose',Args.VerboseMPSF);    
    mms_PSF = merge_n_ZPcoo(AI_PSF,Args.mergeBy,Args.BadFlags,Args.Det_frac,Args.Radius);
    
    if Args.plotShift
        figure('WindowStyle','docked','Color',[1 1 1]);box on;hold on; grid on;
        set(gca,'yscale','log');
        
        plot(AI_PSF(1).CatData.getCol('MAG_PSF'),sqrt((AI_PSF(1).CatData.getCol('Y')-AI_PSF(1).CatData.getCol('Y1')).^2+(AI_PSF(1).CatData.getCol('X')-AI_PSF(1).CatData.getCol('X1')).^2),'.b')
        plot(AI(1).CatData.getCol('MAG_PSF'),sqrt((AI(1).CatData.getCol('Y')-AI(1).CatData.getCol('Y1')).^2+(AI(1).CatData.getCol('X')-AI(1).CatData.getCol('X1')).^2),'.r')
                
        legend(sprintf('MultiPSF (%d sources)',height(AI_PSF(1).CatData.Catalog)),sprintf('PSF (%d sources)',height(AI(1).CatData.Catalog)),'Location','best');
        xlabel('MAF_PSF','interpreter','latex');
        ylabel('Shift (X,Y)-(X1,Y1) [pix]','interpreter','latex');
        
        xlim([8,22]);
        
        if Args.Crowded
            title('Crowded field - Image 1');
        else
            title('Uncrowded field  - Image 1');
        end
        
        if Args.SavePlots
            saveas(gcf,'Shift.fig');
        end
    end

    if Args.plotRMS
        figure('WindowStyle','docked','Color',[1 1 1]);box on;hold on; grid on;
        set(gca,'yscale','log');
        
        plot(nanmedian(mms_PSF.Data.MAG_PSF,1),nanstd(mms_PSF.Data.MAG_PSF,1),'.b');
        plot(nanmedian(mms_base.Data.MAG_PSF,1),nanstd(mms_base.Data.MAG_PSF,1),'.r');
        plot(nanmedian(mms_base.Data.MAG_APER_3,1),nanstd(mms_base.Data.MAG_APER_3,1),'.k');
        
        xlim([9,22])
        ylim([1e-3,10])
        legend(sprintf('MultiPSF (%d sources)',mms_PSF.Nsrc),sprintf('PSF (%d sources)',mms_base.Nsrc),sprintf('Aper3 (%d sources)',mms_base.Nsrc),'Location','best');

        xlabel('MAG','interpreter','latex');
        ylabel('STD','interpreter','latex');
        
        if Args.Crowded
            title('Crowded field');
        else
            title('Uncrowded field');
        end
        
        if Args.SavePlots
            saveas(gcf,'Shift.fig');
        end
    end


end


function mms = merge_n_ZPcoo(AI,mergeBy,BadFlags,Det_frac,Radius)
    % Extract results from defult pipeline
    [~,MS] = imProc.match.mergeCatalogs(AI.','Radius',Radius);
    ms = mergeByCoo(MS, MS(mergeBy));
    mms = ms.setBadPhotToNan('BadFlags', BadFlags, 'MagField', 'MAG_PSF', 'CreateNewObj', true);
    NdetGood = sum(~isnan(mms.Data.MAG_PSF), 1);
    Fndet = NdetGood > Det_frac*mms.Nepoch; % Allow for 15% no detections per source.
    mms = mms.selectBySrcIndex(Fndet, 'CreateNewObj', false);
    
    % apply zp correction to every mag field in MS
    r = lcUtil.zp_meddiff(mms, 'MagField', {'MAG_PSF'}, 'MagErrField', {'MAGERR_PSF'});
    [mms, ~] = applyZP(mms, r.FitZP, 'ApplyToMagField', 'MAG_PSF');
    r = lcUtil.zp_meddiff(mms, 'MagField', {'MAG_APER_3'}, 'MagErrField', {'MAGERR_APER_3'});
    [mms, ~] = applyZP(mms, r.FitZP, 'ApplyToMagField', 'MAG_APER_3');
end