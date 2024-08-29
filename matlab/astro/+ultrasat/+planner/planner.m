function planner(Args)
    % 
    arguments
        Args.MaxTargets     = 4;              % maximal number of ULTRASAT targets covering the object 
        Args.MinCoveredProb = [0.5 0.8 0.9];  % the minimal cumulative probability to be covered 
        Args.ProbThresh     = 0.01;           % the limiting (cleaning) probability per ULTRASAT pointing 
        Args.MockAlerts     = false;
        Args.FirstAndLast   = true;        
        Args.UpdateAndInitialOnly = false;
    end

    % test ToO planner with the whole set of O4 alerts        
    Alerts  = dir ('~/ULTRASAT/SkyGrid/LVC/202*/*/*/*csv');
    
    Nalerts = numel(Alerts);
        
    tic
    
    for Ialert = 1:Nalerts
        fprintf('%d \n',Ialert); 
        try
            File = strcat(Alerts(Ialert).folder,'/',Alerts(Ialert).name);
            Result(Ialert) = ultrasat.planner.plannerToO(File,...
                'MaxTargets',Args.MaxTargets,'ProbThresh',Args.ProbThresh,'DrawMaps',false,...
                'Verbosity',0,'ShowCoverageCurve',1,'MinCoveredProb',Args.MinCoveredProb,'MockAlerts',Args.MockAlerts);
        catch ME
            fprintf('Alert %d file %s: planner failed \n',Ialert,File)
        end
    end
    
    toc
        
    % clean the structure 
    R = Result(arrayfun(@(x) x.NCover(1) > 0, Result));    
    
    fprintf('%d real events of %d total \n',numel(R),Nalerts);
    
    save('R.mat','R')
    
    % Types: "EARLYWARNING" "PRELIMINARY" "INITIAL" "UPDATE" "RETRACTED"     
    if Args.UpdateAndInitialOnly
        R = R(string({R.Type})=='UPDATE' | string({R.Type})=='INITIAL'); % keep only 'UPDATE' and 'INITIAL' alerts        
    end
   
    % of each R.Superevent keep only the first and the last alert
    if Args.FirstAndLast
        SId = {R.Superevent};
        JD = celestial.time.date2jd(celestial.time.str2date(strrep(strrep({R.AlertTime},'T',' '),'Z','.0Z')));
        for i = 1:numel(JD)
            R(i).AlertJD = JD(i);
        end
        [uniqueSId, ~, ~] = unique(SId);
        maxIndices = zeros(1, numel(uniqueSId));  minIndices = zeros(1, numel(uniqueSId));
        for i = 1:numel(uniqueSId)
            % Get indices of structures with the current SId
            currentIndices = find(strcmp(SId, uniqueSId{i}));
            % Find the index of the structure with the maximum JD value
            [~, maxIdx] = max(JD(currentIndices));
            [~, minIdx] = min(JD(currentIndices));
            % Store the original index of this structure
            maxIndices(i) = currentIndices(maxIdx);
            minIndices(i) = currentIndices(minIdx);
        end
        Rn = R(maxIndices);
        R1 = R(minIndices);
    end        
%     
    % plots
    
    figure(1)
    T = sprintf('%d unique alerts of LVC 04 06/23-08/24',numel(Rn)); title(T)
    subplot(2,1,1); histogram([Rn.NCover])
    XL = sprintf('N_{exp} required to cover P > %.2f',Args.MinCoveredProb); xlabel(XL)
    subplot(2,1,2); histogram([Rn.CoveredArea],'BinWidth',250);
    XL = sprintf('area, > %.2f accumulated probability [deg^2]',Args.MinCoveredProb); xlabel(XL)     
              
%     figure(1)
%     Lab = sprintf('Cumulative probability covered by <= %.0f FOVs',Args.MaxTargets);
%     subplot(2,1,1); histogram([R.CoveredProb]); xlabel(Lab)
%     Lab = sprintf('Number of FOVs required to cover P > %.2f',Args.MinCoveredProb);
%     subplot(2,1,2); histogram([R.NCover]); xlabel(Lab)
%     figure(2)
%     Lab = sprintf('Cumulative P >= %.2f',Args.MinCoveredProb);
%     title(Lab)
%     subplot(2,1,1); plot(-7.5-log10([R.FAR]),[R.NCover],'*'); xlabel 'lg(1/FAR), [lg(yr)]'; ylabel 'Number of FOVs'
%     subplot(2,1,2); plot(-7.5-log10([R.FAR]),[R.CoveredArea],'*'); xlabel 'lg(1/FAR), [lg(yr)]'; ylabel 'Area, sq. deg.'
    
end
% 
% 
% load('Res_240real_P0.8.mat')
% load('Res_240real_P0.9.mat')
% figure(1)
% hold on
% subplot(2,2,1)
% histogram([R_real_80.CoveredArea])
% subplot(2,2,3)
% histogram([R_real_90.CoveredArea])
% subplot(2,2,2)
% histogram([R_real_80.NCover])
% subplot(2,2,4)
% histogram([R_real_90.NCover])
% xlabel N_{exposures}
% subplot(2,2,3)
% xlabel 'Area (deg^2)'
% ylabel '90% coverage'
% subplot(2,2,1)
% ylabel '80% coverage'
% title '225 real alerts of LVC 04 (all types) 06.2023-08.2024'
% 
% figure(2)
% title '225 real alerts of LVC 04 (all types) 06.2023-08.2024'
% subplot(2,2,1)
% plot(-7.5-log10([R_real_80.FAR]),[R_real_80.NCover],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
% ylabel 'N_{exposures}'
% subplot(2,2,2)
% plot(-7.5-log10([R_real_90.FAR]),[R_real_90.NCover],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
% subplot(2,2,4)
% plot(-7.5-log10([R_real_90.FAR]),[R_real_90.CoveredArea],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
% subplot(2,2,3)
% plot(-7.5-log10([R_real_80.FAR]),[R_real_80.CoveredArea],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
% ylabel '80% coverage'
% subplot(2,2,4)
% ylabel '90% coverage'