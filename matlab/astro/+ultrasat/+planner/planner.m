function planner(Args)
    % 
    arguments
        Args.MaxTargets     = 4;    % maximal number of ULTRASAT targets covering the object 
        Args.MinCoveredProb = 0.8;  % the minimal cumulative probability to be covered 
        Args.ProbThresh     = 0.01; % the limiting (cleaning) probability per ULTRASAT pointing 
        Args.MockAlerts     = false;
    end

    % test ToO planner with the whole set of O4 alerts of April 1-14, 2024       
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
        
    R=Result([Result.Ntarg]>0);

%     [R.Type " "]: "PRELIMINARY" "INITIAL" "UPDATE" "RETRACTED" 
%     % of each R.Superevent keep only the latest alert? 
    SId = {R.Superevent};
    JD = celestial.time.date2jd(celestial.time.str2date(strrep(strrep({R.AlertTime},'T',' '),'Z','.0Z')));
    [uniqueSId, ~, idx] = unique(SId);
    maxIndices = arrayfun(@(i) find(JD(idx == i) == max(JD(idx == i)), 1, 'first'), 1:numel(uniqueSId));
%     R_unique = R(finalIndices);
%     
    fprintf('%d real events of %d total \n',numel(R),Nalerts);
    
    figure(1)
    Lab = sprintf('Cumulative probability covered by <= %.0f FOVs',Args.MaxTargets);
    subplot(2,1,1); histogram([R.CoveredProb]); xlabel(Lab)
    Lab = sprintf('Number of FOVs required to cover P > %.2f',Args.MinCoveredProb);
    subplot(2,1,2); histogram([R.NCover]); xlabel(Lab)
    figure(2)
    Lab = sprintf('Cumulative P >= %.2f',Args.MinCoveredProb);
    title(Lab)
    subplot(2,1,1); plot(-7.5-log10([R.FAR]),[R.NCover],'*'); xlabel 'lg(1/FAR), [lg(yr)]'; ylabel 'Number of FOVs'
    subplot(2,1,2); plot(-7.5-log10([R.FAR]),[R.CoveredArea],'*'); xlabel 'lg(1/FAR), [lg(yr)]'; ylabel 'Area, sq. deg.'
    
end

function plot_results

load('Res_240real_P0.8.mat')
load('Res_240real_P0.9.mat')
figure(1)
hold on
subplot(2,2,1)
histogram([R_real_80.CoveredArea])
subplot(2,2,3)
histogram([R_real_90.CoveredArea])
subplot(2,2,2)
histogram([R_real_80.NCover])
subplot(2,2,4)
histogram([R_real_90.NCover])
xlabel N_{exposures}
subplot(2,2,3)
xlabel 'Area (deg^2)'
ylabel '90% coverage'
subplot(2,2,1)
ylabel '80% coverage'
title '225 real alerts of LVC 04 (all types) 06.2023-08.2024'

figure(2)
title '225 real alerts of LVC 04 (all types) 06.2023-08.2024'
subplot(2,2,1)
plot(-7.5-log10([R_real_80.FAR]),[R_real_80.NCover],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
ylabel 'N_{exposures}'
subplot(2,2,2)
plot(-7.5-log10([R_real_90.FAR]),[R_real_90.NCover],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
subplot(2,2,4)
plot(-7.5-log10([R_real_90.FAR]),[R_real_90.CoveredArea],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
subplot(2,2,3)
plot(-7.5-log10([R_real_80.FAR]),[R_real_80.CoveredArea],'*'); xlabel 'lg(1/FAR), [lg(yr)]'
ylabel '80% coverage'
subplot(2,2,4)
ylabel '90% coverage'
end