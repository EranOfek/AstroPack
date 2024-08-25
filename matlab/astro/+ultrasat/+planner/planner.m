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
        Result(Ialert) = ultrasat.planner.plannerToO(strcat(Alerts(Ialert).folder,'/',Alerts(Ialert).name),...
                        'MaxTargets',Args.MaxTargets,'ProbThresh',Args.ProbThresh,'DrawMaps',false,...
                        'Verbosity',0,'ShowCoverageCurve',1,'MinCoveredProb',Args.MinCoveredProb,'MockAlerts',Args.MockAlerts);
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