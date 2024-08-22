function planner

    % test ToO planner with the whole set of O4 alerts of April 1-14, 2024       
    Alerts  = dir ('~/ULTRASAT/SkyGrid/LVC/*/*csv');
    
    Nalerts = numel(Alerts);
        
    for Ialert = 1:Nalerts
        fprintf('%d \n',Ialert); 
        Result(Ialert) = ultrasat.planner.plannerToO(strcat(Alerts(Ialert).folder,'/',Alerts(Ialert).name),...
                        'MaxTargets',4,'ProbThresh',0.01,'DrawMaps',false,'Verbosity',0,'ShowCoverageCurve',1);
%                         'MaxTargets',4,'ProbThresh',0.2,'DrawMaps',false,'Verbosity',0);
    end
        
    R=Result([Result.Ntarg]>0);

%     [R.Type " "]: "PRELIMINARY" "INITIAL" "UPDATE" "RETRACTED" 
%     % of each R.Superevent keep only the latest? 
%     SId = {R.Superevent}; 
%     Time = [R.DateObs];
    
    fprintf('%d real events of %d total \n',numel(R),Nalerts);
    
    subplot(2,1,1); histogram([R.CoveredProb]); xlabel 'Covered probability'
    subplot(2,1,2); histogram([R.NCover]); xlabel 'Number of covering FOVs'
    
end