function planner

    % test ToO planner with the whole set of O4 alerts of April 1-14, 2024       
    Alerts  = dir ('~/ULTRASAT/SkyGrid/LVC/*/*csv');
    
    Nalerts = numel(Alerts);
        
    for Ialert = 1:Nalerts
        fprintf('%d \n',Ialert); 
        Result(Ialert) = ultrasat.planner.plannerToO(strcat(Alerts(Ialert).folder,'/',Alerts(Ialert).name),...
                        'MaxTargets',4,'ProbThresh',0.01,'DrawMaps',false,'Verbosity',0,'ShowCoverageCurve',1);
%                         'MaxTargets',4,'ProbThresh',0.2,'DrawMaps',false,'Verbosity',0);
        if Result(Ialert).Superevent(1) == 'M' || Result(Ialert).Superevent(1) == 'T' % filter out Mocks and Tests 
            Ind(Ialert) = 0; 
        else
            Ind(Ialert) = 1;
        end
    end
    
    R = Result(Ind > 0);
    
    fprintf('%d real events of %d total \n',numel(R),Nalerts);
    
    subplot(2,2,1); histogram([R.CoveredProb])
    subplot(2,2,2); histogram([R.Ntarg])
    subplot(2,2,3); histogram([Result.CoveredProb])
    subplot(2,2,4); histogram([Result.Ntarg])

end