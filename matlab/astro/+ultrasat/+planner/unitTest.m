
function Result = unitTest(Args)
    % uplanner.unitTest
    arguments
        Args.Verbose   = true;
        Args.Parts     = {'HCS','LCS','TOO','DDT'}; % {'HCS','LCS','AllSS','TOO','DDT'}; % currently we need HCS to test LCS 
    end
    % unitTest
    Result=false;
    %
    if Args.Verbose
        fprintf('Start uplanner unit Test\n');
        fprintf('---------------------------------\n');
    end

    %
    if ismember('hcs',lower(Args.Parts))                    
        if Args.Verbose
            fprintf('Start testing HCS plan...');
        end
        
        % Example for creating HCS survey:
        HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Name','RA','Dec'},'RowNames',{'S1','N2','N3'}');
        upHCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','HCS');
        upHCS.StartTime = 'now';
        upHCS.EndTime = upHCS.StartTime+calmonths(6)-days(1);
        upHCS.addUniqTargets(HCS_fields.RA('S1'),HCS_fields.Dec('S1'),'Name',HCS_fields.Name('S1'));
        upHCS.buildHCS;
        
        if Args.Verbose
            fprintf('completed\n');
        end                    
    end

    %
    if ismember('lcs',lower(Args.Parts))
        if Args.Verbose
            fprintf('Start testing LCS plan...');
        end
        
        % upHCS.plotMapPlan('disp_uniqTarg',true,'disp_plan',true,'ExtinctionMap',true,'CalObjMap',true,'disp_MissAprvPlan',true)
        
        % Example for creating LCS survey:
        upLCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','LCS');
        upLCS.StartTime = 'now';
        upLCS.EndTime = upLCS.StartTime+caldays(45);
        upLCS.DailyWindowStartTime = duration('03:00:00');
        
        LCS_grid = readtable(fullfile(upLCS.BaseDataDir,'LCS_nonoverlapping_grid.csv'));
        F = LCS_grid.V45==1 & LCS_grid.A_U_1==1;
        upLCS.addUniqTargets(LCS_grid.RA(F),LCS_grid.Dec(F),'Name',num2cell(LCS_grid.Field(F)));
        
        upLCS.updateTargetVisibility('WindowStartTime',upLCS.StartTime,'WindowEndTime',upLCS.EndTime);
        F2 = find(all(upLCS.Vis.SunLimits & upLCS.Vis.EarthLimits & upLCS.Vis.MoonLimits ,1));
        
        % Fakely retrive upHCS ar approved target list
        upLCS.retrieveMissionApprovedPlan('inputPlan',upHCS.Plan);
        
        % check with struct
        load(fullfile(upLCS.BaseDataDir,'api_response.mat')');
        
        upLCS.retrieveMissionApprovedPlan('inputPlan',response);
        
        upLCS.buildLCS('TargetList',F2);
        
        if Args.Verbose
            fprintf('completed\n');
        end
        
        if Args.Verbose
            fprintf('Start testing adjustGroupStartTime, edit/del UniqTarg/PlanRow...');
        end
        
        upLCS.adjustGroupStartTime;  % Check adjustments relative to Approved List
        
        CheckStatus = upLCS.planSelfConsistencyCheck;
        if ~CheckStatus
            return
        end
        
        upLCS.editUniqTarg(4,'Name',"bla");
        upLCS.editUniqTarg(4,'RA',100);
        
        upLCS.editPlanRow(1);
        upLCS.editPlanRow(1,'Tiles',"124");
        upLCS.editPlanRow(1,'updateRowsProp',true);
        upLCS.editPlanRow(1,'Nexposures',2);
        upLCS.editPlanRow(1,'ExpTime',seconds(250));
        upLCS.editPlanRow(10,'ExpTime',seconds(250));
        upLCS.editPlanRow(5,'ExpTime',seconds(250));
        
        upLCS.delPlanRow(10);
        upLCS.delPlanRow(3);
        upLCS.delPlanRow(1)
        
        %upLCS.delUniqTarg(1);
        upLCS.delUniqTarg(5,'abort_if_in_plan',false);
        
        if Args.Verbose
            fprintf('completed\n');
        end                    
    end

    %
    if ismember('too',lower(Args.Parts))                    
        if Args.Verbose
            fprintf('Start ToO plan...\n');
        end                    
        % a simple example for ToO plan:
        if Args.Verbose
            fprintf('a minimal example: ');
        end  
        HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Name','RA','Dec'},'RowNames',{'S1','N2','N3'}');                   
        upTOO = ultrasat.planner.uplanner('AstPlanner','YS','Type','TOO');
        upTOO.buildTOO('RA',HCS_fields.RA,'Dec',HCS_fields.Dec,'Name',HCS_fields.Name);
        if Args.Verbose
            fprintf('%d exposures scheduled\n',height(upTOO.Plan));
            fprintf('completed\n');
            fprintf('-------------------------\n');
        end                    
        
        % a ToO plan from an input probability map:                                        
        upTOO1 = ultrasat.planner.uplanner('AstPlanner','AK','Type','TOO');
        upTOO1.TOOMaxTargets     = 4;
        upTOO1.TOOMinCoveredProb = 0.3;
        upTOO1.TOOWindowDuration = hours(3);  
        upTOO1.TOOAlertProbMap   = readtable('~/matlab/data/ULTRASAT/lvc_2024_04_01_00_40_58_000000.csv');
        if Args.Verbose
            fprintf('a ToO plan from an external probability map:\n');
            fprintf('Maximal number of exposures: %d\n',upTOO1.TOOMaxTargets);
            fprintf('Minimal probability to be covered: %.2f\n',upTOO1.TOOMinCoveredProb);
        end  
        upTOO1.buildTOO('Verbosity',0,'DrawMaps',0);            
                    fprintf('%d exposures scheduled\n',height(upTOO1.Plan));
                    fprintf('-------------------------\n');

        upTOO2 = ultrasat.planner.uplanner('AstPlanner','AK','Type','TOO');
        upTOO2.TOOMaxTargets     = 100;
        upTOO2.TOOMinCoveredProb = 0.9;
        upTOO2.TOOWindowDuration = hours(5);
        upTOO2.TOOAlertProbMap   = readtable('~/matlab/data/ULTRASAT/lvc_2024_04_01_00_40_58_000000.csv');                  
        if Args.Verbose
            fprintf('a ToO plan from an external probability map:\n');
            fprintf('Maximal number of exposures: %d\n',upTOO2.TOOMaxTargets);
            fprintf('Minimal probability to be covered: %.2f\n',upTOO2.TOOMinCoveredProb);
        end                      
        upTOO2.buildTOO('Verbosity',0,'DrawMaps',0);    
                    fprintf('%d exposures scheduled\n',height(upTOO2.Plan));
                    fprintf('-------------------------\n');
    end

    %
    if ismember('ddt',lower(Args.Parts))
        if Args.Verbose
            fprintf('Start DDT plan...');
        end                    
        % Example DDT plan (very basic):
        HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Name','RA','Dec'},'RowNames',{'S1','N2','N3'}');                   
        upDDT = ultrasat.planner.uplanner('AstPlanner','YS','Type','DDT');
        upDDT.addUniqTargets(HCS_fields.RA,HCS_fields.Dec,'Name',num2cell(HCS_fields.Name));
        upDDT.addDDT2Plan([1,2],datetime('now','TimeZone','UTC'));
        upDDT.addDDT2Plan([3,2],datetime('tomorrow','TimeZone','UTC'));
        
        if Args.Verbose
            fprintf('completed\n');
        end                    
    end

    %
    if ismember('allss',lower(Args.Parts))
        if Args.Verbose
            fprintf('Start AllSS plan...\n');
        end
        
        % Example for AllSS plan:
        DitherLeg = 3.0;
%                     upAllSS = ultrasat.planner.uplanner('AstPlanner','YS','Type','AllSS','ExtragalDitherLeg',DitherLeg,...
%                         'Save','~/alss_uniq_targ.mat'); % first time we need to build the AllSS target list and save it
        upAllSS = ultrasat.planner.uplanner('AstPlanner','YS','Type','AllSS','ExtragalDitherLeg',DitherLeg,...
            'Load','~/matlab/data/ULTRASAT/alss_uniq_targ.mat');
        
        upAllSS.StartTime = '2028-07-01'; 
        upAllSS.StartTime = upAllSS.StartTime + hours(12);  % 12 hr are added in order to alleviate visibility constraints 
        upAllSS.EndTime   = upAllSS.StartTime + calmonths(6) - days(1);        
        
        upAllSS.ExtragalMinIntervals   = [1 3 9]; % [1 2 4] [1 3 9]
        upAllSS.BufferEarthDist        = 0.5;
        upAllSS.BufferSunDist          = 0.5;
        upAllSS.BufferMoonDist         = 0.5;
        upAllSS.DailyWindowMaxDuration = hours(5.5);
        
        upAllSS.EmptyDay               = false;
        
        %%%%
        
        upAllSS.EndTime                = upAllSS.StartTime + days(7);
        upAllSS.DailyWindowMaxDuration = hours(24);
        upAllSS.BufferEarthDist        = 3.0;
        upAllSS.ExtragalMinIntervals   = [0 0 0];
        upAllSS.BufferEarthDist        = 8.0; % 6.0;
%                     % currently distributeAllSS cannot work with reduced
%                     % number of extragalactic visits, need to be improved 
% %                     upAllSS.HighLatVisits  = 1;    % only 1 (or 2?) extragal points for the first week?             
        
        upAllSS.buildAllSS('AllowPartial',true,'Verbose',true,...                                                                              
                           'MergeSameTargets',false,'AverageSlew',60);
        % TODO: make a 2-stage plan: 1 dedicated week + all the
        % rest in the rest 180-7 days in 5.5 hr windows (along with the HCS) 
        % note the "Incomplete" variable in buildAllSS
        
        upAllSS.StartTime = upAllSS.EndTime;
        upAllSS.EndTime   = upAllSS.StartTime + calmonths(6) - days(8);
        upAllSS.DailyWindowMaxDuration = hours(5.5);
        upAllSS.ExtragalMinIntervals   = [1 3 9];
        upAllSS.buildAllSS('AllowPartial',true,'Verbose',true,...                                                                              
                           'MergeSameTargets',false,'AverageSlew',60);

        if Args.Verbose
            fprintf('completed\n');
            fprintf('-------------------------\n');
        end
    end
    
    %
    Result=true;
    if Args.Verbose
        fprintf('Unit Test completed succefully\n');
    end
end
