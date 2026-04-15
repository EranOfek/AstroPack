function debugLcs()

    fprintf('Start testing LCS plan...');
    
    % Example for creating LCS survey with buildLCS1:
    upLCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','LCS');
    upLCS.StartTime = '2029-01-01 00:00:00';
    upLCS.EndTime = upLCS.StartTime+caldays(420);
    upLCS.DailyWindowStartTime = duration('00:00:00');
    
    LCS_grid = readtable(fullfile(upLCS.BaseDataDir,'LCS_nonoverlapping_grid_surveys.csv'));
    upLCS.addUniqTargets(LCS_grid.RA,LCS_grid.Dec,'Name',num2cell(LCS_grid.Field));

    % This is the new version of LCS (2026)
    upLCS.buildLCS1;      

    fprintf('Done testing LCS plan...');
end

