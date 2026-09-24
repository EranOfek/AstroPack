function debug_TargetPageExporter()
    % debug_TargetPageExporter - Test script for TargetWebPageExporter
    %
    % This script creates sample target data and test figures to demonstrate
    % and verify the functionality of TargetWebPageExporter.
    
    % Create sample target data
    targetData = createSampleTargetData();
    
    % Get current file directory and create export folder under it
    currentDir = fileparts(mfilename('fullpath'));
    baseFolder = fullfile(currentDir, 'export_output');
    if ~exist(baseFolder, 'dir')
        mkdir(baseFolder);
    end
    
    % Template file path relative to current directory
    templateFile = fullfile(currentDir, 'templates', 'target_template_01.html');
    
    targetId = '123';

    % Create exporter with debug mode enabled
    exporter = ultrasat.planner.webpage.TargetWebPageExporter(...
        targetId, targetData, baseFolder, templateFile, true);
    
    % Create and add test figures
    addTestFigures(exporter);
    
    % Generate and save HTML
    exporter.generateHtmlFromTemplate();
    exporter.saveHtml();
    
    % Preview in browser
    exporter.previewInBrowser();
    
    fprintf('Debug output saved to: %s\n', baseFolder);
end


function targetData = createSampleTargetData()
    % Create a structure with sample target data for testing
    
    targetData = struct();
    
    % Editable Parameters
    targetData.PlanTargetIndex = 42;
    targetData.ExposureTime = 300;
    targetData.EpochsPerVisit = 2;
    targetData.Tiles = [true, false, true, false];
    
    % Unique Target Parameters
    targetData.RA = 123.456;
    targetData.Dec = -45.678;
    targetData.UniqueTargetIndex = 7;
    targetData.Group = 1;
    targetData.ExpectedRoll = 30.5;
    
    % Time Parameters
    targetData.StartTime = datenum('2024-03-15 10:00:00');
    targetData.EndTime = datenum('2024-03-15 11:00:00');
    targetData.MJDStart = 60103.41667;
    targetData.MJDEnd = 60103.45833;
    targetData.TotalDuration = 3600;
    targetData.SlewTimeBefore = 120;
    
    % Distance Parameters
    targetData.MoonDist = 75.3;
    targetData.SunDist = 120.5;
    targetData.EarthDist = 45.2;
    targetData.NoComm = false;
    targetData.HardObs = true;
    
    % Other Parameters
    targetData.Zody = 22.1;
    targetData.LimMag = 18.5;
    targetData.OverlapTargets = 2;
end


function addTestFigures(exporter)
    % Create and add test figures to the exporter
    
    % Create sky map test figure
    fig1 = figure('Visible', 'off');
    ax1 = axes(fig1);
    scatter(ax1, rand(10,1), rand(10,1), 50, 'filled');
    title(ax1, 'Sample Sky Map');
    xlabel(ax1, 'RA');
    ylabel(ax1, 'Dec');
    grid(ax1, 'on');
    exporter.addSkyMap(fig1);
    
    % Create visibility graph test figure
    fig2 = figure('Visible', 'off');
    ax2 = axes(fig2);
    x = linspace(0, 24, 100);
    y = cos(x/24*2*pi) + 1;
    plot(ax2, x, y, 'LineWidth', 2);
    title(ax2, 'Sample Visibility Graph');
    xlabel(ax2, 'Time (hours)');
    ylabel(ax2, 'Visibility');
    grid(ax2, 'on');
    exporter.addVisibilityGraph(fig2);
    
    % Clean up
    close(fig1);
    close(fig2);
end 
