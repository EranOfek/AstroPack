
function debug_PlanPageExporter()
    debug_PlanWebPageExporterWithImages();
    debug_PlanWebPageExporterWithImagesAndTables();
end


function debug_PlanWebPageExporterWithImages()

    planId = '42';
    
    % Get current file directory and create export folder under it
    currentDir = fileparts(mfilename('fullpath'));
    baseFolder = fullfile(currentDir, 'export_output');
    if ~exist(baseFolder, 'dir')
        mkdir(baseFolder);
    end
    
    % Template file path relative to current directory
    templateFile = fullfile(currentDir, 'templates', 'plan_template_01.html');

    % Create exporter instance with plan ID, output folder, and HTML template
    exporter = ultrasat.planner.webpage.PlanWebPageExporter(planId, baseFolder, templateFile);

    % Add your six key figures
    for i = 1:6
        f = figure('Visible', 'off');
        plot(rand(1,10));
        title(sprintf("Figure %d", i));
        % Add image tag based on template placeholders
        switch i
            case 1
                tag = 'img_plan_params';
            case 2 
                tag = 'img_unique_targets';
            case 3
                tag = 'img_plan';
            case 4
                tag = 'img_approved_targets';
            case 5
                tag = 'img_skymap';
            case 6
                tag = 'img_graphs';
        end
        exporter = exporter.addFigureAsImage(f, tag);
        close(f);
    end

    % Generate HTML from template
    exporter = exporter.generateHtmlFromTemplate();

    % Save
    exporter.saveHtml();

    % Store to DB
    %exporter.zipFolder();    
    %zipBytes = exporter.getZipAsBytes();

    % Preview locally (optional)
    exporter.previewInBrowser();
end



function debug_PlanWebPageExporterWithImagesAndTables()
    planId = '43';
    
    % Get current file directory and create export folder under it
    currentDir = fileparts(mfilename('fullpath'));
    baseFolder = fullfile(currentDir, 'export_output');
    if ~exist(baseFolder, 'dir')
        mkdir(baseFolder);
    end
    
    % Template file path relative to current directory
    templateFile = fullfile(currentDir, 'templates', 'plan_template_02.html');

    % Create exporter instance with plan ID, output folder, and HTML template
    exporter = ultrasat.planner.webpage.PlanWebPageExporter(planId, baseFolder, templateFile, true);

    % Add some example figures
    f1 = figure('Visible', 'off');
    plot(rand(1,10));
    title('Plan Parameters');
    exporter = exporter.addFigureAsImage(f1, 'img_plan_params');
    close(f1);

    f2 = figure('Visible', 'off'); 
    plot(rand(1,10));
    title('Unique Targets');
    exporter = exporter.addFigureAsImage(f2, 'img_unique_targets');
    close(f2);

    % Add some example tables
    targetsTable = table(...
        [1;2;3], ...
        ["Target A";"Target B";"Target C"], ...
        [10.5;20.3;15.7], ...
        'VariableNames', {'ID', 'Name', 'Magnitude'});
    exporter = exporter.addTable(targetsTable, 'table_targets');

    paramsTable = table(...
        ["Start Time";"End Time";"Total Duration"], ...
        ["2024-01-01 00:00:00";"2024-01-02 00:00:00";"24 hours"], ...
        'VariableNames', {'Parameter', 'Value'});
    exporter = exporter.addTable(paramsTable, 'table_parameters');

    % Generate HTML from template
    exporter = exporter.generateHtmlFromTemplate();

    % Save and zip it
    exporter.saveHtml();

    % Store to DB
    %exporter.zipFolder();   
    %zipBytes = exporter.getZipAsBytes();

    % Preview locally (optional)
    exporter.previewInBrowser();
end
