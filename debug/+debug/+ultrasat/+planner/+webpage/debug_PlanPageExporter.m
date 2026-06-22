%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+webpage/debug_PlanPageExporter.m
% Author      : Chen Tishler
% Created     : 18/06/2026
% Updated     : 18/06/2026
% Description : Debug driver for debug_PlanPageExporter
%
% Run by      : debug.ultrasat.planner.webpage.debug_PlanPageExporter()
%==========================================================================

function debug_PlanPageExporter()
    % Run both PlanWebPageExporter scenarios: images-only and images+tables.

    debug_PlanWebPageExporterWithImages();
    debug_PlanWebPageExporterWithImagesAndTables();
end

% -------------------------------------------------------------------------

function debug_PlanWebPageExporterWithImages()
    % Exercise template_01 placeholders: six figure slots, values, notes, JSON, S3 upload.

    planId = '42';

    % --- Setup export folder and production template path ---
    currentDir = fileparts(mfilename('fullpath'));
    repoRoot = getenv('ASTROPACK_PATH');
    if isempty(repoRoot)
        error('ASTROPACK_PATH is not set');
    end
    webpageDir = fullfile(repoRoot, 'matlab', 'astro', '+ultrasat', '+planner', '+webpage');
    baseFolder = fullfile(currentDir, 'export_output');
    if ~exist(baseFolder, 'dir')
        mkdir(baseFolder);
    end

    templateFile = fullfile(webpageDir, 'templates', 'plan_template_01.html');

    exporter = ultrasat.planner.webpage.PlanWebPageExporter(planId, baseFolder, templateFile);

    % --- Add six template-mapped figure images ---
    for i = 1:6
        f = figure('Visible', 'off');
        plot(rand(1,10));
        title(sprintf("Figure %d", i));
        % Tags must match {{img_*}} placeholders in plan_template_01.html.
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

    % --- Scalar values, notes, JSON blocks, and persistent tags ---
    exporter = exporter.addValue('value_targets', '10');
    exporter = exporter.addValue('value_parameters', '20');

    exporter = exporter.addPersistentNote('This is a note');

    jsonBlock = struct('name', 'John', 'age', 30, 'city', 'New York');
    exporter = exporter.addJsonBlock('json_targets', jsonBlock);

    exporter = exporter.addPersistentTag('persistent_targets', '10');
    exporter = exporter.addPersistentTag('persistent_parameters', '20');

    % --- Generate, save, preview, and upload ---
    exporter = exporter.generateHtmlFromTemplate();

    exporter.saveHtml();

    % Store to DB (optional - requires zip pipeline)
    %exporter.zipFolder();
    %zipBytes = exporter.getZipAsBytes();

    exporter.previewInBrowser();

    exporter.upload_to_s3('ultrasat-planner-webpages', 'test');
end

% -------------------------------------------------------------------------

function debug_PlanWebPageExporterWithImagesAndTables()
    % Exercise template_02 with table placeholders and enableTables constructor flag.

    planId = '43';

    % --- Setup export folder and production template path ---
    currentDir = fileparts(mfilename('fullpath'));
    repoRoot = getenv('ASTROPACK_PATH');
    if isempty(repoRoot)
        error('ASTROPACK_PATH is not set');
    end
    webpageDir = fullfile(repoRoot, 'matlab', 'astro', '+ultrasat', '+planner', '+webpage');
    baseFolder = fullfile(currentDir, 'export_output');
    if ~exist(baseFolder, 'dir')
        mkdir(baseFolder);
    end

    templateFile = fullfile(webpageDir, 'templates', 'plan_template_02.html');

    % Fourth arg true -> exporter renders {{table_*}} placeholders as HTML tables.
    exporter = ultrasat.planner.webpage.PlanWebPageExporter(planId, baseFolder, templateFile, true);

    % --- Add two template-mapped figures ---
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

    % --- Add tables bound to template placeholders ---
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

    % --- Generate, save, and preview ---
    exporter = exporter.generateHtmlFromTemplate();

    exporter.saveHtml();

    % Store to DB (optional - requires zip pipeline)
    %exporter.zipFolder();
    %zipBytes = exporter.getZipAsBytes();

    exporter.previewInBrowser();
end
