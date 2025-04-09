%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.planner.PlanWebPageExporter.m
% Author: Chen Tishler
% Created: 09/04/2025
% Updated: 09/04/2025
%
%==========================================================================
% https://chatgpt.com/c/67f645e1-0d68-8012-88b2-a74bff4d8e0e

classdef WebPageExporter < handle
    % WebPageExporter - Export structured HTML + images for observation plan
    %
    % This class supports the generation of a structured webpage (index.html)
    % that summarizes an observation plan visually. It supports:
    %   - Adding MATLAB figures to be saved as PNG images
    %   - Adding MATLAB tables to be converted to HTML tables
    %   - Using a predefined HTML template with placeholders
    %   - Generating index.html using the template
    %   - Creating a ZIP file with all required files
    %   - Returning the ZIP as a byte array for database storage
    %
    % Placeholders in the HTML template must include:
    %   For images: {{img_plan_params}}, {{img_unique_targets}}, etc.
    %   For tables: {{table_targets}}, {{table_parameters}}, etc.
    
    properties
        Prefix              % Prefix plan/target/etc
        Id                  % Unique numeric ID for the plan
        OutputFolder        % Path to folder with HTML + images
        ZipPath             % Path to ZIP file of OutputFolder
        Images              % Map of image tags to filenames
        Tables              % Map of table tags to HTML content
        ImageCount          % Counter for internal image numbering
        HtmlText            % Final generated HTML text
        TemplatePath        % Path to the HTML template file
        TemplateText        % Loaded template text
        DebugMode           % Enable debug logging
    end
    
    methods
        function obj = WebPageExporter(Prefix, Id, outputBaseFolder, templatePath, debugMode)
            % Constructor for WebPageExporter
            %
            % Parameters:
            %   Id - Unique text ID for the plan/target
            %   outputBaseFolder - Base folder for output files
            %   templatePath - Path to HTML template file
            %   debugMode - Optional, enable debug logging (default: false)
            arguments
                Prefix
                Id
                outputBaseFolder
                templatePath
                debugMode logical = false
            end
            
            obj.Prefix = Prefix;
            obj.Id = Id;
            obj.OutputFolder = fullfile(outputBaseFolder, sprintf("%s_%s_web", Prefix, Id));
            obj.ZipPath = [obj.OutputFolder, '.zip'];
            obj.Images = containers.Map();
            obj.Tables = containers.Map();
            obj.ImageCount = 0;
            obj.TemplatePath = templatePath;
            obj.DebugMode = debugMode;

            if ~exist(obj.OutputFolder, 'dir')
                mkdir(obj.OutputFolder);
            end

            % Load template once
            fid = fopen(templatePath, 'r');
            raw = fread(fid, inf, 'char');  
            obj.TemplateText = char(raw');  % ' added to avoid Cursor problem with comments after single quote
            fclose(fid);
            
            if obj.DebugMode
                obj.debugLog('Initialized WebPageExporter for Id %s', Id);
            end
        end

        function obj = addFigureAsImage(obj, fig, imageTag)
            % Add a MATLAB figure and export it to PNG
            %
            % Parameters:
            %   fig - MATLAB figure handle
            %   imageTag - Tag in template (e.g., 'img_plan_params')
            arguments
                obj
                fig
                imageTag char
            end
            
            obj.ImageCount = obj.ImageCount + 1;
            fileName = sprintf("img_%d.png", obj.ImageCount);
            fullPath = fullfile(obj.OutputFolder, fileName);
            
            if obj.DebugMode
                obj.debugLog('Exporting figure to %s with tag %s', fullPath, imageTag);
            end
            
            exportgraphics(fig, fullPath, 'BackgroundColor', 'white');
            obj.Images(imageTag) = fileName;
        end

        function obj = addTable(obj, table, tableTag)
            % Add a MATLAB table and convert it to HTML
            %
            % Parameters:
            %   table - MATLAB table object
            %   tableTag - Tag in template (e.g., 'table_targets')
            arguments
                obj
                table
                tableTag char
            end
            
            if obj.DebugMode
                obj.debugLog('Converting table to HTML with tag %s', tableTag);
            end
            
            % Convert table to HTML
            htmlTable = obj.tableToHtml(table);
            obj.Tables(tableTag) = htmlTable;
        end

        function html = tableToHtml(obj, table)
            % Convert MATLAB table to HTML table
            %
            % Parameters:
            %   table - MATLAB table object
            %
            % Returns:
            %   html - HTML string representing the table
            html = '<table class="matlab-table">';
            
            % Add header
            html = [html, '<thead><tr>'];
            for i = 1:width(table)
                html = [html, sprintf('<th>%s</th>', table.Properties.VariableNames{i})];
            end
            html = [html, '</tr></thead>'];
            
            % Add data
            html = [html, '<tbody>'];
            for i = 1:height(table)
                html = [html, '<tr>'];
                for j = 1:width(table)
                    value = table{i,j};
                    if isnumeric(value)
                        value = num2str(value);
                    elseif isdatetime(value)
                        value = datestr(value);
                    end
                    html = [html, sprintf('<td>%s</td>', value)];
                end
                html = [html, '</tr>'];
            end
            html = [html, '</tbody></table>'];
        end

        function obj = generateHtmlFromTemplate(obj)
            % Replace placeholders in template using current PlanId, images, and tables
            
            html = obj.TemplateText;
            
            % Replace plan ID
            html = strrep(html, '{{plan_id}}', obj.Id);
            
            % Replace image placeholders
            imageTags = keys(obj.Images);
            for i = 1:length(imageTags)
                tag = imageTags{i};
                html = strrep(html, ['{{' tag '}}'], obj.Images(tag));
            end
            
            % Replace table placeholders
            tableTags = keys(obj.Tables);
            for i = 1:length(tableTags)
                tag = tableTags{i};
                html = strrep(html, ['{{' tag '}}'], obj.Tables(tag));
            end
            
            % Replace debug information
            html = strrep(html, '{{generation_time}}', datestr(now, 'yyyy-mm-dd HH:MM:SS'));
            html = strrep(html, '{{debug_image_count}}', num2str(length(imageTags)));
            html = strrep(html, '{{debug_table_count}}', num2str(length(tableTags)));
            html = strrep(html, '{{debug_output_folder}}', obj.OutputFolder);
            
            obj.HtmlText = html;
            
            if obj.DebugMode
                obj.debugLog('Generated HTML with %d images and %d tables', ...
                    length(imageTags), length(tableTags));
            end
        end

        function debugLog(obj, format, varargin)
            % Log debug message if debug mode is enabled
            %
            % Parameters:
            %   format - Format string for message
            %   varargin - Arguments for format string
            if obj.DebugMode
                fprintf('[PlanWebPageExporter] %s\n', sprintf(format, varargin{:}));
            end
        end

        function saveHtml(obj)
            % Save the generated HTML to index.html in the output folder
            htmlPath = fullfile(obj.OutputFolder, 'index.html');
            fid = fopen(htmlPath, 'w');
            fwrite(fid, obj.HtmlText);
            fclose(fid);
        end

        function zipFolder(obj)
            % Zip the output folder contents into a single .zip file
            zip(obj.ZipPath, obj.OutputFolder);
        end

        function bytes = getZipAsBytes(obj)
            % Return ZIP file as a byte array for storage
            fid = fopen(obj.ZipPath, 'r');
            bytes = fread(fid, '*uint8');
            fclose(fid);
        end

        function previewInBrowser(obj)           
            % Optional helper: unzip and open the index.html in system browser
            web(fullfile(obj.OutputFolder, 'index.html'), '-browser');
            return;

            unzipFolder = [obj.OutputFolder, '_unzip'];
            if ~exist(unzipFolder, 'dir')
                mkdir(unzipFolder);
            end
            unzip(obj.ZipPath, unzipFolder);
            web(fullfile(unzipFolder, 'index.html'), '-browser');
        end
    end
end
