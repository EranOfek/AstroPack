%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.planner.WebPageExporter.m
% Author: Chen Tishler
% Created: 09/04/2025
% Updated: 10/04/2025
%
%==========================================================================
% https://chatgpt.com/c/67f645e1-0d68-8012-88b2-a74bff4d8e0e
%
% Still POC, continue to develop when required, 
% see https://chatgpt.com/c/67f645e1-0d68-8012-88b2-a74bff4d8e0e
% Options to add: Persistent tags, Notes, Json data (single or multiple objects)
%

classdef WebPageExporter < handle
    % WebPageExporter - Export structured HTML + images from templates
    %
    % This class supports the generation of a structured webpage (index.html)
    % that supports:
    %   - Generating index.html using the template    
    %   - Using a predefined HTML template with placeholders    
    %   - Adding MATLAB figures to be saved as PNG images
    %   - Adding MATLAB tables to be converted to HTML tables     
    %   - Creating a ZIP file with all required files
    %   - Returning the ZIP as a byte array for database storage
    %
    % Placeholders in the HTML template may include (examples):
    %   For images: {{img_plan_params}}, {{img_unique_targets}}, etc.
    %   For tables: {{table_targets}}, {{table_parameters}}, etc.
    
    properties
        Prefix              % Prefix for the entity (plan/target/etc)
        Id                  % Unique ID for the entity
        OutputFolder        % Path to folder with HTML + images
        ZipPath             % Path to ZIP file of OutputFolder
        Values              % Map of values by tags
        Images              % Map of image tags to filenames
        Tables              % Map of table tags to HTML content
        JsonData            % struct
        CommentLines        % cellarray of strings
        ImageCount          % Counter for internal image numbering
        HtmlText            % Final generated HTML text
        TemplatePath        % Path to the HTML template file
        TemplateText        % Loaded template text
        UploaderUrl         % URL of the uploader (local FastAPI service that uploads the folder to AWS S3)
        DebugMode           % Enable debug logging
    end
    
    methods
        function obj = WebPageExporter(Prefix, Id, outputBaseFolder, templatePath, debugMode)
            % Constructor for WebPageExporter
            %
            % Parameters:
            %   Id - Unique text ID for the entity (plan/target/etc)
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
            obj.Values = containers.Map();
            obj.JsonData = struct();
            obj.CommentLines = {};
            obj.ImageCount = 0;
            obj.TemplatePath = templatePath;
            obj.DebugMode = debugMode;
            obj.UploaderUrl = 'localhost:8229/upload_to_s3';

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


        function obj = addFigureAsImage(obj, fig, imageTag, useTagAsFilename)
            % Add a MATLAB figure and export it to PNG
            %
            % Parameters:
            %   fig - MATLAB figure handle
            %   imageTag - Tag in template (e.g., 'img_params')
            %   useTagAsFilename - Optional, use tag as filename instead of numerical index
            arguments
                obj
                fig
                imageTag char
                useTagAsFilename logical = true
            end
            
            if useTagAsFilename
                % Use the template tag as the filename
                cleanTag = strrep(strrep(imageTag, '{{', ''), '}}', '');
                fileName = sprintf("%s.png", cleanTag);
            else
                % Use numerical index for cases where multiple images map to same tag
                obj.ImageCount = obj.ImageCount + 1;
                fileName = sprintf("img_%d.png", obj.ImageCount);
            end
            
            fullPath = fullfile(obj.OutputFolder, fileName);
            
            if obj.DebugMode
                obj.debugLog('Exporting figure to %s with tag %s', fullPath, imageTag);
            end
            
            exportgraphics(fig, fullPath, 'BackgroundColor', 'white');
            obj.Images(imageTag) = fileName;
        end


        function obj = addValue(obj, valueTag, value)
            % Add a value to the Values map
            %
            % Parameters:
            %   valueTag - Tag in template (e.g., 'value_targets')
            %   value - Value to add
            arguments
                obj
                valueTag char
                value
            end
            obj.Values(valueTag) = value;
        end


        function obj = setJsonData(obj, jsonData)
            % Add a JSON block to the JsonBlocks map
            %
            % Parameters:
            %   jsonData - JSON block to add   
            arguments
                obj
                jsonData struct
            end
            obj.JsonData = jsonData;
        end


        function obj = setCommentLines(obj, commentLines)
            % Set the CommentLines array
            %
            % Parameters:
            %   commentLines - Comment lines to add    
            obj.CommentLines = commentLines;
        end


        function obj = addCommentLine(obj, commentLine)
            % Add a comment line to the CommentLines array
            %
            % Parameters:
            %   commentLine - Comment line to add    
            arguments
                obj
                tag char
                value
            end
            obj.CommentLines{end+1} = commentLine;
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
            
            % Create mapping of keys to values
            keys = obj.Values;
            keys('prefix') = obj.Prefix;
            keys('id') = obj.Id;
            keys('generation_time') = datestr(datetime('now','TimeZone','UTC'), 'yyyy-mm-dd HH:MM:SS');
            keys('debug_output_folder') = obj.OutputFolder;            
            keys('debug_image_count') = num2str(obj.Images.Count);  
            keys('debug_table_count') = num2str(obj.Tables.Count);  
            
            % Replace all mapped keys
            keyList = keys.keys;
            for i = 1:length(keyList)
                key = keyList{i};
                html = strrep(html, ['{{' key '}}'], keys(key));
            end
            
            % Replace image placeholders
            imageTags = obj.Images.keys;
            for i = 1:length(imageTags)
                tag = imageTags{i};
                html = strrep(html, ['{{' tag '}}'], obj.Images(tag));
            end
            
            % Replace table placeholders
            tableTags = obj.Tables.keys;
            for i = 1:length(tableTags)
                tag = tableTags{i};
                html = strrep(html, ['{{' tag '}}'], obj.Tables(tag));
            end
            
            % Replace JSON block placeholders
            jsonData = jsonencode(obj.JsonData);
            html = strrep(html, '{{json_data}}', jsonData);
            
            % Replace comment lines
            commentLines = strjoin(obj.CommentLines, '\n');
            html = strrep(html, '{{comment_lines}}', commentLines);
            
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
                fprintf('[WebPageExporter] %s\n', sprintf(format, varargin{:}));
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


        function result = copyFolder(obj, folderPath)
            % Copy the output folder contents to a new folder
            copyfile(obj.OutputFolder, folderPath);
            result = true;
        end


        function result = upload_to_s3(obj, s3Key)
            % Upload the output folder contents to a remote server
            %
            % Parameters:
            %   s3Bucket - S3 bucket name
            %   s3Key - S3 key name
            %
            % Example:
            %   upload(obj, 'my-bucket', 'my-key');
            %
            %   This will upload the contents of the output folder to the S3 bucket
            %   with the key 'my-key'.
            %

            % Initialize API client
            client = ultrasat.api.ClientBase('BaseUrl', obj.UploaderUrl);
            params = struct();
            params.folder = obj.OutputFolder;            
            params.s3_key = s3Key;
            response = client.postRequest('/upload_folder_to_s3/', params);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
            result = response.ok;
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
