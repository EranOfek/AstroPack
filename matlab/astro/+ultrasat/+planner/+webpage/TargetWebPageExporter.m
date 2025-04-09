classdef TargetWebPageExporter < ultrasat.planner.webpage.WebPageExporter
    % TargetWebPageExporter - Export structured HTML + images for target details
    %
    % This class extends PlanWebPageExporter to provide target-specific
    % functionality for generating detailed target web pages.
    
    properties
        targetId          % Name of the target
        TargetData         % Structure containing all target parameters
    end
    
    methods
        function obj = TargetWebPageExporter(targetId, targetData, outputBaseFolder, templatePath, debugMode)
            % Constructor for TargetWebPageExporter
            %
            % Parameters:
            %   targetId - Name of the target
            %   targetData - Structure containing target parameters
            %   outputBaseFolder - Base folder for output files
            %   templatePath - Path to HTML template file
            %   debugMode - Optional, enable debug logging (default: false)
            arguments
                targetId char
                targetData struct
                outputBaseFolder char
                templatePath char
                debugMode logical = false
            end
            
            % Call parent constructor with target name as ID
            obj@ultrasat.planner.webpage.WebPageExporter('target', targetId, outputBaseFolder, templatePath, debugMode);
            
            obj.targetId = targetId;
            obj.TargetData = targetData;
            
            if obj.DebugMode
                obj.debugLog('Initialized TargetWebPageExporter for target %s', targetId);
            end
        end
        
        function obj = generateHtmlFromTemplate(obj)
            % Override parent method to handle target-specific template replacements
            
            html = obj.TemplateText;
            
            % Replace target information
            html = strrep(html, '{{target_name}}', obj.targetId);
            html = strrep(html, '{{plan_target_index}}', num2str(obj.TargetData.PlanTargetIndex));
            html = strrep(html, '{{exposure_time}}', num2str(obj.TargetData.ExposureTime));
            html = strrep(html, '{{epochs_per_visit}}', num2str(obj.TargetData.EpochsPerVisit));
            
            % Handle tiles checkboxes
            tiles = obj.TargetData.Tiles;
            html = strrep(html, '{{tile1_checked}}', obj.getCheckedAttribute(tiles(1)));
            html = strrep(html, '{{tile2_checked}}', obj.getCheckedAttribute(tiles(2)));
            html = strrep(html, '{{tile3_checked}}', obj.getCheckedAttribute(tiles(3)));
            html = strrep(html, '{{tile4_checked}}', obj.getCheckedAttribute(tiles(4)));
            
            % Replace unique target parameters
            html = strrep(html, '{{ra}}', num2str(obj.TargetData.RA));
            html = strrep(html, '{{dec}}', num2str(obj.TargetData.Dec));
            html = strrep(html, '{{unique_target_index}}', num2str(obj.TargetData.UniqueTargetIndex));
            html = strrep(html, '{{group}}', num2str(obj.TargetData.Group));
            html = strrep(html, '{{expected_roll}}', num2str(obj.TargetData.ExpectedRoll));
            
            % Replace time parameters
            html = strrep(html, '{{start_time}}', datestr(obj.TargetData.StartTime));
            html = strrep(html, '{{end_time}}', datestr(obj.TargetData.EndTime));
            html = strrep(html, '{{mjd_start}}', num2str(obj.TargetData.MJDStart));
            html = strrep(html, '{{mjd_end}}', num2str(obj.TargetData.MJDEnd));
            html = strrep(html, '{{total_duration}}', num2str(obj.TargetData.TotalDuration));
            html = strrep(html, '{{slew_time_before}}', num2str(obj.TargetData.SlewTimeBefore));
            
            % Replace distance parameters
            html = strrep(html, '{{moon_dist}}', num2str(obj.TargetData.MoonDist));
            html = strrep(html, '{{sun_dist}}', num2str(obj.TargetData.SunDist));
            html = strrep(html, '{{earth_dist}}', num2str(obj.TargetData.EarthDist));
            html = strrep(html, '{{no_comm}}', num2str(obj.TargetData.NoComm));
            html = strrep(html, '{{hard_obs}}', num2str(obj.TargetData.HardObs));
            
            % Replace other parameters
            html = strrep(html, '{{zody}}', num2str(obj.TargetData.Zody));
            html = strrep(html, '{{lim_mag}}', num2str(obj.TargetData.LimMag));
            html = strrep(html, '{{overlap_targets}}', num2str(obj.TargetData.OverlapTargets));
            
            % Replace image placeholders
            imageTags = keys(obj.Images);
            for i = 1:length(imageTags)
                tag = imageTags{i};
                html = strrep(html, ['{{' tag '}}'], obj.Images(tag));
            end
            
            % Replace debug information
            html = strrep(html, '{{generation_time}}', datestr(now, 'yyyy-mm-dd HH:MM:SS'));
            html = strrep(html, '{{debug_image_count}}', num2str(length(imageTags)));
            html = strrep(html, '{{debug_output_folder}}', obj.OutputFolder);
            
            obj.HtmlText = html;
            
            if obj.DebugMode
                obj.debugLog('Generated HTML for target %s with %d images', ...
                    obj.targetId, length(imageTags));
            end
        end
        
        function checked = getCheckedAttribute(obj, isChecked)
            % Helper method to convert boolean to HTML checked attribute
            if isChecked
                checked = 'checked';
            else
                checked = '';
            end
        end
        
        function obj = addSkyMap(obj, fig)
            % Add sky map figure
            obj = obj.addFigureAsImage(fig, 'img_skymap');
        end
        
        function obj = addVisibilityGraph(obj, fig)
            % Add visibility graph figure
            obj = obj.addFigureAsImage(fig, 'img_visibility');
        end
    end
end 