% ZoomInset class
%   A class for create and control a zoom inset in existing Figure
%
% Examples: 
% plot(Wave, Flux, 'k-');
% Z=plot.ZoomInset
% Z.zoomGui
% Z.delete

classdef ZoomInset < handle
    properties
        ZoomFactorX = 10;
        ZoomFactorY = 10;
        AxisPos     = [0.3 0.3]; %[0.1 0.6 0.3 0.3];
        InsetAx
        Ax
        Fig
        OutWinState = 'on';  % off - don't show zoom inset when mouse is outsode window.
        Timer                % Timer object
        UseTimer = true;     % true - use periodic timer; false - use callback function
    end
    
    methods
        function obj = ZoomInset()
            % Constructor - Initialize zoom inset
            % Author : Eran Ofek + ChatGPT (Dec 2024)
            
            obj.Ax  = gca;
            obj.Fig = gcf;
            obj.createInset();
        end
        
        function delete(obj)
            % Destructor - Stop inset zoom
            
            if isvalid(obj.Timer)
                stop(obj.Timer);
                delete(obj.Timer);
            end
            stopMouseHoverInset();
        end
    
        
        function createInset(obj)
            % Create inset Axes
            
            if numel(obj.AxisPos)==2
                Hgca = gca;
                Pos = Hgca.Position;
                obj.AxisPos = [Pos(1)+0.01, Pos(2)+Pos(4)-obj.AxisPos(2)-0.01, obj.AxisPos];
            end
            
            obj.InsetAx = axes('Position', obj.AxisPos);
            obj.InsetAx.XColor = 'r';
            obj.InsetAx.YColor = 'r';
            obj.InsetAx.Box = 'on';
            obj.InsetAx.Tag = 'InsetAxes';
            
            % Set initial view for inset (empty initially)
            obj.InsetAx.XLim = obj.Ax.XLim;
            obj.InsetAx.YLim = obj.Ax.YLim;
            obj.InsetAx.Visible = obj.OutWinState;
            
            if obj.UseTimer
                % Start periodic timer
                obj.Timer = timer('ExecutionMode', 'fixedRate', ...
                          'Period', 0.1, ... % Adjust period as needed
                          'TimerFcn', @(~, ~) obj.updateInset());
                start(obj.Timer);
            else
                % Set callback for mouse movement
                set(obj.Fig, 'WindowButtonMotionFcn', @(~, ~) obj.updateInset());
            end
        end
        
        function updateInset(obj)
            % Get current point in Axes
            currPoint = get(obj.Ax, 'CurrentPoint');
            x = currPoint(1, 1);
            y = currPoint(1, 2);

            % Check if point is within Axis limits
            if x >= obj.Ax.XLim(1) && x <= obj.Ax.XLim(2) && y >= obj.Ax.YLim(1) && y <= obj.Ax.YLim(2)
                % Update inset view to zoom around current point
                obj.InsetAx.XLim = [x - (obj.Ax.XLim(2) - obj.Ax.XLim(1)) / (2 * obj.ZoomFactorX), ...
                                    x + (obj.Ax.XLim(2) - obj.Ax.XLim(1)) / (2 * obj.ZoomFactorX)];
                obj.InsetAx.YLim = [y - (obj.Ax.YLim(2) - obj.Ax.YLim(1)) / (2 * obj.ZoomFactorY), ...
                                    y + (obj.Ax.YLim(2) - obj.Ax.YLim(1)) / (2 * obj.ZoomFactorY)];
                                
                obj.InsetAx.Visible = 'on';
                
                % Copy data from main Axes to inset
                copyobj(obj.Ax.Children, obj.InsetAx);
            else
                % Hide inset if mouse is outside Axis
                obj.InsetAx.Visible = obj.OutWinState;
            end
        end
        
        function zoomGui(obj)
            % Create GUI to adjust zoom factors
            
            d = dialog('Position', [300 300 250 150], 'Name', 'Zoom Settings');
            
            uicontrol('Parent', d, 'Style', 'text', 'Position', [20 100 100 20], 'String', 'ZoomFactorX:');
            xBox = uicontrol('Parent', d, 'Style', 'edit', 'Position', [130 100 70 20], 'String', num2str(obj.ZoomFactorX));
            
            uicontrol('Parent', d, 'Style', 'text', 'Position', [20 60 100 20], 'String', 'ZoomFactorY:');
            yBox = uicontrol('Parent', d, 'Style', 'edit', 'Position', [130 60 70 20], 'String', num2str(obj.ZoomFactorY));
            
            uicontrol('Parent', d, 'Position', [85 20 70 25], 'String', 'Apply', ...
                'Callback', @(~, ~) applyZoomSettings());
            
            function applyZoomSettings()
                obj.ZoomFactorX = str2double(xBox.String);
                obj.ZoomFactorY = str2double(yBox.String);
                delete(d);
                %obj.zoomGui;
            end
        end
        
        function stopMouseHoverInset()
            % Stop mouse hover effect by clearing callbacks and deleting inset
            Fig = gcf;
            InsetAx = findall(Fig, 'Type', 'Axes', 'Tag', 'InsetAxes');
            if ~isempty(InsetAx)
                delete(InsetAx);
            end
            set(Fig, 'WindowButtonMotionFcn', '');
        end
        
    end
    
    
    
end


