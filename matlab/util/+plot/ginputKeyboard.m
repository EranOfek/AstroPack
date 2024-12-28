function [position, key] = ginputKeyboard(Ax)
    % Click on figure using mouse and keyboard, and get position and keyboard value 
    % Input  : - An optional axis handle. If empty, use current figure.
    % Output : - [X, Y] position.
    %          - Keyboard key. NaN if used the mouse.
    % Author : Eran Ofek + ChatGPT (2024 Dec) 
    % Example: [P, K] = plot.ginputKeyboard

    arguments
        Ax          = [];
    end

    if isempty(Ax)
        % Use the provided axes or figure handle
        fig = gcf; % Get the current figure if axes are provided
        Ax  = gca; %axes(fig);
    else
        fig = Ax.Parent;
        %fig = figure;
        %ax = axes(fig);
        
    end
    
    % Initialize outputs
    key      = NaN;      % Default to NaN if no keyboard input
    position = NaN;      % Default to NaN if no mouse click
    
    % Set up a variable to track the current mouse position
    currentMousePos = NaN(1,2);  % Variable to store current mouse position
    
    % Set callbacks for key press and mouse click
    set(fig, 'WindowKeyPressFcn', @keyPressCallback);
    set(fig, 'WindowButtonDownFcn', @mouseClickCallback);
    set(fig, 'WindowButtonMotionFcn', @mouseMotionCallback);  % Track mouse movement
    
    % Wait for user input (either mouse or keyboard)
    uiwait(fig);
    
    % Callback function for mouse click
    function mouseClickCallback(~, ~)
        % Get the mouse position (in axes coordinates)
        positionMatrix = get(Ax, 'CurrentPoint');  % Get position in the current axes
        position = positionMatrix(1, 1:2);  % Extract X and Y
        key = NaN;  % Set key to NaN since it was a mouse click
        
        % Close the figure to exit
        uiresume(fig);
    end
    
    % Callback function for keyboard input
    function keyPressCallback(~, event)
        % Capture the key press event
        key = event.Key;  % The key pressed on the keyboard
        
        % At the time of the key press, use the current tracked mouse position
        position = currentMousePos;  % The mouse position at the moment of key press
        
        % Close the figure to exit
        uiresume(fig);
    end
    
    % Callback function to update mouse position
    function mouseMotionCallback(~, ~)
        % Get the current mouse position in axes coordinates
        positionMatrix = get(Ax, 'CurrentPoint');  % Get position in the current axes
        currentMousePos = positionMatrix(1, 1:2);  % Store current mouse position
    end
    
end
