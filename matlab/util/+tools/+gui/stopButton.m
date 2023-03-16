function Result=stopButton(Args)
    % Create stop button 
    % Input  : *...,key,val,...
    %            'Msg' - Window message.
    %                   Default is 'Terminate the process'.
    %            'Title' - Windiow title. Default is 'Stop'.
    %            'Text'  - Button text. Default is 'Abort'.
    %            'Pos'   - Position of window [x,y]. Default is [].
    % Author
    % Example: Result=tools.gui.stopButton(Arg'Msg','Stop the process');

    arguments
        Args.Msg     = 'Terminate the process';   % text
        Args.Title   = 'Stop';    % figure title
        Args.Text    = 'Abort';   % button text
        Args.Pos     = []; %[100 100];
    end

    H = msgbox(Args.Msg);
    if ~isempty(Args.Pos)
        H.Position(1:2) = Args.Pos;
    end
    H.Name          = Args.Title;
    Hc = H.Children;
    Hc(2).Children.String = Args.Msg;
    Hc(1).String          = Args.Text;

    % create the two anonymous functions
    Result.StopFun  = @() stopFun(H) ; % false if message box still exists
    %Result.ClearFun = @() clearfun(H) ; % delete message box

end

function Ans = stopFun(H)
    drawnow;
    Ans = ~ishandle(H) ; % true if stop
    if Ans
        delete(H);
    end
    
end

function clearfun(H)
    % clear the message box if it still exists
    if ishandle(H)
        delete(H);
    end
end

