function [Result,H]=stopButton(Args)
    % Create a stop button 
    % Input  : *...,key,val,...
    %            'Msg' - Window message.
    %                   Default is 'Terminate the process'.
    %            'Title' - Windiow title. Default is 'Stop'.
    %            'Text'  - Button text. Default is 'Abort'.
    %            'Pos'   - Position of window [x,y]. Default is [].
    % Output : - A function handle with the stop function.
    %          - Function handle of message box.
    % Author : Eran Ofek (Mar 2023)
    % Example: StopGUI=tools.gui.stopButton(Arg'Msg','Stop the process');
    %          % next you can check
    %          if StopGUI(), % do something; end

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
    Result  = @() stopFun(H) ; % false if message box still exists

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

