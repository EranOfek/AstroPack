function [Res,FigH,Data]=getInteractive(Ha, Type)
    % Get clicked positions (mouse, key, rect) interactively.
    % Input  : - Handle for current axes in which the data points are ploted.
    %            If empty matrix, then use gca. Default is [].
    %          - Type of key to wait for:
    %            'key'   - wait for a keyboard click on the plot (default).
    %            'mouse' - wait for a single mouse click on the plot.
    %            'rect'  - wait for a mouse selection of a rectanguar region.
    %            'mousem'- wait for a multiple left mouse clicks, and use
    %                      right click to abort.
    %          - Waitfor action {'y'|'n'}. Will return only after the user
    %            clicked a key/mouse. Default is 'y'.
    % Output : - Result structure containing the following fields.
    %            .Key    - Keyborad key entered.
    %            .Pos    - Mouse position [X,Y] or rectangule position
    %                      [xmin ymin width height].
    %            .MB     - Mouse botton (1-left; 2-middle; 3-right).
    %                      Note that if Type is 'mousem', the last .MB value
    %                      will be always "3" corresponding to the exit click.
    %          - A structure cotaining:
    %            .Hf - Figure handle.
    %            .Ha - Axes handle
    %          - A structure containing the plot data (.X, .Y, .Z).
    % Author : Eran Ofek (Nov 2023)
    % Example: Res = plot.getInteractive(Ha, 'mouse');

    arguments
        Ha        = [];
        Type      = 'mouse';
        %WaitFor   = 'y';
    end

    % must define Res as global because of the WindowButtonDownFcn call
    global Res;

    if ischar(Ha)
        % This part is used internaly : WindowButtonDownFcn
        % get gcf from gca
        GCF = get(gca,'Parent');
    
        CC  = get(GCF,'CurrentCharacter');
    

        Res.Key = CC;
        Res.Pos = [];
        Res.MB  = [];

        set(gcf,'WindowButtonDownFcn','',...
 	                'KeyPressFcn','');
        %fprintf('Interactive mode terminated\n');
    else


        if ~isempty(Ha)
            axes(Ha);
        else
            Ha = gca;
        end
        Hf = get(Ha, 'Parent');
        FigH.Ha = Ha;
        FigH.Hf = Hf;
    
        switch lower(Type)
            case 'key'
                %fprintf('Interactive mode\n');
                set(Hf,'WindowButtonDownFcn','',...
                       'KeyPressFcn','[Res]=plot.selectInteractive(''key_press'');');
                
            case 'mouse'
                [X,Y,MB] = ginput(1);
                Res.Key  = [];
                Res.Pos  = [X,Y];
                Res.MB   = MB;
            case 'rect'
                Rect     = getrect(Hf);
                Res.Key  = [];
                Res.Pos  = Rect;  % [xmin ymin width height]
                Res.MB   = [];
            case 'mousem'
                [X,Y,MB] = ginput(1);
                Res.Key  = [];
                Res.Pos  = [X,Y];
                Res.MB   = MB;
                while (MB==1)
                    [X,Y,MB] = ginput(1);
                    Res.Pos  = [Res.Pos; [X,Y]];
                    Res.MB   = [Res.MB; MB];
                end
            otherwise
        end
    
        if nargout>2
            AxObjs = Hf.Children;
            DataObjs = AxObjs.Children
            Data.X   = DataObjs.XData;
            Data.Y   = DataObjs.YData;
            Data.Z   = DataObjs.ZData;
        end

        % switch lower(WaitFor)
        %    case 'y'
        %        waitfor(Hf, 'KeyPressFcn', '');
        %    otherwise
        %        % do nothing
        % end
    end


end
