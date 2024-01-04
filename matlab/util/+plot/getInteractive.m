function [Res,FigH,Data,Nearest]=getInteractive(Ha, Type, Args)
    % Get clicked positions (mouse, key, rect) interactively.
    % Input  : - Handle for current axes in which the data points are ploted.
    %            If empty matrix, then use gca. Default is [].
    %          - Type of key to wait for:
    %            'key'   - wait for a keyboard click on the plot (default).
    %            'mouse' - wait for a single mouse click on the plot.
    %            'rect'  - wait for a mouse selection of a rectanguar region.
    %            'mousem'- wait for a multiple left mouse clicks, and use
    %                      right click to abort.
    %          * ...,key,val,...
    %            'DistAxis' - Dimension along to calculate distances to the
    %                   nearest point: 'X'|'Y'|'XY'|'scale'.
    %                   Default is 'X'.
    %            'DataInd' - Index of data in plot.
    %                   If 'end', then use last element, which is the first
    %                   data set that was plotted.
    %                   Default is 1.
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
    %          - A structre containing the information for the nearest data
    %            point.
    %          - A structre containing the information for the nearest data
    %            local max point.
    % Author : Eran Ofek (Nov 2023)
    % Example: Res = plot.getInteractive(Ha, 'mouse');

    arguments
        Ha        = [];
        Type      = 'mouse';
        %WaitFor   = 'y';
        Args.DistAxis   = 'X'; % 'XY' | 'X' | 'Y'
        Args.DataInd    = 1;
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
            DataObjs = AxObjs.Children;
            if ~isnumeric(Args.DataInd)
                Args.DataInd = numel(DataObjs);
            end
            Data.X   = DataObjs(Args.DataInd).XData;
            Data.Y   = DataObjs(Args.DataInd).YData;
            Data.Z   = DataObjs(Args.DataInd).ZData;

            if nargout>3
                % look for nearest point
                switch lower(Args.DistAxis)
                    case 'scale'
                        [DataPosX, DataPosY] = plot.xy2axesPos(Ha, Data.X, Data.Y);
                        [IntPosX,  IntPosY] = plot.xy2axesPos(Ha, Res.Pos(1), Res.Pos(2));

                        Dist = sqrt((IntPosX - DataPosX).^2 + (IntPosY - DataPosY).^2);
                    case 'xy'
                        Dist = sqrt((Res.Pos(1) - Data.X).^2 + (Res.Pos(2) - Data.Y).^2);
                    case 'x'
                        Dist = abs(Res.Pos(1)-Data.X);
                    case 'y'
                        Dist = abs(Res.Pos(2)-Data.Y);
                    otherwise
                        error('Unknown DistAXis option');
                end
                [MinDist, MinInd] = min(Dist);
                Nearest.MinDist = MinDist;
                Nearest.X = Data.X(MinInd);
                Nearest.Y = Data.Y(MinInd);
                Nearest.Ind = MinInd;
                %Nearest.Z = Data.Z(MinInd);

                % if nargout>4
                % % look for neast local maximum
                %     IsLocalMax = find(localmax(Data.Y));
                %     DistLocalMax = sqrt((Res.Pos(1) - Data.X(IsLocalMax)).^2 + (Res.Pos(2) - Data.Y(IsLocalMax)).^2);
                % 
                %     switch lower(Args.DistAxis)
                %         case 'xy'
                %             DistLocalMax = sqrt((Res.Pos(1).^2 - Data.X(IsLocalMax)).^2 + (Res.Pos(2).^2 - Data.Y(IsLocalMax)).^2);
                %         case 'x'
                %             Dist = abs(Res.Pos(1)-Data.X(IsLocalMax));
                %         case 'y'
                %             Dist = abs(Res.Pos(2)-Data.Y(IsLocalMax));
                %         otherwise
                %             error('Unknown DistAXis option');
                %     end
                % 
                %     [MinDist, MinInd] = min(DistLocalMax);
                %     NearestLocalMax.MinDist = MinDist;
                %     NearestLocalMax.X = Data.X(MinInd);
                %     NearestLocalMax.Y = Data.Y(MinInd);
                % end
            end
        end

        % switch lower(WaitFor)
        %    case 'y'
        %        waitfor(Hf, 'KeyPressFcn', '');
        %    otherwise
        %        % do nothing
        % end
    end


end
