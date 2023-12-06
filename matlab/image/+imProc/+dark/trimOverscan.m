function Result = trimOverscan(Obj, Args)
    % Trim OVERSCAN region from images in an AstroImage object.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'OverScan' - Either an header keyword containing
    %                   the overscan region, or an [Xmin Xmax Ymin Ymax]
    %                   vector for the overscan.
    %                   Default is 'OVERSCAN'.
    %            'FinalCrop' - Either a header keyword, or a [Xmin Xmax Ymin Ymax]
    %                   containing the final image to keep.
    %                   If empty, then will attempt to estimate using:
    %                   imUtil.ccdsec.remove_edge_section.
    %                   Default is [].
    %
    %            'OverScanDir' - Indicating the direction of the overscan:
    %                   'x'|'y'|1|2| [].
    %                   'x', or 2 means that the overscan is read along
    %                       the x-axis (meaning the overscan is in
    %                       columns).
    %                   'y', or 1 means that the overscan is read along
    %                       the y-axis.
    %                   [] - choose the direction automatically,
    %                   where the smaller dimension of the overscan
    %                   region is assumed to be the direction.
    %                   Default is [].
    %            'DataProp' - Data property on which to operate.
    %                   Defaultis {'Image','Mask'}.
    %            'UpdateHeader' - A logical indicating if to update
    %                   the CCDSEC header keyword in the header.
    %                   Will also update NAXIS* keywords.
    %                   Will be considered only if RemoveOverScan=true.
    %                   Default is true.
    %            'KeyCCDSEC' - Header keyword CCDSEC to update with the final CCDSEC.
    %                   Default is 'CCDSEC'.
    %
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).  
    %                   Default is false.
    %
    % Output : - An updated AstroImage object.
    % Author : Eran Ofek (Jun 2023)
    % Example: AI = imProc.dark.trimOverscan(AI);

    arguments
        Obj AstroImage
        Args.OverScan                  = 'OVERSCAN';
        Args.FinalCrop                 = [];
        Args.OverScanDir               = [];  % 1 for Y axis
        Args.DataProp                  = {'Image','Mask'};
        Args.UpdateHeader logical      = true;
        Args.KeyCCDSEC                 = 'CCDSEC';
        Args.CreateNewObj logical      = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Ndp = numel(Args.DataProp);

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % get FinalCrop
        if isempty(Args.FinalCrop)
            if ischar(Args.OverScan)
                % read from header
                OverScan = getVal(Result(Iobj).HeaderData, Args.OverScan,'ReadCCDSEC',true);
            else
                % assume OverScan is in CCDSEC format: [Xmin Xmax Ymin Ymax]
                OverScan = Args.OverScan;
            end
            if isempty(OverScan)
                error('Overscan is empty');
            end
            [SizeImI,SizeImJ] = Result(Iobj).sizeImage;
            FinalCrop         = imUtil.ccdsec.remove_edge_section([SizeImI,SizeImJ], OverScan, Args.OverScanDir);
        else
            % use FinalCrop instead of OverScan
            if isnumeric(Args.FinalCrop)
                FinalCrop = Args.FinalCrop;
            else
                % get from header
                FinalCrop = getVal(Result(Iobj).HeaderData, Args.FinalCrop, 'ReadCCDSEC',true);
            end
        end

        % crop images
        for Idp=1:1:Ndp
            if ~isempty(Result(Iobj).(Args.DataProp{Idp}))
                Result(Iobj).(Args.DataProp{Idp}) = Result(Iobj).(Args.DataProp{Idp})(FinalCrop(3):FinalCrop(4), FinalCrop(1):FinalCrop(2));
            end
        end

        % update CCDSEC in header
        if Args.UpdateHeader
            StrCCDSEC = imUtil.ccdsec.ccdsec2str(FinalCrop);
            Result(Iobj).HeaderData.replaceVal(Args.KeyCCDSEC, {StrCCDSEC});
            
            % update the NAXIS keywords
            SizeImage = size(Result(Iobj).Image);
            Result(Iobj).HeaderData.replaceVal('NAXIS2', {SizeImage(1)});
            Result(Iobj).HeaderData.replaceVal('NAXIS1', {SizeImage(2)});
        end
    end


end