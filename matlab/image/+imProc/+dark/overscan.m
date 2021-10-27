function [Result, OverScanAI] = overscan(ImObj, Args)
    % Create overscan images and optionally subtract from images
    % Input  : - An AstroImage object of images that contains an
    %            overscan region.
    %          * ...,key,val,...
    %            'OverScan' - Either an header keyword containing
    %                   the overscan region, or an [Xmin Xmax Ymin Ymax]
    %                   vector for the overscan.
    %                   Default is 'OVERSCAN'.
    %            'Subtract' - A logical indicating if to subtract
    %                   the overscan from the image. Default is true.
    %            'RemoveOverScan' - A logical indicating if to crop
    %                   out the overscan region. Default is true.
    %            'RemoveOthers' - a logical indicating if to crop
    %                   also the Back, Var, and Mask fields.
    %                   Default is true (independent of RemoveOverScan).
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
    %            'Method' - Method by which to calculate the overscan:
    %                   'globalmedian' - global median (default).
    %                   'globalmean' - global mean.
    %                   'median' - median long the overscan 
    %                   'mean' - mean along the overscan
    %                   'medmedfilt' - median along the overscan
    %                           followed by median filtering
    %                           smoothing. Smoothing default is {50}.
    %                   'medsgolayfilt' - median along the overscan
    %                           followed by Svitzk-Golay filtering.
    %                           Smoothing default is {3 50}.
    %            'MethodArgs' - A cell array of additional
    %                   arguments to pass to the method.
    %                   (Defaults are defined for each medthod).
    %            'UpdateCCDSEC' - A logical indicating if to update
    %                   the CCDSEC header keyword in the header.
    %                   Will also update NAXIS* keywords.
    %                   Default is true.
    %            'KeyCCDSEC' - Header keyword CCDSEC. Default is 'CCDSEC'.
    %            'DataProp' - Data property on which to operate.
    %                   Defaultis 'ImageData'.
    %            'DataPropIn' - Data property, in the image component, on which to operate.
    %                   Defaultis 'Image'.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    % Output : - An AstroImage object with the overscan subtracted
    %            (and croped) images.
    %          - OverScanAI is an AstroImage object that stores the
    %            overscan image in the 'ImageData' field, and the
    %            calculated (global or line) overscan in the
    %            'BackData' field.
    % Author : Eran Ofek (May 2021)
    % Example: [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[1 10 1 9600])
    %          [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[6379 6388 1 9600])
    %          [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[6379 6388 1 9600],'Method','medmedfilt')

    arguments
        ImObj AstroImage
        Args.OverScan                    = 'OVERSCAN';  % keyword or CCDSEC
        Args.Subtract(1,1) logical       = true;
        Args.RemoveOverScan(1,1) logical = true;
        Args.RemoveOthers(1,1) logical   = true;
        Args.OverScanDir                 = []; % 'x'|'y',1,2,[]- auto
        Args.Method                      = 'globalmedian';
        Args.MethodArgs cell             = {50};
        Args.UpdateCCDSEC(1,1) logical   = true;
        Args.KeyCCDSEC                   = 'CCDSEC';
        Args.DataProp                    = 'ImageData';
        Args.DataPropIn                  = 'Image';

        Args.CreateNewObj                = [];
    end
    DefArgSGolay = {3 50}; 

    [Result, Args.CreateNewObj] = createNewObj(ImObj, Args.CreateNewObj, nargout);
    
    Nim = numel(ImObj);
    if nargout>1
        OverScanAI = AstroImage(size(ImObj));
    end
    for Iim=1:1:Nim
        if ischar(Args.OverScan)
            % read from header
            OverScan = getVal(ImObj(Iim).HeaderData, Args.OverScan,'ReadCCDSEC',true);
        else
            OverScan = Args.OverScan;
        end

        % cut overscan
        OverScanImage = ImObj(Iim).Image(OverScan(3):OverScan(4), OverScan(1):OverScan(2));


        % overscan direction
        if isempty(Args.OverScanDir)
            % select according to the long axis
            SizeOverScan = size(OverScanImage);
            if SizeOverScan(1)>SizeOverScan(2)
                % If the overscan region is columns than Dir=2
                Dir = 2;
            else
                % If the overscan region is rows than Dir=1
                Dir = 1;
            end
        else
            if isnumeric(Args.OverScanDir)
                Dir = Args.OverScanDir;
            else
                switch lower(Args.OverScanDir)
                    case 'x'
                        Dir = 2;
                    case 'y'
                        Dir = 1;
                    otherwise
                        error('Unknown OverScanDir option');
                end
            end
        end

        % perform analysis on OverScanImage
        switch lower(Args.Method)
            case 'globalmedian'
                OverScanLine = median(OverScanImage,'all','omitnan');
            case 'globalmean'
                OverScanLine = mean(OverScanImage,'all','omitnan');
            case 'median'
                OverScanLine = median(OverScanImage,Dir,'omitnan');
            case 'mean'
                OverScanLine = mean(OverScanImage,Dir,'omitnan');
            case 'medmedfilt'
                % medain followed by median filter
                OverScanLine = median(OverScanImage,Dir,'omitnan');
                OverScanLine = medfilt1(OverScanLine,Args.MethodArgs{:});
            case 'medsgolayfilt'
                % medain followed by Svitzky-Golay filter
                OverScanLine = median(OverScanImage,Dir,'omitnan');
                if numel(Args.MethodArgs)<2
                    Args.MethodArgs = DefArgSGolay;
                end
                OverScanLine = sgolayfilt(OverScanLine,Args.MethodArgs{:});
            otherwise
                error('Unknown Method option');
        end

        % subtract OverScanLine
        if Args.Subtract
            Result(Iim).(Args.DataProp).(Args.DataPropIn) = Result.(Args.DataProp).(Args.DataPropIn) - OverScanLine;
        end

        SizeOriginalImageIJ = size(Result.(Args.DataProp).(Args.DataPropIn));
        CCDSEC = imUtil.ccdsec.remove_edge_section(SizeOriginalImageIJ, OverScan, Dir);
        % remove overscan from image
        if Args.RemoveOverScan
            % calculate CCDSEC (the non-overscan region)
            Result(Iim).(Args.DataProp).(Args.DataPropIn) = Result.(Args.DataProp).(Args.DataPropIn)(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
        end

        % remove overscan from other images
        if Args.RemoveOthers
            % calculate CCDSEC (the non-overscan region)
            if ~isempty(Result(Iim).BackData.Data)
                Result(Iim).BackData.Image = Result.BackData.Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
            end
            if ~isempty(Result(Iim).VarData.Data)
                Result(Iim).VarData.Image = Result.VarData.Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
            end
            if ~isempty(Result(Iim).MaskData.Data)
                Result(Iim).MaskData.Image = Result.MaskData.Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
            end
        end

        % update Header
        if Args.UpdateCCDSEC
            StrCCDSEC = imUtil.ccdsec.ccdsec2str(CCDSEC);
            Result(Iim).HeaderData.replaceVal(Args.KeyCCDSEC, {StrCCDSEC});

            % update the NAXIS keywords
            SizeImage = size(Result(Iim).(Args.DataProp).(Args.DataPropIn));
            Result(Iim).HeaderData.replaceVal('NAXIS1', {SizeImage(1)});
            Result(Iim).HeaderData.replaceVal('NAXIS1', {SizeImage(2)});
        end

        % store over scan in AstroImage (only if requested)
        if nargout>1
            OverScanAI(Iim).Image = OverScanImage;
            OverScanAI(Iim).Back  = OverScanLine;
        end        

    end

end