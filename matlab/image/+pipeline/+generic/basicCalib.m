function [AI] = basicCalib(AI, CI, Args)
    % Basic calibration sub pipeline for dark, overscan, flat, fringing, and header JD fixing.
    %     Performs the following steps:
    %       Optionally convert the images to single precision.
    %       Update and fix the JD to the mid JD of the image.
    %       Dark, overscan, flat, fringe, linear, gain correct the images.
    %       The last step is done via CalibImages/processImages.
    %       
    % Input  : - An AstroImage object.
    %          - A CalibImages object populated with dark/bias and flat
    %            images.
    %            If empty, then will search for the bias/dark/flat images
    %            in the 'Dark','Flat','Fringe' arguments.
    %            Default is [].
    %          * ...,key,val,... 
    %            'CreateNewObj' - false, true. Default is false.
    %            'Convert2single' - Transform raw images to single class.
    %                   Note that this is done by default by
    %                   pipeline.generic.prePrep.
    %                   Default is false.
    %            'LogObj' - An optional MsgLogger object.
    %                   If non empty, then will write error and information
    %                   messages to the specified log file and standard
    %                   output.
    %                   Default is [].
    %            --- optional CalibImages ---
    %            'Dark' - An optional AstroImage object containing a master
    %                   dark image. Will be used only if CalibImages is
    %                   empty.
    %                   Default is [].
    %            'Flat' - Like 'Dark', but for the flat image.
    %                   Default is [].
    %            'Flat' - Like 'Dark', but for the fringe image.
    %                   Default is [].
    %            --- Updating / fixing JD ---
    %            'UpdateJD' - A logical indicating if to add or update the
    %                   JD in the header to the JD of mid exposure.
    %                   Default is true.
    %            'CheckJD' - A logical indicating if to check the JD.
    %                   if JD is round then recalculate it from DATE-OBS
    %                   Default is true.
    %            'ExpTimeKey' - Haeder keyword for Exposure Time.
    %                   Default is 'EXPTIME'.
    %            'DateObsKey' - Header keyword for date.
    %                   Default is 'DATE-OBS'.
    %           --- CalibImages/processImages arguments ---
    %           'BitDict' - Bit dictionary name.
    %                   Default is 'BitMask.Image.Default'.
    %           'SingleFilter' - A logical indicating if the
    %                   provided images were taken using a single filter.
    %                   If true then will use deflatOneFilt,
    %                   otherwaise will use deflat.
    %                   Default is false.
    %           'MaskSaturated' - A logical indicating if to flag
    %                   saturated pixels, in the Mask image.
    %                   Default is true.
    %           'maskSaturatedArgs' - A cell array of additional
    %                   arguments to pass to imProc.mask.maskSaturated
    %                   Default is {}.
    %           'debiasArgs' - A cell array of additional
    %                   arguments to pass to imProc.dark.debias.
    %                   Default is {}.
    %           'SubtractOverscan' - A logical indicating if to
    %                   subtract overscan. Default is true.
    %           'OverScan' - Either an header keyword containing
    %                   the overscan region, or an [Xmin Xmax Ymin Ymax]
    %                   vector for the overscan.
    %                   Default is 'OVERSCAN'.
    %           'FinalCrop' - Either a header keyword, or a [Xmin Xmax Ymin Ymax]
    %                   containing the final image to keep.
    %                   If empty, then will attempt to estimate using:
    %                   imUtil.ccdsec.remove_edge_section.
    %                   Default is [].
    %           'MethodOverScan' - see imProc.dark.overscan.
    %                   Default is 'globalmedian'.
    %           'overscanArgs' - A cell array of additional
    %                   arguments to pass to imProc.dark.overscan
    %                   Default is {}.
    %           'trimOverscanArgs' - A cell array of additional
    %                   arguments to pass to imProc.dark.trimOverscan
    %                   Default is {}.
    %           'TrimOverscan' - A logical indicating if to trim
    %                   overscan.
    %                   Default is true.
    %           'NonLinCorr' - A correction table [Flux, CorrectionFactor]
    %                   or a structure with .Flux and .Corr fields.
    %                   The correction factor is either multiplicative or by
    %                   division (see 'Operator' argument in imProc.calib.nonlinearCorrection).
    %                   Default is @rdivide.
    %                   If empty, will attempt to read the table
    %                   from the Linearity property of the
    %                   CalibImages object.
    %                   If not exist, the will skip this step.
    %                   Default is [].
    %           'NonLinCorrArgs' - A cell array of additional
    %                   arguments to pass to
    %                   imProc.calib.nonlinearCorrection.
    %                   Default is {}.
    %           'deflatArgs' - A cell array of additional
    %                   arguments to pass to imProc.flat.deflat.
    %                   Default is {}.
    %           'CorrectFringing' - A logical indicating if to
    %                   correct fringing. Default is false.
    %           'MultiplyByGain' - A logical indicating if to
    %                   multiply image values by gain.
    %                   Default is true.
    %           'BitNameNaN' - The bit name for NaN pixels.
    %                   If empty, then will not set the bit mask.
    %                   Default is 'NaN'.
    %           'BitNameNegative' - Name of Negative bit-mask.
    %                   Default is 'Negative'.
    %           'SetNegativeTo0' - A logical indicating if to set
    %                   negative pixels to zeros. Default is true.
    %           'InterpolateOberBadPix' - A logical indicating
    %                   if to interpolate over bad pixels.
    %                   Default is true.
    %           'BitNameBadPix' - A cell array of bad pixels over
    %                   which to interpolate.
    %                   Default is {}.
    %           'BitName_Interpolated' - Bit name for interpolated
    %                   pixels. Default is 'Interpolated'.
    %           'interpOverNanArgs' - A cell array of additional
    %                   arguments to pass to imProc.image.interpOverNan.
    %                   Default is {}.
    %
    % Output : - The input AstroImage object after the basic calibration,
    %            inclding dark, flat, etc.
    %
    % Author : Eran Ofek (2025 Sep) 
    % Example: AI=pipeline.generic.basicCalib(AI, CI);

    arguments
        AI
        CI                     = [];
        
        Args.CreateNewObj      = false;
        Args.Convert2single    = false;
        Args.LogObj            = [];

        % optional CalibImages
        Args.Dark              = [];
        Args.Flat              = [];
        Args.Fringe            = [];

        Args.UpdateJD          = true;
        Args.CheckJD           = true;
        Args.ExpTimeKey        = 'EXPTIME';
        Args.DateObsKey        = 'DATE-OBS';

        % bias/dark.flat via CalibImages/processImages              
        % bit dictionary
        Args.BitDict                        = 'BitMask.Image.Default'; % or BitDictionary object.
        Args.SingleFilter logical           = false;
        Args.MaskSaturated logical          = true;
        Args.maskSaturatedArgs cell         = {};
        Args.debiasArgs cell                = {};
        Args.SubtractOverscan logical       = true;
        Args.OverScan                       = 'OVERSCAN';
        Args.FinalCrop                      = [1 6388 25 9600];  % if empty, then auto estimate
        Args.MethodOverScan                 = 'globalmedian';
        Args.overscanArgs cell              = {};
        Args.trimOverscanArgs cell          = {};
        Args.TrimOverscan logical           = true;
        Args.NonLinCorr                     = [];   % nonlinear correction table [flux, factor]
        Args.NonLinCorrArgs cell            = {};   % args for imProc.calib.nonlinearCorrection
        Args.deflatArgs cell                = {};
        Args.CorrectFringing logical        = false;
        Args.MultiplyByGain logical         = true;
        Args.BitNameNaN                     = 'NaN';
        Args.BitNameNegative                = 'Negative';
        Args.SetNegativeTo0 logical         = true;
        Args.InterpolateOverBadPix logical  = false;
        Args.BitNameBadPix                  = {}; %{'Saturated','NaN', 'Negative'};
        Args.BitNameInterpolated            = 'Interpolated';
        Args.interpOverNanArgs cell         = {};
    end

    % Get images

    % number of images
    Nim = numel(AI);

    % createNewObj
    if Args.CreateNewObj
        AI = AI.copy;
    end

    % Convert 2 single (default is false)
    % typically done by: pipeline.generic.prePrep
    if Args.Convert2single
        %AI.cast(Args.ImageClass);  % very slow

        for Iim=1:1:Nim
            AI(Iim).ImageData.Data = single(AI(Iim).ImageData.Data);
            %AI(Iim).Image = single(AI(Iim).Image);
        end
    end
    
    % set/check CalibImages
    Cont = true;
    if isempty(CI)
        if isempty(Args.Dark) || isempty(Args.Flat)
            % Dark and or Flat not supplied
            Cont = false;
        else
            CI                      = CalibImages;
            Args.CalibImages.Dark   = Args.Dark;
            Args.CalibImages.Flat   = Args.Flat;
            Args.CalibImages.Fringe = Args.Fringe;
        end
    else
        if (isempty(CI.Dark.Image) && isempty(CI.Bias.Image)) || isempty(CI.Flat.Image)
            % no bias/flat found
            Cont = false;
        end
    end
    % set error if no calib images
    if ~Cont
        % no bias/flat found
        if isempty(Args.LogObj)
            error('No Dark/Flat supplied');
        else
            Msg = sprintf('Dark/Flat not supplied');
            Args.LogObj.writeMsg(Msg, LogLevel.Error);
        end
    end
    

    % set/check JD in header
    if Args.UpdateJD
        [AI,JD,ExpTime,IsFixed]=imProc.header.fixJD(AI, 'CheckJD',Args.CheckJD,...
                                                 'ExpTimeKey',Args.ExpTimeKey,...
                                                 'DateObsKey',Args.DateObsKey);   
        if IsFixed && ~isempty(Args.LogObj)
            Msg = sprintf('JD in one/more headers was round - populated from %s keyword',Args.DateObsKey);
            Args.LogObj.writeMsg(Msg, LogLevel.Warning);
        end
    else
        JD      = nan(Nim,1);
        ExpTime = nan(Nim,1);
    end

    
    % Note that InterpolateOverSaturated is false, because this is done
    % later on in this function
    % processImages is a method of the CalibImages class
    % If NonLinCorr is empty, then will attempt taking it from the
    % CalibImages object.
    AI = CI.processImages(AI, ...
                              'CreateNewObj',Args.CreateNewObj,...
                              'SingleFilter',Args.SingleFilter,...
                              'BitDict',Args.BitDict,...
                              'InterpolateOverBadPix',Args.InterpolateOverBadPix,...
                              'BitNameBadPix',Args.BitNameBadPix,...
                              'BitNameInterpolated',Args.BitNameInterpolated,...
                              'MaskSaturated',Args.MaskSaturated,...
                              'maskSaturatedArgs',{},...
                              'debiasArgs',Args.debiasArgs,...
                              'SubtractOverscan',Args.SubtractOverscan,...
                              'OverScan',Args.OverScan,...
                              'FinalCrop',Args.FinalCrop,...
                              'MethodOverScan',Args.MethodOverScan,...
                              'NonLinCorr',Args.NonLinCorr,...
                              'NonLinCorrArgs',Args.NonLinCorrArgs,...
                              'deflatArgs',Args.deflatArgs,...
                              'CorrectFringing',Args.CorrectFringing,...
                              'MultiplyByGain',Args.MultiplyByGain);
       
    if ~isempty(Args.LogObj)
        Msg = 'CalibImages/processImages (dark, flat,...) completed';
        Args.LogObj.writeMsg(Msg, LogLevel.Info);
    end
    
    % catch ME
    %     % errors and log file
    %     % allocate TableForDB:
    %     AI         = AstroImage;
    %     % write catch error:
    %     if ~isempty(Args.LogObj)
    %         Obj.writeLog(ME, LogLevel.Error);
    %     else
    %         ME
    %         error('Failed on try catch');
    %     end
    % end

end
