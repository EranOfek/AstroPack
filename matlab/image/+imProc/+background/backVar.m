function [Result, FailedList] = backVar(Obj, Args)
    % Measure background and variance and store in AstroImage object
    %     Given an AstroImage/AstroZOGY object, estimate the background and
    %     variance using the various methods available in:
    %     imUtil.background.backVar
    %     The bac/var will be stored in ther AstroImage.
    %     Optionally, subtract the background.
    % Input  : - An AstroImage/AstroZOGY/AstroDiff object
    %          * ...,key,val,... 
    %            'Method' - A method for Back & Var calculation.
    %                   This can be a cell array of two elements
    %                   for the Back and Var methods or a single element
    %                   for a method that calculates them together (e.g.,
    %                   modeVar_Hist).
    %                   The methods may be function handles, or string of
    %                   pre defined methods.
    %                   For predefined method see:
    %                   imUtil.background.backgroundOption
    %                   Examples:
    %                   @imUtil.background.modeVar_LogHist | @imUtil.background.modeVar_Hist - will use a single function to
    %                           calculate both Back and Var.
    %                   {@median, 'poiss'} | {@median, 'rvar_mex'} | ...
    %                   Default is @imUtil.background.modeVar_Hist
    %            'MethodArgs' - A cell array of additional arguments to pass to:
    %                   imUtil.background.backgroundOption
    %                   Default is {}.
    %            'RN2' - Readout noise ^2 (required by 'poiss').
    %                   Note thta modeVar_LogHist has its own RN2 argument.
    %                   Default is 12.
    %            'Dilute' - Dilute the array by this factor. If empty, do
    %                   not dilute. Note some functions (e.g., @modeVar_LogHist)
    %                   has internal dilution arguments. Default is {}.
    %
    %            'PoissVar' - If true, then calculate variance assuming
    %                   noise is Poissonian, with RN2 and Ncoadd.
    %                   Default is false.
    %            'Ncoadd' - Number of coadd images, used if PoissVar is
    %                   true. Default is 1.
    %
    %            --- Blocks and full image ---
    %            'Block' - If empty, calculate the global back/var of the
    %                   image. Alternatively, a [X, Y] (scalar will be
    %                   extended to two elements array) of the partitioning     
    %                   block size. The back/var will be calculated in each
    %                   block. Default is [].
    %            'Overlap' - Approximate Overlap between blocks.
    %                   Default is [32 32].
    %            'CCDSEC' - A four columns matrix of CCDSEC representing
    %                   predefined blocks. This can be prepared by:
    %                   imUtil.cut.subimage_grid
    %                   If given, this will override the 'Block' argument.
    %                   If [], will be calculated using:
    %                   imUtil.cut.subimage_grid
    %                   Default is [].
    %            'ExtendFull' - A logical indicating if to extend the
    %                   scalar or matrix of back/var in sub images into a
    %                   full size image.
    %                   This is done using: imUtil.image.sparse2full
    %                   Default is true.
    %            'SubBack' - A logical indicating if to subtract
    %                   background. Default is false.
    %            'AddHeaderInfo' - If true, then will update image header
    %                   with the following statistics:
    %            'UseFastMedian' - A logical indicating if to use fast_median
    %                   for the header statistics.
    %                   Default is true.
    %
    %            'ReCalc' - A logical indicating if to recalculate the
    %                   background and variance if the background image is not
    %                   empty.
    %                   Default is false.
    %            'CreateNewObj' - A logical indicating if to create new
    %                   object before modifying the AstroImage.
    %                   Default is false.
    %            'Verbosity' - 0: silent; >0: issue a warning naming each
    %                   element whose background estimation failed.
    %                   Default is 0.
    % Output : - An updated object.
    %          - A vector of indices of the elements for which the background
    %            estimation failed. Their Back/Var are filled with NaN, the
    %            background statistics keywords are written with an undefined
    %            (blank) value, and the background is not subtracted from them
    %            even if SubBack is true.
    % Author : Eran Ofek (2025 Oct) 
    % Example: AI=imProc.background.backVar(AI);
    %          AI=imProc.background.backVar(AI, 'Block',256, 'Method',{@median, @var}, 'ReCalc',true);
    %          AI=imProc.background.backVar(AI, 'SubBack',true);

    arguments
        Obj

        Args.Method     = @imUtil.background.modeVar_Hist; %{@median,@poiss}; %or @modeVar_Hist; or string of predefined...
        Args.MethodArgs = {{},{}};
        Args.RN2        = 12;  % RN^2 - required by 'poiss' | header keyword
        Args.Dilute     = [];   % be careful of double diluting

        Args.PoissVar   = false;   % Calculate variance assuming Poisson noise
        Args.Ncoadd     = 1;  % for Poisson noise calculations

        Args.Block      = [];   % [X, Y]
        Args.Overlap    = [32 32];
        Args.CCDSEC     = [];  % predefined CCDSEC for blocks, override Block

        Args.ExtendFull = true;
        
        Args.SubBack    = false;
        Args.AddHeaderInfo = true;
        Args.UseFastMedian = true;

        Args.ReCalc       = false;
        Args.CreateNewObj = false;
        Args.Verbosity    = 0;    % 0: silent; >0: warn when an estimation fails

        Args.ImageProp    = 'ImageData';
        Args.ImagePropIn  = 'Data';
        Args.BackProp     = 'BackData';
        Args.BackPropIn   = 'Data';
        Args.VarProp      = 'VarData';
        Args.VarPropIn    = 'Data';
        
    end

    Keys = {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR', 'MINBCK', 'MAXBCK', 'BCKMTHD', 'VARMTHD'};

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Nobj = numel(Obj);
    if isnumeric(Args.RN2)
        if isscalar(Args.RN2)
            Args.RN2 = repmat(Args.RN2, Nobj, 1);
        end
    else
        % get Read noise from header
        HeadVal  = Result.getStructKey(Args.RN2);
        Args.RN2 = [HeadVal.(Args.RN2)].^2;
    end
    
    if isa(Args.Method, 'function_handle')
        StrMethod = func2str(Args.Method);
    else
        if iscell(Args.Method)
            StrMethod = 'cell';
        else
            StrMethod = Args.Method;
        end
    end

    % Codes of the background and the variance estimation methods, written
    % to the BCKMTHD and VARMTHD header keywords. A single method (i.e. not
    % a cell array) estimates both, hence the same code in both keywords.
    [MethodDict, MethodCode] = imUtil.background.backdictionary(Args.Method);
    if isscalar(MethodCode)
        MethodCode = [MethodCode, MethodCode];
    end
    % method names, written as the comments of the two method keywords
    MethodComment = cell(1,2);
    for Imeth=1:1:2
        MethodComment{Imeth} = MethodDict([MethodDict.Code]==MethodCode(Imeth)).Name;
    end
    KeyComments = [repmat({''}, 1, numel(Keys)-2), MethodComment];

    FailedList = [];
    for Iobj=1:1:Nobj
        if isempty(Result(Iobj).(Args.BackProp).(Args.BackPropIn)) || Args.ReCalc
            Ok = true;
            try
            switch StrMethod
                case 'backBertin'
                    [Back, Var, BackSmall, VarSmall] = imUtil.background.mex.backBertin(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn), Args.MethodArgs{:});

                case 'backBertinLowerRMS'
                    [Back, Var, BackSmall, VarSmall] = imUtil.background.mex.backBertinLowerRMS(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn), Args.MethodArgs{:});

                otherwise
                    [Back,Var,BackSmall,VarSmall]=imUtil.background.backVar(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn),...
                                                 'Method',Args.Method,...
                                                 'MethodArgs',Args.MethodArgs,...
                                                 'RN2',Args.RN2(Iobj),...
                                                 'Dilute',Args.Dilute,...
                                                 'Block',Args.Block,...
                                                 'Overlap',Args.Overlap,...
                                                 'CCDSEC',Args.CCDSEC,...
                                                 'ExtendFull',Args.ExtendFull);
            end
            catch ME
                FailedList = [FailedList,Iobj];
                Ok         = false;
                if Args.Verbosity>0
                    warning('imProc:background:backVar:estimationFailed',...
                            'Background estimation failed for element %d: %s', Iobj, ME.message);
                end
                % Without this the store below would keep the *previous*
                % element's Back/Var and write them into this image, or throw
                % on an unassigned Back for the first element (issue #1223).
                SizeIm    = size(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn));
                Back      = nan(SizeIm, 'single');
                Var       = nan(SizeIm, 'single');
                BackSmall = [];
                VarSmall  = [];
            end

            % store
            Result(Iobj).BackData.Image = Back;

            if Args.PoissVar
                % assume Var is poisson from Back
                Var      = (Back + Args.RN2(Iobj))./Args.Ncoadd;
                VarSmall = (BackSmall + Args.RN2(Iobj))./Args.Ncoadd;

                % Floor the variance. A background extrapolated below zero over
                % a large dead region drives the Poisson variance to zero or
                % negative, and a zero variance makes S/N Inf downstream, which
                % then aborts source extraction (issue #1223).
                % Floor at the read-noise variance, not at eps: a pixel floored
                % to eps has sqrt(Var)~3e-4 and becomes an enormous local
                % maximum, trading the crash for spurious sources. Since
                % Var=(Back+RN2)/Ncoadd, flooring at RN2/Ncoadd is exactly
                % flooring Back at 0, so floored pixels read as ordinary
                % read-noise-limited pixels. max(eps(..),..) keeps it positive
                % if RN2 comes from a header and is 0.
                % Index rather than max(): max() ignores NaN and would silently
                % replace the NaN failure marker above with the floor value -
                % which also makes an Ok guard here unnecessary.
                MinVar = max(eps('single'), Args.RN2(Iobj)./Args.Ncoadd);
                Var(Var<MinVar)           = MinVar;
                VarSmall(VarSmall<MinVar) = MinVar;
            end
            Result(Iobj).VarData.Image  = Var;
            
    
            % subtract background
            % skipped when the estimation failed: subtracting a NaN background
            % would destroy the image data
            if Args.SubBack && Ok
                Result(Iobj).ImageData.Image = Result(Iobj).ImageData.Image - Back;
            end
    
            % update header
            % On failure the keywords are still written, with an undefined
            % value (issue #1226): the image is saved as a data product even
            % though it has no background, so its header has to say so, and a
            % keyword present without a value is distinguishable from one that
            % never ran (no keyword at all).
            % The value is NaN and not [], although both read back as NaN
            % through getVal/getStructKey: the mex header writers serialize a
            % non-finite value as a blank (FITS undefined) card - the
            % representation agreed in issue #1194 - while an empty value is
            % serialized as '0.', which would claim a zero background and the
            % method code 0.
            if ~isempty(Args.AddHeaderInfo) && ~Ok
                % no method comment here - no method produced a result
                Result(Iobj).HeaderData.replaceVal(Keys, repmat({NaN}, 1, numel(Keys)),...
                                                   'Comment',repmat({''}, 1, numel(Keys)));
            end
            if ~isempty(Args.AddHeaderInfo) && Ok
                %
                MeanBack = mean(BackSmall, 'all');
                StdBack  = std(BackSmall, [],'all');
                MeanVar = mean(VarSmall, 'all');
                MinBack = min(BackSmall, [], 'all');
                MaxBack = max(BackSmall, [], 'all');

                if Args.UseFastMedian
                    MedBack = fast_median(BackSmall(:));
                    MedVar  = fast_median(VarSmall(:));
                else
                    MedBack = median(BackSmall, 'all');
                    MedVar  = median(VarSmall, 'all');
                end
                
                %Keys = {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR', 'MINBCK', 'MAXBCK', 'BCKMTHD', 'VARMTHD'};
                Vals  = {MeanBack, MedBack, StdBack, MeanVar, MedVar, MinBack, MaxBack, MethodCode(1), MethodCode(2)};
                Result(Iobj).HeaderData.replaceVal(Keys, Vals, 'Comment',KeyComments);
                    
            end % if ~isempty(Args.AddHeaderInfo)
        end % if isempty(Result(Iobj).BackData.Image) || Args.ReCalc
    end % for Iobj=1:1:Nobj
end

