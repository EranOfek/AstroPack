function [Result] = backVar(Obj, Args)
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
    % Output : - An updated object.
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

        Args.Block      = [];   % [X, Y]
        Args.Overlap    = [32 32];
        Args.CCDSEC     = [];  % predefined CCDSEC for blocks, override Block

        Args.ExtendFull = true;
        
        Args.SubBack    = false;
        Args.AddHeaderInfo = true;
        Args.UseFastMedian = true;

        Args.ReCalc       = false;
        Args.CreateNewObj = false;

        Args.ImageProp    = 'ImageData';
        Args.ImagePropIn  = 'Image';
        Args.BackProp     = 'BackData';
        Args.BackPropIn   = 'Image';
        Args.VarProp      = 'VarData';
        Args.VarPropIn    = 'Image';
        
    end

    Keys = {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR', 'MINBCK', 'MAXBCK'};

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
    
    for Iobj=1:1:Nobj

        if isempty(Result(Iobj).(Args.BackProp).(Args.BackPropIn)) || Args.ReCalc

            [Back,Var,BackSmall,VarSmall]=imUtil.background.backVar(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn),...
                                                 'Method',Args.Method,...
                                                 'MethodArgs',Args.MethodArgs,...
                                                 'RN2',Args.RN2(Iobj),...
                                                 'Dilute',Args.Dilute,...
                                                 'Block',Args.Block,...
                                                 'Overlap',Args.Overlap,...
                                                 'CCDSEC',Args.CCDSEC,...
                                                 'ExtendFull',Args.ExtendFull);
            % store
            Result(Iobj).BackData.Image = Back;
            Result(Iobj).VarData.Image  = Var;
    
            % subtract
            if Args.SubBack
                Result(Iobj).ImageData.Image = Result(Iobj).ImageData.Image - Back;
            end
    
            % update header
            if ~isempty(Args.AddHeaderInfo)
                %
                MeanBack = mean(BackSmall, 'all');
                StdBack  = std(BackSmall, [],'all');
                MeanVar = mean(VarSmall, 'all');
                MinBack = min(BackSmall, [], 'all');
                MaxBack = min(BackSmall, [], 'all');

                if Args.UseFastMedian
                    MedBack = fast_median(BackSmall(:));
                    MedVar  = fast_median(VarSmall(:));
                else
                    MedBack = median(BackSmall, 'all');
                    MedVar  = median(VarSmall, 'all');
                end
                
                %Keys = {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR', 'MINBCK', 'MAXBCK'};
                Vals  = {MeanBack, MedBack, StdBack, MeanVar, MedVar, MinBack, MaxBack};
                Result(Iobj).HeaderData.replaceVal(Keys,Vals);
                    
            end % if ~isempty(Args.AddHeaderInfo)
        end % if isempty(Result(Iobj).BackData.Image) || Args.ReCalc
    end % for Iobj=1:1:Nobj

end
