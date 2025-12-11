function [Back, Var, BackSmall, VarSmall] = backVar(Image, Args)
    % Estimate the background and variance (image or scalar) of an image
    %   See also: imUtil.background.backVarScalar
    % Input  : - A 2D image.
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
    %                   
    % Output : - A background image (or scalar).
    %          - A variance image (or scalar)
    %          - The small background image - the background evaluated in
    %            each block.
    %          - The small variance image - the variance evaluated in
    %            each block.
    % Author : Eran Ofek (2025 Oct) 
    % Example: R=poissrnd(ones(1726,1726).*100);
    %          [B,V]=imUtil.background.backVar(R);
    %          [B,V]=imUtil.background.backVar(R, 'Method',@imUtil.background.modeVar_LogHist, 'MethodArgs',{{'MinVal',50,'MaxVal',5000}});
    %          [B,V]=imUtil.background.backVar(R, 'Method', {@median, 'rvar_mex'});
    %          [B,V]=imUtil.background.backVar(R, 'Method', {'quantile', @var}, 'MethodArgs',{{0.4},{}});
    %          [B,V]=imUtil.background.backVar(R, 'Method', {@median, 'poiss'}, 'RN2',3.^2); % poisson noise + RN^2
    %          
    %          [B,V]=imUtil.background.backVar(R, 'Method', {@median, 'rvar_mex'}, 'ExtendFull',false); % scalar output
    %          [B,V]=imUtil.background.backVar(R, 'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256], 'ExtendFull',false); % small matrix output
    %          [B,V]=imUtil.background.backVar(R, 'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256]); % full matrix output

    arguments
        Image

        Args.Method     = @imUtil.background.modeVar_LogHist; %{@median,@poiss}; %or @modeVar_Hist; or string of predefined...
        Args.MethodArgs = {{},{}};
        Args.RN2        = 12;  % RN^2 - required by 'poiss'.
        Args.Dilute     = [];   % be careful of double diluting

        Args.Block      = [];   % [X, Y]
        Args.Overlap    = [32 32];
        Args.CCDSEC     = [];  % predefined CCDSEC for blocks, override Block

        Args.ExtendFull = true;
    end

    ImageFieldName = 'Im';

    if isscalar(Args.Block)
        Args.Block = [Args.Block, Args.Block];
    end

    if isempty(Args.Block) && isempty(Args.CCDSEC)
        [BackSmall, VarSmall] = imUtil.background.backVarScalar(Image, 'Method', Args.Method, 'MethodArgs',Args.MethodArgs, 'RN2',Args.RN2, 'Dilute',Args.Dilute);
        Size            = size(Image);
        %CCDSEC          = [1 Size(2) 1 Size(1)];
        %Center          = fliplr(Size).*0.5;
        %NooverlapCCDSEC = CCDSEC;
        %Nxy             = [1 1];

        if Args.ExtendFull
            Back = repmat(BackSmall, Size);
            Var  = repmat(VarSmall, Size);
        else
            Back = BackSmall;
            Var  = VarSamll;
        end
    else
        % potentially non-scalar output
        [SubImage,CCDSEC,Center,NooverlapCCDSEC,NewNoOverlap,Nxy] = imUtil.cut.partition_subimage(Image, Args.CCDSEC,...
                                                                                                  'Output','cell',...
                                                                                                  'SubSizeXY',Args.Block,...
                                                                                                  'Nxy',[],...
                                                                                                  'OverlapXY',Args.Overlap,...
                                                                                                  'FieldName',ImageFieldName);
        % go over images and calc background
        Nsub = numel(SubImage);
        BackSmall = nan(Nsub,1);
        VarSmall  = nan(Nsub,1);
        for Isub=1:1:Nsub
            [BackSmall(Isub), VarSmall(Isub)] = imUtil.background.backVarScalar(SubImage{Isub}, 'Method', Args.Method, 'MethodArgs',Args.MethodArgs, 'RN2',Args.RN2, 'Dilute',Args.Dilute);
        end
   
        % reshape
        BackSmall = reshape(BackSmall, fliplr(Nxy));
        VarSmall  = reshape(VarSmall, fliplr(Nxy));
    

        % do not interpolate/extrapolate sparse back/var to full image
        if Args.ExtendFull
            VecY   = floor(Center(1:Nxy(2),2));
            VecX   = floor(Center(1:Nxy(2):end,1));
            Back   = imUtil.image.sparse2full(BackSmall, VecX, VecY, size(Image), 'Smooth',false);
            Var    = imUtil.image.sparse2full(VarSmall, VecX, VecY, size(Image),  'Smooth',false);    
        else
            Back   = BackSmall;
            Var    = VarSmall;
        end
    end

end
