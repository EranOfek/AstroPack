function [Result, Bias, IsBias, IsNotBias] = debias(ImObj, Bias, Args)
    % Subtract bias (and construct if needed) from a list of images
    % Input  : - An AstroImage object. Either containing images
    %            from which to subtract the bias, or all the images
    %            including the bias images.
    %          - A bias image. If empty, will attempt to construct
    %            the bias/dark image. Default is [].
    %          * ...,key,val,...
    %            'BitDictinaryName' - A BitDictionary name.
    %                   If empty, will use existing BitDictionary.
    %                   Note, that if BitDictionary doesn't exist
    %                   and not provided, the function will fail.
    %                   Default is 'BitMask.Image.Default' (located
    %                   in the config/ directory).
    %            'IsBias' - A function handle for a function that
    %                   selects and validates bias/dark images
    %                   (e.g., @imProc.dark.isBias, @isDark).
    %                   Alternatively, a vector of logicals
    %                   indicating which image is a bias/dark
    %                   image. If empty use all images.
    %                   Default is @imProc.dark.isBias.
    %            'BiasArgs' - A cell array of additional arguments
    %                   to pass to the bias method.
    %                   Default is {}.
    %            'CCDSEC' - A CCDSEC on which to operate the bias
    %                   subtraction (will be used both on the bias
    %                   and target images). If empty, use the
    %                   entire image. Default is [].
    %            'BiasFileNameInHeader' - A logical indicating if to add
    %                   the bias name to the resulted image header.
    %                   Default is true.
    %            'KeyBias' - Keyword name in which to store the bias image
    %                   name. Default is 'BIAS_IM'.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    % Output : - An AstroImage object containing the non-bias
    %            images after bias subtraction and mask propagation.
    %          - A bias image.
    %          - A vector of logicals indicating bias images.
    %          - A vector of logicals indicating non-bias images.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage('LAST.2.1.2_20200821.015445.457_clear_0_science.fits');
    %          AB = imProc.dark.debias(AI,Bias);

    arguments
        ImObj AstroImage
        Bias                              = [];  % A bias (AstroImage) image 
        Args.BitDictinaryName             = 'BitMask.Image.Default';  % char array or BitDictionary
        Args.IsBias                       = @imProc.dark.isBias;  % @isBias, @isDark, vector of logical or [] - use all.                
        Args.BiasArgs cell                = {};
        Args.CCDSEC                       = [];
        Args.BiasFileNameInHeader logical = true;
        Args.KeyBias                      = 'BIAS_IM';
        Args.CreateNewObj                 = [];
    end

    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end

    if isempty(Bias)
        % generate bias image
        %KeyVal                 = namedargs2cell(Args);
        [Bias, IsBias]         = imProc.dark.bias(ImObj, Args.BiasArgs{:}, 'BitDictinaryName',Args.BitDictinaryName,...
                                                                    'IsBias',Args.IsBias); % KeyVal{:});
        IsNotBias              = ~IsBias;
    else
        % bias image is provided
        IsBias                 = [];    % unknown
        IsNotBias              = true(size(ImObj));
    end

    % subtract the bias image
    if ~any(IsNotBias)
        error('No non-bias image from which to subtract bias');
    end
    Result = funBinaryProp(ImObj(IsNotBias), Bias, @minus, 'OpArgs',{},...
                                                'DataProp','ImageData',...
                                                'DataPropIn','Data',...
                                                'CCDSEC',Args.CCDSEC,...
                                                'CCDSEC1',Args.CCDSEC,...
                                                'CCDSEC2',Args.CCDSEC,...
                                                'UseOrForMask',true,...
                                                'CreateNewObj',Args.CreateNewObj,...
                                                'Result',[]);
    % propagate the mask image
    Result = funBinaryProp(ImObj(IsNotBias), Bias, @minus, 'OpArgs',{},...
                                                'DataProp','MaskData',...
                                                'DataPropIn','Data',...
                                                'CCDSEC',Args.CCDSEC,...
                                                'CCDSEC1',Args.CCDSEC,...
                                                'CCDSEC2',Args.CCDSEC,...
                                                'UseOrForMask',true,...
                                                'CreateNewObj',false,...
                                                'Result',Result);
                                            
    % write the original bias name to header
    if Args.BiasFileNameInHeader
        FileName = io.files.removeFilePath(Bias.ImageData.FileName);
        if ~isempty(FileName)
            Result.funHeader(@insertKey, {Args.KeyBias, FileName{1}}, Inf);
        end
    end
end