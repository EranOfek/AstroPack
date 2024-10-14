function [Result, Flat, IsFlat, IsNotFlat] = deflat(ImObj, Flat, Args)
    % Divide by flat (and construct if needed) from a list of images
    %       Treat a single Filter images only.
    % Input  : - An AstroImage object. Either containing images
    %            from which to divide the flat, or all the images
    %            including the flat images.
    %            All the images are assumed to be bias/dark
    %            subtracted.
    %          - A flat image. If empty, will attempt to construct
    %            the flat image. Default is [].
    %          * ...,key,val,...
    %            'BitDictinaryName' - A BitDictionary name.
    %                   If empty, will use existing BitDictionary.
    %                   Note, that if BitDictionary doesn't exist
    %                   and not provided, the function will fail.
    %                   Default is 'BitMask.Image.Default' (located
    %                   in the config/ directory).
    %            'IsFlat' - A function handle for a function that
    %                   selects and validates flat images
    %                   (e.g., @imProc.flat.isFlat).
    %                   Alternatively, a vector of logicals
    %                   indicating which image is a flat
    %                   image. If empty use all images.
    %                   Default is @imProc.flat.isFlat.
    %            'FlatArgs' - A cell array of additional arguments
    %                   to pass to the flat method.
    %                   Default is {}.
    %            'CCDSEC' - A CCDSEC on which to operate the bias
    %                   subtraction (will be used both on the bias
    %                   and target images). If empty, use the
    %                   entire image. Default is [].
    %            'FlatFileNameInHeader' - A logical indicating if to add
    %                   the flat name to the resulted image header.
    %                   Default is true.
    %            'KeyFlat' - Keyword name in which to store the flat image
    %                   name. Default is 'FLAT_IM'.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    %            'KeyFlatID' - If not empty, then will search for this
    %                   header keyword in the flat image and copy it to the
    %                   deflat image. Default is 'ID_FLAT'.
    % Output : - An AstroImage object containing the non-flat
    %            images after division by flat and mask propagation.
    %          - A flat image.
    %          - A vector of logicals indicating flat images.
    %          - A vector of logicals indicating non-flat images.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage('LAST.*_dark.fits');
    %          Bias = imProc.dark.bias(AI);
    %          AI = AstroImage('LAST.*_twflat.fits');
    %          imProc.flat.isFlat(AI)
    %          imProc.dark.debias(AI,Bias);  % note that with CreateNewObj it fails
    %          [Flat,IsFlat,CoaddN]=imProc.flat.flat(AI);
    %          A = AstroImage('LAST.2.1.2_20200821.020129.546_clear_0_science.fits');
    %          imProc.dark.debias(A, Bias);
    %          imProc.flat.deflat(A, Flat);

    arguments
        ImObj AstroImage
        Flat                              = [];  % A flat (AstroImage) image 
        Args.BitDictinaryName             = 'BitMask.Image.Default';  % char array or BitDictionary
        Args.IsFlat                       = @imProc.flat.isFlat;  % @isBias, @isDark, vector of logical or [] - use all.                
        Args.FlatArgs cell                = {};
        Args.CCDSEC                       = [];
        Args.FlatFileNameInHeader logical = true;
        Args.KeyFlat                      = 'FLAT_IM';
        Args.CreateNewObj                 = [];

        Args.KeyFlatID                    = 'ID_FLAT';
    end

    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end

    if isempty(Flat)
        % generate bias image
        %KeyVal                 = namedargs2cell(Args);
        [Flat, IsFlat]         = imProc.flat.flat(ImObj, Args.FlatArgs{:}, 'BitDictinaryName',Args.BitDictinaryName,...
                                                                    'IsFlat',Args.IsFlat); % KeyVal{:});
        IsNotFlat              = ~IsFlat;
    else
        % bias image is provided
        IsFlat                 = [];    % unknown
        IsNotFlat              = true(size(ImObj));
    end

    % divide by the flat image
    if ~any(IsNotFlat)
        error('No non-flat image from which to divide flat');
    end
    Result = funBinaryProp(ImObj(IsNotFlat), Flat, @rdivide, 'OpArgs',{},...
                                                'DataProp','ImageData',...
                                                'DataPropIn','Data',...
                                                'CCDSEC',Args.CCDSEC,...
                                                'CCDSEC1',Args.CCDSEC,...
                                                'CCDSEC2',Args.CCDSEC,...
                                                'UseOrForMask',true,...
                                                'CreateNewObj',Args.CreateNewObj,...
                                                'Result',[]);
    % propagate the mask image
    Result = funBinaryProp(ImObj(IsNotFlat), Flat, @rdivide, 'OpArgs',{},...
                                                'DataProp','MaskData',...
                                                'DataPropIn','Data',...
                                                'CCDSEC',Args.CCDSEC,...
                                                'CCDSEC1',Args.CCDSEC,...
                                                'CCDSEC2',Args.CCDSEC,...
                                                'UseOrForMask',true,...
                                                'CreateNewObj',false,...
                                                'Result',Result);
    % write the original flat name to header
    if Args.FlatFileNameInHeader
        FileName = io.files.removeFilePath(Flat.ImageData.FileName);
        if ~isempty(FileName)
            Result.funHeader(@insertKey, {Args.KeyFlat, FileName{1}}, Inf);
        end
    end

    if ~isempty(Args.KeyFlatID)
        FlatID = Flat.HeaderData.getVal(Args.KeyFlatID);
        Result.setKeyVal(Args.KeyFlatID, FlatID);
    end

end
