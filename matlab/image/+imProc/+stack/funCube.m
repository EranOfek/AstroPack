function varargout = funCube(ImObj, Args)
    % Apply function/s on a single cube
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'OutIsMat' - If true, return a matrix.
    %                    If false, return an AstroImage.
    %                    Default is false.
    %            'CCDSEC' - [Xmin Xmax Ymin Ymax] to stack.
    %                       If empty, use entire image. Default is
    %                       [].
    %            'FunCube' - A function handle, or a cell array of
    %                   function handles to apply on cube.
    %                   Default is {@mean, @var}.
    %                   All the functions are applayed on the same
    %                   cube. Note that the number of functions
    %                   actually applied is min(nargout,numel(FunCube)).
    %                   Default is {@mean, @var}.
    %            'FunArgs' - If FunCube is a function handle then
    %                   this is a cell array of additional
    %                   arguments to pass to the FunCube function.
    %                   If FunCube is a cell array of function
    %                   handles, then this is a cell array of cell
    %                   arrays of additional arguments for each
    %                   function.
    %                   Default is {{3,'omitnan'}, {[],3,'omitnan'}}.
    %            'DataProp' - Data property in the AStroImage from
    %                   which to extract a cube.
    %                   Default is 'ImageData'.
    %            'DataPropIn' - Data property in the ImageComponent
    %                   from which to extract the image data.
    %                   Default is 'Data'.
    %            'SaveInProp' - A cell array of AstroImage
    %                   properties (one to one correspondence to
    %                   FunCube) in which to save the output
    %                   results.
    %                   If empty, then the output is matrices.
    %                   If provided, then the first output argument
    %                   is an AstroImage with the specific field
    %                   populated with the results of the
    %                   corresponding FunCube results.
    %                   Default is {'ImageData','VarData'}.
    %            'DimIndex' - Dimension along which the cube will
    %                   be constructed. Default is 3.
    % Output : * Arbitrary number of arguments.
    %            Each output contains a matrix of a coadd image that
    %            corresponds to one 'FunCube' function.
    % Author : Eran Ofek (Apr 2021)
    % Example: AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});
    %          [Cube1, Cube2] = imProc.stack.funCube(AI);
    %          [CAI] = imProc.stack.funCube(AI,'SaveInProp',{'ImageData','VarData'});

    arguments
        ImObj                              = [];
        Args.OutIsMat                      = false;
        Args.CCDSEC                        = [];
        Args.FunCube                       = {@mean, @var, @median};
        Args.FunArgs cell                  = {{3,'omitnan'}, {[],3,'omitnan'}, {3,'omitnan'}};
        Args.DataProp char                 = 'ImageData'; %,'BackData', 'VarData', 'MaskData'};
        Args.DataPropIn char               = 'Image';
        Args.SaveInProp                    = {'ImageData','ImageData','ImageData'};
        Args.DimIndex                      = 3;
    end

    [Cube] = imProc.image.images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DataProp',{Args.DataProp}, 'DimIndex',Args.DimIndex);


    if ~iscell(Args.FunCube)
        Args.FunCube = {Args.FunCube};
        Args.FunArgs = {Args.FunArgs};
    end
    Nfun = numel(Args.FunCube);
    Nargout = nargout;
    if Nargout>Nfun
        error('Number of output arguments is larger than number of functions');
    else
        if Nargout<Nfun
            Nfun = Nargout;
        end
    end

    if isempty(Args.SaveInProp)
        Nfun = min(Nfun, nargout);
    else
        Nfun = min(Nfun, numel(Args.SaveInProp));
    end

    varargout = cell(1,Nfun);
    for Ifun=1:1:Nargout
        varargout{Ifun} = AstroImage;
        varargout{Ifun}.(Args.SaveInProp{Ifun}).(Args.DataPropIn) = Args.FunCube{Ifun}(Cube, Args.FunArgs{Ifun}{:});

        if Args.OutIsMat
            varargout{Ifun} = varargout{Ifun}.(Args.SaveInProp{Ifun}).(Args.DataPropIn);
        end
    end

    % if ~isempty(Args.SaveInProp)
    %     Result = AstroImage;
    %     %Nprop = numel(Args.SaveInProp);
    %     for Iprop=1:1:Nargout
    %         Result.(Args.SaveInProp{Iprop}).(Args.DataPropIn) = varargout{Iprop};
    %     end
    %     varargout{1} = Result;
    % end
end
