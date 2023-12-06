function Result = divideFactor(ImObj, Factor, Args)
    % Divide factor (constant) from AstroImage
    % Input  : - An AstroImage object.
    %          - An AstroImage object, or a cell array of matrices
    %            (images) or scalars, or a vector of scalars, or a
    %            function_handle.
    %            If an array, then will be converted to a cell of
    %            scalars using num2cell. Each cell element will be
    %            divided from the AstroImage. Of one cell
    %            element then will be subtracted from all images.
    %            If function_handle (unary function) then it will
    %            be used to calculate the factor using this
    %            function.
    %          * ...,key,val,...
    %            'OpArgs' - A cell array of arguments to pass to
    %                   the Offset operator (if operator is provided).
    %            'DataProp' - The data property in the AstroImage
    %                   (both input and offset) that contains the data.
    %                   Default is 'ImageData'.
    %            'DataPropIn' - The data property in the
    %                   ImageComponent that contains the image
    %                   data. Default is 'Data'.
    %            'CreateNewObj' - A logical indicating if the
    %                   output is a new object. If empty, then will
    %                   check the number of output argumnets. If
    %                   zero (e.g., C.subtractOffset(...)) then
    %                   will update the input image object. Else,
    %                   will create a new object.
    % Output : - An AstroImage object.
    % Author : Eran Ofek (Apr 2021)
    % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    %          R  = imProc.stack.divideFactor(AI,1);
    %          R  = imProc.stack.divideFactor(AI,[1 2]);
    %          R  = imProc.stack.divideFactor(AI,{1 2}); % the same
    %          R  = imProc.stack.divideFactor(AI,AI); 
    %          R  = imProc.stack.divideFactor(AI,@mean,'OpArgs',{'all'});

    arguments
        ImObj                     = [];
        Factor                    = [];   % AstroImage, cell of matrices, or array
        Args.OpArgs cell          = {};
        Args.DataProp char        = 'ImageData';
        Args.DataPropIn char      = 'Data';
        Args.CreateNewObj         = [];
    end

    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end

    Result  = imProc.stack.applyUnaryFun(ImObj, Factor, @times, 'OpArgs',{'all'}, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn, 'CreateNewObj',Args.CreateNewObj, 'PreDivide',true);

end
