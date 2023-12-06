function Result = applyUnaryFun(ImObj, Offset, Operator, Args)
    % Applay scalar-unary function (e.g., function that returns a scalar) on AstroImage
    % Input  : - An AstroImage object.
    %          - An AstroImage object, or a cell array of matrices
    %            (images) or scalars, or a vector of scalars, or a
    %            function_handle.
    %            If an array, then will be converted to a cell of
    %            scalars using num2cell. Each cell element will be
    %            applied (using the operator) on the AstroImage. If one cell
    %            element then will be applied on all images.
    %            If function_handle (unary function) then it will
    %            be used to calculate the offset using this
    %            function.
    %          - Operator to use - e.g., subtracting the offset or
    %            dividing the factor. Default is @minus).
    %          * ...,key,val,...
    %            'OpArgs' - A cell array of arguments to pass to
    %                   the Offset operator (if operator is provided).
    %            'PreDivide' - A logical indicating if to take
    %                   the inverse of the Offset prior to applying
    %                   the operator. This can be used to speed up
    %                   performences (in division). Default is false.
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
    %          R  = imProc.stack.applyUnaryFun(AI,1);
    %          R  = imProc.stack.applyUnaryFun(AI,[1 2]);
    %          R  = imProc.stack.applyUnaryFun(AI,{1 2}); % the same
    %          R  = imProc.stack.applyUnaryFun(AI,AI); 
    %          R  = imProc.stack.applyUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
    %          R  = imProc.stack.applyUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});


    arguments
        ImObj                         = [];
        Offset                        = [];   % AstroImage, cell of matrices, or array
        Operator function_handle      = @minus;
        Args.OpArgs cell              = {};
        Args.PreDivide(1,1) logical   = false;
        Args.DataProp char            = 'ImageData';
        Args.DataPropIn char          = 'Data';
        Args.CreateNewObj             = [];
    end

    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end

    if isa(Offset,'function_handle')
        % Offset is an operator
        Offset = funUnaryScalar(ImObj, Offset, 'OpArgs',Args.OpArgs, 'DataProp',{Args.DataProp}, 'DataPropIn',{Args.DataPropIn});
    end
    if isnumeric(Offset)
        Offset = num2cell(Offset);
    end

    if Args.CreateNewObj
        Result = ImObj.copy();
    else
        Result = ImObj;
    end
    Nobj = numel(ImObj);
    Noff = numel(Offset);
    for Iobj=1:1:Nobj
        Ioff = min(Iobj, Noff);
        if iscell(Offset)
            Tmp = Offset{Ioff};
        else
            Tmp = Offset(Ioff).(Args.DataProp).(Args.DataPropIn);
        end
        if Args.PreDivide
            Tmp = 1./Tmp;
        end

        Result(Iobj).(Args.DataProp).(Args.DataPropIn) = Operator(ImObj(Iobj).(Args.DataProp).(Args.DataPropIn), Tmp);
    end
end
