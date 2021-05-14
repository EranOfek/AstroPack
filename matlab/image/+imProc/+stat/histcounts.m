function [N, varargout] = histcounts(Obj, DataProp, ArgsHC)
    % Return the hist of a single property in a single AstroImage object image.
    %       The output is an array in which each element corresponds to an
    %       element in the AstroImage.
    %       The output arguments corresponds to the Image, Back, Var, Mask,
    %       respectively.c
    %       By default NaNs are omitted.
    % Input  : - A single element AstroImage or ImageComponent object.
    %          - Property name. Default is 'Image'. If empty, use default.
    %          * A cell array of arbitrary number of arguments to pass to
    %            histcounts.m. Default is {}.
    % Output : * By default will return up to 4 output arguments for the
    %            std value of the Image, Back, Var and Mask.
    %            Each argument is an array which size equal to the size of
    %            the AstroImage, and each element corresponds to an
    %            AstroImage element.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
    %          [a,b,c] = imProc.stat.histcounts(AI)
    
    arguments
        Obj(1,1)
        DataProp                   = 'Image';
        ArgsHC cell                = {};
    end
    
    if isempty(DataProp)
        DataProp = 'Image';
    end
    
    [N, varargout{1:nargout-1}] = histcounts(Obj.(DataProp), ArgsHC{:});
    
    
end