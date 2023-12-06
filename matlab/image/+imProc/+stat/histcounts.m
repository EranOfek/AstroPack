function [N, varargout] = histcounts(Obj, DataProp, ArgsHC)
    % Return the histcounts of a single property in a single AstroImage object image.
    % Input  : - A single element AstroImage or ImageComponent object.
    %          - Property name. Default is 'Image'. If empty, use default.
    %          * A cell array of arbitrary number of arguments to pass to
    %            histcounts.m. Default is {}.
    % Output : - N (see histcounts)
    %          - Edges (see histcounts)
    %          - Bin (see histcounts)
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