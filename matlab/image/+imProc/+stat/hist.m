function [varargout] = hist(Obj, DataProp, ArgsHC)
    % Plot the histogram of a single property in a single AstroImage object image.
    % Input  : - A single element AstroImage or ImageComponent object.
    %          - Property name. Default is 'Image'. If empty, use default.
    %          * A cell array of arbitrary number of arguments to pass to
    %            histogram.m. Default is {}.
    %            If one argument, then do not need to put in cell.
    % Output : - histogram handle.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
    %          imProc.stat.hist(AI)
    %          imProc.stat.hist(AI,[],100)
    %          imProc.stat.hist(AI,[],{100,'Normalization','cdf'})
    
    arguments
        Obj(1,1)
        DataProp                   = 'Image';
        ArgsHC                     = {};
    end
    
    if ~iscell(ArgsHC)
        ArgsHC = {ArgsHC};
    end
    
    if isempty(DataProp)
        DataProp = 'Image';
    end
    
    [varargout{1:nargout}] = histogram(Obj.(DataProp), ArgsHC{:});
    
end