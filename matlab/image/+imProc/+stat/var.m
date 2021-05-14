function [Out, varargout] = var(Obj, varargin)
    % Return the var of AstroImage object images.
    %       The output is an array in which each element corresponds to an
    %       element in the AstroImage.
    %       The output arguments corresponds to the Image, Back, Var, Mask,
    %       respectively.c
    %       By default NaNs are omitted.
    % Input  : - An AstroImage object.
    %          * Arbitrary arguments to pass to AstroImage/funUnaryScalar.
    % Output : * By default will return up to 4 output arguments for the
    %            var value of the Image, Back, Var and Mask.
    %            Each argument is an array which size equal to the size of
    %            the AstroImage, and each element corresponds to an
    %            AstroImage element.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(10,10), rand(10,10)});
    %          imProc.stat.var(AI)
    %          [a,b] = imProc.stat.var(AI)
    %          AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    %          [a,b,c] = imProc.stat.var(AI)
    
    [Out, varargout{1:nargout-1}] = funUnaryScalar(Obj, @var, 'OpArgs',{[],'all','omitnan'}, varargin{:});
    
    
end