function [Out, varargout] = moment(Obj, Order, varargin)
    % Return the moment of AstroImage object images.
    %       The output is an array in which each element corresponds to an
    %       element in the AstroImage.
    %       The output arguments corresponds to the Image, Back, Var, Mask,
    %       respectively.
    %       By default NaNs are omitted.
    % Input  : - An AstroImage object.
    %          - Moment order.
    %          * Arbitrary arguments to pass to AstroImage/funUnaryScalar.
    % Output : * By default will return up to 4 output arguments for the
    %            quantile value of the Image, Back, Var and Mask.
    %            Each argument is an array which size equal to the size of
    %            the AstroImage, and each element corresponds to an
    %            AstroImage element.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(10,10), rand(10,10)});
    %          imProc.stat.moment(AI, 3)
    %          [a,b] = imProc.stat.moment(AI, 4)
    %          AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    %          [a,b,c] = imProc.stat.moment(AI, 5)
    
    [Out, varargout{1:nargout-1}] = funUnaryScalar(Obj, @moment, 'OpArgs',{Order, 'all'}, varargin{:});
    
    
end