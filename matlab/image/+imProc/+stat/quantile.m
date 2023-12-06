function [Out, varargout] = quantile(Obj, Quant, varargin)
    % Return the quantile of AstroImage object images.
    %       The output is an array in which each element corresponds to an
    %       element in the AstroImage.
    %       The output arguments corresponds to the Image, Back, Var, Mask,
    %       respectively.
    %       By default NaNs are omitted.
    % Input  : - An AstroImage object.
    %          - Quantile (between 0 and 1).
    %          * Arbitrary arguments to pass to AstroImage/funUnaryScalarWithMask.
    % Output : * By default will return up to 4 output arguments for the
    %            quantile value of the Image, Back, Var and Mask.
    %            Each argument is an array which size equal to the size of
    %            the AstroImage, and each element corresponds to an
    %            AstroImage element.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(10,10), rand(10,10)});
    %          imProc.stat.quantile(AI, 0.2)
    %          [a,b] = imProc.stat.quantile(AI, 0.3)
    %          AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    %          [a,b,c] = imProc.stat.quantile(AI, 0.95)
    %          quantile on non saturated pixels
    %          [a,b]=imProc.stat.quantile(AI,'BitNames',{'Saturated'},'UseNot',true)
    
    [Out, varargout{1:nargout-1}] = funUnaryScalarWithMask(Obj, @quantile, 'OpArgs',{Quant, 'all'}, varargin{:});
    
    
end