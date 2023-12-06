function [Out, varargout] = range(Obj, varargin)
    % Return the range of AstroImage object images.
    %       The output is an array in which each element corresponds to an
    %       element in the AstroImage.
    %       The output arguments corresponds to the Image, Back, Var, Mask,
    %       respectively.
    %       By default NaNs are omitted. A NaN value indicate the image is
    %       empty.
    % Input  : - An AstroImage object.
    %          * Arbitrary arguments to pass to AstroImage/funUnaryScalarWithMask.
    % Output : * By default will return up to 4 output arguments for the
    %            range value of the Image, Back, Var and Mask.
    %            Each argument is an array which size equal to the size of
    %            the AstroImage, and each element corresponds to an
    %            AstroImage element.
    % Author : Eran Ofek (May 2021)
    % Example: AI = AstroImage({rand(10,10), rand(10,10)});
    %          imProc.stat.range(AI)
    %          [a,b] = imProc.stat.range(AI)
    %          AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    %          [a,b,c] = imProc.stat.range(AI)
    %          range on non saturated pixels
    %          [a,b]=imProc.stat.range(AI,'BitNames',{'Saturated'},'UseNot',true)
    
    [Out, varargout{1:nargout-1}] = funUnaryScalarWithMask(Obj, @range, 'OpArgs',{'all'}, varargin{:});
    
end