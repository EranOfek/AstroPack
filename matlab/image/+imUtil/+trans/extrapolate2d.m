function [Val,Tran]=extrapolate2d(X,Y,Z,XI,YI,varargin)
%
% Example: imUtil.trans.extrapolate2d([1 1 1000 1000],[1 1000 1 1000],[1 1 1000 1000].*2 + 1)


X = X(:);
Y = Y(:);
Z = Z(:);


InPar = inputParser;
addOptional(InPar,'TranName','Poly1');
addOptional(InPar,'Tran',[]);
addOptional(InPar,'TranDir',@forward);
parse(InPar,varargin{:});
InPar = InPar.Results;

if isempty(InPar.Tran)
    Tran = tran2dCl(InPar.TranName);
else
    Tran = InPar.Tran;
end


Tran = fit_simple(Tran,[X Y],Z)
Tran.ParX
Tran.ParY

[OutX, OutY] = InPar.TranDir(Tran,[XI, YI])

