function OutImage = imTransform(Image, Tran, Args)
% Image transformation
% 
% Example: TC=Tran2D; TC.ParY=zeros(1,13);  TC.ParX=zeros(1,13); 
%          TC.ParX(1:2) = 1; TC.ParX(5)=0.03; TC.ParX(7)=0.01;
%          TC.ParY(1) = 2; TC.ParY(3)=1.01; TC.ParY(5)=0.01; TC.ParY(8)=0.001;
%          Image = rand(1000,1000);
%          OutImage = imUtil.trans.imTransform(Image, TC);


arguments
    Image
    Tran
    Args.TranDir         = {@forward, @backward};
    Args.InterpMethod    = 'makima';
    Args.ExtrapFill      = NaN;
    Args.InCCDSEC        = [];
    Args.OutCCDSEC       = [];
    Args.Xin             = [];
    Args.Yin             = [];
end

if ~iscell(Args.TranDir)
    Args.TranDir = {Args.TranDir};
end

if isa(Tran,'Tran2D')
    % do nothing
else
    % FFU: transform other formats to Tran2D
end

% trim image by InCCDSEC
if ~isempty(Args.InCCDSEC)
    Image = Image(Args.InCCDSEC(3):Args.InCCDSEC(4), Args.InCCDSEC(1):Args.InCCDSEC(2));
end


SizeYX = size(Image);
% prepare the Xin/Yin vectors
if isempty(Args.Xin)
    Args.Xin = (1:1:SizeYX(2));
end
if isempty(Args.Yin)
    Args.Yin = (1:1:SizeYX(1));
end

% Prepare Xin/Yin full grid
[MatX, MatY] = meshgrid(Args.Xin, Args.Yin);

% transform the coordinates
% FFU: slow
Ntran = numel(Tran);
X     = MatX(:);
Y     = MatY(:);
for Itran=1:1:Ntran
    [X ,Y] = Args.TranDir{Itran}(Tran(Itran),[X, Y]);
end
TranMatX = reshape(X, SizeYX);
TranMatY = reshape(Y, SizeYX);

% 2D interpolation

OutImage = interp2(Args.Xin(:).', Args.Yin(:), Image, TranMatX, TranMatY, Args.InterpMethod, Args.ExtrapFill);

%OutImage = interp2(TranMatX, TranMatY, Image, MatX, MatY, Args.InterpMethod, Args.ExtrapFill);

% Output CCDSEC
if ~isempty(Args.OutCCDSEC)
    OutImage = OutImage(Args.OutCCDSEC(3):Args.OutCCDSEC(4), Args.OutCCDSEC(1):Args.OitCCDSEC(2));
end



