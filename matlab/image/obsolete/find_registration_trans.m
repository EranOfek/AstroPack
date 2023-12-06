function find_registration_trans(Cat,Ref,varargin)
%
% Example: imUtil.patternMatch.find_registration_trans


InPar = inputParser;

addOptional(InPar,'StepX',3);
addOptional(InPar,'StepY',3);

addOptional(InPar,'MaxMethod','thresh_fracmax');
addOptional(InPar,'Threshold',5);
addOptional(InPar,'FracOfMax',0.8);

addOptional(InPar,'CatColX',1);
addOptional(InPar,'CatColY',2);
addOptional(InPar,'RefColX',1);
addOptional(InPar,'RefColY',2);

parse(InPar,varargin{:});
InPar = InPar.Results;



if nargin==0
    % simulation mode
    
    
    Nstar = 1000;
    Ref = rand(Nstar,2).*2048 - 1024;
    Cat = Ref.*[1 -1];
    
%     Ref = sortrows(Ref,2);
%     Noverlap = 100;
%     Cat = [Ref(1:Noverlap,1), Ref(1:Noverlap,2)];
%     Cat = [Cat; rand(Nstar-Noverlap,2).*2048];
%     Cat(:,1) = Cat(:,1) + 520 + randn(Nstar,1).*0.3;
%     Cat(:,2) = Cat(:,2) + 430 + randn(Nstar,1).*0.3;
%     Cat      = sortrows(Cat,2);
%     Ref      = Ref.*[1 -1];


end

CatRangeX = [min(Cat(:,InPar.CatColX)), max(Cat(:,InPar.CatColX))];
CatRangeY = [min(Cat(:,InPar.CatColY)), max(Cat(:,InPar.CatColY))];
RefRangeX = [min(Ref(:,InPar.RefColX)), max(Ref(:,InPar.RefColX))];
RefRangeY = [min(Ref(:,InPar.RefColY)), max(Ref(:,InPar.RefColY))];


[ImCat,ImCatVecX,ImCatVecY,ImCatBinX,ImCatBinY]=imUtil.patternMatch.hist2d(Cat(:,InPar.CatColX),Cat(:,InPar.CatColY),CatRangeX,CatRangeY,InPar.StepX,InPar.StepY);

[ImRef,ImRefVecX,ImRefVecY,ImRefBinX,ImRefBinY]=imUtil.patternMatch.hist2d(Ref(:,InPar.RefColX),Ref(:,InPar.RefColY),RefRangeX,RefRangeY,InPar.StepX,InPar.StepY);


[optimizer, metric] = imregconfig('monomodal');
tform = imregtform(ImCat, ImRef, 'affine', optimizer, metric);

tform.T

