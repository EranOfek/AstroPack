function [Flag,Ind]=flag_overdense(Cat,varargin)
% Flag sources that are found in overdense (box shaped) regions
% Package: +imUtil.cat
% Description: Given a catalog of [X,Y] positions, flag sources that are
%              found in regions which their density is above a threshold.
%              The function uses two algorithms:
%              1. source number in box above threshold number.
%              2. source number in box is Nsigma above background.
%              One or both algorithms can be used.
% Input  : - [X,Y] catalog.
%          * Pairs of ...,key,val,... The following keywords are avaialble:
%            'MaxNinBox' - The threshold of the number of sources found in
%                   a box of size [StepX, StepY] above which to flag the
%                   source.
%                   If empty, then the first algorithm (source number in
%                   box above threshold number) will not be used.
%                   Default is 10.
%            'Nsigma' - This is the threshold for the second algorithm
%                   (source number in box is Nsigma above background), in
%                   units of the number of sigmas above background.
%                   If empty. then this algorithm will not be used.
%                   Default is 10.
%            'MinStd' - Minimum std in the second method, in order to
%                   avoide zero std. Default is 0.5.
%            'MeanFun' - Function handle to use in the background
%                   estimation. Default is @median.
%            'MeanFunPar' - A cell array of parameters to passt to the
%                   'MeanFun'. Default is {'all','omitnan'}.
%            'StdFun' - Function handle to use in the std
%                   estimation. Default is @imUtil.background.rstd.
%            'StdFunPar' - A cell array of parameters to passt to the
%                   'MeanFun'. Default is {}.
%            'StepX' - X box size for source counting.
%                   Default is 10.
%            'StepY' - Y box size for source counting.
%                   Default is 10.
%            'CCDSEC' - [Xmin Xmax Ymin Ymax] of image in which to do the
%                   source counting. This region must include all sources.
%                   If empty, then use min/max of coordinates -/+ 'Delta'.
%                   Default is [].
%            'ColX' - X column in catalog. Default is 1.
%            'ColY' - Y column in catalog. Default is 2.
%            'Delta' - The Delta to add to min/max coordinates in case
%                   CCDSEC is empty.
%                   Default is 0.5.
% Output : - A logical flag indicating if the corresponding source in the
%            input catalog is in overdense region.
%          - An integer indicating to which box the source belongs.
%            0 if source is not in an overdense region.
%            Note that if both algorithms are used then the flagging from
%            the second algorith will dominate.
%      By: Eran O. Ofek                         May 2020
% Example: Cat=rand(100,2).*1000; Cat=[Cat;rand(20,2).*10]; Cat=[Cat;rand(20,2).*10+[50 100]];
%          Flag=imUtil.cat.flag_overdense(Cat)


InPar = inputParser;

addOptional(InPar,'MaxNinBox',10);
addOptional(InPar,'Nsigma',10);
addOptional(InPar,'MinStd',0.5);
addOptional(InPar,'MeanFun',@median);
addOptional(InPar,'MeanFunPar',{'all','omitnan'});
addOptional(InPar,'StdFun',@imUtil.background.rstd);
addOptional(InPar,'StdFunPar',{});
addOptional(InPar,'StepX',10);
addOptional(InPar,'StepY',10);
addOptional(InPar,'CCDSEC',[]);

addOptional(InPar,'ColX',1);
addOptional(InPar,'ColY',2);
addOptional(InPar,'Delta',0.5);
parse(InPar,varargin{:});
InPar = InPar.Results;


if isempty(InPar.CCDSEC)
    InPar.CCDSEC = [min(Cat(:,InPar.ColX))-InPar.Delta, max(Cat(:,InPar.ColX))+InPar.Delta, min(Cat(:,InPar.ColY))-InPar.Delta, max(Cat(:,InPar.ColY))+InPar.Delta];
end

Ncat = size(Cat,1);
%[Mat,~,~,BinX,BinY] = imUtil.patternMatch.hist2d(Cat(:,InPar.ColX),Cat(:,InPar.ColY), InPar.CCDSEC(1:2),InPar.CCDSEC(3:4), InPar.StepX, InPar.StepY);
[Mat,~,~,BinY,BinX] = tools.array.hist2d_fast(Cat(:,InPar.ColY),Cat(:,InPar.ColX), InPar.CCDSEC(3:4),InPar.CCDSEC(1:2), InPar.StepY, InPar.StepX);



Flag = false(Ncat,1);
if ~isempty(InPar.MaxNinBox)

    [I,J]=imUtil.image.ind2sub_fast(size(Mat),find(Mat>InPar.MaxNinBox));
    Nbox = numel(I);

    Ind  = zeros(Ncat,1);
    for Ibox=1:1:Nbox
        NewFlag = BinY==I(Ibox) & BinX==J(Ibox);
        Flag = Flag | NewFlag;
        Ind(NewFlag) = Ibox;
    end

end

if ~isempty(InPar.Nsigma)
    Mean = InPar.MeanFun(Mat,InPar.MeanFunPar{:});
    Std  = InPar.StdFun(Mat,InPar.StdFunPar{:});
    Std  = max(Std,InPar.MinStd);
    
    [I,J]=imUtil.image.ind2sub_fast(size(Mat),find(Mat>(Mean+Std.*InPar.Nsigma)));
    Nbox = numel(I);

    Ind  = zeros(Ncat,1);
    for Ibox=1:1:Nbox
        NewFlag = BinY==I(Ibox) & BinX==J(Ibox);
        Flag = Flag | NewFlag;
        Ind(NewFlag) = Ibox;
    end

end
    