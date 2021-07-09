function [Flag,Ind]=flag_overdense_colrow(Cat,varargin)
% Flag sources that are found in overdense (box shaped) regions
% Package: +imUtil.cat
% Description: Given a catalog of [X,Y] positions, flag sources that are
%              found in regions which their density is above a threshold.
%              The function uses two algorithms:
%              1. source number in row/column above threshold number.
%              2. source number in row/column is Nsigma above background.
%              One or both algorithms can be used.
% Input  : - [X,Y] catalog.
%          * Pairs of ...,key,val,... The following keywords are avaialble:
%            'Dim' - Dimension overwhich to calculate the histogram.
%                   if 1, then willl look for overdensity in columns, if 2
%                   in rows. Default is 1.
%            'MaxNinBox' - The threshold of the number of sources found in
%                   a column/row of size [Step] above which to flag the
%                   source.
%                   If empty, then the first algorithm (source number in
%                   box above threshold number) will not be used.
%                   Default is [].
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
%            'Step' - histogram step size.
%                   Default is 3.
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
%            input catalog is in overdense line/row.
%          - An integer indicating to which line/row the source belongs.
%            0 if source is not in an overdense region.
%            Note that if both algorithms are used then the flagging from
%            the second algorith will dominate.
%      By: Eran O. Ofek                         May 2020
% Example: Cat=rand(100,2).*1000; Cat=[Cat;rand(50,2).*[1 1000]+[10 0] ]; 
%          [Flag,Ind]=imUtil.cat.flag_overdense_colrow(Cat)


InPar = inputParser;

addOptional(InPar,'Dim',1);  % 1 columns, 2 rows
addOptional(InPar,'MaxNinBox',[]);
addOptional(InPar,'Nsigma',10);
addOptional(InPar,'MinStd',0.5);
addOptional(InPar,'MeanFun',@median);      % NOTE: nanmedian is ~x7 slower than median with 'omitnan'
addOptional(InPar,'MeanFunPar',{'all','omitnan'});
addOptional(InPar,'StdFun',@imUtil.background.rstd);
addOptional(InPar,'StdFunPar',{});

addOptional(InPar,'Step',3);
addOptional(InPar,'CCDSEC',[]);  % or [min max]

addOptional(InPar,'ColX',1);
addOptional(InPar,'ColY',2);
addOptional(InPar,'Delta',0.5);
parse(InPar,varargin{:});
InPar = InPar.Results;


if isempty(InPar.CCDSEC)
    InPar.CCDSEC = [min(Cat(:,InPar.ColX))-InPar.Delta, max(Cat(:,InPar.ColX))+InPar.Delta, min(Cat(:,InPar.ColY))-InPar.Delta, max(Cat(:,InPar.ColY))+InPar.Delta];
end
if numel(InPar.CCDSEC)==4
    if InPar.Dim==1
        Range = InPar.CCDSEC(1:2);
    elseif InPar.Dim==2
        Range = InPar.CCDSEC(3:4);
    else
        error('Dim must be 1 or 2');
    end
else
    Range = InPar.CCDSEC;
end

if InPar.Dim==1
    Src = Cat(:,InPar.ColX);
elseif InPar.Dim==2
    Src = Cat(:,InPar.ColY);
else
    error('Dim must be 1 or 2');
end
    



Ncat = size(Cat,1);
Edges = (Range(1):InPar.Step:(Range(2)+InPar.Step));
[Nh,~,BinX] = histcounts(Src,Edges);


Flag = false(Ncat,1);
if ~isempty(InPar.MaxNinBox)

    I = find(Nh>InPar.MaxNinBox);

    Nbox = numel(I);
    Flag = false(Ncat,1);
    Ind  = zeros(Ncat,1);
    for Ibox=1:1:Nbox
        NewFlag = BinX==I(Ibox);
        Flag = Flag | NewFlag;
        Ind(NewFlag) = Ibox;
    end

end


if ~isempty(InPar.Nsigma)
    Mean = InPar.MeanFun(Nh,InPar.MeanFunPar{:});
    Std  = InPar.StdFun(Nh,InPar.StdFunPar{:});
    Std  = max(Std,InPar.MinStd);
    
    I = find(Nh>(Mean+Std.*InPar.Nsigma));
    Nbox = numel(I);

    Ind  = zeros(Ncat,1);
    for Ibox=1:1:Nbox
        NewFlag = BinX==I(Ibox);
        Flag = Flag | NewFlag;
        Ind(NewFlag) = Ibox;
    end

end
