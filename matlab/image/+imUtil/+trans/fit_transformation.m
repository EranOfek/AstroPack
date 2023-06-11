function Res=fit_transformation(H,Y,varargin)
% 
% Package: mUtil.trans
% Description: In a 2-D image interpolate over NaNs or over pixels in
%              which a bit mask has a specific bit equal true.
% Input  : - 
%          * Arbitrary number of pairs of input arguments ...,key,val,...
%            
% Output : - Interpolated image
% Tested : Matlab R2011b
%     By : Eran O. Ofek                    Feb 2014
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: 



InPar = inputParser;
addOptional(InPar,'ErrY',[]); 
addOptional(InPar,'Mag',[]); 
addOptional(InPar,'MagBin',[1]);
addOptional(InPar,'MinNinBin',3);
addOptional(InPar,'MagRange',[13 19]); 
addOptional(InPar,'MinNbin',3); 
addOptional(InPar,'Nsigma',3);
addOptional(InPar,'Niter',2); 
addOptional(InPar,'InterpMethod','linear'); 
addOptional(InPar,'FitMethod','\');
addOptional(InPar,'FitAlgo','chol'); % chol | orth
parse(InPar,varargin{:});
InPar = InPar.Results;

if ~iscell(H)
    H = {H};
end

N = numel(H);
[Nrow,Ncol] = size(Y);

if N~=Ncol
    error('Number of design matrices must be identical to the number of columns in Y');
end

if isempty(InPar.ErrY)
    InPar.ErrY = ones(Nrow,Ncol);
end

FlagMag = true(Nrow,1);
% first iteration:
Iter = 0;
while Iter<InPar.Niter
    Iter = Iter + 1;
    
    for I=1:1:N
        switch lower(InPar.FitMethod)
            case '\'
                Par{I} = H{I}\Y(:,I);
                ParErr{I} = nan(size(Par{I}));
            case 'lscov'
                [Par{I},ParErr{I}] = lscov(H{I}(FlagMag,:) ,Y(FlagMag,I),1./(InPar.ErrY(FlagMag,I).^2),InPar.FitAlgo);
            otherwise
                error('Unknown FitMethod option');
        end
        Res.AllResid{I} = AlphaDelta(:,I) - H{I}*Par{I};
        
    end

    % second iteration
    if Iter~=Niter && ~isempty(InPar.Mag)
        % calculate the rms as a function of magnitude
        MagEdges = (InPar.MagRange(1):InPar.MagBin:InPar.MagRange(2));
        InPar.Mag

        ResidTotal = sqrt(Res.Resid{1}.^2 + Res.Resid{2}.^2);

        B = timeSeries.bin.binning([InPar.Mag, ResidTotal] ,InPar.MagBin,InPar.MagRange,{'MidBin',@median,@Util.stat.rstd,@numel});

        Flag = B(:,4) >= InPar.MinNinBin;
        if (sum(Flag)>=InPar.MinNbin)
            MeanResid = interp1(B(Flag,1),B(Flag,2),InPar.Mag,InPar.InterpMethod);
            StdResid  = interp1(B(Flag,1),B(Flag,2),InPar.Mag,InPar.InterpMethod);

            FlagMag = ResidTotal<(MeanResid + InPar.Nsigma.*StdResid);

            InPar.ErrY    = MeanResid;
            
            MinStd = min(B(:,2));

        else
            % failed
            MeanResid = NaN;
            FlagMag   = true(Nrow,1);
            MinStd    = NaN;
        end
    else
        FlagMag = true(Nrow,1);
    end
end
    
for I=1:1:2
    Res.Par{I}      = Par{I};
    Res.ParErr{I}   = ParErr{I};
    Res.AllResid{I} = AlphaDelta(:,I) - H{I}*Par{I};
    Res.AllStd{I}   = std(Res.AllResid{I});
    Res.AllRStd{I}  = imUtil.background.rvar(Res.AllResid{I});
    Res.Flag        = FlagMag;
    
    Res.Resid{I}    = AlphaDelta(FlagMag,I) - H{I}(FlagMag,:)*Par{I};
    Res.Std{I}      = std(Res.Resid{I});
    Res.RStd{I}     = imUtil.background.rvar(Res.Resid{I});
    Res.ResidTotal  = sqrt(Res.Resid{1}.^2 + Res.Resid{2}.^2);
    Res.Chi2        = sum((Res.ResidTotal./MeanResid).^2);
    Res.Nobs        = sum(FlagMag);
    Res.MinStd      = MinStd;
    
end