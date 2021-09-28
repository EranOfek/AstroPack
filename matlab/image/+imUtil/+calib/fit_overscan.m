function [OverScan,ImageOSS,X,Y]=fit_overscan(Image,varargin)
% fit the overscan region in an image and subtract
% Package: +imUtil.calib
% Description: Given an image and an the coordinates of the over scan
%              region in CCDSEC format (i.e., [Xmin Xmax Ymin Ymax]),
%              calculate the overscan 1-D function, and optionally subtract
%              it from the image.
% Input  : - A cube of images in which the image index is in the 3rd
%            dimension.
%            If not provided then will run in simulation mode.
%          * Pairs of ...,key,val,... arguments. Options are:
%            'Dim' - Dimension over which to average the overscan.
%                   If empty, then will attempt to guess this by taking the
%                   dimension in 'OverSec' over which the number of pixels
%                   is maximal.
%                   Default is empty.
%            'OverSec' - A vector of [Xmin Xmax Ymin Ymax] of the overscan
%                   region. This parameter must be provided.
%                   Default is empty.
%            'MeanFun' - A function handle for collapsing the overscan
%                   region over the 'Dim' dimension.
%                   Default is @median.
%            'MeanFunPar' - A cell array of additional parameters to pass
%                   to the 'MeanFun'. If the dimension is given, then it
%                   must be 1. Default is {1,'omitnan'}.
%            'SmoothFun' - A function handle to use for smoothing the
%                   collapsed overscan vector.
%                   If empty, then do not smooth.
%                   Default is empty.
%            'SmoothFunPar' - A cell array of additional parameters to pass
%                   to the 'SmoothFun'. If the dimension is given, then it
%                   must be 1. Default is {}.
%            'RemoveOS' - A logical indicating if to remove the overscan
%                   region from the image.
%                   Default is true.
%            'ReadNoise' - Detector readnoise in units in which the image
%                   is provided. Default is 10.
%            'ThresholdDiffSigma' - In the diff of the un-smoothed overscan
%                   rows in which the abs value of the diffrence is above a
%                   threshold are flaged.
%                   The threshold is calculated from ReadNoise/sqrt(NpixOS)
%                   *ThresholdDiffSigma, whree NpixOS is the number of
%                   pixels in the overscan.
%                   Default is 8.
% Output : - A structure with the following fields:
%            .OverScanSmooth - The smooth overscan.
%            .OverScanVec - The collapse overscan before smoothing.
%            .OverScanDiff - the diff of OverScanVec, where te first
%                   element is NaN.
%            .Threshold - The threshold in units of the image.
%            .FlagLargeDiff - A vector of logical indicating if a
%                   row/column in the overscan has a jump which is more than
%                   ThresholdDiffSigma above the expected noise.
%          - Overscan subtrcated image.
%          - Vector of X indices in in which the non-overscan image reside.   
%          - Vector of Y indices in in which the non-overscan image reside.
%      By: Eran O. Ofek                         Jun 2020
% Example: imUtil.calib.fit_overscan(rand(10,10),'OverSec',[1 3 1 10])

InPar = inputParser;
addOptional(InPar,'Dim',[]);
addOptional(InPar,'OverSec',[]);
addOptional(InPar,'MeanFun',@median);
addOptional(InPar,'MeanFunPar',{1,'omitnan'});  % dim is always 1
addOptional(InPar,'SmoothFun',[]);
addOptional(InPar,'SmoothFunPar',{});
addOptional(InPar,'RemoveOS',true);
addOptional(InPar,'ReadNoise',10);
addOptional(InPar,'ThresholdDiffSigma',8);

parse(InPar,varargin{:});
InPar = InPar.Results;

if isempty(InPar.OverSec)
    error('OverSec input argument must be provided');
end

if isempty(InPar.Dim)
    % try to get dimension for which the overscan is provided given the
    % OverSec
    % vector
    % For example: [4501 4600 1 1024] will chose the x-axis (Dim=2) because the
    % range of the x-axis is maller (100 vs. 1024).

    DX = InPar.OverSec(2)-InPar.OverSec(1);
    DY = InPar.OverSec(4)-InPar.OverSec(3);
    
    if DX>DY
        InPar.Dim = 1;
    else
        InPar.Dim = 2;
    end
    
end

% extract overscan region
OverScanData = Image(InPar.OverSec(3):InPar.OverSec(4), InPar.OverSec(1):InPar.OverSec(2));

% make sure that the dim over which to average the data is Dim=1
if InPar.Dim==2
    OverScanData = OverScanData.';
end

% number of pixels in the overscan dimension
NpixOS = size(OverScanData,1);


% mean over overscan
OverScanVec = InPar.MeanFun(OverScanData,InPar.MeanFunPar{:});

% search gradients in overscan


% smooth overscan
if ~isempty(InPar.SmoothFun)
    OverScanSmooth = InPar.SmoothFun(OverScanVec,InPar.SmoothFunPar{:});
else
    OverScanSmooth = OverScanVec;
end
OverScanVec    = OverScanVec(:);
OverScanSmooth = OverScanSmooth(:);

if nargout>1
    if InPar.Dim==1
        OverScanSmooth = OverScanSmooth.';
    end
    
    % overscan subtracted image
    ImageOSS = Image - OverScanSmooth;
    
    if InPar.RemoveOS
        % remove overscan region
        SizeIm = size(Image);
        X = (1:1:SizeIm(2));
        Y = (1:1:SizeIm(1));
        if InPar.Dim==1
            Y = Y(Y<InPar.OverSec(3) | Y>InPar.OverSec(4));
        else
            X = X(X<InPar.OverSec(1) | X>InPar.OverSec(2));
        end
        ImageOSS = ImageOSS(Y,X);
    end
end
        
OverScan.OverScanSmooth = OverScanSmooth(:);
OverScan.OverScanVec    = OverScanVec;
OverScan.OverScanDiff   = [NaN;diff(OverScanVec)];
OverScan.Threshold      = (InPar.ReadNoise./sqrt(NpixOS)) .* InPar.ThresholdDiffSigma;
OverScan.FlagLargeDiff  = abs(OverScan.OverScanDiff)> OverScan.Threshold;


    
    

    

