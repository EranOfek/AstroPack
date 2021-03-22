function [XC,Res,ResPeak]=xcorr2_fft(Mat1,Mat2,Args)
% Cross-correlation of two matrices using fft, and search local maxima.
% Package: imUtil.filter
% Description: Cross correlate two 2D images. If a single image is provided
%              then calculate the autocorrelation function.
%              The cross-correlation is done using fft (unlike xcorr2.m).
% Input  : - A 2D image.
%          - A 2D reference image.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'NormMethod' - Method by which to normalize the cross
%                     correlation. Options are:
%                     'StD' - by the std of the data (i.e., xcorr
%                           definition).
%                     'max' - By the maximum of the data.
%                           This may be useful when the noise properties
%                           are not well defined.
%                     Default is 'StD'.
%            'MaxMethod' - The method by which the 2D histogram peaks will
%                     be selected. The following options are available:
%                     'thresh' - Select maxima larger than some threshold.
%                     'max1' - Select the highest maxima.
%                     'maxall' - Select all the maxima.
%                     'max_fracmax' - Select all the maxima above a
%                               fraction (given by FracOfMax) of the
%                               highest maximum.
%                     'thresh_fracmax' - Select all the maxima above the
%                               threshold and above a
%                               fraction (given by FracOfMax) of the
%                               highest maximum.
%                     Alternatively this can be a positive integer (N).
%                     In this case will return the N highest maxima.
%                     Default is 'thresh_fracmax'
%            'Threshold' - Peak selection threshold relative to the S/N.
%                     Default is 5.
%            'FracOfMax' - The parameter that that used in '*_fracmax' and
%                     for selecting peaks.
%                     Only peaks that above the maximal peak multiplied by
%                     this parameter will be returned.
%            'Conn' - local maxima finding connectivity parameter.
%                     For details see imUtil.image.local_maxima.
%                     Default is 8.
%            'SubBack' - Subtract background and divide by std the image
%                     prior to the autocorrelation.
%                     Default is true.
%            'BackFun' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is @nanmedian.
%            'BackFunPar' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is {'all'}.
%            'VarFun' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is @imUtil.backgroundau.rvar.
%            'VarFunPar' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is {}.
%            'SubSizeXY' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is [128 128].
%            'OverlapXY' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is [16 16].
%            'MinVariance' - The minimum variance in in the 2D histogram,
%                     That is used to calculate the S/N.
%                     Default is 0.
% Output : - The cross correlation matrix, where zero shift is at the
%            center of the matrix.
%          - A structure containing information about the cross-corr
%            matrix. The following fields are available:
%            'VecX' - Vector of X shifts corresponding to the X-axis of the
%                   output matrix.
%            'VecY' - Vector of Y shifts corresponding to the Y-axis of the
%                   output matrix.
%            'X0'   - Index of the 0 shift X-axis position in the
%                   cross-corr matrix.
%            'Y0'   - Index of the 0 shift Y-axis position in the
%                   cross-corr matrix.
%          - A structure array containing information about the local
%            maxima of the cross-correlation matrix.
%            The following fields are available: 
%            'StD' - StD of the cross-correlation matrix.
%            'MaxVal' - Value of peak
%            'MaxX' - X position of peak in cross-cor. matrix.
%            'MaxY' - Y position of peak in cross-cor. matrix.
%            'MaxSN' - S/N (relative to the StD) of the peak.
%            'ShiftX' - X-axis shift of first image relative to the
%                   reference image. 
%            'ShiftY' - Y-axis shift of first image relative to the
%                   reference image. 
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [XC,Res]=imUtil.filter.xcorr2_fft(imUtil.kernel2.gauss,imUtil.kernel2.gauss);
%          [XC,Res,ResPeak]=imUtil.filter.xcorr2_fft(imUtil.kernel2.gauss(2,[41 30],[20 15])+randn(30,41).*0.001,imUtil.kernel2.gauss(2,[41 30],[22 10]),'NormMethod','max');
%          [XC,Res,ResPeak]=imUtil.filter.xcorr2_fft(randn(30,31));
% Reliable: 2
%--------------------------------------------------------------------------


arguments
    Mat1
    Mat2                       = [];
    Args.NormMethod            = 'StD';
    Args.MaxMethod             = 'thresh_fracmax';
    Args.FracOfMax             = 0.5;
    Args.Threshold             = 5;
    Args.Conn                  = 8;
    Args.SubBack(1,1) logical  = false;
    Args.BackFun               = @nanmedian;
    Args.BackFunPar cell       = {'all'};
    Args.VarFun                = @imUtil.background.rvar;
    Args.VarFunPar cell        = {};
    Args.SubSizeXY             = [128 128];
    Args.OverlapXY             = [16 16];
    Args.MinVariance           = 0;
end

% 
% if nargin<2
%     Mat2 = [];
% end
% 
% InPar = inputParser;
% 
% addOptional(InPar,'NormMethod','StD');
% 
% addOptional(InPar,'MaxMethod','thresh_fracmax');
% addOptional(InPar,'FracOfMax',0.5);
% addOptional(InPar,'Threshold',5);
% addOptional(InPar,'Conn',8);
% 
% addOptional(InPar,'SubBack',false); %
% addOptional(InPar,'BackFun',@nanmedian); % @median);
% addOptional(InPar,'BackFunPar',{'all'});      % {[1 2],'omitnan'});
% addOptional(InPar,'VarFun',@imUtil.background.rvar);    % if empty, then will try to read var from second output of BackFun...
% addOptional(InPar,'VarFunPar',{}); % {[1 2]});
% addOptional(InPar,'SubSizeXY',[128 128]);  % or 'full'
% addOptional(InPar,'OverlapXY',[16 16]); 
% addOptional(InPar,'MinVariance',0);
% 
% parse(InPar,varargin{:});
% InPar = InPar.Results;


if Args.SubBack
    % subtrct backfround - Mat1
    [Back1,Var1]=imUtil.background.background(Mat1,...
                                               'BackFun',Args.BackFun,...
                                               'BackFunPar',Args.BackFunPar,...
                                               'VarFun',Args.VarFun,...
                                               'VarFunPar',Args.VarFunPar,...
                                               'SubSizeXY',Args.SubSizeXY,...
                                               'OverlapXY',Args.OverlapXY);

    % normalize the H2 histogram surface by expected region of pverlap.
    Var1 = max(Var1,Args.MinVariance);
    SN1 = (Mat1 - Back1)./sqrt(Var1);
    
    if ~isempty(Mat2)
        % subtrct backfround - Mat2
        [Back2,Var2]=imUtil.background.background(Mat2,...
                                                   'BackFun',Args.BackFun,...
                                                   'BackFunPar',Args.BackFunPar,...
                                                   'VarFun',Args.VarFun,...
                                                   'VarFunPar',Args.VarFunPar,...
                                                   'SubSizeXY',Args.SubSizeXY,...
                                                   'OverlapXY',Args.OverlapXY);

        % normalize the H2 histogram surface by expected region of pverlap.
        Var2 = max(Var2,Args.MinVariance);
        SN2  = (Mat2 - Back2)./sqrt(Var2);
    else
        SN2 = SN1;
    end
else
    SN1 = Mat1;
    if isempty(Mat2)
        SN2 = SN1;
    else
        SN2 = Mat2;
    end
end
    

SizeM = size(SN1);
XC = imUtil.filter.filter2_fft(SN1,SN2);

% normalize to correlation units -1..+1 range...
switch lower(Args.NormMethod)
    case 'std'
        % standard normalization
        XC = XC./numel(XC)./sqrt(std(SN1,[],'all').*std(SN2,[],'all'));
    case 'max'
        % normalize such the max is 1
        XC = XC./max(XC(:));
    otherwise
        error('Unknown NormMethod option');
end


if nargout>1
    SizeM = SizeM - 0.5;
    Res.VecX = (-floor(SizeM(2).*0.5):1:ceil(SizeM(2).*0.5));
    Res.VecY = (-floor(SizeM(1).*0.5):1:ceil(SizeM(1).*0.5));
    Res.X0   = find(Res.VecX==0);
    Res.Y0   = find(Res.VecY==0); 
    
    
    % locate local maxima
    if nargout>2
        ResPeak.StD  = 1./sqrt(numel(XC)-3);
        switch lower(Args.MaxMethod)
            case 'max1'
                [ResPeak.MaxVal,MaxInd] = max(XC(:)); %,[],'all');
                [MaxY,MaxX] = imUtil.image.ind2sub_fast(size(XC),MaxInd);
                
            otherwise
                switch lower(Args.MaxMethod)
                    case {'thresh','thresh_fracmax'}
                        % normalize by StD such that selection will be by
                        % S/N
                        Norm = ResPeak.StD;
                    otherwise
                        Norm = 1;
                end
                [~,Pos]=imUtil.image.local_maxima(XC,1,Args.Threshold.*Norm,Args.Conn);
                Pos = sortrows(Pos,3,'descend');
                
                Flag = imUtil.patternMatch.select_maxima(Pos(:,3)./Norm,'MaxMethod',Args.MaxMethod,'FracOfMax',Args.FracOfMax,'Threshold',Args.Threshold);
                MaxX   = Pos(Flag,1);
                MaxY   = Pos(Flag,2);
                ResPeak.MaxVal = Pos(Flag,3);
        end
        ResPeak.MaxX   = MaxX;
        ResPeak.MaxY   = MaxY;
        ResPeak.MaxSN  = ResPeak.MaxVal./ResPeak.StD;
        ResPeak.ShiftX = ResPeak.MaxX - Res.X0;
        ResPeak.ShiftY = ResPeak.MaxY - Res.Y0;
        
                        
    end
end
