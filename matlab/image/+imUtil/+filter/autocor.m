function [AC,Res]=autocor(Mat,Args)
% Autocorrelation of a 2D image 
% Package: imUtil.filter
% Description: Autocorrelation of a 2D image, with optional background
%              subtraction and StD normalization.
% Input  : - A matrix.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'Norm' - A logical indicating if to normalize the ACF such
%                   that output will be correlation. Default is true.
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
%                     Default is 1.
% Output : - The Aitocorrekation function
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [AC,Res]=imUtil.filter.autocor(imUtil.kernel2.gauss);
%          [AC,Res]=imUtil.filter.autocor(randn(30,31));
% Reliable: 2
%--------------------------------------------------------------------------


arguments
    Mat
    Args.Norm logical                      = true;
    Args.SubBack(1,1) logical              = true;
    Args.BackFun                           = @median;
    Args.BackFunPar                        = {'all','omitnan'};
    Args.VarFun                            = @imUtil.background.rvar;
    Args.VarFunPar                         = {};
    Args.SubSizeXY                         = [128 128];
    Args.OverlapXY                         = [16 16];
    Args.MinVariance                       = 0;
end



if Args.SubBack
    % subtrct backfround
    [Back,Var]=imUtil.background.background(Mat,...
                                               'BackFun',Args.BackFun,...
                                               'BackFunPar',Args.BackFunPar,...
                                               'VarFun',Args.VarFun,...
                                               'VarFunPar',Args.VarFunPar,...
                                               'SubSizeXY',Args.SubSizeXY,...
                                               'Overlap',Args.OverlapXY);

    % normalize the H2 histogram surface by expected region of pverlap.
    Var = max(Var,Args.MinVariance);
    SN = (Mat - Back)./sqrt(Var);
else
    SN = Mat;
end
    
SizeM = size(SN);

AC = imUtil.filter.filter2_fft(SN,SN);

if Args.Norm
    Npix    = numel(SN);
    AC = AC./(var(SN,[],'all').*Npix);
end

if nargout>1
    SizeM = SizeM - 0.5;
    Res.VecX = (-floor(SizeM(2).*0.5):1:ceil(SizeM(2).*0.5));
    Res.VecY = (-floor(SizeM(1).*0.5):1:ceil(SizeM(1).*0.5));
    Res.X0   = find(Res.VecX==0);
    Res.Y0   = find(Res.VecY==0); 
end
