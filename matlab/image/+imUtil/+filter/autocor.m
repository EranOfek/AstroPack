function [AC,Res]=autocor(Mat,varargin)
% Autocorrelation of a 2D image 
% Package: imUtil.filter
% Description: Autocorrelation of a 2D image, with optional background
%              subtraction and StD normalization.
% Input  : - A matrix.
%          * Pairs of ...,key,val,... Possible keywords include:
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

InPar = inputParser;

addOptional(InPar,'SubBack',true); %
addOptional(InPar,'BackFun',@nanmedian); % @median);
addOptional(InPar,'BackFunPar',{'all'});      % {[1 2],'omitnan'});
addOptional(InPar,'VarFun',@imUtil.background.rvar);    % if empty, then will try to read var from second output of BackFun...
addOptional(InPar,'VarFunPar',{}); % {[1 2]});
addOptional(InPar,'SubSizeXY',[128 128]);  % or 'full'
addOptional(InPar,'OverlapXY',[16 16]); 
addOptional(InPar,'MinVariance',0);

parse(InPar,varargin{:});
InPar = InPar.Results;


if InPar.SubBack
    % subtrct backfround
    [Back,Var]=imUtil.background.background(Mat,...
                                               'BackFun',InPar.BackFun,...
                                               'BackFunPar',InPar.BackFunPar,...
                                               'VarFun',InPar.VarFun,...
                                               'VarFunPar',InPar.VarFunPar,...
                                               'SubSizeXY',InPar.SubSizeXY,...
                                               'Overlap',InPar.OverlapXY);

    % normalize the H2 histogram surface by expected region of pverlap.
    Var = max(Var,InPar.MinVariance);
    SN = (Mat - Back)./sqrt(Var);
else
    SN = Mat;
end
    
SizeM = size(SN);
AC = imUtil.filter.filter2_fft(SN,SN);

if nargout>1
    SizeM = SizeM - 0.5;
    Res.VecX = (-floor(SizeM(2).*0.5):1:ceil(SizeM(2).*0.5));
    Res.VecY = (-floor(SizeM(1).*0.5):1:ceil(SizeM(1).*0.5));
    Res.X0   = find(Res.VecX==0);
    Res.Y0   = find(Res.VecY==0); 
end
