function [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = stackCube(Cube, Args)
% Stack (coadd) a cube of images using various functions
% Input  : - A cube of images in which the image index in the 3rd dim.
%          - * ...,key,val,...
%              'StackMethod' - Stacking method. Options are:
%                   'sum'
%                   ['mean']
%                   'median'
%                   'var'
%                   'min'
%                   'max'
%                   'range'
%                   'quantile' - rquires a quqntile argument.
%                   'wmean' 
%                   'sigmaclip' - for arguments see: imUtil.image.mean_sigclip
%                   'wsigmaclip' - for arguments see: imUtil.image.wmean_sigclip
%              'MethodArgs' - A cell array of arguments to pass to the
%                   method function. Default is {}.
%              'VarCube' - A cube of variances. Default is [].
%              'MedianVarCorrForEmpirical' - A logical indicating if to
%                   correct the variance calculation by the ratio between
%                   the variance of the median and variance of the mean.
%                   Default is false.
%              'DivideEmpiricalByN' - A logical indicating if to divide
%                   CoaddVarEmpirical by N. Default is false.
%              'DivideVarByN' - A logical indicating if to divide
%                   CoaddVar by N. Default is false.
%              'CalcCoaddVarEmpirical' - Logical indicating if to calc the
%                   CoaddVarEmpirical. Default is true.
%              'CalcCoaddVar' - Logical indicating if to calc the
%                   CoaddVar. Default is true.
%              'CalcCoaddN' - Logical indicating if to calc the
%                   CoaddN. Default is true.
% Output : - Coadd image.
%          - CoaddVarEmpirical - This is the empirical variance of the
%               data. In case the variance of the mean is needed set DivideEmpiricalByN
%               to true.
%          - CoaddVar - This is the variance of the
%               data. In case the variance of the mean is needed set DivideByN
%               to true.
%          - CoaddN - This is the number of images used in the stacking of
%               each pixel.
% Author : Eran Ofek (Apr 2021)
% Example: Cube = 2.*randn(5,5,1000); Cube(1,1,1)=1000;
%          [Coadd] = imUtil.image.stackCube(Cube);
%          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube)
%          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4)
%          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','sum')
%          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','quantile','MethodArgs',{0.1})
%          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','sigmaclip');
%          [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','wsigmaclip');


arguments
    Cube
    Args.StackMethod                            = 'mean';
    Args.MethodArgs cell                        = {};
    Args.VarCube                                = [];
    Args.MedianVarCorrForEmpirical(1,1) logical = false;
    Args.DivideEmpiricalByN(1,1) logical        = false;
    Args.DivideVarByN(1,1) logical              = false;
    Args.CalcCoaddVarEmpirical(1,1) logical     = true;
    Args.CalcCoaddVar(1,1) logical              = true;
    Args.CalcCoaddN(1,1) logical                = true;
end

if nargout<4
    Args.CalacCoaddN = false;
    if nargout<3
        Args.CalcCoaddVar = false;
        if nargout<2
            Args.CalcCoaddVarEmpirical = false;
        end
    end
end    

CoaddVarEmpirical = [];
CoaddVar          = [];
CoaddN            = [];

switch lower(Args.StackMethod)
    case 'sum'
        Coadd = nansum(Cube, 3);
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        if Args.CalcCoaddVar
            CoaddVar = nansum(Args.VarCube,3);
        end
        
    case 'mean'
        Coadd = nanmean(Cube, 3);
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        if Args.CalcCoaddVar && ~isempty(Args.VarCube)
            CoaddVar = nansum(Args.VarCube,3);  %./(CoaddN.^2);
        end
        
    case 'median'
        Coadd = nanmedian(Cube, 3);
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        if Args.CalcCoaddVar && ~isempty(Args.VarCube)
            % The following correction factor is the ratio betwen the
            % variance of the median and the variance of the
            % mean  (Kenney and Keeping 1962, p. 211).
            CorrFactor = pi.*(2.*CoaddN+1)./(4.*CoaddN);
            CoaddVar   = CorrFactor.*nansum(Args.VarCube,3); %./(CoaddN.^2);
        end
        
    case 'var'
        Coadd = nanvar(Cube, [], 3);
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        if Args.CalcCoaddVar
            CoaddVar = Coadd./(2.*CoaddN - 2);
        end
        
    case 'min'
        Coadd = min(Cube, [], 3, 'omitnan');
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        % CoaddVar is not defined
        
    case 'max'
        Coadd = max(Cube, [], 3, 'omitnan');
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        % CoaddVar is not defined
        
    case 'range'
        Coadd = range(Cube, 3);
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        % CoaddVar is not defined
        
    case 'quantile'
        Coadd = quantile(Cube, Args.MethodArgs{:}, 3);
        if Args.CalcCoaddN
            CoaddN  = sum(~isnan(Cube),3);
        end
        
        % CoaddVar is not defined
        
    case 'wmean'
        if isempty(Args.VarCube)
            error('Can not calc wmean without the variance cube');
        end
        InvVarCube = 1./Args.VarCube;
        Coadd      = nansum(Cube.*InvVarCube, 3) ./ nansum(InvVarCube, 3);
        CoaddN     = sum(~isnan(Cube),3);
        CoaddVar   = Args.VarCube; %InvVarCube; %.*CoaddN;
        
    case 'sigmaclip'
         [Coadd, CoaddVarEmpirical, ~, CoaddN] = imUtil.image.mean_sigclip(Cube, 3, Args.MethodArgs{:});
         % e.g., {'MeanFun',@nanamean, 'StdFun','rstd','Nsigma',[5
         % 5],'MaxIter',3}
         
         if Args.CalcCoaddVar
            CoaddVar = nansum(Args.VarCube,3); %./(CoaddN.^2);
         end
        
    case 'wsigmaclip'
        [Coadd, ~, ~, CoaddN] = imUtil.image.wmean_sigclip(Cube,Args.VarCube, 3, Args.MethodArgs{:});
         % e.g., {'MeanFun',@nanamean, 'StdFun','rstd','Nsigma',[5
         % 5],'MaxIter',3}
         
         if Args.CalcCoaddVar
            CoaddVar = nansum(Args.VarCube,3); %./(CoaddN.^2);
         end
         
    otherwise
        error('Unknown StackMethod option');
end

if Args.CalcCoaddVarEmpirical && isempty(CoaddVarEmpirical)
    % calc empirical variance
    CoaddVarEmpirical = var(Cube, [], 3, 'omitnan');
    
    if Args.MedianVarCorrForEmpirical
        CorrFactor = pi.*(2.*CoaddN+1)./(4.*CoaddN);
        CoaddVarEmpirical = CoaddVarEmpirical.*CorrFactor;
    end
    
end

if Args.DivideEmpiricalByN
    CoaddVarEmpirical = CoaddVarEmpirical./CoaddN;
end


if Args.DivideVarByN
    CoaddVar = CoaddVar./CoaddN;
end


