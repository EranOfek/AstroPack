function Res=pixel_flat_response(Cube,varargin)
% Fit the pixel response to light as a function of intensity in a cube of images
% Package: +imUtil.calib
% Description: Given a cube of flat field images which have different mean
%              intensity, fit some linear models to each model intensity
%              vs. a user specified expected intensity or the mean value of
%              the image.
%              This function can be used to fit the flat image in each
%              pixel, to fit non-linaeity and to look for pixels with
%              anomalous response.
%              The function by default fit a null hypothesis model of the
%              form intensity=alpha*MeanIntensity (where alpha is a fitted
%              free parameter).
%              In addition the function fit additional user specified
%              models (e.g., offset + linear term, or quadratic model).
%              The Delta Chi^2 between these models and the null hypothesis
%              is calculated.
% Input  : - A cube of images in which the image index is in the 3rd
%            dimension.
%            If not provided then will run in simulation mode.
%          * Pairs of ...,key,val,... arguments. Options are:
%            'Gain' - Gain. The cube will be multiplied by these factor.
%                   This is needed beacuse the function assume the images
%                   noise is Poisson. Default is 1.
%            'ReadNoise' - Readnoise in electrons, used for the \chi^2
%                   calculation. Default is 5.
%            'MeanFun' - If 'Intensity' is not porovided, this is a function
%                   handle that will operate on each image in the cube in
%                   order to calculate its mean value.
%                   Default is @median.
%            'MeanFunPar' - A cell array of additional parameters to pass
%                   to the 'MeanFun' function handle.
%                   Default is {[1 2],'omitnan'}.
%            'Intensity' - A vector if intensities for each image in the
%                   cube. This can be the mean intensity of each image
%                   (after gain correction), or exposure time (if flat is
%                   based on constant illumination surface).
%                   If empty, then will estimate the intensity using the
%                   'MeanFun' option.
%                   Default is empty.
%            'Model' - A cell array of additional models to fit.
%                   Each element in the cell array is a string that
%                   corresponds to one of the following models:
%                   'c+x' : - bias + alpha*Intensity model
%                   'c+x+X^2' - bias + alpha*I + beta.*I.^2 model
%                   'x+x^2' - alpha*I + beta.*I.^2 model
%                   Default is {'c+x','c+x+x^2','x+x^2'}.
% Output : - A structure of the fit results. The following fields are
%            available:
%            .H0 (data for the null hypothesis fit.
%               A structure with the following fields:
%               .Model - Model name - i.e., 'x'
%               .Par.Par - A matrix of the fitted parameters (response
%                       image  - i.e., flat field image).
%               .Chi2 - A matrix of \chi^2 per pixel.
%               .Npar - The number of free parameters.
%               .Ndof - The number of degrees of freedom (Nobs-Npar).
%               .ProbChi2 - 1 - The cumulative probability of the chi2,dof.
%            .H1 (data for the alternative hypothsis fits).
%               This is a structure array with element per alternative
%               model:
%               .Model - Model name - e.g., 'c+x'
%               .Par(i).Par - A matrix of the fitted parameters (response
%                       image  - i.e., flat field image).
%                       where i is the free parameter index.
%                       For example, in the 'c+x' model i=1 is for the bias
%                       level and i=2 is for the slope (response).
%               .Chi2 - A matrix of \chi^2 per pixel.
%               .Npar - The number of free parameters.
%               .Ndof - The number of degrees of freedom (Nobs-Npar).
%               .ProbDeltaChi2 - 1 - The cumulative probability of the
%                       Delta \chi^2 between H1 and H0 where Npar-1 is the
%                       number of degrees of freedom.
%                       small or 0 where the model is prefered over H0.
%      By: Eran O. Ofek                         Apr 2020
% Example: A=[1 2;3 4;5 6]; Cube(:,:,1)=A; Cube(:,:,2)=A.*2; Cube(:,:,3)=A.*5;
%          Cube(:,:,4)=A.*7; Cube(:,:,5)=A.*10; Cube(:,:,6)=A.*30; Cube=Cube.*1000;
%          CubeP = poissrnd(Cube);
%          Res=imUtil.calib.pixel_flat_response(CubeP)

if nargin==0
    % simulation mode
    A=[1 2;3 4;5 6]; Cube(:,:,1)=A; Cube(:,:,2)=A.*2; Cube(:,:,3)=A.*5;
    Cube(:,:,4)=A.*7; Cube(:,:,5)=A.*10; Cube(:,:,6)=A.*30;
    Cube=Cube.*1000;
    CubeP = poissrnd(Cube);
    % making one of the pixels non-linear
    CubeP(1,1,5)=3000;
    CubeP(1,1,6)=3500;
    
    Cube = CubeP;
end

InPar = inputParser;
addOptional(InPar,'Gain',1);
addOptional(InPar,'ReadNoise',5);
addOptional(InPar,'MeanFun',@median);
addOptional(InPar,'MeanFunPar',{[1 2],'omitnan'});
addOptional(InPar,'Intensity',[]);
addOptional(InPar,'Model',{'c+x','c+x+x^2','x+x^2'});

parse(InPar,varargin{:});
InPar = InPar.Results;

% multiply by Gain
Cube = Cube.*InPar.Gain;

if isempty(InPar.Intensity)
    % estimate the intensity expectation from the images mean
    H = squeeze(InPar.MeanFun(Cube,InPar.MeanFunPar{:}));  
else
    InPar.Intensity = InPar.Intensity(:);
    H = InPar.Intensity;
end




SizeCube = size(Cube);

Y = reshape(Cube,SizeCube(1).*SizeCube(2),SizeCube(3)).';

% null hypothesis model - linear relation without offset
% 'x'
H0 = [H];
Par0 = H0\Y;
%Chi2_0 = sum((Y - H0*Par0).^2./Y);
Chi2_0 = sum((Y - H0*Par0).^2./(H0*Par0 + InPar.ReadNoise.^2) );
Res.H0.Model            = 'x';
Res.H0.Par(1).Par  = reshape(Par0,SizeCube(1),SizeCube(2));
Res.H0.Chi2        = reshape(Chi2_0,SizeCube(1),SizeCube(2));
Res.H0.Npar        = 1;
Res.H0.Ndof        = SizeCube(3) - Res.H0.Npar;
Res.H0.ProbChi2    = 1-chi2cdf(Res.H0.Chi2,Res.H0.Ndof);

Nmodel = numel(InPar.Model);
for Imodel=1:1:Nmodel
    switch lower(InPar.Model{Imodel})
        case 'c+x'
            % a linear model with offset
            % intensity = constent + alpha*ExpTime
            Res.H1(Imodel).Model = InPar.Model{Imodel};
            
            H1 = [ones(SizeCube(3),1), H];

        case 'c+x+x^2'
            % intensity = constent + alpha*ExpTime + beta*ExpTime^2
            Res.H1(Imodel).Model = InPar.Model{Imodel};
            
            H1 = [ones(SizeCube(3),1), H, H.^2];

        case 'x+x^2'
            % intensity = alpha*ExpTime + beta*ExpTime^2
            Res.H1(Imodel).Model = InPar.Model{Imodel};
            
            H1 = [H, H.^2];

            
        otherwise
            error('Unknown Model option');
    end
       
    % the model fitting
    
    Par1 = H1\Y;
    %Chi2_1 = sum((Y - H1*Par1).^2./Y);
    Chi2_1 = sum((Y - H1*Par1).^2./(H1*Par1 + InPar.ReadNoise.^2) );
    %DeltaChi2_1v0 = Chi2_1 - Chi2_0;
    %chi2cdf(DeltaChi2_1v0,1);

    Npar = size(H1,2);
    for Ipar=1:1:Npar
        Res.H1(Imodel).Par(Ipar).Par = reshape(Par1(Ipar,:),SizeCube(1),SizeCube(2));
    end
    Res.H1(Imodel).Chi2      = reshape(Chi2_1,SizeCube(1),SizeCube(2));
    Res.H1(Imodel).DeltaChi2 = Res.H0.Chi2 - Res.H1(Imodel).Chi2;
    Res.H1(Imodel).Npar      = Npar;
    Res.H1(Imodel).Ndof      = SizeCube(3) - Npar;
    Res.H1(Imodel).ProbDeltaChi2 = 1-chi2cdf(Res.H1(Imodel).DeltaChi2, Res.H1(Imodel).Npar-Res.H0.Npar);

end

